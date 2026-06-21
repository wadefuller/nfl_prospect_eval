# 02i_build_te_features.R
# ─────────────────────────────────────────────────────────────────────────────
# Builds the TE feature dataframe (parallel to wr_features_raw / rb_features_raw)
# from the cached receiving season-stats table built by 02_build_features.R.
#
# TE features are essentially a subset of the WR receiving stack:
#   rec, rec_yards, rec_td, ypr, rec_td_rate, per-game rates, dominator,
#   penult/ante season + YoY trend, tier, recruiting.
#
# Block grades aren't available from cfbfastR; PBP-derived target/EPA features
# go through the same cache as WRs and are joined downstream.
#
# Output: data/te_features_raw.rds
# ─────────────────────────────────────────────────────────────────────────────

setwd("~/Projects/R/college_nfl_model")
suppressMessages({
  library(tidyverse)
  library(nflreadr)
})
source("functions/helpers.R")

targets   <- readRDS("data/targets.rds") |> filter(position == "TE")

deploy_picks <- tryCatch(
  load_draft_picks() |>
    filter(position == "TE", season %in% 2024:2026) |>
    transmute(pfr_player_name, gsis_id,
              position = "TE", college,
              draft_year = season,
              round = as.integer(round), pick = as.integer(pick),
              age = as.numeric(age)),
  error = function(e) tibble())

targets <- bind_rows(targets, deploy_picks) |>
  distinct(pfr_player_name, draft_year, position, .keep_all = TRUE)
cat(sprintf("TE targets (incl. deploy 2024-2026): %d\n", nrow(targets)))
recv_raw  <- readRDS("data/cfb_receiving_raw.rds") |> as_tibble()

recruit_lookup <- if (file.exists("data/cfb_recruiting_raw.rds")) {
  readRDS("data/cfb_recruiting_raw.rds") |>
    filter(recruit_type == "HighSchool") |>
    mutate(name_clean = strip_suffix(clean_name(name))) |>
    filter(!is.na(rating)) |>
    group_by(name_clean) |>
    slice_max(rating, n = 1, with_ties = FALSE) |>
    ungroup() |>
    rename(recruit_stars = stars, recruit_rating = rating, recruit_rank = ranking) |>
    select(name_clean, recruit_stars, recruit_rating, recruit_rank, recruit_year)
} else NULL

team_games <- if (file.exists("data/cfb_team_games_raw.rds")) {
  raw <- readRDS("data/cfb_team_games_raw.rds")
  if ("team_games" %in% names(raw)) {
    raw |> group_by(cfb_season, team) |> summarise(team_games = sum(team_games), .groups = "drop")
  } else {
    raw |> group_by(cfb_season, team) |> summarise(team_games = n(), .groups = "drop")
  }
} else tibble(team = character(), cfb_season = integer(), team_games = integer())

team_recv_vol <- recv_raw |>
  group_by(cfb_season, team) |>
  summarise(team_rec_yards = sum(receiving_yds, na.rm = TRUE), .groups = "drop")

cat(sprintf("TE targets: %d, receiving rows: %d\n", nrow(targets), nrow(recv_raw)))

clean_recv <- recv_raw |>
  filter(!str_detect(player, "^#")) |>
  transmute(
    name_clean      = canonical_cfb_name(clean_name(player)),
    school          = team,
    conference,
    cfb_season      = as.integer(cfb_season),
    rec             = coalesce(receiving_rec, 0),
    rec_yards       = coalesce(receiving_yds, 0),
    rec_td          = coalesce(receiving_td, 0),
    ypr             = receiving_ypr
  ) |>
  mutate(name_clean = strip_suffix(name_clean)) |>
  group_by(name_clean, school, cfb_season) |>
  summarise(across(c(rec, rec_yards, rec_td, ypr), ~ max(.x, na.rm = TRUE)),
            conference = first(conference),
            .groups = "drop") |>
  mutate(across(c(rec, rec_yards, rec_td, ypr),
                ~ if_else(is.infinite(.x), NA_real_, .x)))

draft <- targets |>
  select(gsis_id, pfr_player_name, position, college, draft_year, round, pick, age) |>
  mutate(
    name_clean        = clean_name(strip_suffix(pfr_player_name)),
    final_cfb_season  = draft_year - 1L,
    penult_cfb_season = draft_year - 2L,
    ante_cfb_season   = draft_year - 3L
  )

best_season <- clean_recv |>
  inner_join(draft |> select(name_clean, draft_year, college),
             by = "name_clean", relationship = "many-to-many") |>
  filter(cfb_season %in% c(draft_year - 1L, draft_year - 2L)) |>
  group_by(name_clean, draft_year, college) |>
  slice_max(rec_yards, n = 1, with_ties = FALSE) |>
  ungroup() |>
  rename(best_school_final = school, conference_final = conference,
         rec_final = rec, rec_yards_final = rec_yards,
         rec_td_final = rec_td, ypr_final = ypr,
         best_cfb_season = cfb_season) |>
  mutate(best_season_is_final = as.integer(best_cfb_season == draft_year - 1L))

penult_stats <- clean_recv |>
  inner_join(draft |> select(name_clean, draft_year),
             by = "name_clean", relationship = "many-to-many") |>
  filter(cfb_season == draft_year - 2L) |>
  group_by(name_clean, draft_year) |>
  slice_max(rec_yards, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(name_clean, draft_year,
            rec_penult = rec, rec_yards_penult = rec_yards,
            rec_td_penult = rec_td)

ante_stats <- clean_recv |>
  inner_join(draft |> select(name_clean, draft_year),
             by = "name_clean", relationship = "many-to-many") |>
  filter(cfb_season == draft_year - 3L) |>
  group_by(name_clean, draft_year) |>
  slice_max(rec_yards, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(name_clean, draft_year, rec_yards_ante = rec_yards)

te_team_games <- best_season |>
  left_join(team_games, by = c("best_school_final" = "team",
                                "best_cfb_season" = "cfb_season")) |>
  transmute(name_clean, draft_year, team_games)

te_team_vol <- best_season |>
  left_join(team_recv_vol, by = c("best_school_final" = "team",
                                   "best_cfb_season" = "cfb_season")) |>
  transmute(name_clean, draft_year, team_rec_yards)

# Reuse the WR PBP cache — it aggregates ALL receivers including TEs.
# Same feature set works: catch_rate, target_share, EPA per target, etc.
wr_pbp <- if (file.exists("data/cfb_wr_pbp_features.rds")) {
  readRDS("data/cfb_wr_pbp_features.rds") |>
    transmute(
      pbp_name_clean = strip_suffix(clean_name(player)),
      pbp_school     = pos_team,
      cfb_season     = as.integer(season),
      catch_rate_te        = catch_rate_wr,
      yards_per_target_te  = yards_per_target_wr,
      yards_per_rec_te     = yards_per_rec_wr,
      explosive_rec_rate_te = explosive_rec_rate,
      target_share_te      = target_share_wr,
      targets_per_game_te  = targets_per_game_wr,
      epa_per_target_te    = epa_per_target,
      epa_per_play_te_pbp  = epa_per_play_wr_pbp
    ) |>
    group_by(pbp_name_clean, pbp_school, cfb_season) |>
    slice_max(target_share_te, n = 1, with_ties = FALSE) |>
    ungroup()
} else NULL

te_pbp_best <- if (!is.null(wr_pbp)) {
  best_season |>
    mutate(.nc_strip = strip_suffix(name_clean)) |>
    left_join(wr_pbp,
              by = c(".nc_strip" = "pbp_name_clean",
                     "best_school_final" = "pbp_school",
                     "best_cfb_season" = "cfb_season")) |>
    transmute(name_clean, draft_year,
              catch_rate_te, yards_per_target_te, yards_per_rec_te,
              explosive_rec_rate_te, target_share_te, targets_per_game_te,
              epa_per_target_te, epa_per_play_te_pbp)
} else NULL

te_features_raw <- draft |>
  left_join(best_season  |> select(-college), by = c("name_clean", "draft_year")) |>
  left_join(penult_stats, by = c("name_clean", "draft_year")) |>
  left_join(ante_stats,   by = c("name_clean", "draft_year")) |>
  left_join(te_team_games, by = c("name_clean", "draft_year")) |>
  left_join(te_team_vol,   by = c("name_clean", "draft_year"))

if (!is.null(te_pbp_best)) {
  te_features_raw <- te_features_raw |>
    left_join(te_pbp_best, by = c("name_clean", "draft_year"))
}

te_features_raw <- te_features_raw |>
  mutate(
    has_penult         = as.integer(!is.na(rec_yards_penult)),
    has_cfb_data       = !is.na(rec_yards_final),
    rec_yds_yoy        = rec_yards_final - coalesce(rec_yards_penult, 0),
    rec_td_rate        = rec_td_final / pmax(rec_final, 1),
    rec_yards_per_game = if_else(team_games > 0, rec_yards_final / team_games, NA_real_),
    rec_per_game       = if_else(team_games > 0, rec_final       / team_games, NA_real_),
    teammate_rec_yards = pmax(team_rec_yards - rec_yards_final, 0),
    dominator_rate     = if_else(team_rec_yards > 0,
                                  rec_yards_final / team_rec_yards, NA_real_),
    has_te_pbp         = as.integer(!is.na(if ("catch_rate_te" %in% names(te_features_raw))
                                            catch_rate_te else NA_real_)),
    school_final       = best_school_final,
    tier               = factor(classify_tier(conference_final,
                                              school = coalesce(school_final, college)),
                                 levels = c("P4", "G5", "Other"))
  )

# Recruiting (mirrors QB script).
if (!is.null(recruit_lookup)) {
  te_features_raw <- te_features_raw |>
    mutate(name_clean_base = strip_suffix(name_clean)) |>
    left_join(recruit_lookup, by = c("name_clean_base" = "name_clean")) |>
    mutate(
      college_years_raw = draft_year - recruit_year,
      .recruit_valid    = !is.na(college_years_raw) &
                           college_years_raw >= 2 & college_years_raw <= 6,
      college_years     = if_else(.recruit_valid, college_years_raw, NA_real_),
      recruit_rating    = if_else(.recruit_valid, recruit_rating, NA_real_),
      recruit_stars     = if_else(.recruit_valid, recruit_stars,  NA_integer_),
      recruit_rank      = if_else(.recruit_valid, recruit_rank,   NA_integer_),
      recruit_year      = if_else(.recruit_valid, recruit_year,   NA_integer_)
    ) |>
    select(-college_years_raw, -.recruit_valid, -name_clean_base) |>
    mutate(has_recruiting = as.integer(!is.na(recruit_rating)),
           has_recruit_year = as.integer(!is.na(recruit_year)))
}

cat(sprintf("TE feature rows: %d  (matched final-season stats: %d)\n",
            nrow(te_features_raw), sum(te_features_raw$has_cfb_data, na.rm = TRUE)))

saveRDS(te_features_raw, "data/te_features_raw.rds")
cat("Saved: data/te_features_raw.rds\n")

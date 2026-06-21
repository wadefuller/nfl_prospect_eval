# 02h_build_qb_features.R
# ─────────────────────────────────────────────────────────────────────────────
# Builds the QB feature dataframe (parallel to wr_features_raw / rb_features_raw)
# from the cached passing + rushing season-stats tables built by 02_build_features.R.
#
# QB-specific feature themes:
#   - Passing volume + efficiency (yards, TD, INT, completion %, YPA)
#   - Mobility (rush yards/TD/YPC — captures dual-threat upside)
#   - Trend signals (year-over-year passing yards)
#   - Pre-draft athleticism (combine columns are joined downstream in 03)
#
# Output: data/qb_features_raw.rds
# ─────────────────────────────────────────────────────────────────────────────

setwd("~/Projects/R/college_nfl_model")
suppressMessages({
  library(tidyverse)
  library(nflreadr)
})
source("functions/helpers.R")

# ── Inputs ─────────────────────────────────────────────────────────────────
# Training rows come from targets.rds (2002-2023). Append deploy-class rows
# (2024-2026) so model_data carries everything we'll need to score from.
targets   <- readRDS("data/targets.rds") |> filter(position == "QB")

deploy_picks <- tryCatch(
  load_draft_picks() |>
    filter(position == "QB", season %in% 2024:2026) |>
    transmute(pfr_player_name, gsis_id,
              position = "QB", college,
              draft_year = season,
              round = as.integer(round), pick = as.integer(pick),
              age = as.numeric(age)),
  error = function(e) tibble())

targets <- bind_rows(targets, deploy_picks) |>
  distinct(pfr_player_name, draft_year, position, .keep_all = TRUE)
cat(sprintf("QB targets (incl. deploy 2024-2026): %d\n", nrow(targets)))
pass_raw  <- readRDS("data/cfb_passing_raw.rds") |> as_tibble()
rush_raw  <- readRDS("data/cfb_rushing_raw.rds") |> as_tibble()
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

cat(sprintf("QB targets: %d, passing rows: %d\n", nrow(targets), nrow(pass_raw)))

# ── Helpers ────────────────────────────────────────────────────────────────
# canonical_cfb_name handles nickname variants for known players (Mike vs Michael).
# strip_suffix lets us match "Caleb Williams Jr." with "Caleb Williams".

clean_pass <- pass_raw |>
  filter(!str_detect(player, "^#"),
         # Drop rows where the position field clearly says non-QB (cfbfastR
         # sometimes lists WRs with a trick-play passing line)
         is.na(position) | position == "" | position == "QB") |>
  transmute(
    name_clean      = canonical_cfb_name(clean_name(player)),
    school          = team,
    conference,
    cfb_season      = as.integer(cfb_season),
    pass_att        = coalesce(passing_att, 0),
    pass_comp       = coalesce(passing_completions, 0),
    pass_yds        = coalesce(passing_yds, 0),
    pass_td         = coalesce(passing_td, 0),
    pass_int        = coalesce(passing_int, 0),
    pass_pct        = passing_pct,
    pass_ypa        = passing_ypa
  ) |>
  # Suffix-strip + de-dupe within (name, school, season).
  mutate(name_clean = strip_suffix(name_clean)) |>
  group_by(name_clean, school, cfb_season) |>
  summarise(across(c(pass_att, pass_comp, pass_yds, pass_td, pass_int,
                      pass_pct, pass_ypa),
                    ~ max(.x, na.rm = TRUE)),
            conference = first(conference),
            .groups = "drop") |>
  # Some max-of-empty produce -Inf; clamp to NA.
  mutate(across(everything(),
                ~ if (is.numeric(.x)) if_else(is.infinite(.x), NA_real_, .x) else .x))

# Rushing component — QBs scramble; mobility predicts NFL fit.
clean_rush_qb <- rush_raw |>
  filter(!str_detect(player, "^#"),
         is.na(position) | position == "" | position == "QB") |>
  transmute(
    name_clean = canonical_cfb_name(clean_name(player)),
    school     = team,
    cfb_season = as.integer(cfb_season),
    rush_car   = coalesce(rushing_car, 0),
    rush_yds   = coalesce(rushing_yds, 0),
    rush_td    = coalesce(rushing_td, 0)
  ) |>
  mutate(name_clean = strip_suffix(name_clean)) |>
  group_by(name_clean, school, cfb_season) |>
  summarise(across(c(rush_car, rush_yds, rush_td), ~ max(.x, na.rm = TRUE)),
            .groups = "drop") |>
  mutate(across(c(rush_car, rush_yds, rush_td),
                ~ if_else(is.infinite(.x), NA_real_, .x)))

# ── Build features per draftee ─────────────────────────────────────────────
# Strategy:
#   For each QB prospect, identify their final / penult / ante college season
#   using draft_year - {1, 2, 3} and a school. Best-season is the year with
#   the most pass_yds among (final, penult).

draft <- targets |>
  select(gsis_id, pfr_player_name, position, college, draft_year, round, pick, age) |>
  mutate(
    name_clean        = clean_name(strip_suffix(pfr_player_name)),
    final_cfb_season  = draft_year - 1L,
    penult_cfb_season = draft_year - 2L,
    ante_cfb_season   = draft_year - 3L
  )

# Pick best of (final, penult) for the headline "final-season" stats.
recv_best <- bind_rows(
  clean_pass |> filter(cfb_season >= 2002) |>
    mutate(is_final = cfb_season >= 2002)
) |>
  rename(best_school = school)

# For each prospect, fetch their final / penult / ante rows and the best
# of (final, penult) as the headline.

best_season <- clean_pass |>
  inner_join(draft |> select(name_clean, draft_year, college),
             by = "name_clean",
             relationship = "many-to-many") |>
  filter(cfb_season %in% c(draft_year - 1L, draft_year - 2L)) |>
  group_by(name_clean, draft_year, college) |>
  slice_max(pass_yds, n = 1, with_ties = FALSE) |>
  ungroup() |>
  rename(best_school_final = school, conference_final = conference,
         pass_att_final = pass_att, pass_comp_final = pass_comp,
         pass_yds_final = pass_yds, pass_td_final = pass_td,
         pass_int_final = pass_int, pass_pct_final = pass_pct,
         pass_ypa_final = pass_ypa,
         best_cfb_season = cfb_season) |>
  mutate(best_season_is_final = as.integer(best_cfb_season == draft_year - 1L))

# Penult stats — strict draft_year - 2.
penult_stats <- clean_pass |>
  inner_join(draft |> select(name_clean, draft_year),
             by = "name_clean",
             relationship = "many-to-many") |>
  filter(cfb_season == draft_year - 2L) |>
  group_by(name_clean, draft_year) |>
  slice_max(pass_yds, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(name_clean, draft_year,
            pass_yds_penult = pass_yds,
            pass_td_penult  = pass_td,
            pass_att_penult = pass_att)

# Ante stats — strict draft_year - 3.
ante_stats <- clean_pass |>
  inner_join(draft |> select(name_clean, draft_year),
             by = "name_clean",
             relationship = "many-to-many") |>
  filter(cfb_season == draft_year - 3L) |>
  group_by(name_clean, draft_year) |>
  slice_max(pass_yds, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(name_clean, draft_year,
            pass_yds_ante = pass_yds)

# Rushing stats joined on best_cfb_season for the mobility component.
qb_rush_best <- best_season |>
  left_join(clean_rush_qb,
            by = c("name_clean", "best_school_final" = "school",
                   "best_cfb_season" = "cfb_season")) |>
  transmute(name_clean, draft_year,
            rush_car_final = coalesce(rush_car, 0),
            rush_yds_final = coalesce(rush_yds, 0),
            rush_td_final  = coalesce(rush_td,  0))

# Team games for per-game rates (best-season).
qb_team_games <- best_season |>
  left_join(team_games, by = c("best_school_final" = "team",
                                "best_cfb_season" = "cfb_season")) |>
  transmute(name_clean, draft_year, team_games)

# Optional PBP join — adds epa_per_dropback, sack_rate, etc.
qb_pbp <- if (file.exists("data/cfb_qb_pbp_features.rds")) {
  readRDS("data/cfb_qb_pbp_features.rds") |>
    transmute(
      pbp_name_clean = strip_suffix(clean_name(player)),
      pbp_school     = pos_team,
      cfb_season     = as.integer(season),
      epa_per_dropback,
      epa_per_attempt,
      completion_pct_pbp,
      sack_rate,
      int_rate,
      negative_play_rate,
      explosive_pass_rate,
      late_down_epa,
      qb_share_team
    ) |>
    # Keep highest-dropback row per (name, school, season) — strip_suffix can
    # collide a Jr.+non-Jr. but we already filtered upstream.
    group_by(pbp_name_clean, pbp_school, cfb_season) |>
    slice_max(qb_share_team, n = 1, with_ties = FALSE) |>
    ungroup()
} else NULL

qb_pbp_best <- if (!is.null(qb_pbp)) {
  best_season |>
    mutate(.nc_strip = strip_suffix(name_clean)) |>
    left_join(qb_pbp,
              by = c(".nc_strip" = "pbp_name_clean",
                     "best_school_final" = "pbp_school",
                     "best_cfb_season" = "cfb_season")) |>
    transmute(name_clean, draft_year,
              epa_per_dropback, epa_per_attempt, completion_pct_pbp,
              sack_rate, int_rate, negative_play_rate,
              explosive_pass_rate, late_down_epa, qb_share_team)
} else NULL

# Build the full features dataframe.
qb_features_raw <- draft |>
  left_join(best_season   |> select(-college), by = c("name_clean", "draft_year")) |>
  left_join(penult_stats, by = c("name_clean", "draft_year")) |>
  left_join(ante_stats,   by = c("name_clean", "draft_year")) |>
  left_join(qb_rush_best, by = c("name_clean", "draft_year")) |>
  left_join(qb_team_games, by = c("name_clean", "draft_year"))

if (!is.null(qb_pbp_best)) {
  qb_features_raw <- qb_features_raw |>
    left_join(qb_pbp_best, by = c("name_clean", "draft_year"))
}

qb_features_raw <- qb_features_raw |>
  mutate(
    has_penult         = as.integer(!is.na(pass_yds_penult)),
    has_cfb_data       = !is.na(pass_yds_final),
    pass_yds_yoy       = pass_yds_final - coalesce(pass_yds_penult, 0),
    pass_td_int_ratio  = pass_td_final / pmax(pass_int_final, 1),
    pass_yds_per_game  = if_else(team_games > 0, pass_yds_final / team_games, NA_real_),
    pass_td_per_game   = if_else(team_games > 0, pass_td_final  / team_games, NA_real_),
    # Mobility composite
    rush_yds_per_carry = if_else(rush_car_final > 0,
                                  rush_yds_final / rush_car_final, NA_real_),
    has_mobility       = as.integer(rush_car_final >= 50),
    # PBP coverage flag (true if any PBP-derived field landed)
    has_qb_pbp         = as.integer(!is.na(if ("epa_per_dropback" %in% names(qb_features_raw))
                                            epa_per_dropback else NA_real_)),
    school_final       = best_school_final,
    tier               = factor(classify_tier(conference_final,
                                              school = coalesce(school_final, college)),
                                 levels = c("P4", "G5", "Other"))
  )

# ── Recruiting (suffix-stripped name + non-NA rating, name-only join) ────────
if (!is.null(recruit_lookup)) {
  qb_features_raw <- qb_features_raw |>
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

cat(sprintf("QB feature rows: %d  (matched final-season stats: %d)\n",
            nrow(qb_features_raw), sum(qb_features_raw$has_cfb_data, na.rm = TRUE)))

saveRDS(qb_features_raw, "data/qb_features_raw.rds")
cat("Saved: data/qb_features_raw.rds\n")

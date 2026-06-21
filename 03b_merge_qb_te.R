# 03b_merge_qb_te.R
# ─────────────────────────────────────────────────────────────────────────────
# Merges QB and TE NFL targets with their college features.
# Mirrors the WR/RB pipeline in 03_merge_and_clean.R but keeps the position
# pairs in their own script so the original 300-line WR/RB merge isn't
# touched. Calls the same shared `attach_*` helpers from `functions/helpers.R`.
#
# Outputs:
#   data/qb_model_data.rds
#   data/te_model_data.rds
# ─────────────────────────────────────────────────────────────────────────────

setwd("~/Projects/R/college_nfl_model")
suppressMessages({
  library(tidyverse)
  library(nflreadr)
})
source("functions/helpers.R")

targets   <- readRDS("data/targets.rds")
qb_feat   <- readRDS("data/qb_features_raw.rds")
te_feat   <- readRDS("data/te_features_raw.rds")

# Deploy-class prospects (2024+) often have no NFL gsis_id yet. Leaving them
# NA on both sides causes the gsis_id left-join to fan out every NA-gsis
# target row against every NA-gsis feature row (8-10x duplication). Synthesize
# a deterministic key from name + draft_year for any NA gsis_id so the join
# stays 1-to-1 for deploy prospects.
synth_gsis <- function(df, name_col) {
  nm <- df[[name_col]] |> stringr::str_to_lower() |>
    stringr::str_remove_all("[^a-z ]") |> stringr::str_squish()
  ifelse(is.na(df$gsis_id),
         paste0("synth-", nm, "-", df$draft_year),
         df$gsis_id)
}
targets$gsis_id <- synth_gsis(targets, "pfr_player_name")
qb_feat$gsis_id <- synth_gsis(qb_feat, "pfr_player_name")
te_feat$gsis_id <- synth_gsis(te_feat, "pfr_player_name")

# Construct the prospect roster for each position by unioning targets
# (2002-2023 with NFL outcomes) and the deploy-class rows we know exist in
# the feature files. Deploy rows get NA outcomes — fine, since training
# filters on has_cfb_data + ppg.
deploy_template <- function(feat, pos) {
  feat |>
    filter(draft_year >= 2024) |>
    transmute(
      gsis_id          = if ("gsis_id" %in% names(feat)) gsis_id else NA_character_,
      pfr_player_name, position = pos, college, draft_year,
      round = as.integer(round), pick = as.integer(pick), age,
      raw_ppg = NA_real_, weighted_ppg = NA_real_,
      shrunk_ppg = NA_real_, total_top2_gms = NA_real_,
      n_qual_seasons = NA_integer_,
      # NA — NOT 0 — so training scripts filter these out while scoring still
      # produces a row per deploy prospect.
      made_it = NA_integer_, ppg = NA_real_, avg_top2_ppg = NA_real_
    )
}
deploy_qb <- deploy_template(qb_feat, "QB")
deploy_te <- deploy_template(te_feat, "TE")
existing_keys <- targets |> transmute(key = paste(pfr_player_name, draft_year, position))
deploy_qb <- deploy_qb |> filter(!paste(pfr_player_name, draft_year, position) %in% existing_keys$key)
deploy_te <- deploy_te |> filter(!paste(pfr_player_name, draft_year, position) %in% existing_keys$key)
targets <- bind_rows(targets, deploy_qb, deploy_te)

draft_teams <- load_draft_picks() |>
  filter(position %in% c("QB", "TE"), !is.na(gsis_id)) |>
  transmute(gsis_id, draft_year = season, draft_team = team) |>
  distinct(gsis_id, draft_year, .keep_all = TRUE)

landing_lkp <- if (file.exists("data/landing_spot_features.rds")) {
  readRDS("data/landing_spot_features.rds")
} else NULL

# ── Helpers ────────────────────────────────────────────────────────────────

clean_name_local <- function(x) x |> str_to_lower() |> str_remove_all("[^a-z ]") |> str_squish()
strip_suffix_local <- function(x) str_remove(x, "\\s+(jr|sr|ii|iii|iv|v)$")
height_to_inches <- function(ht) {
  ft  <- as.integer(str_extract(ht, "^\\d+"))
  ins <- as.integer(str_extract(ht, "(?<=-)\\d+$"))
  ft * 12L + ins
}

# Combine — same source as WR/RB, but filter to QB / TE positions.
combine_raw <- load_combine() |>
  filter(pos %in% c("QB", "TE")) |>
  mutate(
    name_clean  = strip_suffix_local(clean_name_local(player_name)),
    height_in   = height_to_inches(ht),
    school_norm = normalize_school(school)
  ) |>
  select(name_clean, draft_year, pos, school_norm,
         height_in, weight = wt, forty, vertical, broad_jump)

# ── QB merge ───────────────────────────────────────────────────────────────

qb_targets <- targets |> filter(position == "QB")

qb_data <- qb_targets |>
  left_join(
    qb_feat |> select(
      gsis_id,
      tier,
      pass_yds_final, pass_td_final, pass_int_final,
      pass_att_final, pass_comp_final, pass_pct_final, pass_ypa_final,
      pass_yds_penult, pass_td_penult, pass_att_penult,
      pass_yds_ante, pass_yds_yoy, pass_td_int_ratio,
      pass_yds_per_game, pass_td_per_game,
      rush_car_final, rush_yds_final, rush_td_final,
      rush_yds_per_carry, has_mobility,
      has_penult, has_cfb_data,
      best_season_is_final,
      recruit_stars, recruit_rating, recruit_rank,
      college_years, has_recruiting, has_recruit_year,
      # PBP-derived efficiency (NA if 02j hasn't been run)
      any_of(c("epa_per_dropback","epa_per_attempt","completion_pct_pbp",
                "sack_rate","int_rate","negative_play_rate",
                "explosive_pass_rate","late_down_epa","qb_share_team",
                "has_qb_pbp"))
    ),
    by = "gsis_id"
  ) |>
  mutate(name_clean = strip_suffix_local(clean_name_local(pfr_player_name))) |>
  join_combine_two_pass(combine_raw |> filter(pos == "QB") |> select(-pos)) |>
  mutate(
    log_pick           = log(pick + 1),
    sqrt_pick          = sqrt(pick),
    round              = as.factor(round),
    tier               = factor(tier, levels = c("P4", "G5", "Other")),
    has_penult         = as.integer(coalesce(has_penult, FALSE)),
    pass_yds_penult    = coalesce(pass_yds_penult, 0),
    pass_td_penult     = coalesce(pass_td_penult, 0),
    pass_att_penult    = coalesce(pass_att_penult, 0),
    pass_yds_ante      = coalesce(pass_yds_ante, 0),
    draft_year_sc      = scale(draft_year)[, 1],
    has_recruiting     = as.integer(!is.na(recruit_rating)),
    has_combine        = as.integer(!is.na(forty)),
    speed_score        = (weight * 200) / (forty^4),
    has_cfb_data       = !is.na(pass_yds_final)
  )

# ── TE merge ───────────────────────────────────────────────────────────────

te_targets <- targets |> filter(position == "TE")

te_data <- te_targets |>
  left_join(
    te_feat |> select(
      gsis_id,
      tier,
      rec_final, rec_yards_final, rec_td_final,
      ypr_final,
      rec_penult, rec_yards_penult, rec_td_penult,
      rec_yards_ante,
      has_penult, has_cfb_data,
      rec_yds_yoy, rec_td_rate,
      teammate_rec_yards, dominator_rate,
      rec_yards_per_game, rec_per_game,
      best_season_is_final,
      recruit_stars, recruit_rating, recruit_rank,
      college_years, has_recruiting, has_recruit_year,
      # PBP-derived efficiency (reuses WR PBP cache)
      any_of(c("catch_rate_te","yards_per_target_te","yards_per_rec_te",
                "explosive_rec_rate_te","target_share_te","targets_per_game_te",
                "epa_per_target_te","epa_per_play_te_pbp",
                "has_te_pbp"))
    ),
    by = "gsis_id"
  ) |>
  mutate(name_clean = strip_suffix_local(clean_name_local(pfr_player_name))) |>
  join_combine_two_pass(combine_raw |> filter(pos == "TE") |> select(-pos)) |>
  mutate(
    log_pick           = log(pick + 1),
    sqrt_pick          = sqrt(pick),
    round              = as.factor(round),
    tier               = factor(tier, levels = c("P4", "G5", "Other")),
    has_penult         = as.integer(coalesce(has_penult, FALSE)),
    rec_penult         = coalesce(rec_penult, 0),
    rec_yards_penult   = coalesce(rec_yards_penult, 0),
    rec_td_penult      = coalesce(rec_td_penult, 0),
    rec_yards_ante     = coalesce(rec_yards_ante, 0),
    draft_year_sc      = scale(draft_year)[, 1],
    has_recruiting     = as.integer(!is.na(recruit_rating)),
    has_combine        = as.integer(!is.na(forty)),
    # Speed score matters less for TEs but kept as a structural feature
    speed_score        = (weight * 200) / (forty^4),
    has_cfb_data       = !is.na(rec_yards_final),
    # TE-specific archetype: big-body (240+ lb) = blocking-leaning, smaller
    # frame = move-TE / slot-leaning
    is_move_te         = as.integer(coalesce(weight < 240, FALSE))
  )

# ── Landing spot ──────────────────────────────────────────────────────────
# QBs use a passing-volume landing context; we don't currently build one
# distinct from WR. Leave landing fields NA for QB. TE shares the WR-side
# landing data (WR1 incumbent age tells us about target share opportunity).
qb_data <- qb_data |> mutate(
  vacated_tgt_pct     = NA_real_, incumbent_tgt_share = NA_real_,
  n_ret_wr_50tgt      = NA_real_, incumbent_wr1_age   = NA_real_,
  expected_depth_rank = NA_real_, team_targets_prior  = NA_real_,
  has_landing_data    = 0L
)
te_data <- attach_landing_features(te_data, "WR", landing_lkp = landing_lkp,
                                     draft_teams_lkp = draft_teams)

message(sprintf("TE landing coverage: %d / %d (%.0f%%)",
                sum(te_data$has_landing_data), nrow(te_data),
                100 * mean(te_data$has_landing_data)))

# ── Draft-capital delta ───────────────────────────────────────────────────
qb_data <- attach_draft_capital_features(qb_data)
te_data <- attach_draft_capital_features(te_data)

# Comp-stack (built by 08b on a prior run; deploy comps appended by 08c).
# attach_comp_features() reads data/comp_features.rds and is position-agnostic.
if (file.exists("data/comp_features.rds")) {
  qb_data <- attach_comp_features(qb_data)
  te_data <- attach_comp_features(te_data)
  message(sprintf("QB comp coverage: %d / %d (%.0f%%)",
                  sum(qb_data$has_comp_features), nrow(qb_data),
                  100 * mean(qb_data$has_comp_features)))
  message(sprintf("TE comp coverage: %d / %d (%.0f%%)",
                  sum(te_data$has_comp_features), nrow(te_data),
                  100 * mean(te_data$has_comp_features)))
}

message(sprintf("QB mock coverage: %d / %d (%.0f%%)",
                sum(qb_data$has_mock_data), nrow(qb_data),
                100 * mean(qb_data$has_mock_data)))
message(sprintf("TE mock coverage: %d / %d (%.0f%%)",
                sum(te_data$has_mock_data), nrow(te_data),
                100 * mean(te_data$has_mock_data)))

# ── Save ──────────────────────────────────────────────────────────────────

saveRDS(qb_data, "data/qb_model_data.rds")
saveRDS(te_data, "data/te_model_data.rds")
message("\nSaved: data/qb_model_data.rds and data/te_model_data.rds")

# Coverage summary
cat("\n── QB ──\n")
cat("Total rows        :", nrow(qb_data), "\n")
cat("Has CFB data      :", sum(qb_data$has_cfb_data), "\n")
cat("made_it = 1       :", sum(qb_data$made_it), "\n")
cat("made_it rate      :", round(mean(qb_data$made_it), 3), "\n")
cat("ppg mean (all)    :", round(mean(qb_data$ppg), 2), "\n")
cat("ppg mean (made_it):", round(mean(qb_data$ppg[qb_data$made_it == 1]), 2), "\n")

cat("\n── TE ──\n")
cat("Total rows        :", nrow(te_data), "\n")
cat("Has CFB data      :", sum(te_data$has_cfb_data), "\n")
cat("made_it = 1       :", sum(te_data$made_it), "\n")
cat("made_it rate      :", round(mean(te_data$made_it), 3), "\n")
cat("ppg mean (all)    :", round(mean(te_data$ppg), 2), "\n")
cat("ppg mean (made_it):", round(mean(te_data$ppg[te_data$made_it == 1]), 2), "\n")

# 03_merge_and_clean.R
# ─────────────────────────────────────────────────────────────────────────────
# Merges NFL targets with college features; produces clean model-ready datasets.
#
# Outputs:
#   data/wr_model_data.rds   — WR data (all players, includes made_it + ppg)
#   data/rb_model_data.rds   — RB data
#
# Target columns from 01_build_targets.R:
#   ppg         — shrinkage-adjusted, games-weighted PPG (busts = 0)
#   made_it     — binary flag (1 = had qualifying NFL season)
#   avg_top2_ppg — raw (unshrunk) top-2 average (NA for busts, kept for reference)
# ─────────────────────────────────────────────────────────────────────────────
setwd("~/Projects/R/college_nfl_model")
library(tidyverse)
library(nflreadr)

source("functions/helpers.R")

targets      <- readRDS("data/targets.rds")
wr_feat_raw  <- readRDS("data/wr_features_raw.rds")
rb_feat_raw  <- readRDS("data/rb_features_raw.rds")

# draft_team lookup — from nflreadr (not retained in targets.rds)
draft_teams <- load_draft_picks() |>
  filter(position %in% c("WR", "RB"), !is.na(gsis_id)) |>
  transmute(gsis_id, draft_year = season, draft_team = team) |>
  distinct(gsis_id, draft_year, .keep_all = TRUE)

# Landing spot features (built by 02d_build_landing_spot_features.R)
landing_lkp <- if (file.exists("data/landing_spot_features.rds")) {
  readRDS("data/landing_spot_features.rds")
} else {
  message("landing_spot_features.rds not found — run 02d first. Skipping.")
  NULL
}

# ── Combine data ──────────────────────────────────────────────────────────────
clean_name <- function(x) {
  x |> str_to_lower() |> str_remove_all("[^a-z ]") |> str_squish()
}
strip_suffix <- function(x) {
  str_remove(x, "\\s+(jr|sr|ii|iii|iv|v)$")
}
height_to_inches <- function(ht) {
  # Converts "6-2" or "6-02" format to total inches
  ft  <- as.integer(str_extract(ht, "^\\d+"))
  ins <- as.integer(str_extract(ht, "(?<=-)\\d+$"))
  ft * 12L + ins
}

# Combine athleticism — keeps NA-draft-year rows (used as fallback by
# join_combine_two_pass) and the normalized school for that fallback.
# nflreadr's combine has draft_year=NA for ~42% of WR/RB rows (recent
# classes not yet backfilled + historical UDFAs); without the fallback we
# silently lose combine for ~50+ drafted prospects per recent class.
combine_raw <- load_combine() |>
  filter(pos %in% c("WR", "RB")) |>
  mutate(
    name_clean  = strip_suffix(clean_name(player_name)),
    height_in   = height_to_inches(ht),
    school_norm = normalize_school(school)
  ) |>
  select(name_clean, draft_year, pos, school_norm,
         height_in, weight = wt, forty, vertical, broad_jump)

# ── WR ────────────────────────────────────────────────────────────────────────

wr_targets <- targets |> filter(position == "WR")

wr_data <- wr_targets |>
  left_join(
    wr_feat_raw |>
      select(
        gsis_id,
        tier,
        rec_final, rec_yards_final, rec_td_final,
        ypr,
        rec_penult, rec_yards_penult, rec_td_penult,
        has_penult, rec_yds_yoy,
        rec_yards_ante, teammate_rec_yards, rec_td_rate,
        rec_yards_per_game, rec_per_game,
        # Target share & PPA efficiency (2016+ seasons, NAs elsewhere)
        usg_pass, usg_passing_downs, avg_PPA_pass, total_PPA_pass,
        # Recruiting composite (247Sports)
        recruit_stars, recruit_rating, recruit_rank,
        # Age-adjusted & teammate context
        college_years, age_relative, n_drafted_skill, elite_teammate,
        # Best-season flag (0 = player peaked before their final college season)
        best_season_is_final,
        # PBP features (2014+ seasons, NAs elsewhere; has_wr_pbp flags era).
        # cfbfastR has no air_yards / YAC, so aDOT + YAC/rec are unavailable.
        catch_rate_wr, yards_per_target_wr, yards_per_rec_wr,
        explosive_rec_rate, target_share_wr, targets_per_game_wr,
        epa_per_target_wr, epa_per_play_wr_pbp
      ),
    by = "gsis_id"
  ) |>
  # Join combine athleticism (two-pass: strict by year, fallback by name+school
  # for the ~42% of nflreadr combine rows with NA draft_year).
  mutate(name_clean = strip_suffix(clean_name(pfr_player_name))) |>
  join_combine_two_pass(combine_raw |> filter(pos == "WR") |> select(-pos)) |>
  mutate(
    log_pick         = log(pick + 1),
    sqrt_pick        = sqrt(pick),
    round            = as.factor(round),
    tier             = factor(tier, levels = c("P4", "G5", "Other")),
    has_penult       = as.integer(coalesce(has_penult, FALSE)),
    rec_penult       = coalesce(rec_penult,       0),
    rec_yards_penult = coalesce(rec_yards_penult, 0),
    rec_td_penult    = coalesce(rec_td_penult,    0),
    rec_yards_ante   = coalesce(rec_yards_ante,   0),
    draft_year_sc    = scale(draft_year)[, 1],
    # Age-adjusted & teammate context
    age_relative     = coalesce(age_relative, 0),
    n_drafted_skill  = coalesce(as.integer(n_drafted_skill), 0L),
    elite_teammate   = coalesce(as.integer(elite_teammate), 0L),
    has_recruit_year = as.integer(!is.na(college_years)),
    # Missingness indicators (era-driven, not random) — integer for XGBoost
    has_ppa          = as.integer(!is.na(avg_PPA_pass)),
    has_usage        = as.integer(!is.na(usg_pass)),
    has_wr_pbp       = as.integer(!is.na(target_share_wr) | !is.na(catch_rate_wr)),
    has_recruiting   = as.integer(!is.na(recruit_rating)),
    has_combine      = as.integer(!is.na(forty)),
    # Speed Score: weight-adjusted forty time (Bill Barnwell). Higher = more explosive
    # for body size. NA when combine data missing (median-imputed at model time).
    speed_score      = (weight * 200) / (forty^4),
    # Possession WR archetype: heavy + slow = contested-catch specialist with limited
    # NFL ceiling. NA-safe: if combine missing, defaults to 0 (not possession WR).
    is_possession_wr = as.integer(coalesce(weight > 215 & forty > 4.50, FALSE)),
    # Dominator rate: share of team receiving volume. NA when teammate data is missing
    # (do NOT zero-fill — that would inflate to 1.0 for players without teammate context).
    dominator_rate   = if_else(!is.na(teammate_rec_yards),
                                rec_yards_final / (rec_yards_final + teammate_rec_yards + .001),
                                NA_real_),
    # NB: dropped age_adj_yards (was r=0.99 with rec_yards_final, never used).
    # Breakout-age + within-age-bucket dominator residual replace it as the
    # age-aware production signals.
    .dummy_drop_marker_wr = NA  # placeholder so the closing paren stays valid
  ) |>
  select(-.dummy_drop_marker_wr) |>
  mutate(has_cfb_data = !is.na(rec_yards_final))

# ── RB ────────────────────────────────────────────────────────────────────────

rb_targets <- targets |> filter(position == "RB")

rb_data <- rb_targets |>
  left_join(
    rb_feat_raw |>
      select(
        gsis_id,
        tier,
        carries_final, rush_yards_final, rush_td_final,
        ypc,
        rb_rec, rb_rec_yards, rb_rec_td,
        rush_yards_penult, carries_penult, rush_td_penult,
        has_penult, rush_yds_yoy, recv_share,
        rush_yards_ante, teammate_rush_yards, rush_td_rate,
        total_touches,
        # Composite scrimmage features
        scrimmage_yards, scrimmage_td, yards_per_touch,
        # Per-game rates
        rush_yards_per_game, carries_per_game, scrimmage_yards_per_game,
        # Rush usage & PPA efficiency (2016+ seasons, NAs elsewhere)
        usg_rush, usg_passing_downs, avg_PPA_rush, total_PPA_rush,
        # All-phase usage & PPA (captures pass-catching RB value)
        usg_overall, usg_pass, avg_PPA_all, total_PPA_all,
        # Recruiting composite (247Sports)
        recruit_stars, recruit_rating, recruit_rank,
        # Age-adjusted & teammate context
        college_years, age_relative, n_drafted_skill, elite_teammate,
        # PBP features — Tier 1 + EPA/play (2005+ seasons, NAs elsewhere)
        explosive_rate, breakaway_rate, target_share, targets_per_game,
        catch_rate, epa_per_rush, epa_per_play_pbp,
        carries_per_game_pbp, ypc_pbp
      ),
    by = "gsis_id"
  ) |>
  mutate(name_clean = strip_suffix(clean_name(pfr_player_name))) |>
  join_combine_two_pass(combine_raw |> filter(pos == "RB") |> select(-pos)) |>
  mutate(
    log_pick          = log(pick + 1),
    sqrt_pick         = sqrt(pick),
    round             = as.factor(round),
    tier              = factor(tier, levels = c("P4", "G5", "Other")),
    has_penult        = as.integer(coalesce(has_penult, FALSE)),
    # Cast to double so recipe prepped column types match scoring-time NA-safe
    # imputation output (step_impute_median returns double). Keeping integer
    # types triggered a <double> → <integer> coercion error in bake().
    rb_rec            = as.numeric(coalesce(rb_rec,            0)),
    rb_rec_yards      = as.numeric(coalesce(rb_rec_yards,      0)),
    rb_rec_td         = as.numeric(coalesce(rb_rec_td,         0)),
    rush_yards_penult = as.numeric(coalesce(rush_yards_penult, 0)),
    carries_penult    = as.numeric(coalesce(carries_penult,    0)),
    rush_td_penult    = as.numeric(coalesce(rush_td_penult,    0)),
    rush_yards_ante   = as.numeric(coalesce(rush_yards_ante,   0)),
    draft_year_sc     = scale(draft_year)[, 1],
    # Age-adjusted & teammate context
    age_relative      = coalesce(age_relative, 0),
    n_drafted_skill   = coalesce(as.integer(n_drafted_skill), 0L),
    elite_teammate    = coalesce(as.integer(elite_teammate), 0L),
    has_recruit_year  = as.integer(!is.na(college_years)),
    # Missingness indicators (era-driven, not random) — integer for XGBoost
    has_ppa           = as.integer(!is.na(avg_PPA_rush)),
    has_usage         = as.integer(!is.na(usg_rush)),
    has_pbp           = as.integer(!is.na(explosive_rate) | !is.na(target_share)),
    has_recruiting    = as.integer(!is.na(recruit_rating)),
    has_combine       = as.integer(!is.na(forty)),
    has_cfb_data      = !is.na(rush_yards_final),
    # Speed Score: weight-adjusted forty time. Especially meaningful for RBs —
    # a 230-lb back at 4.40 is a fundamentally different prospect than a 195-lb back.
    speed_score       = (weight * 200) / (forty^4),
    # Scat back archetype: backs under 195 lbs rarely become feature backs in the NFL —
    # they end up as specialists/returners regardless of college production.
    is_scat_back      = as.integer(coalesce(weight < 195, FALSE)),
    # Dominator rate: share of team rushing volume. NA when teammate data is missing
    # (do NOT zero-fill — that would inflate to 1.0 for players without teammate context).
    dominator_rate    = if_else(!is.na(teammate_rush_yards),
                                 rush_yards_final / (rush_yards_final + teammate_rush_yards + .001),
                                 NA_real_),
    # NB: dropped age_adj_yards (was r=1.00 with rush_yards_final, never used).
    .dummy_drop_marker_rb = NA
  ) |>
  select(-.dummy_drop_marker_rb)

# ── Summary ───────────────────────────────────────────────────────────────────

cat("\n── WR ──────────────────────────────────────────\n")
cat("Total rows        :", nrow(wr_data), "\n")
cat("Has CFB data      :", sum(wr_data$has_cfb_data), "\n")
cat("made_it = 1       :", sum(wr_data$made_it), "\n")
cat("made_it rate      :", round(mean(wr_data$made_it), 3), "\n")
cat("ppg mean (all)    :", round(mean(wr_data$ppg), 2), "\n")
cat("ppg mean (made_it):", round(mean(wr_data$ppg[wr_data$made_it == 1]), 2), "\n")

cat("\n── RB ──────────────────────────────────────────\n")
cat("Total rows        :", nrow(rb_data), "\n")
cat("Has CFB data      :", sum(rb_data$has_cfb_data), "\n")
cat("made_it = 1       :", sum(rb_data$made_it), "\n")
cat("made_it rate      :", round(mean(rb_data$made_it), 3), "\n")
cat("ppg mean (all)    :", round(mean(rb_data$ppg), 2), "\n")
cat("ppg mean (made_it):", round(mean(rb_data$ppg[rb_data$made_it == 1]), 2), "\n")

# ── Distribution of target ────────────────────────────────────────────────────

cat("\nWR ppg distribution (all, busts=0):\n")
quantile(wr_data$ppg, probs = c(0, .1, .25, .5, .75, .9, 1)) |>
  round(2) |> print()

cat("\nRB ppg distribution (all, busts=0):\n")
quantile(rb_data$ppg, probs = c(0, .1, .25, .5, .75, .9, 1)) |>
  round(2) |> print()

# ── Landing spot features ─────────────────────────────────────────────────────
# Uses shared attach_landing_features() from helpers.R — same function called
# at scoring time, so the deployed model never sees a different feature shape
# than what it trained on.

wr_data <- attach_landing_features(wr_data, "WR", landing_lkp = landing_lkp,
                                     draft_teams_lkp = draft_teams)
rb_data <- attach_landing_features(rb_data, "RB", landing_lkp = landing_lkp,
                                     draft_teams_lkp = draft_teams)

# Breakout features (precomputed by 02g_build_breakout_features.R).
# Same shared attacher used by score_class so train/score parity holds.
wr_data <- attach_breakout_features(wr_data, "WR")
rb_data <- attach_breakout_features(rb_data, "RB")

# Age-adjusted dominator (residual from per-age population mean, precomputed
# alongside the breakout features).
wr_data <- attach_age_adj_dominator(wr_data, "WR")
rb_data <- attach_age_adj_dominator(rb_data, "RB")
message(sprintf("WR breakout coverage: %d / %d (%.0f%%)",
                sum(wr_data$has_breakout, na.rm = TRUE), nrow(wr_data),
                100 * mean(wr_data$has_breakout, na.rm = TRUE)))
message(sprintf("RB breakout coverage: %d / %d (%.0f%%)",
                sum(rb_data$has_breakout, na.rm = TRUE), nrow(rb_data),
                100 * mean(rb_data$has_breakout, na.rm = TRUE)))

message(sprintf("WR landing coverage: %d / %d (%.0f%%)",
                sum(wr_data$has_landing_data), nrow(wr_data),
                100 * mean(wr_data$has_landing_data)))
message(sprintf("RB landing coverage: %d / %d (%.0f%%)",
                sum(rb_data$has_landing_data), nrow(rb_data),
                100 * mean(rb_data$has_landing_data)))

# ── Draft-capital delta (mock-vs-actual) ─────────────────────────────────────
# Same shared attacher used by score_class so train/score parity holds.

wr_data <- attach_draft_capital_features(wr_data)
rb_data <- attach_draft_capital_features(rb_data)

message(sprintf("WR mock coverage: %d / %d (%.0f%%)",
                sum(wr_data$has_mock_data), nrow(wr_data),
                100 * mean(wr_data$has_mock_data)))
message(sprintf("RB mock coverage: %d / %d (%.0f%%)",
                sum(rb_data$has_mock_data), nrow(rb_data),
                100 * mean(rb_data$has_mock_data)))

# ── Save ──────────────────────────────────────────────────────────────────────

saveRDS(wr_data, "data/wr_model_data.rds")
saveRDS(rb_data, "data/rb_model_data.rds")
message("\nSaved: data/wr_model_data.rds and data/rb_model_data.rds")

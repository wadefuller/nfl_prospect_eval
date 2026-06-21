# 07b_score_qb_te.R
# ─────────────────────────────────────────────────────────────────────────────
# Scores QB and TE prospects (training + deploy class) using the trained
# hurdle + bucket ensemble. Output schema mirrors WR/RB so the website
# export script can union all four positions seamlessly.
#
# Inputs:
#   data/qb_model_data.rds, data/te_model_data.rds
#   models/qb_{bust,production,xgb_bucket,clm_bucket}_model.rds
#   models/te_{bust,production,xgb_bucket,clm_bucket}_model.rds
#
# Output:
#   output/qb_te_class_scores.rds — concatenated with WR/RB downstream
# ─────────────────────────────────────────────────────────────────────────────

setwd("~/Projects/R/college_nfl_model")
suppressMessages({
  library(tidyverse)
  library(tidymodels)
  library(xgboost)
  library(rstanarm)
  library(nflreadr)
})
source("functions/helpers.R")
source("functions/feature_specs.R")
source("functions/ordinal_helpers.R")

QB_YEAR_CUTOFF <- 2008
TE_YEAR_CUTOFF <- 2008

qb_data <- readRDS("data/qb_model_data.rds") |>
  mutate(draft_year_sc = scale(draft_year)[, 1])
te_data <- readRDS("data/te_model_data.rds") |>
  mutate(draft_year_sc = scale(draft_year)[, 1])

qb_bust <- readRDS("models/qb_bust_model.rds")
te_bust <- readRDS("models/te_bust_model.rds")
qb_prod <- readRDS("models/qb_production_model.rds")
te_prod <- readRDS("models/te_production_model.rds")
qb_xgb_bucket <- readRDS("models/qb_xgb_bucket_model.rds")
te_xgb_bucket <- readRDS("models/te_xgb_bucket_model.rds")
qb_clm_bucket <- readRDS("models/qb_clm_bucket_model.rds")
te_clm_bucket <- readRDS("models/te_clm_bucket_model.rds")

# ── Hurdle combiner ───────────────────────────────────────────────────────
# QB classifier signal is meaningful so we don't shrink. TE is more class-
# imbalanced (~82% positive rate) so a light shrink toward base rate.
clip01 <- function(x) pmax(0, pmin(1, x))
combine_qb <- function(p) clip01(0.85 * sqrt(p))
combine_te <- function(p) {
  # Light shrink toward 0.65 base rate to avoid the saturate-at-1 problem
  alpha <- 0.6
  clip01(alpha * p + (1 - alpha) * 0.65)
}

score_one_position <- function(data, bust_model, prod_model,
                                xgb_bucket, clm_bucket, pos, combiner) {
  scored <- data |> filter(has_cfb_data)
  if (nrow(scored) == 0) return(tibble())

  bust_prob    <- predict(bust_model$fit, scored, type = "prob") |>
    pull(.pred_made_it)
  if (isTRUE(prod_model$type == "quantile")) {
    df_baked     <- bake(prod_model$recipe, new_data = scored)
    X_score      <- as.matrix(df_baked |> select(-any_of(c("log_ppg", "draft_year"))))
    log_ppg_pred <- predict(prod_model$fit, xgboost::xgb.DMatrix(X_score))
  } else {
    log_ppg_pred <- predict(prod_model$fit, scored) |> pull(.pred)
  }
  p_eff        <- combiner(bust_prob)
  exp_ppg      <- pmax(p_eff * exp(log_ppg_pred), 0)

  scored <- scored |>
    mutate(p_made_it = bust_prob, p_eff = p_eff, exp_ppg = exp_ppg) |>
    attach_bucket_predictions(xgb_bucket, clm_bucket, pos)

  # Final blended exp_ppg = 0.30 * hurdle + 0.70 * bucket (matches WR/RB blend)
  scored |> mutate(
    exp_ppg_blended = case_when(
      is.na(exp_ppg_bucket) ~ exp_ppg,
      TRUE ~ 0.30 * exp_ppg + 0.70 * exp_ppg_bucket
    ))
}

qb_scores <- score_one_position(qb_data, qb_bust, qb_prod,
                                 qb_xgb_bucket, qb_clm_bucket, "QB", combine_qb)
te_scores <- score_one_position(te_data, te_bust, te_prod,
                                 te_xgb_bucket, te_clm_bucket, "TE", combine_te)

cat(sprintf("\nQB scored: %d rows\n", nrow(qb_scores)))
cat(sprintf("TE scored: %d rows\n", nrow(te_scores)))

# ── Align with WR/RB output schema ─────────────────────────────────────────
# The WR/RB all_class_scores.rds has these columns at minimum:
#   name, position, draft_year, round, pick, college, tier,
#   p_made_it, p_eff, exp_ppg,
#   p_bust, p_bench, p_flex, p_elite, p_league_winner (+ lo/hi),
#   exp_ppg_bucket (+ lo/hi), bucket_top1,
#   actual_ppg, actual_raw_ppg, actual_made_it, n_qual_seasons,
#   has_cfb_data, weight, height_in, forty
# (and many position-specific stats which we'll let coalesce to NA per position).
common_cols <- function(d) d |>
  transmute(
    name           = pfr_player_name,
    position,
    draft_year     = as.numeric(draft_year),
    round          = as.numeric(as.character(round)),
    pick           = as.numeric(pick),
    college,
    tier           = as.character(tier),
    p_made_it,
    p_eff,
    exp_ppg        = exp_ppg_blended,
    p_bust, p_bench, p_flex, p_elite, p_league_winner,
    p_bust_lo, p_bench_lo, p_flex_lo, p_elite_lo, p_league_winner_lo,
    p_bust_hi, p_bench_hi, p_flex_hi, p_elite_hi, p_league_winner_hi,
    exp_ppg_bucket, exp_ppg_bucket_lo, exp_ppg_bucket_hi, bucket_top1,
    actual_ppg     = if ("ppg" %in% names(d)) ppg else NA_real_,
    actual_raw_ppg = if ("avg_top2_ppg" %in% names(d)) avg_top2_ppg else NA_real_,
    actual_made_it = if ("made_it" %in% names(d)) made_it else NA_integer_,
    n_qual_seasons = if ("n_qual_seasons" %in% names(d)) n_qual_seasons else NA_integer_,
    has_cfb_data,
    weight, height_in, forty,
    # Position-specific volume / efficiency the export script can pick up.
    # QB:
    pass_yds_final     = if ("pass_yds_final"     %in% names(d)) pass_yds_final     else NA_real_,
    pass_td_final      = if ("pass_td_final"      %in% names(d)) pass_td_final      else NA_real_,
    pass_int_final     = if ("pass_int_final"     %in% names(d)) pass_int_final     else NA_real_,
    pass_att_final     = if ("pass_att_final"     %in% names(d)) pass_att_final     else NA_real_,
    pass_pct_final     = if ("pass_pct_final"     %in% names(d)) pass_pct_final     else NA_real_,
    pass_ypa_final     = if ("pass_ypa_final"     %in% names(d)) pass_ypa_final     else NA_real_,
    pass_yds_per_game  = if ("pass_yds_per_game"  %in% names(d)) pass_yds_per_game  else NA_real_,
    pass_td_per_game   = if ("pass_td_per_game"   %in% names(d)) pass_td_per_game   else NA_real_,
    rush_yds_final     = if ("rush_yds_final"     %in% names(d)) rush_yds_final     else NA_real_,
    rush_yds_per_carry = if ("rush_yds_per_carry" %in% names(d)) rush_yds_per_carry else NA_real_,
    epa_per_dropback   = if ("epa_per_dropback"   %in% names(d)) epa_per_dropback   else NA_real_,
    epa_per_attempt    = if ("epa_per_attempt"    %in% names(d)) epa_per_attempt    else NA_real_,
    completion_pct_pbp = if ("completion_pct_pbp" %in% names(d)) completion_pct_pbp else NA_real_,
    sack_rate          = if ("sack_rate"          %in% names(d)) sack_rate          else NA_real_,
    int_rate           = if ("int_rate"           %in% names(d)) int_rate           else NA_real_,
    explosive_pass_rate = if ("explosive_pass_rate" %in% names(d)) explosive_pass_rate else NA_real_,
    qb_share_team      = if ("qb_share_team"      %in% names(d)) qb_share_team      else NA_real_,
    has_qb_pbp         = if ("has_qb_pbp"         %in% names(d)) has_qb_pbp         else NA_integer_,
    # TE:
    rec_final          = if ("rec_final"          %in% names(d)) rec_final          else NA_real_,
    rec_yards_final    = if ("rec_yards_final"    %in% names(d)) rec_yards_final    else NA_real_,
    rec_td_final       = if ("rec_td_final"       %in% names(d)) rec_td_final       else NA_real_,
    rec_yards_per_game = if ("rec_yards_per_game" %in% names(d)) rec_yards_per_game else NA_real_,
    rec_per_game       = if ("rec_per_game"       %in% names(d)) rec_per_game       else NA_real_,
    ypr_te             = if ("ypr_final"          %in% names(d)) ypr_final          else NA_real_,
    rec_td_rate_te     = if ("rec_td_rate"        %in% names(d)) rec_td_rate        else NA_real_,
    dominator_rate_te  = if ("dominator_rate"     %in% names(d)) dominator_rate     else NA_real_,
    catch_rate_te      = if ("catch_rate_te"      %in% names(d)) catch_rate_te      else NA_real_,
    yards_per_target_te = if ("yards_per_target_te" %in% names(d)) yards_per_target_te else NA_real_,
    target_share_te    = if ("target_share_te"    %in% names(d)) target_share_te    else NA_real_,
    targets_per_game_te = if ("targets_per_game_te" %in% names(d)) targets_per_game_te else NA_real_,
    epa_per_target_te  = if ("epa_per_target_te"  %in% names(d)) epa_per_target_te  else NA_real_,
    explosive_rec_rate_te = if ("explosive_rec_rate_te" %in% names(d)) explosive_rec_rate_te else NA_real_,
    has_te_pbp         = if ("has_te_pbp"         %in% names(d)) has_te_pbp         else NA_integer_
  )

out <- bind_rows(common_cols(qb_scores), common_cols(te_scores)) |>
  arrange(draft_year, position, pick)

saveRDS(out, "output/qb_te_class_scores.rds")
write_csv(out,  "output/qb_te_class_scores.csv")
cat(sprintf("\nSaved: output/qb_te_class_scores.rds (%d rows)\n", nrow(out)))

# Sanity print
cat("\n=== Top 2026 QBs ===\n")
out |> filter(draft_year == 2026, position == "QB") |>
  arrange(desc(exp_ppg)) |> head(5) |>
  select(name, position, pick, exp_ppg, p_league_winner, bucket_top1) |>
  print()

cat("\n=== Top 2026 TEs ===\n")
out |> filter(draft_year == 2026, position == "TE") |>
  arrange(desc(exp_ppg)) |> head(5) |>
  select(name, position, pick, exp_ppg, p_league_winner, bucket_top1) |>
  print()

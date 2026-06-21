# 19b_train_ordinal_qb_te.R
# ─────────────────────────────────────────────────────────────────────────────
# Trains the 5-bucket Bayesian ensemble (XGB multiclass + stan_polr) for
# QB and TE. Mirrors 19_train_ordinal_models.R but uses the simpler QB/TE
# recipe (no era-gated PBP/usage features).
#
# Outputs:
#   models/qb_xgb_bucket_model.rds + models/qb_clm_bucket_model.rds
#   models/te_xgb_bucket_model.rds + models/te_clm_bucket_model.rds
# ─────────────────────────────────────────────────────────────────────────────

setwd("~/Projects/R/college_nfl_model")
suppressMessages({
  library(tidyverse)
  library(tidymodels)
  library(xgboost)
  library(rstanarm)
})

source("functions/helpers.R")
source("functions/feature_specs.R")
source("functions/ordinal_helpers.R")

set.seed(42)

QB_YEAR_CUTOFF <- 2008
TE_YEAR_CUTOFF <- 2008

# Recipe for QB/TE — includes era-gated PBP zero-fill via has_qb_pbp / has_te_pbp.
build_recipe <- function(model_df, outcome, pos) {
  rec <- recipe(as.formula(paste(outcome, "~ .")), data = model_df) |>
    update_role(draft_year, new_role = "ID")
  if (identical(pos, "QB") && "has_qb_pbp" %in% names(model_df)) {
    rec <- rec |> step_mutate(
      epa_per_dropback     = if_else(has_qb_pbp == 0L, 0, epa_per_dropback),
      epa_per_attempt      = if_else(has_qb_pbp == 0L, 0, epa_per_attempt),
      completion_pct_pbp   = if_else(has_qb_pbp == 0L, 0, completion_pct_pbp),
      sack_rate            = if_else(has_qb_pbp == 0L, 0, sack_rate),
      int_rate             = if_else(has_qb_pbp == 0L, 0, int_rate),
      negative_play_rate   = if_else(has_qb_pbp == 0L, 0, negative_play_rate),
      explosive_pass_rate  = if_else(has_qb_pbp == 0L, 0, explosive_pass_rate),
      late_down_epa        = if_else(has_qb_pbp == 0L, 0, late_down_epa),
      qb_share_team        = if_else(has_qb_pbp == 0L, 0, qb_share_team)
    )
  }
  if (identical(pos, "TE") && "has_te_pbp" %in% names(model_df)) {
    rec <- rec |> step_mutate(
      catch_rate_te        = if_else(has_te_pbp == 0L, 0, catch_rate_te),
      yards_per_target_te  = if_else(has_te_pbp == 0L, 0, yards_per_target_te),
      yards_per_rec_te     = if_else(has_te_pbp == 0L, 0, yards_per_rec_te),
      explosive_rec_rate_te = if_else(has_te_pbp == 0L, 0, explosive_rec_rate_te),
      target_share_te      = if_else(has_te_pbp == 0L, 0, target_share_te),
      targets_per_game_te  = if_else(has_te_pbp == 0L, 0, targets_per_game_te),
      epa_per_target_te    = if_else(has_te_pbp == 0L, 0, epa_per_target_te),
      epa_per_play_te_pbp  = if_else(has_te_pbp == 0L, 0, epa_per_play_te_pbp)
    )
  }
  rec |>
    step_unknown(all_nominal_predictors(), new_level = "unknown") |>
    step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
    step_impute_median(all_numeric_predictors()) |>
    step_nzv(all_predictors())
}

qb_data <- readRDS("data/qb_model_data.rds") |>
  mutate(draft_year_sc = scale(draft_year)[, 1])
te_data <- readRDS("data/te_model_data.rds") |>
  mutate(draft_year_sc = scale(draft_year)[, 1])

cat("\n── Bucket distribution: QB ──\n")
qb_data |> filter(has_cfb_data, draft_year >= QB_YEAR_CUTOFF) |>
  mutate(bucket = assign_bucket(ppg, "QB")) |>
  count(bucket) |> mutate(pct = round(100 * n / sum(n), 1)) |> print()
cat("\n── Bucket distribution: TE ──\n")
te_data |> filter(has_cfb_data, draft_year >= TE_YEAR_CUTOFF) |>
  mutate(bucket = assign_bucket(ppg, "TE")) |>
  count(bucket) |> mutate(pct = round(100 * n / sum(n), 1)) |> print()

# Position-specific filter wrapper.
train_with_cutoff <- function(data, features, pos, cutoff, kind = c("xgb", "clm")) {
  kind <- match.arg(kind)
  feats <- intersect(features, names(data))
  if (kind == "xgb") {
    # Filter once and pass to train_xgb_bucket
    data <- data |> filter(draft_year >= cutoff)
    train_xgb_bucket(data, feats, pos, build_recipe)
  } else {
    data <- data |> filter(draft_year >= cutoff)
    train_stan_bucket(data, feats, pos)
  }
}

cat("\n══ QB XGB multiclass ══\n")
qb_xgb <- train_with_cutoff(qb_data, QB_PROD_FEATURES, "QB", QB_YEAR_CUTOFF, "xgb")
cat("  trained on", length(qb_xgb$fit$feature_names), "features\n")
cat("\n══ QB ordinal (Bayesian stan_polr) ══\n")
qb_clm <- train_with_cutoff(qb_data, ORD_FEATURES_QB, "QB", QB_YEAR_CUTOFF, "clm")
if (!is.null(qb_clm)) cat("  max R̂:",
  round(max(qb_clm$fit$stan_summary[, "Rhat"], na.rm = TRUE), 3), "\n")

cat("\n══ TE XGB multiclass ══\n")
te_xgb <- train_with_cutoff(te_data, TE_PROD_FEATURES, "TE", TE_YEAR_CUTOFF, "xgb")
cat("  trained on", length(te_xgb$fit$feature_names), "features\n")
cat("\n══ TE ordinal (Bayesian stan_polr) ══\n")
te_clm <- train_with_cutoff(te_data, ORD_FEATURES_TE, "TE", TE_YEAR_CUTOFF, "clm")
if (!is.null(te_clm)) cat("  max R̂:",
  round(max(te_clm$fit$stan_summary[, "Rhat"], na.rm = TRUE), 3), "\n")

saveRDS(qb_xgb, "models/qb_xgb_bucket_model.rds")
saveRDS(qb_clm, "models/qb_clm_bucket_model.rds")
saveRDS(te_xgb, "models/te_xgb_bucket_model.rds")
saveRDS(te_clm, "models/te_clm_bucket_model.rds")
cat("\nSaved 4 QB/TE bucket models.\n")

# Quick in-sample sanity check
sanity_check <- function(full, xgb_obj, clm_obj, pos) {
  cat(sprintf("\n── %s in-sample top-10 by exp_ppg_bucket ──\n", pos))
  full_with_pred <- attach_bucket_predictions(full, xgb_obj, clm_obj, pos)
  full_with_pred |>
    filter(has_cfb_data, draft_year >= 2018) |>
    arrange(desc(exp_ppg_bucket)) |> head(10) |>
    select(pfr_player_name, college, draft_year, pick, ppg,
           p_bust, p_bench, p_flex, p_elite, p_league_winner, exp_ppg_bucket) |>
    mutate(across(starts_with("p_"), ~ round(.x, 2))) |>
    print()
}

sanity_check(qb_data, qb_xgb, qb_clm, "QB")
sanity_check(te_data, te_xgb, te_clm, "TE")

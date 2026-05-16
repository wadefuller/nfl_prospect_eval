# 19_train_ordinal_models.R
# ─────────────────────────────────────────────────────────────────────────────
# Trains the ordinal-bucket models (XGBoost multiclass + ordinal::clm) on the
# full training data and saves them to models/. Coexists with the continuous
# hurdle models — does not replace them. Predicts a distribution over outcome
# buckets {bust, flex, elite, league_winner}.
#
# Architecture validated by 18_ordinal_ensemble.R: ensemble MAE 2.50 (vs the
# continuous hurdle's 2.62) on the 8-fold rolling temporal CV.
#
# Outputs:
#   models/wr_xgb_bucket_model.rds   — XGB multiclass (full feature spec)
#   models/rb_xgb_bucket_model.rds
#   models/wr_clm_bucket_model.rds   — proportional-odds (curated subset)
#   models/rb_clm_bucket_model.rds
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

# Pull build_recipe() from the temporal CV harness — same era zero-fill +
# normalization as the production model uses.
src11 <- readLines("11_temporal_cv.R")
fn_start    <- grep("^build_recipe <- function", src11)
load_marker <- grep("^# ── 5. Load data", src11)
eval(parse(text = src11[fn_start[1]:(load_marker - 1)]))

cat("Loading training data + comp features...\n")
wr_full <- readRDS("data/wr_model_data.rds") |> attach_comp_features()
rb_full <- readRDS("data/rb_model_data.rds") |> attach_comp_features()

# Recompute draft_year_sc on the full training distribution so deployment
# predictions use the same scaling.
wr_full <- wr_full |>
  mutate(draft_year_sc = scale(draft_year)[, 1])
rb_full <- rb_full |>
  mutate(draft_year_sc = scale(draft_year)[, 1])

# Bucket distribution sanity check
cat("\n── Bucket distribution in training data ──\n")
bind_rows(
  wr_full |> filter(has_cfb_data) |>
    mutate(bucket = assign_bucket(ppg, "WR"), pos = "WR"),
  rb_full |> filter(has_cfb_data, draft_year >= 2010) |>
    mutate(bucket = assign_bucket(ppg, "RB"), pos = "RB")
) |>
  count(pos, bucket) |>
  group_by(pos) |> mutate(pct = round(100 * n / sum(n), 1)) |>
  print()

# ── Train ──────────────────────────────────────────────────────────────────

cat("\n══ WR XGB multiclass ══\n")
wr_xgb <- train_xgb_bucket(wr_full, WR_PROD_FEATURES, "WR", build_recipe)
cat("  trained on", length(wr_xgb$fit$feature_names), "features\n")

cat("\n══ WR ordinal (Bayesian stan_polr) ══\n")
wr_clm <- train_stan_bucket(wr_full, ORD_FEATURES_WR, "WR")
cat("  max R̂:", round(max(wr_clm$fit$stan_summary[, "Rhat"], na.rm = TRUE), 3), "\n")

cat("\n══ RB XGB multiclass ══\n")
rb_xgb <- train_xgb_bucket(rb_full, RB_PROD_FEATURES, "RB", build_recipe)
cat("  trained on", length(rb_xgb$fit$feature_names), "features\n")

cat("\n══ RB ordinal (Bayesian stan_polr) ══\n")
rb_clm <- train_stan_bucket(rb_full, ORD_FEATURES_RB, "RB")
cat("  max R̂:", round(max(rb_clm$fit$stan_summary[, "Rhat"], na.rm = TRUE), 3), "\n")

# ── Save ──────────────────────────────────────────────────────────────────

saveRDS(wr_xgb, "models/wr_xgb_bucket_model.rds")
saveRDS(rb_xgb, "models/rb_xgb_bucket_model.rds")
saveRDS(wr_clm, "models/wr_clm_bucket_model.rds")
saveRDS(rb_clm, "models/rb_clm_bucket_model.rds")

cat("\nSaved 4 ordinal-bucket models to models/\n")

# ── In-sample sanity check on top training prospects ──────────────────────

sanity_check <- function(full, xgb_obj, clm_obj, pos, label) {
  cat(sprintf("\n── %s in-sample top-10 by exp_ppg_bucket ──\n", label))
  full_with_pred <- attach_bucket_predictions(full, xgb_obj, clm_obj, pos)
  full_with_pred |>
    filter(has_cfb_data, draft_year >= 2018) |>
    arrange(desc(exp_ppg_bucket)) |> head(10) |>
    select(pfr_player_name, college, draft_year, pick, ppg,
           p_bust, p_bench, p_flex, p_elite, p_league_winner, exp_ppg_bucket) |>
    mutate(across(starts_with("p_"), ~ round(.x, 2))) |>
    print()
}

sanity_check(wr_full, wr_xgb, wr_clm, "WR", "WR")
sanity_check(rb_full, rb_xgb, rb_clm, "RB", "RB")

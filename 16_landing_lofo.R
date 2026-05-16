# 16_landing_lofo.R
# ─────────────────────────────────────────────────────────────────────────────
# Leave-one-feature-out importance for the landing-spot feature family.
#
# After the 2026-05-09 team-code fix lifted training landing coverage from 64%
# to 94%, OOS MAE moved 2.559 → 2.654. This script tests whether the landing
# features are net helpful, net harmful, or noisy by re-running the temporal
# CV with each feature ablated.
#
# Variants tested:
#   baseline (all landing)
#   drop ALL landing (full family)
#   drop each individual landing feature
#
# `has_landing_data` (the coverage flag) is kept in every variant — it's the
# missingness indicator, not a value, and is non-redundant with the data.
#
# Outputs:
#   output/landing_lofo/results.csv
# ─────────────────────────────────────────────────────────────────────────────

setwd("~/Projects/R/college_nfl_model")
suppressMessages({
  library(tidyverse)
  library(tidymodels)
  library(xgboost)
})

source("functions/helpers.R")
source("functions/feature_specs.R")

set.seed(42)

# Pull CV functions from 11_temporal_cv.R without running its body.
src11 <- readLines("11_temporal_cv.R")
fn_start    <- grep("^build_recipe <- function", src11)
load_marker <- grep("^# ── 5. Load data", src11)
eval(parse(text = src11[fn_start[1]:(load_marker - 1)]))

mae_fn <- function(a, p) mean(abs(a - p))
cor_fn <- function(a, p) cor(a, p, use = "complete.obs")

# ── Load data once ────────────────────────────────────────────────────────────

cat("Loading model data...\n")
wr_full <- readRDS("data/wr_model_data.rds") |> attach_comp_features()
rb_full <- readRDS("data/rb_model_data.rds") |> attach_comp_features()

TEST_YEARS  <- 2016:2023
MIN_TRAIN   <- 40
MIN_TEST    <- 5

# ── Run one CV pass with given drop set ──────────────────────────────────────
# Approach: zero out the dropped feature in the data. step_nzv() in the
# recipe then strips it (variance == 0). Same trick used in 14_comp_lofo.R.

run_cv <- function(drop_feats, variant_label) {
  zero_dropped <- function(df) {
    for (col in drop_feats) {
      if (col %in% names(df)) df[[col]] <- 0
    }
    df
  }
  oos_results <- list()
  for (K in TEST_YEARS) {
    for (pos in c("WR", "RB")) {
      full <- if (pos == "WR") wr_full else rb_full
      full <- zero_dropped(full)
      train <- full |> filter(draft_year < K)
      test  <- full |> filter(draft_year == K, has_cfb_data)
      train_cfb <- train |> filter(has_cfb_data,
                                    if (pos == "RB") draft_year >= 2010 else TRUE)
      if (nrow(train_cfb) < MIN_TRAIN || nrow(test) < MIN_TEST) next

      dy_m <- mean(train_cfb$draft_year); dy_s <- sd(train_cfb$draft_year)
      train <- train |> mutate(draft_year_sc = (draft_year - dy_m) / dy_s)
      test  <- test  |> mutate(draft_year_sc = (draft_year - dy_m) / dy_s)

      bust_feats <- if (pos == "WR") WR_BUST_FEATURES else RB_BUST_FEATURES
      prod_feats <- if (pos == "WR") WR_PROD_FEATURES else RB_PROD_FEATURES

      bust_fit <- train_cv_bust(train, bust_feats, pos)
      prod_fit <- if (pos == "WR") train_cv_prod_wr(train, prod_feats)
                  else             train_cv_prod_rb(train, prod_feats)

      base_rate_train <- mean(train_cfb$made_it)
      train_bust_pred <- predict(bust_fit, train_cfb |> mutate(
        made_it = factor(made_it, levels = c(0L, 1L), labels = c("bust", "made_it"))
      ), type = "prob") |> pull(.pred_made_it)
      iso_fit <- isoreg(train_bust_pred, train_cfb$made_it)
      iso     <- list(x = iso_fit$x[iso_fit$ord], y = iso_fit$yf)

      preds <- predict_cv_fold(bust_fit, prod_fit, test, pos,
                                base_rate = base_rate_train, iso = iso)
      oos_results[[paste(pos, K)]] <- preds |>
        select(pfr_player_name, position, draft_year, ppg, exp_ppg)
    }
  }
  oos <- bind_rows(oos_results)
  tibble(
    variant = variant_label,
    n       = nrow(oos),
    mae     = mae_fn(oos$ppg, oos$exp_ppg),
    cor     = cor_fn(oos$ppg, oos$exp_ppg),
    wr_mae  = mae_fn(oos$ppg[oos$position == "WR"],
                      oos$exp_ppg[oos$position == "WR"]),
    rb_mae  = mae_fn(oos$ppg[oos$position == "RB"],
                      oos$exp_ppg[oos$position == "RB"])
  )
}

# ── Variant definitions ──────────────────────────────────────────────────────
# Landing features split by position. expected_depth_rank exists on both sides
# but holds a position-specific value (was renamed from expected_depth_rank_wr/
# expected_depth_rank_rb during attach). Drop both at once when ablating it.

WR_LANDING <- c("vacated_tgt_pct", "incumbent_tgt_share", "n_ret_wr_50tgt",
                "incumbent_wr1_age", "team_targets_prior")
RB_LANDING <- c("vacated_carry_pct", "incumbent_carry_share", "n_ret_rb_100carry",
                "incumbent_rb1_age", "team_carries_prior")
BOTH       <- c("expected_depth_rank")
ALL_LANDING <- c(WR_LANDING, RB_LANDING, BOTH)

variants <- list(
  list(label = "baseline (all landing)",          drop = character()),
  list(label = "drop ALL landing",                drop = ALL_LANDING),
  list(label = "drop vacated_tgt_pct",            drop = "vacated_tgt_pct"),
  list(label = "drop incumbent_tgt_share",        drop = "incumbent_tgt_share"),
  list(label = "drop n_ret_wr_50tgt",             drop = "n_ret_wr_50tgt"),
  list(label = "drop incumbent_wr1_age",          drop = "incumbent_wr1_age"),
  list(label = "drop team_targets_prior",         drop = "team_targets_prior"),
  list(label = "drop vacated_carry_pct",          drop = "vacated_carry_pct"),
  list(label = "drop incumbent_carry_share",      drop = "incumbent_carry_share"),
  list(label = "drop n_ret_rb_100carry",          drop = "n_ret_rb_100carry"),
  list(label = "drop incumbent_rb1_age",          drop = "incumbent_rb1_age"),
  list(label = "drop team_carries_prior",         drop = "team_carries_prior"),
  list(label = "drop expected_depth_rank",        drop = "expected_depth_rank"),
  list(label = "drop ALL WR landing",             drop = c(WR_LANDING, BOTH)),
  list(label = "drop ALL RB landing",             drop = c(RB_LANDING, BOTH))
)

results <- map_dfr(variants, function(v) {
  cat(sprintf("\n══ %s ══\n", v$label))
  res <- run_cv(v$drop, v$label)
  cat(sprintf("  MAE=%.3f  Cor=%.3f  WR_MAE=%.3f  RB_MAE=%.3f\n",
              res$mae, res$cor, res$wr_mae, res$rb_mae))
  res
})

baseline <- results |> dplyr::filter(variant == "baseline (all landing)")
results <- results |> mutate(
  delta_mae    = mae    - baseline$mae,
  delta_cor    = cor    - baseline$cor,
  delta_wr_mae = wr_mae - baseline$wr_mae,
  delta_rb_mae = rb_mae - baseline$rb_mae
)

cat("\n══ Landing LOFO summary (sorted by ΔMAE — negative = dropping helped) ══\n")
results |> arrange(delta_mae) |>
  select(variant, mae, delta_mae, cor, delta_cor,
         wr_mae, delta_wr_mae, rb_mae, delta_rb_mae) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  print(n = Inf, width = Inf)

dir.create("output/landing_lofo", showWarnings = FALSE, recursive = TRUE)
write_csv(results, "output/landing_lofo/results.csv")
cat("\nSaved: output/landing_lofo/results.csv\n")

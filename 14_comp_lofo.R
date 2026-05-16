# 14_comp_lofo.R
# ─────────────────────────────────────────────────────────────────────────────
# Leave-one-feature-out importance for the three comp-stack outputs:
#   - comp_weighted_ppg
#   - comp_median_ppg
#   - comp_bust_rate
#
# For each ablation, re-runs the rolling temporal CV (2016–2023) and records
# the resulting OOS MAE / Pearson correlation. The baseline keeps all three;
# the "drop_all" variant removes the comp-stack entirely (treat has_comp=0
# everywhere, equivalent to no comp signal).
#
# Outputs:
#   output/comp_lofo/results.csv
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

# Reuse the CV helpers from 11_temporal_cv.R — pull just the function defs
# without running the script body.
src11 <- readLines("11_temporal_cv.R")
fn_start <- grep("^build_recipe <- function|^train_cv_bust <- function|^train_cv_prod_wr <- function|^train_cv_prod_rb <- function|^predict_cv_fold <- function", src11)
fn_end   <- grep("^# ── ", src11)
# Pull build_recipe through predict_cv_fold (everything before "# ── 5. Load data ──")
load_marker <- grep("^# ── 5. Load data", src11)
eval(parse(text = src11[fn_start[1]:(load_marker - 1)]))

mae_fn  <- function(a, p) mean(abs(a - p))
cor_fn  <- function(a, p) cor(a, p, use = "complete.obs")

# ── Load data with comp features attached ─────────────────────────────────────

cat("Loading model data...\n")
wr_full <- readRDS("data/wr_model_data.rds") |> attach_comp_features()
rb_full <- readRDS("data/rb_model_data.rds") |> attach_comp_features()

TEST_YEARS  <- 2016:2023
MIN_TRAIN   <- 40
MIN_TEST    <- 5

# ── Run one CV pass with the given feature lists ──────────────────────────────

run_cv <- function(wr_bust_feats, wr_prod_feats, rb_bust_feats, rb_prod_feats,
                   drop_comps,
                   variant_label) {
  # The recipe in build_recipe() references comp_* columns by name in
  # step_mutate. Rather than rewrite the recipe per variant, we zero the
  # dropped comp columns in both train and test data — step_nzv() in the
  # recipe then strips them out automatically (variance == 0).
  zero_dropped <- function(df) {
    for (col in drop_comps) {
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

      bust_feats <- if (pos == "WR") wr_bust_feats else rb_bust_feats
      prod_feats <- if (pos == "WR") wr_prod_feats else rb_prod_feats

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

# ── Variant definitions ───────────────────────────────────────────────────────
# Each variant drops zero or more comp output features from BOTH bust and prod
# spec lists. has_comp_features stays in the spec — it's the missingness flag,
# not a leaked target.

drop_feats <- function(feats, drop) setdiff(feats, drop)

ALL_COMPS <- c("comp_weighted_ppg", "comp_median_ppg", "comp_bust_rate")

variants <- list(
  list(label = "baseline (all 3 comps)", drop = character()),
  list(label = "drop comp_weighted_ppg", drop = "comp_weighted_ppg"),
  list(label = "drop comp_median_ppg",   drop = "comp_median_ppg"),
  list(label = "drop comp_bust_rate",    drop = "comp_bust_rate"),
  list(label = "drop ALL comps",         drop = ALL_COMPS)
)

results <- map_dfr(variants, function(v) {
  cat(sprintf("\n══ %s ══\n", v$label))
  res <- run_cv(
    wr_bust_feats = WR_BUST_FEATURES,
    wr_prod_feats = WR_PROD_FEATURES,
    rb_bust_feats = RB_BUST_FEATURES,
    rb_prod_feats = RB_PROD_FEATURES,
    drop_comps    = v$drop,
    variant_label = v$label
  )
  cat(sprintf("  MAE=%.3f  Cor=%.3f  WR_MAE=%.3f  RB_MAE=%.3f\n",
              res$mae, res$cor, res$wr_mae, res$rb_mae))
  res
})

# Add deltas vs baseline
baseline <- results |> filter(variant == "baseline (all 3 comps)")
results <- results |> mutate(
  delta_mae    = mae    - baseline$mae,
  delta_cor    = cor    - baseline$cor,
  delta_wr_mae = wr_mae - baseline$wr_mae,
  delta_rb_mae = rb_mae - baseline$rb_mae
)

cat("\n══ Comp LOFO summary ══\n")
results |> select(variant, mae, delta_mae, cor, delta_cor,
                  wr_mae, delta_wr_mae, rb_mae, delta_rb_mae) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  print(width = Inf)

dir.create("output/comp_lofo", showWarnings = FALSE, recursive = TRUE)
write_csv(results, "output/comp_lofo/results.csv")
cat("\nSaved: output/comp_lofo/results.csv\n")

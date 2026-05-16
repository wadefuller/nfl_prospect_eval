# 22_age_features_lofo.R
# Quick LOFO on the new age features to see which (if any) improve OOS MAE.
# Same zero-out-the-data trick as 14_comp_lofo.R / 16_landing_lofo.R.

setwd("~/Projects/R/college_nfl_model")
suppressMessages({
  library(tidyverse)
  library(tidymodels)
  library(xgboost)
})

source("functions/helpers.R")
source("functions/feature_specs.R")

set.seed(42)

src11 <- readLines("11_temporal_cv.R")
fn_start    <- grep("^build_recipe <- function", src11)
load_marker <- grep("^# ── 5. Load data", src11)
eval(parse(text = src11[fn_start[1]:(load_marker - 1)]))

mae_fn <- function(a, p) mean(abs(a - p))
cor_fn <- function(a, p) cor(a, p, use = "complete.obs")

wr_full <- readRDS("data/wr_model_data.rds") |> attach_comp_features()
rb_full <- readRDS("data/rb_model_data.rds") |> attach_comp_features()

TEST_YEARS  <- 2016:2023
MIN_TRAIN   <- 40
MIN_TEST    <- 5

run_cv <- function(drop_feats, label) {
  zero_drop <- function(df) {
    for (c in drop_feats) if (c %in% names(df)) df[[c]] <- 0
    df
  }
  oos_results <- list()
  for (K in TEST_YEARS) {
    for (pos in c("WR", "RB")) {
      full <- if (pos == "WR") wr_full else rb_full
      full <- zero_drop(full)
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
    variant = label,
    n       = nrow(oos),
    mae     = mae_fn(oos$ppg, oos$exp_ppg),
    cor     = cor_fn(oos$exp_ppg, oos$ppg),
    wr_mae  = mae_fn(oos$ppg[oos$position == "WR"],
                      oos$exp_ppg[oos$position == "WR"]),
    rb_mae  = mae_fn(oos$ppg[oos$position == "RB"],
                      oos$exp_ppg[oos$position == "RB"])
  )
}

variants <- list(
  list(label = "baseline (all new age feats)",  drop = character()),
  list(label = "drop breakout_age_imputed",     drop = "breakout_age_imputed"),
  list(label = "drop peak_dominator_pre22",     drop = "peak_dominator_pre22"),
  list(label = "drop peak_yards_pre21",         drop = "peak_yards_pre21"),
  list(label = "drop n_seasons_dominant",       drop = "n_seasons_dominant"),
  list(label = "drop dominator_age_resid",      drop = "dominator_age_resid"),
  list(label = "drop dominator_age_z",          drop = "dominator_age_z"),
  list(label = "drop all 5 breakout feats",
       drop = c("breakout_age_imputed", "peak_dominator_pre22",
                 "peak_yards_pre21", "n_seasons_dominant", "has_breakout")),
  list(label = "drop both age-adj dominator",
       drop = c("dominator_age_resid", "dominator_age_z")),
  list(label = "drop ALL new age feats",
       drop = c("breakout_age_imputed", "peak_dominator_pre22",
                 "peak_yards_pre21", "n_seasons_dominant", "has_breakout",
                 "dominator_age_resid", "dominator_age_z"))
)

results <- map_dfr(variants, function(v) {
  cat(sprintf("\n══ %s ══\n", v$label))
  res <- run_cv(v$drop, v$label)
  cat(sprintf("  MAE=%.3f  Cor=%.3f  WR=%.3f  RB=%.3f\n",
              res$mae, res$cor, res$wr_mae, res$rb_mae))
  res
})

baseline <- results |> dplyr::filter(variant == "baseline (all new age feats)")
results <- results |> mutate(
  delta_mae    = mae    - baseline$mae,
  delta_wr_mae = wr_mae - baseline$wr_mae,
  delta_rb_mae = rb_mae - baseline$rb_mae
)

cat("\n══ LOFO summary (sorted by ΔMAE — negative = dropping helped) ══\n")
results |> arrange(delta_mae) |>
  select(variant, mae, delta_mae, wr_mae, delta_wr_mae, rb_mae, delta_rb_mae) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  print(n = Inf, width = Inf)

dir.create("output/age_lofo", showWarnings = FALSE, recursive = TRUE)
write_csv(results, "output/age_lofo/results.csv")

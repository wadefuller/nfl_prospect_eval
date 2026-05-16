# 17_year_cutoff_sweep.R
# ─────────────────────────────────────────────────────────────────────────────
# Sweep training-data start year for WR / RB independently to test whether
# the older era (sparse coverage on PBP / PPA / mocks / landing) helps or
# hurts OOS MAE when used as training data.
#
# Currently:
#   WR: trains on all years (2002+), pre-2010 rows weighted at 0.4
#   RB: trains on 2010+ only (already excludes pre-2010)
#
# We test cutoffs WR ∈ {2002, 2010, 2012, 2014, 2016} and
# RB ∈ {2010, 2012, 2014, 2016}.
#
# Outputs:
#   output/year_cutoff_sweep/results.csv
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

src11 <- readLines("11_temporal_cv.R")
fn_start    <- grep("^build_recipe <- function", src11)
load_marker <- grep("^# ── 5. Load data", src11)
eval(parse(text = src11[fn_start[1]:(load_marker - 1)]))

mae_fn <- function(a, p) mean(abs(a - p))
cor_fn <- function(a, p) cor(a, p, use = "complete.obs")

cat("Loading model data...\n")
wr_full <- readRDS("data/wr_model_data.rds") |> attach_comp_features()
rb_full <- readRDS("data/rb_model_data.rds") |> attach_comp_features()

TEST_YEARS  <- 2016:2023
MIN_TRAIN   <- 40
MIN_TEST    <- 5

# Run CV with explicit per-position year cutoffs.
run_cv <- function(wr_min_year, rb_min_year, label) {
  oos_results <- list()
  for (K in TEST_YEARS) {
    for (pos in c("WR", "RB")) {
      full <- if (pos == "WR") wr_full else rb_full
      min_yr <- if (pos == "WR") wr_min_year else rb_min_year
      train <- full |> filter(draft_year < K, draft_year >= min_yr)
      test  <- full |> filter(draft_year == K, has_cfb_data)
      train_cfb <- train |> filter(has_cfb_data)
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
    label   = label,
    n       = nrow(oos),
    mae     = mae_fn(oos$ppg, oos$exp_ppg),
    cor     = cor_fn(oos$ppg, oos$exp_ppg),
    wr_mae  = mae_fn(oos$ppg[oos$position == "WR"],
                      oos$exp_ppg[oos$position == "WR"]),
    rb_mae  = mae_fn(oos$ppg[oos$position == "RB"],
                      oos$exp_ppg[oos$position == "RB"]),
    wr_n    = sum(oos$position == "WR"),
    rb_n    = sum(oos$position == "RB")
  )
}

# ── Cutoff sweep ──────────────────────────────────────────────────────────────
# Hold one position constant at current cutoff while varying the other.

cur_wr <- 2002  # current effective (pre-2010 downweighted but kept)
cur_rb <- 2010  # current

variants <- list(
  list(label = "baseline (WR 2002, RB 2010)",  wr = 2002, rb = 2010),
  list(label = "WR 2010, RB 2010",             wr = 2010, rb = 2010),
  list(label = "WR 2012, RB 2010",             wr = 2012, rb = 2010),
  list(label = "WR 2014, RB 2010",             wr = 2014, rb = 2010),
  list(label = "WR 2002, RB 2012",             wr = 2002, rb = 2012),
  list(label = "WR 2002, RB 2014",             wr = 2002, rb = 2014),
  list(label = "WR 2010, RB 2012",             wr = 2010, rb = 2012),
  list(label = "WR 2010, RB 2014",             wr = 2010, rb = 2014),
  list(label = "WR 2012, RB 2012",             wr = 2012, rb = 2012),
  list(label = "WR 2014, RB 2014",             wr = 2014, rb = 2014)
)

results <- map_dfr(variants, function(v) {
  cat(sprintf("\n══ %s ══\n", v$label))
  res <- run_cv(v$wr, v$rb, v$label)
  cat(sprintf("  MAE=%.3f  Cor=%.3f  WR=%.3f  RB=%.3f  (n_wr=%d, n_rb=%d)\n",
              res$mae, res$cor, res$wr_mae, res$rb_mae, res$wr_n, res$rb_n))
  res
})

baseline <- results |> dplyr::filter(label == "baseline (WR 2002, RB 2010)")
results <- results |> mutate(
  delta_mae    = mae    - baseline$mae,
  delta_wr_mae = wr_mae - baseline$wr_mae,
  delta_rb_mae = rb_mae - baseline$rb_mae
)

cat("\n══ Year-cutoff sweep results (sorted by ΔMAE) ══\n")
results |> arrange(delta_mae) |>
  select(label, mae, delta_mae, wr_mae, delta_wr_mae, rb_mae, delta_rb_mae) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  print(n = Inf, width = Inf)

dir.create("output/year_cutoff_sweep", showWarnings = FALSE, recursive = TRUE)
write_csv(results, "output/year_cutoff_sweep/results.csv")
cat("\nSaved: output/year_cutoff_sweep/results.csv\n")

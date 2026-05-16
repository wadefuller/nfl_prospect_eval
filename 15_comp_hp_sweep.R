# 15_comp_hp_sweep.R
# ─────────────────────────────────────────────────────────────────────────────
# Sweep the comp-stack hyperparameters (n_comps, bandwidth, pick_window)
# and pick the configuration that minimises rolling temporal CV MAE.
#
# Strategy:
#   1. For each (n_comps, bandwidth, pick_window) configuration, rebuild
#      data/comp_features.rds with that config.
#   2. Re-attach comp features to wr/rb_model_data and run the temporal CV.
#   3. Record CV MAE / Cor / position breakdowns.
#
# The vectorized Mahalanobis means each comp build is ~1.4s; the dominant
# cost is the CV harness (~7-10s/config). A 27-cell grid takes ~5-7 min.
#
# Outputs:
#   output/comp_hp_sweep/results.csv
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

# ── Pull CV functions from 11_temporal_cv.R (without running its body) ───────

src11 <- readLines("11_temporal_cv.R")
fn_start    <- grep("^build_recipe <- function", src11)
load_marker <- grep("^# ── 5. Load data", src11)
eval(parse(text = src11[fn_start[1]:(load_marker - 1)]))

# Pull the comp-build helpers from 08b_build_comp_features.R (functions only,
# not the run body — we'll call build_comp_features_for_position() ourselves).

src08b <- readLines("08b_build_comp_features.R")
build_marker <- grep("^# ── Run", src08b)
# Eval everything up to the "── Run" section
eval(parse(text = src08b[seq_len(build_marker[1] - 1)]))

mae_fn  <- function(a, p) mean(abs(a - p))
cor_fn  <- function(a, p) cor(a, p, use = "complete.obs")

# ── Load model data once ──────────────────────────────────────────────────────

cat("Loading model data...\n")
wr_raw <- readRDS("data/wr_model_data.rds")
rb_raw <- readRDS("data/rb_model_data.rds")

TEST_YEARS  <- 2016:2023
MIN_TRAIN   <- 40
MIN_TEST    <- 5

# ── Run one CV pass given pre-attached comp features ─────────────────────────

run_cv <- function(wr_full, rb_full) {
  oos_results <- list()
  for (K in TEST_YEARS) {
    for (pos in c("WR", "RB")) {
      full <- if (pos == "WR") wr_full else rb_full
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
    n      = nrow(oos),
    mae    = mae_fn(oos$ppg, oos$exp_ppg),
    cor    = cor_fn(oos$ppg, oos$exp_ppg),
    wr_mae = mae_fn(oos$ppg[oos$position == "WR"],
                     oos$exp_ppg[oos$position == "WR"]),
    rb_mae = mae_fn(oos$ppg[oos$position == "RB"],
                     oos$exp_ppg[oos$position == "RB"])
  )
}

# ── Build comps for one config, attach, run CV ───────────────────────────────

evaluate_config <- function(n_comps, bandwidth, pick_window) {
  cfg <- list(
    weights       = c(measurables = 1.0, production = 1.5, profile = 1.0, context = 0.5),
    n_comps       = n_comps,
    bandwidth     = bandwidth,
    pick_window   = pick_window,
    min_pool_size = 30
  )

  wr_comps <- build_comp_features_for_position(wr_raw, WR_COMP_FEATURES, "WR", cfg)
  rb_comps <- build_comp_features_for_position(rb_raw, RB_COMP_FEATURES, "RB", cfg)
  all_comps <- bind_rows(wr_comps, rb_comps)

  # Attach by writing a temp comp_features.rds that attach_comp_features reads
  saveRDS(all_comps, "data/comp_features.rds")
  wr_full <- wr_raw |> attach_comp_features()
  rb_full <- rb_raw |> attach_comp_features()
  run_cv(wr_full, rb_full)
}

# ── Sweep grid ────────────────────────────────────────────────────────────────

grid <- expand_grid(
  n_comps     = c(8, 15, 25),
  bandwidth   = c(1.0, 2.0, 4.0),
  pick_window = c(40, 60, 100)
)

cat(sprintf("Evaluating %d configurations...\n", nrow(grid)))

# Stash the original comp_features.rds so we can restore it after the sweep
file.copy("data/comp_features.rds", "data/comp_features.rds.pre_sweep",
          overwrite = TRUE)

results <- pmap_dfr(grid, function(n_comps, bandwidth, pick_window) {
  cat(sprintf("\n── n_comps=%d, bandwidth=%.1f, pick_window=%d ──\n",
              n_comps, bandwidth, pick_window))
  invisible(capture.output(  # silence the inner build_comp loop
    res <- evaluate_config(n_comps, bandwidth, pick_window)
  ))
  cat(sprintf("  MAE=%.3f  Cor=%.3f  WR=%.3f  RB=%.3f\n",
              res$mae, res$cor, res$wr_mae, res$rb_mae))
  bind_cols(tibble(n_comps = n_comps, bandwidth = bandwidth,
                    pick_window = pick_window), res)
})

# Restore the pre-sweep comp_features.rds
file.copy("data/comp_features.rds.pre_sweep", "data/comp_features.rds",
          overwrite = TRUE)
file.remove("data/comp_features.rds.pre_sweep")

# ── Summary ───────────────────────────────────────────────────────────────────

cat("\n══ Hyperparameter sweep results (sorted by MAE) ══\n")
results |> arrange(mae) |>
  mutate(across(c(mae, cor, wr_mae, rb_mae), ~ round(.x, 3))) |>
  print(n = Inf, width = Inf)

best <- results |> arrange(mae) |> dplyr::slice(1)
cat(sprintf("\n══ BEST: n_comps=%d, bandwidth=%.1f, pick_window=%d ══\n",
            best$n_comps, best$bandwidth, best$pick_window))
cat(sprintf("  MAE=%.3f  Cor=%.3f  WR=%.3f  RB=%.3f\n",
            best$mae, best$cor, best$wr_mae, best$rb_mae))

dir.create("output/comp_hp_sweep", showWarnings = FALSE, recursive = TRUE)
write_csv(results, "output/comp_hp_sweep/results.csv")
cat("\nSaved: output/comp_hp_sweep/results.csv\n")

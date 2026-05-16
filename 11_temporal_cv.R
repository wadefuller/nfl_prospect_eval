# 11_temporal_cv.R
# ─────────────────────────────────────────────────────────────────────────────
# Rolling temporal cross-validation — the standard model evaluation.
#
# For each test year K in 2016:2023:
#   Train bust + production models on draft_year < K
#   Predict on draft_year == K
#   Compute hurdle: exp_ppg = p_made_it × exp(log_ppg_pred)
#
# Uses FIXED hyperparameters (close to the tuned production values) so each
# fold trains in seconds, not minutes. This evaluates the model SPECIFICATION
# (feature set + architecture + recipe), not the per-run tuning noise.
#
# Year cutoffs mirror the production pipeline:
#   WR bust:  all available years (2002+), pre-2010 weighted 0.4
#   WR prod:  all available years (2002+), producers only
#   RB bust:  2010+ (pre-2010 feature coverage too sparse)
#   RB prod:  2010+ quantile model (τ = 0.70 pinball loss)
#
# Outputs (all in output/temporal_cv/):
#   oos_predictions.csv / .rds   — all OOS predictions with actuals
#   metrics_summary.csv          — per-year + overall + by-position MAE/RMSE/Cor
#   01_pred_vs_actual.png        — scatter: predicted vs actual PPG
#   02_calibration.png           — hit-rate calibration by prob bucket
#   03_per_year_mae.png          — MAE trend by test year
#   04_residuals_by_round.png    — residual boxplots by draft round
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(tidymodels)
library(xgboost)

set.seed(42)

setwd("~/Projects/R/college_nfl_model")
source("functions/helpers.R")

dir.create("output/temporal_cv", showWarnings = FALSE, recursive = TRUE)

# ── 1. Feature sets (mirrored exactly from 04_model_bust.R / 05_model_production.R) ──

source("functions/feature_specs.R")

# ── 2. Build era-aware recipe ──────────────────────────────────────────────────
# Mirrors the step_mutate zero-filling from 04/05 so pre-2016 players get 0
# for PPA/usage rather than a post-2016-dominated imputed median.

build_recipe <- function(model_df, outcome, pos) {
  has_comp <- "has_comp_features" %in% names(model_df)
  if (pos == "WR") {
    rec <- recipe(as.formula(paste(outcome, "~ .")), data = model_df) |>
      update_role(draft_year, new_role = "ID") |>
      step_mutate(
        avg_PPA_pass        = if_else(has_ppa    == 0L, 0, avg_PPA_pass),
        total_PPA_pass      = if_else(has_ppa    == 0L, 0, total_PPA_pass),
        usg_pass            = if_else(has_usage  == 0L, 0, usg_pass),
        usg_passing_downs   = if_else(has_usage  == 0L, 0, usg_passing_downs),
        catch_rate_wr       = if_else(has_wr_pbp == 0L, 0, catch_rate_wr),
        yards_per_target_wr = if_else(has_wr_pbp == 0L, 0, yards_per_target_wr),
        yards_per_rec_wr    = if_else(has_wr_pbp == 0L, 0, yards_per_rec_wr),
        explosive_rec_rate  = if_else(has_wr_pbp == 0L, 0, explosive_rec_rate),
        target_share_wr     = if_else(has_wr_pbp == 0L, 0, target_share_wr),
        targets_per_game_wr = if_else(has_wr_pbp == 0L, 0, targets_per_game_wr),
        epa_per_target_wr   = if_else(has_wr_pbp == 0L, 0, epa_per_target_wr),
        epa_per_play_wr_pbp = if_else(has_wr_pbp == 0L, 0, epa_per_play_wr_pbp)
      )
    if (has_comp) {
      rec <- rec |> step_mutate(
        comp_weighted_ppg = if_else(has_comp_features == 0L, 0, comp_weighted_ppg),
        comp_bust_rate    = if_else(has_comp_features == 0L, 0, comp_bust_rate)
      )
    }
    rec |>
      step_unknown(all_nominal_predictors(), new_level = "unknown") |>
      step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
      step_impute_median(all_numeric_predictors()) |>
      step_nzv(all_predictors())
  } else {
    rec <- recipe(as.formula(paste(outcome, "~ .")), data = model_df) |>
      update_role(draft_year, new_role = "ID") |>
      step_mutate(
        avg_PPA_rush      = if_else(has_ppa   == 0L, 0, avg_PPA_rush),
        total_PPA_rush    = if_else(has_ppa   == 0L, 0, total_PPA_rush),
        avg_PPA_all       = if_else(has_ppa   == 0L, 0, avg_PPA_all),
        total_PPA_all     = if_else(has_ppa   == 0L, 0, total_PPA_all),
        usg_rush          = if_else(has_usage == 0L, 0, usg_rush),
        usg_pass          = if_else(has_usage == 0L, 0, usg_pass),
        usg_overall       = if_else(has_usage == 0L, 0, usg_overall),
        usg_passing_downs = if_else(has_usage == 0L, 0, usg_passing_downs),
        explosive_rate    = if_else(has_pbp == 0L, 0, explosive_rate),
        breakaway_rate    = if_else(has_pbp == 0L, 0, breakaway_rate),
        target_share      = if_else(has_pbp == 0L, 0, target_share),
        targets_per_game  = if_else(has_pbp == 0L, 0, targets_per_game),
        catch_rate        = if_else(has_pbp == 0L, 0, catch_rate),
        epa_per_rush      = if_else(has_pbp == 0L, 0, epa_per_rush),
        epa_per_play_pbp  = if_else(has_pbp == 0L, 0, epa_per_play_pbp)
      )
    if (has_comp) {
      rec <- rec |> step_mutate(
        comp_weighted_ppg = if_else(has_comp_features == 0L, 0, comp_weighted_ppg),
        comp_bust_rate    = if_else(has_comp_features == 0L, 0, comp_bust_rate)
      )
    }
    rec |>
      step_unknown(all_nominal_predictors(), new_level = "unknown") |>
      step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
      step_impute_median(all_numeric_predictors()) |>
      step_nzv(all_predictors())
  }
}

# ── 3. Training functions ──────────────────────────────────────────────────────

# 3a. Bust model (XGBoost classification via tidymodels workflow)
# Fixed hyperparams chosen to approximate the tuned production values.
# Note: the production WR bust model uses importance_weights(0.4) for pre-2010
# classes. We omit case_weights here — they have a small effect and adding them
# via add_case_weights() interacts poorly with the fixed-hp workflow approach.
# The CV result is therefore very slightly pessimistic for WR, which is fine.
train_cv_bust <- function(train_data, features, pos) {
  year_filter <- if (pos == "RB") quote(has_cfb_data & draft_year >= 2010) else
                                  quote(has_cfb_data)
  model_df <- train_data |>
    filter(!!year_filter) |>
    mutate(made_it = factor(made_it, levels = c(0L, 1L), labels = c("bust", "made_it")))

  av       <- intersect(features, names(model_df))
  model_df <- model_df |> select(all_of(c("made_it", av, "draft_year")))

  rec  <- build_recipe(model_df, "made_it", pos)
  spec <- boost_tree(trees = 800, tree_depth = 4, learn_rate = 0.02,
                     min_n = 8, sample_size = 0.8) |>
    set_engine("xgboost",
               scale_pos_weight = sum(model_df$made_it == "bust") /
                                  sum(model_df$made_it == "made_it"),
               nthread = 4) |>
    set_mode("classification")

  fit(workflow() |> add_recipe(rec) |> add_model(spec), model_df)
}

# 3b. WR production model (XGBoost regression via tidymodels workflow)
# Trained on producers only (ppg > 0), log(ppg) as outcome.
train_cv_prod_wr <- function(train_data, features) {
  model_df <- train_data |>
    filter(has_cfb_data, ppg > 0) |>
    mutate(log_ppg = log(ppg))

  av <- intersect(features, names(model_df))
  model_df <- model_df |> select(all_of(c("log_ppg", av, "draft_year")))

  rec  <- build_recipe(model_df, "log_ppg", "WR")
  spec <- boost_tree(
    trees = 800, tree_depth = 3, learn_rate = 0.02,
    min_n = 8, sample_size = 0.8
  ) |>
    set_engine("xgboost", nthread = 4) |>
    set_mode("regression")

  wf <- workflow() |> add_recipe(rec) |> add_model(spec)
  fit(wf, model_df)
}

# 3c. RB production model (raw XGBoost with τ=0.70 pinball loss)
# Mirrors train_quantile_rb_model() from 05_model_production.R.
# τ=0.70 chosen via 12_tau_sweep.R (2026-04-22).
train_cv_prod_rb <- function(train_data, features, tau = 0.70) {
  model_df <- train_data |>
    filter(has_cfb_data, draft_year >= 2010, ppg > 0) |>
    mutate(log_ppg = log(ppg))

  av <- intersect(features, names(model_df))

  rec <- recipe(log_ppg ~ .,
                data = model_df |> select(all_of(c("log_ppg", av)))) |>
    step_unknown(all_nominal_predictors(), new_level = "unknown") |>
    step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
    step_mutate(
      avg_PPA_rush      = if_else(has_ppa   == 0L, 0, avg_PPA_rush),
      total_PPA_rush    = if_else(has_ppa   == 0L, 0, total_PPA_rush),
      avg_PPA_all       = if_else(has_ppa   == 0L, 0, avg_PPA_all),
      total_PPA_all     = if_else(has_ppa   == 0L, 0, total_PPA_all),
      usg_rush          = if_else(has_usage == 0L, 0, usg_rush),
      usg_pass          = if_else(has_usage == 0L, 0, usg_pass),
      usg_overall       = if_else(has_usage == 0L, 0, usg_overall),
      usg_passing_downs = if_else(has_usage == 0L, 0, usg_passing_downs),
      # PBP features (2014+) — zero-fill for pre-era via has_pbp
      explosive_rate    = if_else(has_pbp == 0L, 0, explosive_rate),
      breakaway_rate    = if_else(has_pbp == 0L, 0, breakaway_rate),
      target_share      = if_else(has_pbp == 0L, 0, target_share),
      targets_per_game  = if_else(has_pbp == 0L, 0, targets_per_game),
      catch_rate        = if_else(has_pbp == 0L, 0, catch_rate),
      epa_per_rush      = if_else(has_pbp == 0L, 0, epa_per_rush),
      epa_per_play_pbp  = if_else(has_pbp == 0L, 0, epa_per_play_pbp),
      # Comp-stack features — zero-fill for players without comp coverage
      comp_weighted_ppg = if_else(has_comp_features == 0L, 0, comp_weighted_ppg),
      comp_bust_rate    = if_else(has_comp_features == 0L, 0, comp_bust_rate)
    ) |>
    step_impute_median(all_numeric_predictors()) |>
    step_nzv(all_predictors())

  prep_rec <- prep(rec, training = model_df |> select(all_of(c("log_ppg", av))))
  baked    <- bake(prep_rec, new_data = NULL)

  X <- as.matrix(baked |> select(-log_ppg))
  y <- baked$log_ppg

  pinball_obj <- function(preds, dtrain) {
    err  <- getinfo(dtrain, "label") - preds
    grad <- ifelse(err >= 0, -tau, 1 - tau)
    hess <- rep(tau * (1 - tau), length(preds))
    list(grad = grad, hess = hess)
  }

  model <- xgb.train(
    params  = list(eta = 0.02, max_depth = 4, min_child_weight = 8,
                   subsample = 0.8, colsample_bytree = 0.7,
                   lambda = 1.0, alpha = 0.1, nthread = 4),
    data    = xgb.DMatrix(X, label = y),
    nrounds = 500,
    obj     = pinball_obj,
    verbose = 0
  )

  list(fit = model, recipe = prep_rec, type = "quantile")
}

# ── 4. Prediction function ─────────────────────────────────────────────────────
# Returns test_data with p_made_it and exp_ppg columns added.

predict_cv_fold <- function(bust_fit, prod_fit, test_data, pos,
                             base_rate = NULL, iso = NULL) {
  # Bust model expects made_it as a factor — add it so the workflow can bake
  td <- test_data |>
    mutate(made_it = factor(made_it, levels = c(0L, 1L), labels = c("bust", "made_it")))

  p_made_it <- predict(bust_fit, td, type = "prob") |> pull(.pred_made_it)

  if (isTRUE(prod_fit$type == "quantile")) {
    # RB quantile model: raw xgboost — need to bake then predict manually
    baked        <- bake(prod_fit$recipe, new_data = td)
    X            <- as.matrix(baked |> select(-any_of("log_ppg")))
    log_ppg_pred <- predict(prod_fit$fit, xgb.DMatrix(X))
  } else {
    # WR production model: fitted tidymodels workflow — predict directly (not $fit)
    log_ppg_pred <- predict(prod_fit, td) |> pull(.pred)
  }

  # ── Hurdle-probability combiner (tuned via 13_bust_tune.R, LOFO 2026-04-28) ──
  #   WR: p_eff = clip(0.85 * sqrt(p))
  #   RB: p_eff = clip(iso(p) - 0.05)   (iso fit on this fold's training data)
  # Mirrors helpers.R::score_class().
  clip01 <- function(x) pmax(0, pmin(1, x))
  iso_apply <- function(p, iso) {
    if (is.null(iso)) return(p)
    approx(iso$x, iso$y,
           xout = pmax(pmin(p, max(iso$x)), min(iso$x)),
           rule = 2, ties = "ordered")$y
  }
  p_eff <- if (pos == "RB") {
    clip01(iso_apply(p_made_it, iso) - 0.05)
  } else {
    clip01(0.85 * sqrt(p_made_it))
  }

  test_data |>
    mutate(
      p_made_it = p_made_it,                # raw classifier for diagnostics
      p_eff     = p_eff,                    # combiner output used in product
      exp_ppg   = pmax(p_eff * exp(log_ppg_pred), 0)
    )
}

# ── 5. Load data ───────────────────────────────────────────────────────────────

cat("Loading model data...\n")
wr_full <- readRDS("data/wr_model_data.rds") |> attach_comp_features()
rb_full <- readRDS("data/rb_model_data.rds") |> attach_comp_features()

# ── 6. Rolling temporal CV ─────────────────────────────────────────────────────

TEST_YEARS  <- 2016:2023
MIN_TRAIN   <- 40   # minimum training rows (with CFB data)
MIN_TEST    <- 5    # minimum test rows

cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║  ROLLING TEMPORAL CV: Train <K, Test K (2016–2023)          ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

oos_results <- list()

for (K in TEST_YEARS) {
  cat(sprintf("── Test year %d ──────────────────────────────\n", K))

  for (pos in c("WR", "RB")) {
    full <- if (pos == "WR") wr_full else rb_full

    train <- full |> filter(draft_year < K)
    test  <- full |> filter(draft_year == K, has_cfb_data)

    # Guard: sufficient training and test data
    train_cfb <- train |> filter(has_cfb_data,
                                  if (pos == "RB") draft_year >= 2010 else TRUE)
    if (nrow(train_cfb) < MIN_TRAIN || nrow(test) < MIN_TEST) {
      cat(sprintf("  %s: skip (train_cfb=%d, test=%d)\n",
                  pos, nrow(train_cfb), nrow(test)))
      next
    }

    n_prod <- sum(train_cfb$ppg > 0)
    cat(sprintf("  %s: train=%d (producers=%d), test=%d\n",
                pos, nrow(train_cfb), n_prod, nrow(test)))

    # Recompute draft_year_sc from training distribution (fold-specific)
    dy_m <- mean(train_cfb$draft_year)
    dy_s <- sd(train_cfb$draft_year)
    train <- train |> mutate(draft_year_sc = (draft_year - dy_m) / dy_s)
    test  <- test  |> mutate(draft_year_sc = (draft_year - dy_m) / dy_s)

    # Train
    bust_features <- if (pos == "WR") WR_BUST_FEATURES else RB_BUST_FEATURES
    prod_features <- if (pos == "WR") WR_PROD_FEATURES else RB_PROD_FEATURES

    bust_fit <- train_cv_bust(train, bust_features, pos)
    prod_fit <- if (pos == "WR") {
      train_cv_prod_wr(train, prod_features)
    } else {
      train_cv_prod_rb(train, prod_features)
    }

    # Predict — pass per-fold isotonic recalibration of bust predictions
    # (used by the RB combiner). WR ignores it.
    base_rate_train <- mean(train_cfb$made_it)
    train_bust_pred <- predict(bust_fit, train_cfb |> mutate(
      made_it = factor(made_it, levels = c(0L, 1L), labels = c("bust", "made_it"))
    ), type = "prob") |> pull(.pred_made_it)
    iso_fit <- isoreg(train_bust_pred, train_cfb$made_it)
    iso     <- list(x = iso_fit$x[iso_fit$ord], y = iso_fit$yf)

    preds <- predict_cv_fold(bust_fit, prod_fit, test, pos,
                              base_rate = base_rate_train, iso = iso)

    key <- paste(pos, K)
    oos_results[[key]] <- preds |>
      mutate(test_year = K) |>
      select(pfr_player_name, position, draft_year, test_year,
             round, pick, college, tier,
             made_it, ppg, p_made_it, exp_ppg)
  }
}

# ── 7. Combine OOS predictions ─────────────────────────────────────────────────

oos <- bind_rows(oos_results) |>
  mutate(
    hit      = made_it == 1L,
    residual = ppg - exp_ppg,
    round_grp = case_when(
      as.integer(round) == 1L ~ "Round 1",
      as.integer(round) == 2L ~ "Round 2",
      as.integer(round) >= 3L ~ "Round 3+"
    ) |> factor(levels = c("Round 1", "Round 2", "Round 3+"))
  )

cat(sprintf("\nTotal OOS predictions: %d players (%d WR, %d RB)\n",
            nrow(oos),
            sum(oos$position == "WR"),
            sum(oos$position == "RB")))
cat(sprintf("Test years: %s\n", paste(sort(unique(oos$test_year)), collapse = ", ")))

# ── 8. Metrics ─────────────────────────────────────────────────────────────────

mae_fn  <- function(a, p) mean(abs(a - p))
rmse_fn <- function(a, p) sqrt(mean((a - p)^2))
cor_fn  <- function(a, p) cor(a, p, use = "complete.obs")
bias_fn <- function(a, p) mean(a - p)

# Overall
cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║  RESULTS                                                     ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

# By position
pos_metrics <- oos |>
  group_by(position) |>
  summarise(
    n    = n(),
    mae  = round(mae_fn(ppg, exp_ppg), 3),
    rmse = round(rmse_fn(ppg, exp_ppg), 3),
    cor  = round(cor_fn(ppg, exp_ppg), 3),
    bias = round(bias_fn(ppg, exp_ppg), 3),
    .groups = "drop"
  )

overall <- oos |>
  summarise(
    position = "ALL",
    n    = n(),
    mae  = round(mae_fn(ppg, exp_ppg), 3),
    rmse = round(rmse_fn(ppg, exp_ppg), 3),
    cor  = round(cor_fn(ppg, exp_ppg), 3),
    bias = round(bias_fn(ppg, exp_ppg), 3)
  )

summary_table <- bind_rows(pos_metrics, overall)
cat("Overall and by-position:\n")
print(summary_table)

# Per test year
cat("\nPer test year:\n")
year_metrics <- oos |>
  group_by(test_year) |>
  summarise(
    n   = n(),
    mae = round(mae_fn(ppg, exp_ppg), 3),
    cor = round(cor_fn(ppg, exp_ppg), 3),
    wr_mae = round(mae_fn(ppg[position=="WR"], exp_ppg[position=="WR"]), 3),
    rb_mae = round(mae_fn(ppg[position=="RB"], exp_ppg[position=="RB"]), 3),
    .groups = "drop"
  )
print(year_metrics, n = 10)

# Bust accuracy
bust_acc <- oos |>
  summarise(
    overall     = mean(round(p_made_it) == made_it),
    wr          = mean(round(p_made_it[position=="WR"]) == made_it[position=="WR"]),
    rb          = mean(round(p_made_it[position=="RB"]) == made_it[position=="RB"])
  )
cat(sprintf("\nBust classification accuracy (threshold=0.5): ALL=%.1f%%  WR=%.1f%%  RB=%.1f%%\n",
            bust_acc$overall*100, bust_acc$wr*100, bust_acc$rb*100))

# Compare with in-sample if available
if (file.exists("output/all_class_scores.csv")) {
  is_scores <- tryCatch(
    read_csv("output/all_class_scores.csv", show_col_types = FALSE),
    error = function(e) NULL
  )
  if (!is.null(is_scores)) {
    is21 <- is_scores |>
      filter(draft_year %in% 2021:2023, !is.na(actual_ppg))
    if (nrow(is21) > 0) {
      cat(sprintf("\n  %-42s  MAE    Cor    N\n", "Method"))
      cat(sprintf("  %-42s  ─────  ─────  ────\n", ""))
      cat(sprintf("  %-42s  %.3f  %.3f  %d\n",
                  "In-sample (2021–23, in training)",
                  mean(abs(is21$exp_ppg - is21$actual_ppg)),
                  cor(is21$exp_ppg, is21$actual_ppg),
                  nrow(is21)))
      cat(sprintf("  %-42s  %.3f  %.3f  %d\n",
                  sprintf("Rolling temporal CV (%d–%d)",
                          min(oos$test_year), max(oos$test_year)),
                  overall$mae, overall$cor, overall$n))
    }
  }
}

# ── 9. Save outputs ────────────────────────────────────────────────────────────

write_csv(oos,          "output/temporal_cv/oos_predictions.csv")
saveRDS(oos,            "output/temporal_cv/oos_predictions.rds")
write_csv(year_metrics, "output/temporal_cv/metrics_by_year.csv")
write_csv(summary_table,"output/temporal_cv/metrics_summary.csv")

cat("\nSaved: output/temporal_cv/oos_predictions.csv\n")
cat("Saved: output/temporal_cv/metrics_summary.csv\n")

# ── 10. Diagnostic plots ───────────────────────────────────────────────────────

theme_nfl <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background   = element_rect(fill = "#1a1a2e", color = NA),
      panel.background  = element_rect(fill = "#16213e", color = NA),
      panel.grid.major  = element_line(color = "#2a2a4a", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      text              = element_text(color = "#c8ccd4"),
      plot.title        = element_text(color = "#ffffff", size = 15, face = "bold",
                                       margin = margin(b = 4)),
      plot.subtitle     = element_text(color = "#8b9ab5", size = 11,
                                       margin = margin(b = 12)),
      plot.caption      = element_text(color = "#5a6a85", size = 9),
      axis.text         = element_text(color = "#8b9ab5"),
      axis.title        = element_text(color = "#c8ccd4"),
      strip.text        = element_text(color = "#ffffff", face = "bold"),
      strip.background  = element_rect(fill = "#0f3460", color = NA),
      legend.background = element_rect(fill = "#1a1a2e", color = NA),
      legend.text       = element_text(color = "#c8ccd4"),
      legend.title      = element_text(color = "#ffffff"),
      plot.margin       = margin(16, 16, 12, 16)
    )
}

WIN_COLOR  <- "#22c55e"
LOSS_COLOR <- "#ef4444"
GOLD_COLOR <- "#f59e0b"
BLUE_COLOR <- "#3b82f6"
MUTED      <- "#8b9ab5"

# ── Plot 1: Predicted vs Actual scatter ──────────────────────────────────────

label_data <- oos |>
  filter(abs(residual) > 4 | (hit & ppg > 14)) |>
  mutate(last_name = str_extract(pfr_player_name, "\\S+$"))

p1 <- oos |>
  ggplot(aes(x = exp_ppg, y = ppg)) +
  geom_abline(slope = 1, intercept = 0, color = MUTED, linetype = "dashed",
              linewidth = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = GOLD_COLOR, fill = GOLD_COLOR,
              alpha = 0.15, linewidth = 1) +
  geom_point(aes(color = hit, size = round == 1), alpha = 0.70) +
  ggrepel::geom_text_repel(
    data = label_data,
    aes(label = last_name),
    color = "#c8ccd4", size = 2.8, max.overlaps = 12,
    segment.color = MUTED, segment.alpha = 0.5
  ) +
  scale_color_manual(values = c("FALSE" = LOSS_COLOR, "TRUE" = WIN_COLOR),
                     labels = c("FALSE" = "Bust", "TRUE" = "Hit"),
                     name = "Outcome") +
  scale_size_manual(values  = c("FALSE" = 2.2, "TRUE" = 3.8),
                    labels  = c("FALSE" = "Rd 2+", "TRUE" = "Rd 1"),
                    name = "Draft") +
  facet_wrap(~position) +
  labs(
    title    = "Rolling Temporal CV: Predicted vs Actual PPG",
    subtitle = sprintf("OOS predictions %d–%d (N=%d) · train <K, test =K · dashed = perfect calibration",
                       min(oos$test_year), max(oos$test_year), nrow(oos)),
    x = "Expected PPG (bust-adjusted)", y = "Actual PPG",
    caption  = "Busts counted as 0 PPG · ggrepel labels = largest misses + star players"
  ) +
  theme_nfl()

ggsave("output/temporal_cv/01_pred_vs_actual.png", p1,
       width = 11, height = 6, dpi = 150)
message("Saved: 01_pred_vs_actual.png")

# ── Plot 2: Hit-rate calibration ──────────────────────────────────────────────

calibration <- oos |>
  mutate(prob_bucket = cut(p_made_it,
                           breaks = c(0, 0.65, 0.72, 0.78, 0.83, 0.87, 0.91, 1.0),
                           include.lowest = TRUE)) |>
  group_by(position, prob_bucket) |>
  summarise(n = n(), pred_prob = mean(p_made_it), obs_rate = mean(hit),
            .groups = "drop") |>
  filter(!is.na(prob_bucket), n >= 3)

p2 <- calibration |>
  ggplot(aes(x = pred_prob, y = obs_rate)) +
  geom_abline(slope = 1, intercept = 0, color = MUTED, linetype = "dashed",
              linewidth = 0.6) +
  geom_line(aes(color = position), linewidth = 1.2) +
  geom_point(aes(color = position, size = n), alpha = 0.85) +
  scale_color_manual(values = c("WR" = BLUE_COLOR, "RB" = GOLD_COLOR)) +
  scale_size_continuous(range = c(3, 9), name = "n players") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title    = "Hit-Rate Calibration (OOS)",
    subtitle = "Predicted hit probability vs observed hit rate by bucket · dashed = perfect",
    x = "Predicted hit probability", y = "Observed hit rate",
    color = "Position"
  ) +
  theme_nfl()

ggsave("output/temporal_cv/02_calibration.png", p2,
       width = 9, height = 6, dpi = 150)
message("Saved: 02_calibration.png")

# ── Plot 3: Per-year MAE ──────────────────────────────────────────────────────

year_long <- year_metrics |>
  select(test_year, WR = wr_mae, RB = rb_mae) |>
  pivot_longer(-test_year, names_to = "position", values_to = "mae")

p3 <- year_long |>
  ggplot(aes(x = test_year, y = mae, color = position, group = position)) +
  geom_hline(yintercept = overall$mae, color = MUTED, linetype = "dashed",
             linewidth = 0.7) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3.5, alpha = 0.9) +
  scale_color_manual(values = c("WR" = BLUE_COLOR, "RB" = GOLD_COLOR)) +
  scale_x_continuous(breaks = TEST_YEARS) +
  annotate("text", x = min(TEST_YEARS) + 0.2, y = overall$mae + 0.08,
           label = sprintf("Overall avg: %.3f", overall$mae),
           color = MUTED, size = 3.5, hjust = 0) +
  labs(
    title    = "OOS MAE by Test Year",
    subtitle = "Each point: model trained on all data before that year, tested on that year",
    x = "Test Year", y = "MAE (PPG)", color = "Position"
  ) +
  theme_nfl()

ggsave("output/temporal_cv/03_per_year_mae.png", p3,
       width = 10, height = 5.5, dpi = 150)
message("Saved: 03_per_year_mae.png")

# ── Plot 4: Residuals by draft round ─────────────────────────────────────────

p4 <- oos |>
  ggplot(aes(x = round_grp, y = residual, fill = position)) +
  geom_hline(yintercept = 0, color = MUTED, linetype = "dashed", linewidth = 0.6) +
  geom_boxplot(alpha = 0.6, outlier.color = MUTED, outlier.size = 1.5, width = 0.5) +
  geom_jitter(aes(color = position), width = 0.15, alpha = 0.45, size = 1.6) +
  scale_fill_manual(values  = c("WR" = BLUE_COLOR, "RB" = GOLD_COLOR)) +
  scale_color_manual(values = c("WR" = BLUE_COLOR, "RB" = GOLD_COLOR)) +
  facet_wrap(~position) +
  labs(
    title    = "OOS Residuals by Draft Round",
    subtitle = "Actual − Expected PPG · positive = model underestimated · rolling CV 2016–2023",
    x = NULL, y = "Residual (actual − expected PPG)",
    fill = "Position", color = "Position"
  ) +
  theme_nfl() +
  theme(legend.position = "none")

ggsave("output/temporal_cv/04_residuals_by_round.png", p4,
       width = 10, height = 5.5, dpi = 150)
message("Saved: 04_residuals_by_round.png")

cat(sprintf(
  "\n══ Rolling Temporal CV complete ══\n  %d players · %d–%d\n  MAE=%.3f  RMSE=%.3f  Cor=%.3f  Bias=%.3f\n  WR: MAE=%.3f  Cor=%.3f\n  RB: MAE=%.3f  Cor=%.3f\n",
  overall$n, min(oos$test_year), max(oos$test_year),
  overall$mae, overall$rmse, overall$cor, overall$bias,
  pos_metrics$mae[pos_metrics$position=="WR"],
  pos_metrics$cor[pos_metrics$position=="WR"],
  pos_metrics$mae[pos_metrics$position=="RB"],
  pos_metrics$cor[pos_metrics$position=="RB"]
))
cat("Outputs in: output/temporal_cv/\n")

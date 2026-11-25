# 05_model_production.R
# ─────────────────────────────────────────────────────────────────────────────
# Stage 2 XGBoost production regression — predict log(PPG) for producers
#
# Target: shrinkage-adjusted, games-weighted half-PPR PPG for players with
# positive NFL PPG. Busts are handled by the separate classifier trained in
# 04_model_bust.R, then combined with this model in score_class().
#
# Separate models for WR and RB. RB uses quantile pinball loss at tau = 0.70.
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(tidymodels)
library(xgboost)
library(vip)

source("functions/helpers.R")
source("functions/feature_specs.R")

set.seed(42)

# ── Helper ────────────────────────────────────────────────────────────────────

train_production_model <- function(data, features, position_label) {
  cat("\n══ Production model (hurdle stage 2 — producers only):", position_label, "══\n")

  # Producers only: busts are handled by the separate bust model.
  # Training on log(ppg) stabilises the right tail and gives cleaner residuals.
  # WR: use all available history.
  # RB: keep 2010+ (pre-2010 RB feature coverage is genuinely too sparse).
  year_cutoff <- if (position_label == "WR") 0 else 2010
  model_df <- data |>
    filter(has_cfb_data, draft_year > year_cutoff, ppg > 0) |>
    mutate(log_ppg = log(ppg)) |>
    select(all_of(c("log_ppg", features, "draft_year")))

  cat(sprintf("Training rows (producers%s): %d\n",
              if (position_label == "WR") ", all years" else ", 2010+",
              nrow(model_df)))
  cat("log_ppg range: [",
      round(min(model_df$log_ppg), 2), ",",
      round(max(model_df$log_ppg), 2), "]\n")
  cat("log_ppg mean:", round(mean(model_df$log_ppg), 2), "\n")
  cat("log_ppg SD  :", round(sd(model_df$log_ppg), 2), "\n")
  cat("(original ppg mean:", round(mean(exp(model_df$log_ppg)), 2), ")\n")

  # Time-based CV: expanding training window, 2010+ only
  cutoffs <- c(2012, 2015, 2017, 2019, 2021)
  row_ids <- seq_len(nrow(model_df))

  splits_list <- map(cutoffs, function(cy) {
    train_idx <- row_ids[model_df$draft_year <= cy]
    test_idx  <- row_ids[model_df$draft_year > cy & model_df$draft_year <= cy + 3]
    if (length(train_idx) < 10 || length(test_idx) == 0) return(NULL)
    make_splits(list(analysis = train_idx, assessment = test_idx), data = model_df)
  }) |> compact()

  splits <- manual_rset(splits_list, id = paste0("time_fold_", seq_along(splits_list)))

  cat("CV folds:", nrow(splits), "\n")

  # Era-aware imputation: zero-fill PPA/usage for pre-era players instead of
  # imputing with the post-2016-dominated global median.
  # Column sets differ by position so recipes are built separately.
  rec <- if (position_label == "WR") {
    recipe(log_ppg ~ ., data = model_df) |>
      update_role(draft_year, new_role = "ID") |>
      step_mutate(
        avg_PPA_pass        = if_else(has_ppa    == 0L, 0, avg_PPA_pass),
        total_PPA_pass      = if_else(has_ppa    == 0L, 0, total_PPA_pass),
        usg_pass            = if_else(has_usage  == 0L, 0, usg_pass),
        usg_passing_downs   = if_else(has_usage  == 0L, 0, usg_passing_downs),
        # WR PBP features (2014+) — zero-fill pre-era players.
        catch_rate_wr       = if_else(has_wr_pbp == 0L, 0, catch_rate_wr),
        yards_per_target_wr = if_else(has_wr_pbp == 0L, 0, yards_per_target_wr),
        yards_per_rec_wr    = if_else(has_wr_pbp == 0L, 0, yards_per_rec_wr),
        explosive_rec_rate  = if_else(has_wr_pbp == 0L, 0, explosive_rec_rate),
        target_share_wr     = if_else(has_wr_pbp == 0L, 0, target_share_wr),
        targets_per_game_wr = if_else(has_wr_pbp == 0L, 0, targets_per_game_wr),
        epa_per_target_wr   = if_else(has_wr_pbp == 0L, 0, epa_per_target_wr),
        epa_per_play_wr_pbp = if_else(has_wr_pbp == 0L, 0, epa_per_play_wr_pbp),
        # Comp-stack features — zero-fill for players without comp coverage
        comp_weighted_ppg = if_else(has_comp_features == 0L, 0, comp_weighted_ppg),
        comp_bust_rate    = if_else(has_comp_features == 0L, 0, comp_bust_rate)
      )
  } else {
    recipe(log_ppg ~ ., data = model_df) |>
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
        # Comp-stack features — zero-fill for players without comp coverage
        comp_weighted_ppg = if_else(has_comp_features == 0L, 0, comp_weighted_ppg),
        comp_bust_rate    = if_else(has_comp_features == 0L, 0, comp_bust_rate)
      )
  }

  rec <- rec |>
    step_unknown(all_nominal_predictors(), new_level = "unknown") |>
    step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
    step_impute_median(all_numeric_predictors()) |>
    step_nzv(all_predictors())

  # XGBoost spec — early stopping + L1/L2 regularization
  xgb_spec <- boost_tree(
    trees          = 1500,             # high ceiling — early stopping prevents overfit
    tree_depth     = tune(),
    min_n          = tune(),
    learn_rate     = tune(),
    loss_reduction = tune(),
    sample_size    = tune(),
    mtry           = tune(),
    stop_iter      = 25               # early stopping after 25 rounds no improvement
  ) |>
    set_engine("xgboost",
               lambda    = tune("lambda"),
               alpha     = tune("alpha"),
               nthread   = 4,
               validation = 0.15) |>
    set_mode("regression")

  wf <- workflow() |>
    add_recipe(rec) |>
    add_model(xgb_spec)

  # Expanded grid: 6 standard params + lambda/alpha added manually
  n_grid <- 60
  base_grid <- grid_space_filling(
    tree_depth(range = c(2, 6)),
    min_n(range = c(3, 20)),
    learn_rate(range = c(-3, -1.2)),
    loss_reduction(range = c(-5, 0)),
    sample_prop(range = c(0.6, 1.0)),
    finalize(mtry(), model_df |> select(all_of(features))),
    size = n_grid
  )

  set.seed(42 + nrow(model_df))
  grid <- base_grid |>
    mutate(
      lambda = 10^runif(n_grid, -2, 2),
      alpha  = 10^runif(n_grid, -3, 0)
    )

  tune_res <- tune_grid(
    wf,
    resamples = splits,
    grid      = grid,
    metrics   = metric_set(rmse, mae, rsq),
    control   = control_grid(save_pred = TRUE, verbose = FALSE)
  )

  best_params <- select_best(tune_res, metric = "rmse")
  cat("\nBest params:\n"); print(best_params)

  cat("\nCV metrics (best model):\n")
  collect_metrics(tune_res) |>
    filter(.config == best_params$.config) |>
    select(.metric, mean, std_err) |>
    print()

  # Final fit
  final_wf  <- finalize_workflow(wf, best_params)
  final_fit <- fit(final_wf, data = model_df)

  # Variable importance
  vi <- final_fit |>
    extract_fit_parsnip() |>
    vi() |>
    slice_head(n = 15)

  cat("\nTop 15 variable importances:\n")
  print(vi)

  # In-sample residuals (log scale + back-transformed)
  preds_is <- predict(final_fit, model_df) |>
    bind_cols(model_df |> select(log_ppg, draft_year)) |>
    mutate(
      resid_log = log_ppg - .pred,
      actual_ppg = exp(log_ppg),
      pred_ppg   = exp(.pred),
      resid_ppg  = actual_ppg - pred_ppg
    )

  cat("\nResidual quantiles — log scale (in-sample):\n")
  quantile(preds_is$resid_log, probs = c(.1, .25, .5, .75, .9)) |>
    round(3) |>
    print()

  cat("\nResidual quantiles — PPG scale (in-sample):\n")
  quantile(preds_is$resid_ppg, probs = c(.1, .25, .5, .75, .9)) |>
    round(2) |>
    print()

  cat("\nIn-sample RMSE (PPG scale):",
      round(sqrt(mean(preds_is$resid_ppg^2)), 3), "\n")
  cat("In-sample R² (PPG scale)  :",
      round(cor(preds_is$actual_ppg, preds_is$pred_ppg)^2, 3), "\n")

  list(
    fit      = final_fit,
    tune_res = tune_res,
    best     = best_params,
    vi       = vi
  )
}

# ── Quantile production model for RBs ────────────────────────────────────────
# Uses XGBoost pinball loss at τ=0.70 to correct the systematic underestimation
# of early-round RBs. Same time-based CV grid search as the MSE model.
# τ=0.70 picked by rolling temporal-CV sweep (12_tau_sweep.R, 2026-04-22):
#   τ ∈ {0.50..0.85}; best RB OOS MAE 3.10 @ τ=0.70 (was 3.14 @ τ=0.65).
#   Correlation also peaks at τ=0.70 (0.46 vs 0.41 at τ=0.65).
# Returns a list with the same slots as train_production_model() plus
# type="quantile" and a baked recipe so score_class() can preprocess correctly.

train_quantile_rb_model <- function(data, features, tau = 0.70) {
  cat(sprintf("\n══ RB quantile production model (τ=%.2f) ══\n", tau))

  year_cutoff <- 2010
  model_df <- data |>
    filter(has_cfb_data, draft_year > year_cutoff, ppg > 0) |>
    mutate(
      log_ppg       = log(ppg),
      draft_year_sc = scale(draft_year)[, 1],
      tier          = factor(tier, levels = c("P4", "G5", "Other"))
    )

  cat("Training rows (producers 2010+):", nrow(model_df), "\n")

  # Preprocess with same recipe structure as MSE model
  rec <- recipe(log_ppg ~ .,
                data = model_df |> select(all_of(c("log_ppg", features)))) |>
    # PBP features (2014+) — zero-fill for pre-era so the tree can learn
    # "pre-PBP era" as a distinct regime via has_pbp rather than being pulled
    # to the post-2014-dominated global median.
    step_mutate(
      explosive_rate   = if_else(has_pbp == 0L, 0, explosive_rate),
      breakaway_rate   = if_else(has_pbp == 0L, 0, breakaway_rate),
      target_share     = if_else(has_pbp == 0L, 0, target_share),
      targets_per_game = if_else(has_pbp == 0L, 0, targets_per_game),
      catch_rate       = if_else(has_pbp == 0L, 0, catch_rate),
      epa_per_rush     = if_else(has_pbp == 0L, 0, epa_per_rush),
      epa_per_play_pbp = if_else(has_pbp == 0L, 0, epa_per_play_pbp),
      # Comp-stack features — zero-fill for players without comp coverage
      comp_weighted_ppg = if_else(has_comp_features == 0L, 0, comp_weighted_ppg),
      comp_bust_rate    = if_else(has_comp_features == 0L, 0, comp_bust_rate)
    ) |>
    step_unknown(all_nominal_predictors(), new_level = "unknown") |>
    step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
    step_impute_median(all_numeric_predictors()) |>
    step_nzv(all_predictors())

  prep_rec <- prep(rec, training = model_df |> select(all_of(c("log_ppg", features))))
  baked    <- bake(prep_rec, new_data = NULL)

  X_all  <- as.matrix(baked |> select(-log_ppg))
  y_all  <- baked$log_ppg
  yr_all <- model_df$draft_year

  # Pinball (quantile) loss and eval metric
  pinball_obj <- function(preds, dtrain) {
    err  <- getinfo(dtrain, "label") - preds
    grad <- ifelse(err >= 0, -tau, 1 - tau)
    hess <- rep(tau * (1 - tau), length(preds))
    list(grad = grad, hess = hess)
  }
  pinball_eval <- function(preds, dtrain) {
    err  <- getinfo(dtrain, "label") - preds
    loss <- mean(ifelse(err >= 0, tau * err, (tau - 1) * err))
    list(metric = "pinball", value = loss)
  }

  # Time-based CV folds (same cutoffs as MSE model)
  cutoffs <- c(2012, 2015, 2017, 2019, 2021)
  folds <- map(cutoffs, function(cy) {
    list(train = which(yr_all <= cy),
         test  = which(yr_all > cy & yr_all <= cy + 3))
  }) |> keep(~ length(.x$train) >= 10 & length(.x$test) > 0)

  cat("CV folds:", length(folds), "\n")

  # Hyperparameter grid
  set.seed(42 + nrow(model_df))
  n_grid <- 40
  grid <- tibble(
    eta              = 10^runif(n_grid, -2.5, -1.2),
    max_depth        = sample(2:6, n_grid, replace = TRUE),
    min_child_weight = sample(c(3, 5, 8, 12, 20), n_grid, replace = TRUE),
    subsample        = runif(n_grid, 0.6, 1.0),
    colsample_bytree = runif(n_grid, 0.5, 0.9),
    lambda           = 10^runif(n_grid, -2, 2),
    alpha            = 10^runif(n_grid, -3, 0)
  )

  cat(sprintf("Grid search: %d combos × %d folds...\n", n_grid, length(folds)))

  cv_scores <- map_dbl(seq_len(n_grid), function(i) {
    params <- c(as.list(grid[i, ]), list(nthread = 4))
    fold_losses <- map_dbl(folds, function(fold) {
      dtrain <- xgb.DMatrix(X_all[fold$train, ], label = y_all[fold$train])
      dtest  <- xgb.DMatrix(X_all[fold$test,  ], label = y_all[fold$test])
      m <- xgb.train(params = params, data = dtrain, nrounds = 800,
                     obj = pinball_obj, feval = pinball_eval,
                     watchlist = list(test = dtest),
                     early_stopping_rounds = 30, maximize = FALSE, verbose = 0)
      m$best_score
    })
    mean(fold_losses)
  })

  best_i      <- which.min(cv_scores)
  best_params <- c(as.list(grid[best_i, ]), list(nthread = 4))
  cat(sprintf("Best CV pinball loss: %.4f (combo %d)\n", cv_scores[best_i], best_i))

  # Determine nrounds via early stopping on a held-out validation window
  val_idx   <- which(yr_all >= 2021)
  train_idx <- which(yr_all < 2021)
  dtrain_es <- xgb.DMatrix(X_all[train_idx, ], label = y_all[train_idx])
  dval_es   <- xgb.DMatrix(X_all[val_idx,   ], label = y_all[val_idx])

  es_model <- xgb.train(
    params = best_params, data = dtrain_es, nrounds = 1200,
    obj = pinball_obj, feval = pinball_eval,
    watchlist = list(val = dval_es),
    early_stopping_rounds = 40, maximize = FALSE, verbose = 0
  )
  best_rounds <- es_model$best_iteration
  cat("Best nrounds:", best_rounds, "\n")

  # Final model on ALL training data
  dtrain_full <- xgb.DMatrix(X_all, label = y_all)
  final_model <- xgb.train(
    params  = best_params,
    data    = dtrain_full,
    nrounds = best_rounds,
    obj     = pinball_obj,
    verbose = 0
  )

  # Variable importance
  vi <- xgb.importance(model = final_model) |>
    as_tibble() |>
    slice_head(n = 15)
  cat("\nTop 15 variable importances:\n"); print(vi)

  # In-sample diagnostics
  log_pred_is <- predict(final_model, dtrain_full)
  pred_ppg_is <- exp(log_pred_is)
  actual_ppg_is <- exp(y_all)
  cat("\nIn-sample RMSE (PPG):", round(sqrt(mean((actual_ppg_is - pred_ppg_is)^2)), 3), "\n")
  cat("In-sample R²  (PPG):", round(cor(actual_ppg_is, pred_ppg_is)^2, 3), "\n")
  cat(sprintf("RB in-sample correlation (producers only): %.3f\n",
              cor(actual_ppg_is, pred_ppg_is)))

  list(
    fit     = final_model,
    recipe  = prep_rec,
    type    = "quantile",
    tau     = tau,
    best    = best_params,
    nrounds = best_rounds,
    vi      = vi
  )
}

# ── Train ──────────────────────────────────────────────────────────────────────

wr_data <- readRDS("data/wr_model_data.rds") |> attach_comp_features()
rb_data <- readRDS("data/rb_model_data.rds") |> attach_comp_features()

wr_prod <- train_production_model(wr_data, WR_PROD_FEATURES, "WR")
rb_prod <- train_quantile_rb_model(rb_data, RB_PROD_FEATURES, tau = 0.70)

# ── Save ──────────────────────────────────────────────────────────────────────

saveRDS(wr_prod, "models/wr_production_model.rds")
saveRDS(rb_prod, "models/rb_production_model.rds")

message("\nSaved: models/wr_production_model.rds and models/rb_production_model.rds")

# ── Visualise: actual vs predicted ────────────────────────────────────────────

plot_actual_vs_pred <- function(model_obj, data, position_label) {
  # Production model is trained on producers 2010+ — apply to all 2010+ players
  # to show how the hurdle combination would look
  eval_data <- data |> filter(has_cfb_data, draft_year >= 2010)

  if (isTRUE(model_obj$type == "quantile")) {
    df_baked  <- bake(model_obj$recipe, new_data = eval_data)
    X_eval    <- as.matrix(df_baked |> select(-any_of("log_ppg")))
    log_preds <- predict(model_obj$fit, xgb.DMatrix(X_eval))
    preds <- eval_data |>
      select(ppg, pfr_player_name, draft_year, round, made_it) |>
      mutate(.pred = log_preds, pred_ppg = exp(.pred))
  } else {
    preds <- predict(model_obj$fit, eval_data) |>
      bind_cols(
        eval_data |> select(ppg, pfr_player_name, draft_year, round, made_it)
      ) |>
      mutate(pred_ppg = exp(.pred))  # back-transform from log scale
  }

  corr_prod <- cor(
    preds$ppg[preds$ppg > 0],
    preds$pred_ppg[preds$ppg > 0],
    use = "complete.obs"
  ) |> round(3)
  cat(sprintf("\n%s in-sample correlation (producers only, actual vs pred PPG): %.3f\n",
              position_label, corr_prod))

  # Top performers the model liked
  cat("\nHighest predicted PPG (producers only):\n")
  preds |>
    filter(ppg > 0) |>
    slice_max(pred_ppg, n = 10) |>
    select(pfr_player_name, draft_year, round, pred_ppg, ppg) |>
    mutate(across(c(pred_ppg, ppg), ~ round(.x, 2))) |>
    print()

  # Biggest misses among producers
  cat("\nBiggest underestimates (actual star, model was low):\n")
  preds |>
    filter(ppg > 0) |>
    mutate(miss = ppg - pred_ppg) |>
    slice_max(miss, n = 5) |>
    select(pfr_player_name, draft_year, round, pred_ppg, ppg) |>
    mutate(across(c(pred_ppg, ppg), ~ round(.x, 2))) |>
    print()
}

plot_actual_vs_pred(wr_prod, wr_data, "WR")
plot_actual_vs_pred(rb_prod, rb_data, "RB")

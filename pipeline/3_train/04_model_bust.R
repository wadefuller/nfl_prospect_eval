# 04_model_bust.R
# ─────────────────────────────────────────────────────────────────────────────
# Stage 1: XGBoost classification — predict P(made_it = 1)
#
# "Made it" = player had at least one qualifying season (≥8 games) in their
# first 3 NFL years with meaningful fantasy production.
#
# Separate models for WR and RB.
# Cross-validated with time-aware (draft-year) splits.
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(tidymodels)
library(xgboost)

source("functions/helpers.R")
source("functions/feature_specs.R")

set.seed(42)

# ── Helper: train + evaluate one position ────────────────────────────────────

train_bust_model <- function(data, features, position_label) {
  cat("\n══ Bust model:", position_label, "══\n")

  # WR: use all available history — pre-2010 classes are small but their outcomes
  # are fully resolved and recruiting/combine coverage is decent. Downweight them
  # via importance_weights so they inform without dominating.
  # RB: keep 2010+ cutoff (pre-2010 RB feature coverage is genuinely too sparse).
  if (position_label == "WR") {
    model_df <- data |>
      filter(has_cfb_data) |>
      select(all_of(c("made_it", features, "draft_year"))) |>
      mutate(
        made_it  = factor(made_it, levels = c(0, 1), labels = c("bust", "made_it")),
        case_wt  = importance_weights(if_else(draft_year < 2010, 0.4, 1.0))
      )
  } else {
    model_df <- data |>
      filter(has_cfb_data, draft_year >= 2010) |>
      select(all_of(c("made_it", features, "draft_year"))) |>
      mutate(made_it = factor(made_it, levels = c(0, 1), labels = c("bust", "made_it")))
  }

  cat("Rows with CFB data:", nrow(model_df), "\n")
  cat("made_it dist:", table(model_df$made_it), "\n")

  # Time-based CV: train on earlier draft classes, validate on later
  # Rolling-origin splits with expanding training window.
  # Require at least 10 training rows AND at least 5 producers in training
  # (the 2009 cutoff has 0 RB producers → useless fold).
  cutoffs <- c(2012, 2015, 2017, 2019, 2021)
  row_ids <- seq_len(nrow(model_df))

  splits_list <- map(cutoffs, function(cy) {
    train_idx <- row_ids[model_df$draft_year <= cy]
    test_idx  <- row_ids[model_df$draft_year > cy & model_df$draft_year <= cy + 3]
    # Guard: need enough train data with both classes, and non-empty test
    n_pos <- sum(model_df$made_it[train_idx] == "made_it")
    if (length(train_idx) < 10 || length(test_idx) == 0 || n_pos < 5) return(NULL)
    make_splits(list(analysis = train_idx, assessment = test_idx), data = model_df)
  }) |> compact()

  splits <- manual_rset(splits_list, id = paste0("time_fold_", seq_along(splits_list)))

  cat("CV folds:", nrow(splits), "\n")

  # Recipe: era-aware imputation then standard encoding.
  # PPA/usage features are zero-filled for pre-era players rather than
  # median-imputed — avoids inflating pre-2016 players with post-2016 medians.
  # Column sets differ by position so recipes are built separately.
  rec <- if (position_label == "WR") {
    recipe(made_it ~ ., data = model_df) |>
      update_role(draft_year, new_role = "ID") |>
      step_mutate(
        avg_PPA_pass        = if_else(has_ppa    == 0L, 0, avg_PPA_pass),
        total_PPA_pass      = if_else(has_ppa    == 0L, 0, total_PPA_pass),
        usg_pass            = if_else(has_usage  == 0L, 0, usg_pass),
        usg_passing_downs   = if_else(has_usage  == 0L, 0, usg_passing_downs),
        # WR PBP features (2014+) — zero-fill for pre-era players so XGBoost
        # can learn "pre-PBP era" as a distinct regime via has_wr_pbp flag.
        catch_rate_wr       = if_else(has_wr_pbp == 0L, 0, catch_rate_wr),
        yards_per_target_wr = if_else(has_wr_pbp == 0L, 0, yards_per_target_wr),
        yards_per_rec_wr    = if_else(has_wr_pbp == 0L, 0, yards_per_rec_wr),
        explosive_rec_rate  = if_else(has_wr_pbp == 0L, 0, explosive_rec_rate),
        target_share_wr     = if_else(has_wr_pbp == 0L, 0, target_share_wr),
        targets_per_game_wr = if_else(has_wr_pbp == 0L, 0, targets_per_game_wr),
        epa_per_target_wr   = if_else(has_wr_pbp == 0L, 0, epa_per_target_wr),
        epa_per_play_wr_pbp = if_else(has_wr_pbp == 0L, 0, epa_per_play_wr_pbp),
        # Comp-stack features — zero-fill for players without comp coverage
        # (early years where strictly-past pool too small; or no CFB data).
        # has_comp_features flag lets the tree learn the regime.
        comp_weighted_ppg = if_else(has_comp_features == 0L, 0, comp_weighted_ppg),
        comp_bust_rate    = if_else(has_comp_features == 0L, 0, comp_bust_rate)
      )
  } else {
    recipe(made_it ~ ., data = model_df) |>
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
        # PBP features (2014+) — zero-fill for pre-era players so XGBoost
        # can learn "pre-PBP era" as a distinct regime via has_pbp flag.
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
      )
  }

  rec <- rec |>
    step_unknown(all_nominal_predictors(), new_level = "unknown") |>
    step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
    step_impute_median(all_numeric_predictors()) |>
    step_nzv(all_predictors())

  # XGBoost spec — early stopping + L1/L2 regularization
  n_features <- length(features) + 2  # +2 for one-hot tier dummies
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
               scale_pos_weight = sum(model_df$made_it == "bust") /
                                  sum(model_df$made_it == "made_it"),
               lambda    = tune("lambda"),   # L2 regularization
               alpha     = tune("alpha"),    # L1 regularization
               nthread   = 4,
               validation = 0.15) |>
    set_mode("classification")

  wf <- workflow() |>
    add_recipe(rec) |>
    add_model(xgb_spec)

  # WR: attach era-based importance weights so pre-2010 classes contribute
  # but don't dominate the gradient signal.
  if (position_label == "WR") {
    wf <- wf |> add_case_weights(case_wt)
  }

  # Expanded grid: 6 standard params + lambda/alpha added manually
  n_grid <- 60
  base_grid <- grid_space_filling(
    tree_depth(range = c(2, 6)),
    min_n(range = c(5, 30)),
    learn_rate(range = c(-3, -1.2)),
    loss_reduction(range = c(-5, 0)),
    sample_prop(range = c(0.6, 1.0)),
    finalize(mtry(), model_df |> select(all_of(features))),
    size = n_grid
  )

  # Add L1/L2 regularization columns (log-uniform draws)
  set.seed(42 + nrow(model_df))
  grid <- base_grid |>
    mutate(
      lambda = 10^runif(n_grid, -2, 2),    # L2: 0.01 to 100
      alpha  = 10^runif(n_grid, -3, 0)     # L1: 0.001 to 1
    )

  tune_res <- tune_grid(
    wf,
    resamples = splits,
    grid      = grid,
    metrics   = metric_set(roc_auc, pr_auc, accuracy),
    control   = control_grid(save_pred = TRUE, verbose = FALSE)
  )

  # Best by ROC-AUC
  best_params <- select_best(tune_res, metric = "roc_auc")
  cat("\nBest params:\n"); print(best_params)

  cat("\nCV metrics (best model):\n")
  collect_metrics(tune_res) |>
    filter(.config == best_params$.config) |>
    select(.metric, mean, std_err) |>
    print()

  # Final fit on all training data
  final_wf  <- finalize_workflow(wf, best_params)
  final_fit <- fit(final_wf, data = model_df)

  # Variable importance
  vi <- final_fit |>
    extract_fit_parsnip() |>
    vip::vi() |>
    slice_head(n = 15)

  cat("\nTop 15 variable importances:\n")
  print(vi)

  # ── Isotonic recalibration (used by RB combiner; harmless for WR) ──────────
  # Fit isotonic regression p_train -> made_it on the training rows. Saved as
  # (x, y) breakpoints so score_class() can apply via approx() without needing
  # the isoreg object. See 13_bust_tune.R for the OOS-tuned combiner choice.
  train_pred <- predict(final_fit, model_df, type = "prob") |>
    pull(.pred_made_it)
  train_actual <- as.integer(model_df$made_it == "made_it")
  iso_fit <- isoreg(train_pred, train_actual)
  iso_x   <- iso_fit$x[iso_fit$ord]
  iso_y   <- iso_fit$yf

  list(
    fit       = final_fit,
    tune_res  = tune_res,
    best      = best_params,
    vi        = vi,
    iso_x     = iso_x,
    iso_y     = iso_y
  )
}

# ── Train ──────────────────────────────────────────────────────────────────────

wr_data <- readRDS("data/wr_model_data.rds") |> attach_comp_features()
rb_data <- readRDS("data/rb_model_data.rds") |> attach_comp_features()

wr_bust <- train_bust_model(wr_data, WR_BUST_FEATURES, "WR")
rb_bust <- train_bust_model(rb_data, RB_BUST_FEATURES, "RB")

# ── Save ──────────────────────────────────────────────────────────────────────

saveRDS(wr_bust, "models/wr_bust_model.rds")
saveRDS(rb_bust, "models/rb_bust_model.rds")

message("\nSaved: models/wr_bust_model.rds and models/rb_bust_model.rds")

# ── Calibration check (in-sample) ─────────────────────────────────────────────
# Quick sanity: predicted P(made_it) by draft round bucket

check_calibration <- function(model_obj, data, features, label) {
  preds <- predict(model_obj$fit, data |> filter(has_cfb_data), type = "prob") |>
    bind_cols(data |> filter(has_cfb_data) |> select(made_it, round))

  cat("\n", label, " — avg P(made_it) by round:\n", sep = "")
  preds |>
    group_by(round) |>
    summarize(
      n           = n(),
      actual_rate = mean(made_it),
      pred_prob   = mean(.pred_made_it)
    ) |>
    print()
}

check_calibration(wr_bust, wr_data, WR_BUST_FEATURES, "WR")
check_calibration(rb_bust, rb_data, RB_BUST_FEATURES, "RB")

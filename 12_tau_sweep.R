# 12_tau_sweep.R
# Sweep RB quantile τ ∈ {0.50, 0.55, 0.60, 0.65, 0.70} via temporal CV.
# Pick τ that minimizes RB OOS MAE. Emits:
#   output/temporal_cv/tau_sweep.csv  — per-tau RB MAE/bias/cor
#
# Only the RB production leg is swept; WR + bust + shrinkage held fixed.

library(tidyverse)
library(tidymodels)
library(xgboost)

set.seed(42)
setwd("~/Projects/R/college_nfl_model")
source("functions/helpers.R")

# Re-declare feature sets (mirror 11_temporal_cv.R) ──────────────────────────
RB_BUST_FEATURES <- c(
  "sqrt_pick", "age", "draft_year_sc", "tier",
  "carries_final", "rush_yards_final", "rush_td_final", "ypc",
  "rb_rec", "rb_rec_yards", "rb_rec_td",
  "scrimmage_td", "yards_per_touch",
  "rush_yards_penult", "carries_penult",
  "rush_yards_ante",
  "rush_yds_yoy", "rush_td_rate", "recv_share",
  "rush_yards_per_game", "carries_per_game", "scrimmage_yards_per_game",
  "teammate_rush_yards", "dominator_rate",
  "total_touches",
  "weight", "height_in", "forty", "vertical", "broad_jump",
  "speed_score",
  "usg_rush", "usg_passing_downs", "avg_PPA_rush", "total_PPA_rush",
  "usg_overall", "usg_pass", "avg_PPA_all", "total_PPA_all",
  "explosive_rate", "breakaway_rate",
  "target_share", "targets_per_game", "catch_rate",
  "epa_per_rush", "epa_per_play_pbp",
  "recruit_stars", "recruit_rating", "recruit_rank",
  "college_years", "age_relative", "n_drafted_skill", "elite_teammate",
  "has_penult", "has_ppa", "has_usage", "has_pbp",
  "has_recruiting", "has_combine", "has_recruit_year",
  "is_scat_back"
)

RB_PROD_FEATURES <- c(
  "sqrt_pick", "age", "draft_year_sc", "tier",
  "carries_final", "rush_yards_final", "rush_td_final", "ypc",
  "rb_rec", "rb_rec_yards", "rb_rec_td",
  "scrimmage_td", "yards_per_touch",
  "rush_yards_penult", "carries_penult",
  "rush_yards_ante",
  "rush_yds_yoy", "rush_td_rate", "recv_share",
  "teammate_rush_yards", "dominator_rate",
  "total_touches",
  "weight", "height_in", "forty", "vertical", "broad_jump",
  "speed_score",
  "usg_rush", "usg_passing_downs", "avg_PPA_rush", "total_PPA_rush",
  "usg_overall", "usg_pass", "avg_PPA_all", "total_PPA_all",
  "explosive_rate", "breakaway_rate",
  "target_share", "targets_per_game", "catch_rate",
  "epa_per_rush", "epa_per_play_pbp",
  "recruit_stars", "recruit_rating", "recruit_rank",
  "college_years", "age_relative", "n_drafted_skill", "elite_teammate",
  "has_penult", "has_ppa", "has_usage", "has_pbp",
  "has_recruiting", "has_combine", "has_recruit_year",
  "is_scat_back"
)

# Build RB recipe (mirror 11_temporal_cv.R::build_recipe for RB)
build_rb_recipe <- function(model_df, outcome) {
  recipe(as.formula(paste(outcome, "~ .")), data = model_df) |>
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
    ) |>
    step_unknown(all_nominal_predictors(), new_level = "unknown") |>
    step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
    step_impute_median(all_numeric_predictors()) |>
    step_nzv(all_predictors())
}

# RB bust trainer (fixed HPs mirror 11_temporal_cv.R)
train_cv_bust_rb <- function(train_data, features) {
  model_df <- train_data |>
    filter(has_cfb_data, draft_year >= 2010) |>
    mutate(made_it = factor(made_it, levels = c(0L, 1L), labels = c("bust", "made_it")))
  av       <- intersect(features, names(model_df))
  model_df <- model_df |> select(all_of(c("made_it", av, "draft_year")))

  rec  <- build_rb_recipe(model_df, "made_it")
  spec <- boost_tree(trees = 800, tree_depth = 4, learn_rate = 0.02,
                     min_n = 8, sample_size = 0.8) |>
    set_engine("xgboost",
               scale_pos_weight = sum(model_df$made_it == "bust") /
                                  sum(model_df$made_it == "made_it"),
               nthread = 4) |>
    set_mode("classification")
  fit(workflow() |> add_recipe(rec) |> add_model(spec), model_df)
}

# RB production trainer (quantile, τ parametric)
train_cv_prod_rb <- function(train_data, features, tau) {
  model_df <- train_data |>
    filter(has_cfb_data, draft_year >= 2010, ppg > 0) |>
    mutate(log_ppg = log(ppg))
  av <- intersect(features, names(model_df))

  sel <- c("log_ppg", av, "draft_year")
  rec <- build_rb_recipe(model_df |> select(all_of(sel)), "log_ppg")
  prep_rec <- prep(rec, training = model_df |> select(all_of(sel)))
  baked    <- bake(prep_rec, new_data = NULL) |>
    select(-any_of("draft_year"))

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

predict_rb_fold <- function(bust_fit, prod_fit, test_data, base_rate) {
  td <- test_data |>
    mutate(made_it = factor(made_it, levels = c(0L, 1L),
                            labels = c("bust", "made_it")))
  p_made_it <- predict(bust_fit, td, type = "prob") |> pull(.pred_made_it)

  baked <- bake(prod_fit$recipe, new_data = td) |> select(-any_of("draft_year"))
  X     <- as.matrix(baked |> select(-any_of("log_ppg")))
  log_ppg_pred <- predict(prod_fit$fit, xgb.DMatrix(X))

  alpha <- 0.25  # RB shrinkage (from Fix #3)
  p_eff <- alpha * p_made_it + (1 - alpha) * base_rate

  test_data |>
    mutate(p_made_it = p_made_it,
           p_eff     = p_eff,
           exp_ppg   = pmax(p_eff * exp(log_ppg_pred), 0))
}

# ── Sweep loop ──────────────────────────────────────────────────────────────

cat("Loading RB model data...\n")
rb_full <- readRDS("data/rb_model_data.rds")

TEST_YEARS <- 2016:2023
MIN_TRAIN  <- 40
MIN_TEST   <- 5

TAUS <- c(0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85)

sweep_results <- list()

for (tau in TAUS) {
  cat(sprintf("\n══ τ = %.2f ══\n", tau))
  oos_fold <- list()

  for (K in TEST_YEARS) {
    train <- rb_full |> filter(draft_year < K)
    test  <- rb_full |> filter(draft_year == K, has_cfb_data)
    train_cfb <- train |> filter(has_cfb_data, draft_year >= 2010)

    if (nrow(train_cfb) < MIN_TRAIN || nrow(test) < MIN_TEST) next

    dy_m <- mean(train_cfb$draft_year)
    dy_s <- sd(train_cfb$draft_year)
    train <- train |> mutate(draft_year_sc = (draft_year - dy_m) / dy_s)
    test  <- test  |> mutate(draft_year_sc = (draft_year - dy_m) / dy_s)

    bust_fit <- train_cv_bust_rb(train, RB_BUST_FEATURES)
    prod_fit <- train_cv_prod_rb(train, RB_PROD_FEATURES, tau = tau)
    base_rate_train <- mean(train_cfb$made_it)

    preds <- predict_rb_fold(bust_fit, prod_fit, test, base_rate_train)
    oos_fold[[as.character(K)]] <- preds |>
      mutate(test_year = K) |>
      select(pfr_player_name, draft_year, test_year, round, pick,
             made_it, ppg, p_made_it, exp_ppg)
    cat(sprintf("  K=%d: n_test=%d  fold_MAE=%.3f\n",
                K, nrow(preds), mean(abs(preds$ppg - preds$exp_ppg))))
  }

  oos <- bind_rows(oos_fold)
  sweep_results[[as.character(tau)]] <- tibble(
    tau  = tau,
    n    = nrow(oos),
    mae  = mean(abs(oos$ppg - oos$exp_ppg)),
    rmse = sqrt(mean((oos$ppg - oos$exp_ppg)^2)),
    cor  = cor(oos$ppg, oos$exp_ppg),
    bias = mean(oos$ppg - oos$exp_ppg)
  )

  cat(sprintf("  τ=%.2f → RB OOS MAE=%.3f  bias=%+.3f  cor=%.3f\n",
              tau, sweep_results[[as.character(tau)]]$mae,
              sweep_results[[as.character(tau)]]$bias,
              sweep_results[[as.character(tau)]]$cor))
}

summary_df <- bind_rows(sweep_results) |> arrange(mae)

cat("\n══ RB τ sweep (sorted by MAE) ══\n")
print(summary_df)

write_csv(summary_df, "output/temporal_cv/tau_sweep.csv")
cat("\nSaved: output/temporal_cv/tau_sweep.csv\n")

best_tau <- summary_df$tau[1]
cat(sprintf("\n▶ Best τ = %.2f (MAE=%.3f)\n", best_tau, summary_df$mae[1]))

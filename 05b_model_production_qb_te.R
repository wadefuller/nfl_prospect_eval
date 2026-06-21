# 05b_model_production_qb_te.R
# ─────────────────────────────────────────────────────────────────────────────
# Trains the production (log-PPG) regressor for QB and TE prospects on
# producers (made_it == 1) only. Mirrors 05_model_production.R structure.
#
# Outputs: models/qb_production_model.rds, models/te_production_model.rds
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

QB_YEAR_CUTOFF <- 2008
TE_YEAR_CUTOFF <- 2008

build_recipe <- function(model_df, pos = NULL) {
  rec <- recipe(log_ppg ~ ., data = model_df) |>
    update_role(draft_year, new_role = "ID")
  if (identical(pos, "QB") && "has_qb_pbp" %in% names(model_df)) {
    rec <- rec |> step_mutate(
      epa_per_dropback     = if_else(has_qb_pbp == 0L, 0, epa_per_dropback),
      epa_per_attempt      = if_else(has_qb_pbp == 0L, 0, epa_per_attempt),
      completion_pct_pbp   = if_else(has_qb_pbp == 0L, 0, completion_pct_pbp),
      sack_rate            = if_else(has_qb_pbp == 0L, 0, sack_rate),
      int_rate             = if_else(has_qb_pbp == 0L, 0, int_rate),
      negative_play_rate   = if_else(has_qb_pbp == 0L, 0, negative_play_rate),
      explosive_pass_rate  = if_else(has_qb_pbp == 0L, 0, explosive_pass_rate),
      late_down_epa        = if_else(has_qb_pbp == 0L, 0, late_down_epa),
      qb_share_team        = if_else(has_qb_pbp == 0L, 0, qb_share_team)
    )
  }
  if (identical(pos, "TE") && "has_te_pbp" %in% names(model_df)) {
    rec <- rec |> step_mutate(
      catch_rate_te        = if_else(has_te_pbp == 0L, 0, catch_rate_te),
      yards_per_target_te  = if_else(has_te_pbp == 0L, 0, yards_per_target_te),
      yards_per_rec_te     = if_else(has_te_pbp == 0L, 0, yards_per_rec_te),
      explosive_rec_rate_te = if_else(has_te_pbp == 0L, 0, explosive_rec_rate_te),
      target_share_te      = if_else(has_te_pbp == 0L, 0, target_share_te),
      targets_per_game_te  = if_else(has_te_pbp == 0L, 0, targets_per_game_te),
      epa_per_target_te    = if_else(has_te_pbp == 0L, 0, epa_per_target_te),
      epa_per_play_te_pbp  = if_else(has_te_pbp == 0L, 0, epa_per_play_te_pbp)
    )
  }
  rec |>
    step_unknown(all_nominal_predictors(), new_level = "unknown") |>
    step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
    step_impute_median(all_numeric_predictors()) |>
    step_nzv(all_predictors())
}

train_production_model <- function(data, features, position_label, year_cutoff) {
  cat(sprintf("\n══ Production model: %s ══\n", position_label))
  features <- intersect(features, names(data))

  model_df <- data |>
    filter(has_cfb_data, draft_year >= year_cutoff, !is.na(ppg), ppg > 0) |>
    mutate(log_ppg = log(ppg)) |>
    select(all_of(c("log_ppg", features, "draft_year")))

  cat("Producer rows :", nrow(model_df), "\n")
  cat("log_ppg range :", round(range(model_df$log_ppg), 2), "\n")
  cat("ppg mean      :", round(mean(exp(model_df$log_ppg)), 2), "\n")
  if (nrow(model_df) < 30) {
    warning(sprintf("[%s] too few producers — skipping", position_label))
    return(NULL)
  }

  rec  <- build_recipe(model_df, position_label)
  spec <- boost_tree(
    trees         = 500, tree_depth = 4, learn_rate = 0.025,
    mtry          = min(15, length(features)), min_n = 8,
    sample_size   = 0.8
  ) |>
    set_engine("xgboost", nthread = 4) |>
    set_mode("regression")

  wf <- workflow() |> add_recipe(rec) |> add_model(spec)
  fit_obj <- fit(wf, data = model_df)

  # In-sample residuals at the high end
  preds <- predict(fit_obj, model_df) |>
    bind_cols(model_df |> select(log_ppg)) |>
    mutate(pred_ppg = exp(.pred), actual_ppg = exp(log_ppg))
  cat("\nTop 5 underestimates (actual ≫ pred):\n")
  print(preds |> mutate(diff = actual_ppg - pred_ppg) |>
        arrange(desc(diff)) |> head(5))

  list(fit = fit_obj, features = features, position = position_label)
}

# ── Quantile (pinball) regression for TE ─────────────────────────────────────
# TE has an extreme right tail (Bowers, Kelce do 12+ PPG) but most producers
# are 3-5 PPG. MSE regression pulls predictions toward the median, badly
# under-projecting elite TEs. Switching to pinball loss at τ=0.65 biases the
# fit toward the upper tail. Same pattern used for RB.

train_te_quantile_model <- function(data, features, tau = 0.65,
                                     year_cutoff = TE_YEAR_CUTOFF) {
  cat(sprintf("\n══ TE quantile production model (τ=%.2f) ══\n", tau))
  features <- intersect(features, names(data))
  model_df <- data |>
    filter(has_cfb_data, draft_year >= year_cutoff, !is.na(ppg), ppg > 0) |>
    mutate(log_ppg = log(ppg)) |>
    select(all_of(c("log_ppg", features, "draft_year")))
  cat("Producer rows:", nrow(model_df), "\n")

  rec <- build_recipe(model_df, "TE")
  prep_rec <- prep(rec, training = model_df)
  baked    <- bake(prep_rec, new_data = NULL) |> select(-any_of("draft_year"))
  X <- as.matrix(baked |> select(-log_ppg))
  y <- baked$log_ppg

  pinball_obj <- function(preds, dtrain) {
    err  <- getinfo(dtrain, "label") - preds
    grad <- ifelse(err >= 0, -tau, 1 - tau)
    hess <- rep(tau * (1 - tau), length(preds))
    list(grad = grad, hess = hess)
  }

  set.seed(42)
  dtrain <- xgb.DMatrix(X, label = y)
  fit <- xgb.train(
    params  = list(eta = 0.025, max_depth = 4, min_child_weight = 8,
                   subsample = 0.8, colsample_bytree = 0.85, nthread = 4),
    data    = dtrain,
    nrounds = 500,
    obj     = pinball_obj,
    verbose = 0
  )

  # In-sample diagnostics
  log_pred <- predict(fit, dtrain)
  cat(sprintf("In-sample correlation: %.3f\n",
              cor(exp(log_pred), exp(y))))
  cat(sprintf("In-sample mean predicted PPG: %.2f (actual: %.2f)\n",
              mean(exp(log_pred)), mean(exp(y))))
  cat("Top 5 underestimates:\n")
  resid <- exp(y) - exp(log_pred)
  print(tibble(actual = round(exp(y), 2),
                pred   = round(exp(log_pred), 2),
                resid  = round(resid, 2)) |>
        arrange(desc(resid)) |> head(5))

  list(fit = fit, recipe = prep_rec, type = "quantile", tau = tau,
       features = features, position = "TE")
}

qb_data <- readRDS("data/qb_model_data.rds")
te_data <- readRDS("data/te_model_data.rds")

qb_prod <- train_production_model(qb_data, QB_PROD_FEATURES, "QB", QB_YEAR_CUTOFF)
te_prod <- train_te_quantile_model(te_data, TE_PROD_FEATURES, tau = 0.65)

if (!is.null(qb_prod)) saveRDS(qb_prod, "models/qb_production_model.rds")
if (!is.null(te_prod)) saveRDS(te_prod, "models/te_production_model.rds")
cat("\nSaved QB + TE production models.\n")

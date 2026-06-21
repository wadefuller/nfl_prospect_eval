# 11b_temporal_cv_qb_te.R
# ─────────────────────────────────────────────────────────────────────────────
# Rolling temporal cross-validation for QB and TE — the honest OOS eval.
#
# For each test year K in 2016:2023:
#   Train bust + production models on draft_year < K
#   Predict on draft_year == K
#   exp_ppg = combiner(p_made_it) × exp(log_ppg_pred)
#
# Mirrors 11_temporal_cv.R for WR/RB. Architectural pairs (match production):
#   QB bust   = tidymodels XGB classification
#   QB prod   = tidymodels XGB regression on log(ppg)   (MSE)
#   TE bust   = tidymodels XGB classification
#   TE prod   = raw XGBoost with τ=0.65 pinball loss
#
# Combiners (must match 07b_score_qb_te.R):
#   QB: p_eff = clip(0.85 * sqrt(p))
#   TE: p_eff = clip(0.6 * p + 0.4 * 0.65)
#
# Comp-blend sweep included so we can pick QB/TE blend weights based on
# OOS evidence rather than the in-sample test that motivated the current
# QB=1.0, TE=0.5 defaults.
#
# Outputs (output/temporal_cv_qb_te/):
#   oos_predictions.csv            — per-player OOS predictions
#   metrics_summary.csv            — per-position aggregate metrics
#   metrics_by_year.csv            — per-position × test_year
#   comp_blend_sweep_{qb,te}.csv   — MAE for blend weights 0..1
# ─────────────────────────────────────────────────────────────────────────────

setwd("~/Projects/R/college_nfl_model")
suppressMessages({
  library(tidyverse)
  library(tidymodels)
  library(xgboost)
})

source("functions/helpers.R")
source("functions/feature_specs.R")

# Load 08's comp helpers (era_normalize, prep_comp_pool, scaling, distance,
# find_comps, run_comps + COMP_FEATURES lists) without re-running its
# full pipeline.
COMP_LOAD_ONLY <- TRUE
suppressMessages(source("08_player_comps.R"))
rm(COMP_LOAD_ONLY)

# Configs learned from the most recent full 08 run on past-+-current data.
# Re-using them in the CV avoids ~5 min of per-fold weight optimization;
# the optima are stable enough that this is a non-issue.
QB_COMP_CONFIG <- list(
  weights   = c(measurables = 1.5, production = 2.0, profile = 1.0, context = 1.0),
  n_comps   = 3, bandwidth = 2.0
)
TE_COMP_CONFIG <- list(
  weights   = c(measurables = 1.0, production = 1.0, profile = 0.5, context = 1.0),
  n_comps   = 7, bandwidth = 2.0
)

set.seed(42)
dir.create("output/temporal_cv_qb_te", recursive = TRUE, showWarnings = FALSE)

# ── 1. Recipes ─────────────────────────────────────────────────────────────
# Mirror step_mutate zero-fills from 04b/05b so era-conditional features
# (PBP, comp-stack) get 0 rather than a post-era-dominated median for
# players that predate the feature.

build_recipe <- function(model_df, outcome, pos) {
  has_comp <- "has_comp_features" %in% names(model_df)
  rec <- recipe(as.formula(paste(outcome, "~ .")), data = model_df) |>
    update_role(draft_year, new_role = "ID")

  if (pos == "QB" && "has_qb_pbp" %in% names(model_df)) {
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
  if (pos == "TE" && "has_te_pbp" %in% names(model_df)) {
    rec <- rec |> step_mutate(
      catch_rate_te         = if_else(has_te_pbp == 0L, 0, catch_rate_te),
      yards_per_target_te   = if_else(has_te_pbp == 0L, 0, yards_per_target_te),
      yards_per_rec_te      = if_else(has_te_pbp == 0L, 0, yards_per_rec_te),
      explosive_rec_rate_te = if_else(has_te_pbp == 0L, 0, explosive_rec_rate_te),
      target_share_te       = if_else(has_te_pbp == 0L, 0, target_share_te),
      targets_per_game_te   = if_else(has_te_pbp == 0L, 0, targets_per_game_te),
      epa_per_target_te     = if_else(has_te_pbp == 0L, 0, epa_per_target_te),
      epa_per_play_te_pbp   = if_else(has_te_pbp == 0L, 0, epa_per_play_te_pbp)
    )
  }
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

# ── 2. Training ────────────────────────────────────────────────────────────

train_cv_bust <- function(train_data, features, pos) {
  features <- intersect(features, names(train_data))
  model_df <- train_data |>
    filter(has_cfb_data, !is.na(made_it)) |>
    select(all_of(c("made_it", features, "draft_year"))) |>
    mutate(made_it = factor(made_it, levels = c(0, 1),
                             labels = c("bust", "made_it")))
  if (nrow(model_df) < 30 || length(unique(model_df$made_it)) < 2) return(NULL)

  rec  <- build_recipe(model_df, "made_it", pos)
  spec <- boost_tree(trees = 800, tree_depth = 4, learn_rate = 0.02,
                     min_n = 8, sample_size = 0.8) |>
    set_engine("xgboost",
               scale_pos_weight = sum(model_df$made_it == "bust") /
                                  max(1, sum(model_df$made_it == "made_it")),
               nthread = 4) |>
    set_mode("classification")
  fit(workflow() |> add_recipe(rec) |> add_model(spec), data = model_df)
}

# QB production: MSE on log(ppg) via tidymodels workflow (matches 05b non-quantile path)
train_cv_prod_qb <- function(train_data, features) {
  features <- intersect(features, names(train_data))
  model_df <- train_data |>
    filter(has_cfb_data, !is.na(ppg), ppg > 0) |>
    mutate(log_ppg = log(ppg)) |>
    select(all_of(c("log_ppg", features, "draft_year")))
  if (nrow(model_df) < 20) return(NULL)

  rec  <- build_recipe(model_df, "log_ppg", "QB")
  spec <- boost_tree(trees = 500, tree_depth = 4, learn_rate = 0.025,
                     min_n = 8, sample_size = 0.8) |>
    set_engine("xgboost", nthread = 4) |>
    set_mode("regression")
  fit(workflow() |> add_recipe(rec) |> add_model(spec), data = model_df)
}

# TE production: raw XGBoost with τ=0.65 pinball loss (matches 05b quantile path)
train_cv_prod_te <- function(train_data, features, tau = 0.65) {
  features <- intersect(features, names(train_data))
  model_df <- train_data |>
    filter(has_cfb_data, !is.na(ppg), ppg > 0) |>
    mutate(log_ppg = log(ppg)) |>
    select(all_of(c("log_ppg", features, "draft_year")))
  if (nrow(model_df) < 20) return(NULL)

  rec      <- build_recipe(model_df, "log_ppg", "TE")
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
  fit <- xgb.train(
    params  = list(eta = 0.025, max_depth = 4, min_child_weight = 8,
                   subsample = 0.8, colsample_bytree = 0.85, nthread = 4),
    data    = xgb.DMatrix(X, label = y),
    nrounds = 500, obj = pinball_obj, verbose = 0
  )
  list(fit = fit, recipe = prep_rec, type = "quantile")
}

# ── 3. Prediction (matches 07b combiners) ──────────────────────────────────

clip01     <- function(x) pmax(0, pmin(1, x))
combine_qb <- function(p) clip01(0.85 * sqrt(p))
combine_te <- function(p) clip01(0.6 * p + 0.4 * 0.65)

predict_cv_fold <- function(bust_fit, prod_fit, test_data, pos) {
  if (is.null(bust_fit) || is.null(prod_fit)) return(tibble())

  p_made_it <- predict(bust_fit, test_data, type = "prob") |>
    pull(.pred_made_it)

  if (isTRUE(prod_fit$type == "quantile")) {
    baked        <- bake(prod_fit$recipe, new_data = test_data)
    X            <- as.matrix(baked |> select(-any_of(c("log_ppg", "draft_year"))))
    log_ppg_pred <- predict(prod_fit$fit, xgb.DMatrix(X))
  } else {
    log_ppg_pred <- predict(prod_fit, test_data) |> pull(.pred)
  }

  p_eff <- if (pos == "QB") combine_qb(p_made_it) else combine_te(p_made_it)

  test_data |>
    mutate(p_made_it = p_made_it,
           p_eff     = p_eff,
           exp_ppg   = pmax(p_eff * exp(log_ppg_pred), 0))
}

# ── 4. Data ────────────────────────────────────────────────────────────────

cat("Loading model data...\n")
qb_full <- readRDS("data/qb_model_data.rds")
te_full <- readRDS("data/te_model_data.rds")

# qb/te_model_data.rds carry comp_weighted_ppg from 03b's call to
# attach_comp_features. That snapshot uses a comp pool over the entire
# training universe — slightly leaky for OOS evaluation, since for a
# test-year prospect its kNN comps can include same-year peers.
#
# We rebuild comps PER FOLD below (max_pool_year = K - 1, strictly past)
# and use those honest numbers in the comp-blend sweep. The pre-baked
# column is kept around under a different name as a baseline for
# reference but never used in the metrics.
# Note: the leaky comp_weighted_ppg lives on the data as a model feature
# (training-time mirror), so leave it intact for the bust/production model
# fit. The fold-honest comp value is joined separately as
# `comp_weighted_ppg_honest` below and used in the blend sweep.

# Per-fold comp helper. Calls run_comps from 08 with a strict past-only
# pool and aggregates the long-form comps to one comp_weighted_ppg per
# prospect. Returns a tibble with (pfr_player_name, draft_year,
# comp_weighted_ppg).
fold_comps <- function(model_data, feat_list, pos, test_df, max_year, config) {
  scores_df <- test_df |>
    transmute(
      name       = pfr_player_name,
      position   = pos,
      draft_year = draft_year,
      pick       = pick,
      round      = round,
      college    = college,
      tier       = tier,
      exp_ppg    = NA_real_,   # placeholder cols 08's print path expects
      p_made_it  = NA_real_
    )
  res <- run_comps(model_data, feat_list, pos, scores_df,
                   pick_window = 40, config = config,
                   max_pool_year = max_year)
  if (is.null(res$comps) || nrow(res$comps) == 0) {
    return(tibble(pfr_player_name = character(),
                  draft_year      = integer(),
                  comp_weighted_ppg = numeric()))
  }
  res$comps |>
    group_by(name, draft_year) |>
    summarize(comp_weighted_ppg = weighted.mean(comp_ppg, similarity),
              .groups = "drop") |>
    rename(pfr_player_name = name)
}

# ── 5. Rolling temporal CV ─────────────────────────────────────────────────

TEST_YEARS <- 2016:2023
MIN_TRAIN  <- 30
MIN_TEST   <- 3

cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║  ROLLING TEMPORAL CV (QB/TE): Train <K, Test K (2016-2023)   ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

run_cv <- function(full, features, comp_features, comp_config, pos) {
  oos <- list()
  for (K in TEST_YEARS) {
    train <- full |> filter(draft_year < K, !is.na(made_it))
    test  <- full |> filter(draft_year == K, has_cfb_data, !is.na(made_it))
    if (sum(train$has_cfb_data, na.rm = TRUE) < MIN_TRAIN || nrow(test) < MIN_TEST) {
      cat(sprintf("  %s %d: skip (train_cfb=%d, test=%d)\n",
                  pos, K, sum(train$has_cfb_data, na.rm = TRUE), nrow(test)))
      next
    }
    n_prod <- sum(train$ppg > 0, na.rm = TRUE)
    cat(sprintf("  %s %d: train=%d (producers=%d), test=%d\n",
                pos, K, sum(train$has_cfb_data, na.rm = TRUE), n_prod, nrow(test)))

    bust_fit <- train_cv_bust(train, features, pos)
    prod_fit <- if (pos == "QB") train_cv_prod_qb(train, features)
                else              train_cv_prod_te(train, features)

    pred <- predict_cv_fold(bust_fit, prod_fit, test, pos) |>
      mutate(test_year = K)

    # Honest per-fold comps: pool = strictly past
    fcomps <- tryCatch(
      fold_comps(full, comp_features, pos, test,
                 max_year = K - 1, config = comp_config),
      error = function(e) {
        warning(sprintf("[%s %d] comp fold failed: %s", pos, K, conditionMessage(e)))
        tibble(pfr_player_name = character(),
               draft_year = integer(),
               comp_weighted_ppg = numeric())
      })

    pred <- pred |>
      left_join(fcomps |> rename(comp_weighted_ppg_honest = comp_weighted_ppg),
                by = c("pfr_player_name", "draft_year")) |>
      select(pfr_player_name, position, draft_year, test_year,
             round, pick,
             made_it, ppg, p_made_it, p_eff, exp_ppg,
             comp_weighted_ppg,         # leaky (training-time snapshot)
             comp_weighted_ppg_honest)  # past-only per-fold

    oos[[as.character(K)]] <- pred
  }
  bind_rows(oos)
}

qb_oos <- run_cv(qb_full, QB_PROD_FEATURES, QB_COMP_FEATURES, QB_COMP_CONFIG, "QB")
te_oos <- run_cv(te_full, TE_PROD_FEATURES, TE_COMP_FEATURES, TE_COMP_CONFIG, "TE")
oos    <- bind_rows(qb_oos, te_oos)

# ── 6. Aggregate metrics ───────────────────────────────────────────────────

summary_metrics <- function(d, label) {
  d_ok <- d |> filter(!is.na(exp_ppg), !is.na(ppg))
  if (nrow(d_ok) == 0) return(NULL)
  prod_idx <- d_ok$ppg > 0
  tibble(
    split       = label,
    n           = nrow(d_ok),
    n_producers = sum(prod_idx),
    MAE         = mean(abs(d_ok$exp_ppg - d_ok$ppg)),
    MAE_prod    = if (sum(prod_idx) > 0) mean(abs(d_ok$exp_ppg[prod_idx] - d_ok$ppg[prod_idx])) else NA,
    RMSE        = sqrt(mean((d_ok$exp_ppg - d_ok$ppg)^2)),
    cor         = cor(d_ok$exp_ppg, d_ok$ppg),
    bias        = mean(d_ok$exp_ppg - d_ok$ppg),
    bust_acc    = mean((d_ok$p_made_it > 0.5) == (d_ok$made_it == 1L)),
    brier       = mean((d_ok$p_made_it - as.numeric(d_ok$made_it))^2)
  )
}

metrics <- bind_rows(
  summary_metrics(oos,                          "ALL"),
  summary_metrics(oos |> filter(position == "QB"), "QB"),
  summary_metrics(oos |> filter(position == "TE"), "TE")
)

by_year <- oos |>
  group_by(position, test_year) |>
  group_modify(~ summary_metrics(.x, "year")) |>
  ungroup() |>
  select(-split)

cat("\n══ OVERALL ══\n"); print(metrics)
cat("\n══ BY POSITION × TEST YEAR ══\n"); print(by_year, n = 30)

# ── 7. Comp-blend sweep ────────────────────────────────────────────────────
# Does pulling in comp_weighted_ppg actually help out of sample?

cat("\n══ COMP BLEND SWEEP (OOS hurdle × comp) ══\n")
# Sweeps both the leaky (training-time) and honest (past-only per-fold)
# comp signals so we can see how much the leak inflated comp performance.
blend_sweep <- function(d, comp_col, w_grid = seq(0, 1, 0.1)) {
  d_ok <- d |> filter(!is.na(exp_ppg), !is.na(ppg), !is.na(.data[[comp_col]]))
  if (nrow(d_ok) < 5) return(NULL)
  purrr::map_dfr(w_grid, function(w) {
    pred <- w * d_ok$exp_ppg + (1 - w) * d_ok[[comp_col]]
    prod_idx <- d_ok$ppg > 0
    tibble(
      comp_source = comp_col,
      w_model  = w,
      n        = nrow(d_ok),
      MAE      = mean(abs(pred - d_ok$ppg)),
      MAE_prod = if (sum(prod_idx) > 0) mean(abs(pred[prod_idx] - d_ok$ppg[prod_idx])) else NA,
      cor      = cor(pred, d_ok$ppg)
    )
  })
}

sweep_both <- function(d) {
  bind_rows(
    blend_sweep(d, "comp_weighted_ppg"),         # leaky baseline
    blend_sweep(d, "comp_weighted_ppg_honest")   # honest per-fold
  )
}

qb_sweep <- sweep_both(oos |> filter(position == "QB"))
te_sweep <- sweep_both(oos |> filter(position == "TE"))
print_sweep <- function(label, sweep) {
  if (is.null(sweep) || nrow(sweep) == 0) return(invisible())
  cat(sprintf("\n%s:\n", label))
  print(sweep |> mutate(across(where(is.numeric), ~ round(., 3))), n = 30)
  for (src in unique(sweep$comp_source)) {
    sub <- sweep |> filter(comp_source == src)
    best <- sub$w_model[which.min(sub$MAE)]
    cat(sprintf("  → Best %s (%s): model=%.1f / comp=%.1f (OOS MAE %.3f)\n",
                label, src, best, 1 - best, min(sub$MAE)))
  }
}
print_sweep("QB", qb_sweep)
print_sweep("TE", te_sweep)

# ── 8. Save ────────────────────────────────────────────────────────────────

write_csv(oos,     "output/temporal_cv_qb_te/oos_predictions.csv")
saveRDS(oos,       "output/temporal_cv_qb_te/oos_predictions.rds")
write_csv(metrics, "output/temporal_cv_qb_te/metrics_summary.csv")
write_csv(by_year, "output/temporal_cv_qb_te/metrics_by_year.csv")
if (!is.null(qb_sweep)) write_csv(qb_sweep, "output/temporal_cv_qb_te/comp_blend_sweep_qb.csv")
if (!is.null(te_sweep)) write_csv(te_sweep, "output/temporal_cv_qb_te/comp_blend_sweep_te.csv")

cat("\nSaved:\n",
    "  output/temporal_cv_qb_te/oos_predictions.{csv,rds}\n",
    "  output/temporal_cv_qb_te/metrics_{summary,by_year}.csv\n",
    "  output/temporal_cv_qb_te/comp_blend_sweep_{qb,te}.csv\n", sep = "")

# 13_bust_tune.R
# ─────────────────────────────────────────────────────────────────────────────
# Tune the hurdle-probability combiner to minimise OOS MAE.
#
# Architecture: exp_ppg = p_eff * exp(log_ppg_pred)
# where p_eff = T(p_made_it) is some transform of the bust-classifier output.
#
# Candidate transforms (per position):
#   identity:     p_eff = p
#   shrink:       p_eff = a * p + (1 - a) * base_rate              (1 param)
#   linear:       p_eff = a * p + b                                (2 params)
#   isotonic:     p_eff = isoreg(p -> hit) on training fold
#   iso+shrink:   chain isotonic recalibration with shrink-to-base
#   iso+linear:   chain isotonic recalibration with a*p+b
#   power_shrink: p_eff = a * p^gamma + (1-a) * base_rate          (2 params)
#
# We score each transform on the same OOS fold structure as 11_temporal_cv.R,
# but emit raw (untransformed) p_made_it + prod_pred so all transforms are
# post-hoc evaluations on the same predictions.
#
# Outputs:
#   output/bust_tune/oos_raw.csv         — p_made_it, p_iso, prod_pred, actuals
#   output/bust_tune/sweep_results.csv   — MAE for every transform/param combo
#   output/bust_tune/best_combiners.csv  — chosen transform per position
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(tidymodels)
library(xgboost)

set.seed(42)

setwd("~/Projects/R/college_nfl_model")
source("functions/helpers.R")

dir.create("output/bust_tune", recursive = TRUE, showWarnings = FALSE)

# ── 1. Feature sets (canonical from functions/feature_specs.R) ───────────────

source("functions/feature_specs.R")

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

train_cv_bust <- function(train_data, features, pos) {
  year_filter <- if (pos == "RB") quote(has_cfb_data & draft_year >= 2010) else
                                  quote(has_cfb_data)
  model_df <- train_data |>
    filter(!!year_filter) |>
    mutate(made_it = factor(made_it, levels = c(0L, 1L), labels = c("bust", "made_it")))

  av <- intersect(features, names(model_df))
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

train_cv_prod_wr <- function(train_data, features) {
  model_df <- train_data |>
    filter(has_cfb_data, ppg > 0) |>
    mutate(log_ppg = log(ppg))

  av <- intersect(features, names(model_df))
  model_df <- model_df |> select(all_of(c("log_ppg", av, "draft_year")))

  rec  <- build_recipe(model_df, "log_ppg", "WR")
  spec <- boost_tree(trees = 800, tree_depth = 3, learn_rate = 0.02,
                     min_n = 8, sample_size = 0.8) |>
    set_engine("xgboost", nthread = 4) |>
    set_mode("regression")

  fit(workflow() |> add_recipe(rec) |> add_model(spec), model_df)
}

train_cv_prod_rb <- function(train_data, features, tau = 0.70) {
  model_df <- train_data |>
    filter(has_cfb_data, draft_year >= 2010, ppg > 0) |>
    mutate(log_ppg = log(ppg))

  av <- intersect(features, names(model_df))
  has_comp <- "has_comp_features" %in% av

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
  rec <- rec |>
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

predict_raw <- function(bust_fit, prod_fit, test_data) {
  td <- test_data |>
    mutate(made_it = factor(made_it, levels = c(0L, 1L), labels = c("bust", "made_it")))
  p_made_it <- predict(bust_fit, td, type = "prob") |> pull(.pred_made_it)
  if (isTRUE(prod_fit$type == "quantile")) {
    baked <- bake(prod_fit$recipe, new_data = td)
    X     <- as.matrix(baked |> select(-any_of("log_ppg")))
    log_ppg_pred <- predict(prod_fit$fit, xgb.DMatrix(X))
  } else {
    log_ppg_pred <- predict(prod_fit, td) |> pull(.pred)
  }
  test_data |>
    mutate(p_made_it    = p_made_it,
           prod_pred    = exp(log_ppg_pred))
}

# ── 2. Run rolling temporal CV, save raw OOS p_made_it + prod_pred ──────────

RAW_PATH <- "output/bust_tune/oos_raw.csv"
USE_CACHE <- file.exists(RAW_PATH) && !isTRUE(getOption("bust_tune.refresh", FALSE))

if (USE_CACHE) {
  cat(sprintf("Reusing cached raw OOS from %s (set options(bust_tune.refresh=TRUE) to force).\n",
              RAW_PATH))
  raw <- read_csv(RAW_PATH, show_col_types = FALSE)
} else {

cat("Loading model data...\n")
wr_full <- readRDS("data/wr_model_data.rds") |> attach_comp_features()
rb_full <- readRDS("data/rb_model_data.rds") |> attach_comp_features()

TEST_YEARS <- 2016:2023
MIN_TRAIN  <- 40
MIN_TEST   <- 5

raw_results <- list()

for (K in TEST_YEARS) {
  cat(sprintf("── Test year %d ──\n", K))
  for (pos in c("WR", "RB")) {
    full <- if (pos == "WR") wr_full else rb_full
    train <- full |> filter(draft_year < K)
    test  <- full |> filter(draft_year == K, has_cfb_data)
    train_cfb <- train |> filter(has_cfb_data,
                                  if (pos == "RB") draft_year >= 2010 else TRUE)
    if (nrow(train_cfb) < MIN_TRAIN || nrow(test) < MIN_TEST) {
      cat(sprintf("  %s: skip (train=%d, test=%d)\n", pos, nrow(train_cfb), nrow(test)))
      next
    }
    cat(sprintf("  %s: train=%d, test=%d\n", pos, nrow(train_cfb), nrow(test)))

    dy_m <- mean(train_cfb$draft_year)
    dy_s <- sd(train_cfb$draft_year)
    train <- train |> mutate(draft_year_sc = (draft_year - dy_m) / dy_s)
    test  <- test  |> mutate(draft_year_sc = (draft_year - dy_m) / dy_s)

    bust_features <- if (pos == "WR") WR_BUST_FEATURES else RB_BUST_FEATURES
    prod_features <- if (pos == "WR") WR_PROD_FEATURES else RB_PROD_FEATURES

    bust_fit <- train_cv_bust(train, bust_features, pos)
    prod_fit <- if (pos == "WR") train_cv_prod_wr(train, prod_features)
                else            train_cv_prod_rb(train, prod_features)

    base_rate_train <- mean(train_cfb$made_it)

    # Train-fold isotonic calibration of p_made_it
    train_bust_pred <- predict(bust_fit, train_cfb, type = "prob") |>
      pull(.pred_made_it)
    iso_fit <- isoreg(train_bust_pred, train_cfb$made_it)
    iso_x   <- iso_fit$x[iso_fit$ord]
    iso_y   <- iso_fit$yf
    iso_apply <- function(p) {
      approx(iso_x, iso_y,
             xout = pmax(pmin(p, max(iso_x)), min(iso_x)),
             rule = 2, ties = "ordered")$y
    }

    pred <- predict_raw(bust_fit, prod_fit, test) |>
      mutate(p_iso     = iso_apply(p_made_it),
             base_rate = base_rate_train,
             test_year = K)

    raw_results[[paste(pos, K)]] <- pred |>
      select(pfr_player_name, position, draft_year, test_year,
             round, pick, made_it, ppg,
             p_made_it, p_iso, prod_pred, base_rate)
  }
}

raw <- bind_rows(raw_results)
cat(sprintf("\nRaw OOS rows: %d (WR=%d, RB=%d)\n",
            nrow(raw), sum(raw$position == "WR"), sum(raw$position == "RB")))
write_csv(raw, RAW_PATH)
}  # end USE_CACHE branch

# ── 3. Sweep transforms ─────────────────────────────────────────────────────

clip01 <- function(x) pmax(0, pmin(1, x))
mae    <- function(a, p) mean(abs(a - p))

eval_for <- function(df, p_eff) mae(df$ppg, pmax(p_eff * df$prod_pred, 0))

alpha_grid <- seq(0.0, 1.0, by = 0.05)
beta_grid  <- seq(-0.15, 0.15, by = 0.025)
gamma_grid <- c(0.5, 0.75, 1.0, 1.25, 1.5, 2.0, 3.0)

sweep_pos <- function(df_pos, label) {
  cat(sprintf("\n══ Sweeping %s (n=%d, base_p_mean=%.3f) ══\n",
              label, nrow(df_pos), mean(df_pos$p_made_it)))

  rows <- list()
  add  <- function(...) rows[[length(rows)+1]] <<- tibble(position = label, ...)

  add(transform="identity", alpha=1, beta=0, gamma=1, isotonic=FALSE,
      mae=eval_for(df_pos, df_pos$p_made_it))

  for (a in alpha_grid)
    add(transform="shrink", alpha=a, beta=0, gamma=1, isotonic=FALSE,
        mae=eval_for(df_pos, clip01(a*df_pos$p_made_it + (1-a)*df_pos$base_rate)))

  for (a in alpha_grid) for (b in beta_grid)
    add(transform="linear", alpha=a, beta=b, gamma=1, isotonic=FALSE,
        mae=eval_for(df_pos, clip01(a*df_pos$p_made_it + b)))

  for (a in alpha_grid) for (g in gamma_grid)
    add(transform="power_shrink", alpha=a, beta=0, gamma=g, isotonic=FALSE,
        mae=eval_for(df_pos, clip01(a*(df_pos$p_made_it^g) + (1-a)*df_pos$base_rate)))

  add(transform="isotonic", alpha=1, beta=0, gamma=1, isotonic=TRUE,
      mae=eval_for(df_pos, df_pos$p_iso))

  for (a in alpha_grid)
    add(transform="iso_shrink", alpha=a, beta=0, gamma=1, isotonic=TRUE,
        mae=eval_for(df_pos, clip01(a*df_pos$p_iso + (1-a)*df_pos$base_rate)))

  for (a in alpha_grid) for (b in beta_grid)
    add(transform="iso_linear", alpha=a, beta=b, gamma=1, isotonic=TRUE,
        mae=eval_for(df_pos, clip01(a*df_pos$p_iso + b)))

  bind_rows(rows) |> arrange(mae)
}

wr_sweep <- sweep_pos(raw |> filter(position == "WR"), "WR")
rb_sweep <- sweep_pos(raw |> filter(position == "RB"), "RB")
all_sweep <- bind_rows(wr_sweep, rb_sweep)
write_csv(all_sweep, "output/bust_tune/sweep_results.csv")

cat("\nWR top 8 by MAE:\n");  print(wr_sweep |> head(8))
cat("\nRB top 8 by MAE:\n");  print(rb_sweep |> head(8))

# Baseline (current production: WR identity, RB shrink alpha=0.25)
baseline_wr_mae <- wr_sweep |> filter(transform == "identity") |> pull(mae)
baseline_rb_mae <- rb_sweep |> filter(transform == "shrink", abs(alpha - 0.25) < 1e-6) |>
  pull(mae)

cat(sprintf("\nBaseline WR MAE (identity):       %.4f\n", baseline_wr_mae))
cat(sprintf("Baseline RB MAE (shrink alpha=.25): %.4f\n", baseline_rb_mae))

best_wr <- wr_sweep |> dplyr::slice(1)
best_rb <- rb_sweep |> dplyr::slice(1)

cat(sprintf("\nBest WR: %s (alpha=%.2f beta=%.3f gamma=%.2f iso=%s)  MAE=%.4f  Δ=%.4f\n",
            best_wr$transform, best_wr$alpha, best_wr$beta, best_wr$gamma,
            best_wr$isotonic, best_wr$mae, best_wr$mae - baseline_wr_mae))
cat(sprintf("Best RB: %s (alpha=%.2f beta=%.3f gamma=%.2f iso=%s)  MAE=%.4f  Δ=%.4f\n",
            best_rb$transform, best_rb$alpha, best_rb$beta, best_rb$gamma,
            best_rb$isotonic, best_rb$mae, best_rb$mae - baseline_rb_mae))

write_csv(bind_rows(best_wr, best_rb), "output/bust_tune/best_combiners.csv")

# ── 4. Combined OOS metrics under chosen combiners ──────────────────────────

apply_combiner <- function(df, row) {
  p <- if (row$isotonic) df$p_iso else df$p_made_it
  switch(row$transform,
         identity     = p,
         shrink       = clip01(row$alpha * p + (1 - row$alpha) * df$base_rate),
         linear       = clip01(row$alpha * p + row$beta),
         power_shrink = clip01(row$alpha * (p ^ row$gamma) + (1 - row$alpha) * df$base_rate),
         isotonic     = p,
         iso_shrink   = clip01(row$alpha * p + (1 - row$alpha) * df$base_rate),
         iso_linear   = clip01(row$alpha * p + row$beta),
         stop("unknown transform"))
}

raw_wr <- raw |> filter(position == "WR") |>
  mutate(p_eff = apply_combiner(pick(p_made_it, p_iso, base_rate), best_wr),
         exp_ppg = pmax(p_eff * prod_pred, 0))
raw_rb <- raw |> filter(position == "RB") |>
  mutate(p_eff = apply_combiner(pick(p_made_it, p_iso, base_rate), best_rb),
         exp_ppg = pmax(p_eff * prod_pred, 0))
new_oos <- bind_rows(raw_wr, raw_rb)

new_metrics <- new_oos |>
  group_by(position) |>
  summarise(n = n(),
            mae = round(mean(abs(ppg - exp_ppg)), 3),
            cor = round(cor(ppg, exp_ppg), 3),
            bias = round(mean(ppg - exp_ppg), 3),
            .groups = "drop")
overall_new <- new_oos |>
  summarise(position = "ALL", n = n(),
            mae = round(mean(abs(ppg - exp_ppg)), 3),
            cor = round(cor(ppg, exp_ppg), 3),
            bias = round(mean(ppg - exp_ppg), 3))

cat("\n══ OOS metrics with tuned combiners (in-sample tuned, optimistic) ══\n")
print(bind_rows(new_metrics, overall_new))

# ── 5. Leave-one-fold-out validation ────────────────────────────────────────
# For each test year K, pick the best combiner using OOS from all OTHER years.
# Evaluate on year K. Sum gives a fair OOS estimate of the *tuned* combiner.

cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║  LOFO VALIDATION — fair OOS for the tuned combiner          ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n")

all_combiners <- bind_rows(wr_sweep, rb_sweep) |>
  distinct(position, transform, alpha, beta, gamma, isotonic)

eval_on <- function(df, row) {
  pmax(apply_combiner(df, row) * df$prod_pred, 0)
}

lofo_results <- list()
for (pos in c("WR", "RB")) {
  pos_raw <- raw |> filter(position == pos)
  combos  <- all_combiners |> filter(position == pos)

  for (K in sort(unique(pos_raw$test_year))) {
    train_df <- pos_raw |> filter(test_year != K)
    test_df  <- pos_raw |> filter(test_year == K)

    # Score every combiner on the rest-of-folds, pick winner
    train_maes <- purrr::map_dbl(seq_len(nrow(combos)), function(i) {
      mean(abs(train_df$ppg - eval_on(train_df, combos[i, ])))
    })
    win_idx <- which.min(train_maes)
    winner <- combos[win_idx, ] |> mutate(train_mae = train_maes[win_idx])
    test_pred <- eval_on(test_df, winner)
    test_mae  <- mean(abs(test_df$ppg - test_pred))

    lofo_results[[length(lofo_results)+1]] <- tibble(
      position  = pos,
      test_year = K,
      n         = nrow(test_df),
      transform = winner$transform,
      alpha     = winner$alpha,
      beta      = winner$beta,
      gamma     = winner$gamma,
      isotonic  = winner$isotonic,
      train_mae = winner$train_mae,
      test_mae  = test_mae
    )
  }
}

lofo <- bind_rows(lofo_results)
write_csv(lofo, "output/bust_tune/lofo_results.csv")

cat("\nLOFO per-fold winners:\n")
print(lofo, n = Inf)

# Weighted (by fold n) test MAE under LOFO
lofo_summary <- lofo |>
  rename(fold_n = n) |>
  group_by(position) |>
  summarise(
    rows     = sum(fold_n),
    test_mae = round(weighted.mean(test_mae, fold_n), 4),
    .groups  = "drop"
  ) |>
  rename(n = rows)
overall_lofo <- lofo |>
  rename(fold_n = n) |>
  summarise(position = "ALL",
            n        = sum(fold_n),
            test_mae = round(weighted.mean(test_mae, fold_n), 4))

cat("\nLOFO MAE (fair OOS for the *tuned* combiner):\n")
print(bind_rows(lofo_summary, overall_lofo))

baseline_overall <- raw |>
  mutate(p_eff = if_else(position == "WR",
                         p_made_it,
                         pmin(pmax(0.25 * p_made_it + 0.75 * base_rate, 0), 1)),
         exp_ppg = pmax(p_eff * prod_pred, 0)) |>
  summarise(mae = mean(abs(ppg - exp_ppg))) |> pull(mae)
cat(sprintf("\nBaseline overall OOS MAE (current production combiner): %.4f\n",
            baseline_overall))

cat("\nDone. If LOFO MAE < baseline, update functions/helpers.R + 11_temporal_cv.R\n")
cat("with the chosen transform (most-frequent winner per position is the safe pick).\n")

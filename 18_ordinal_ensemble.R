# 18_ordinal_ensemble.R
# ─────────────────────────────────────────────────────────────────────────────
# Experimental ensemble: XGBoost multiclass + ordinal regression, predicting
# a distribution over outcome buckets {bust, flex, elite, league_winner}.
#
# Coexists with the current hurdle model (P(made_it) × E[PPG | made_it]) —
# does NOT replace it. The hurdle model is great for ranking; the bucket
# distribution is more interpretable for fans (e.g. "30% bust, 5% league
# winner" tells a richer story than "exp_ppg = 6.2").
#
# Architecture:
#   1. Define position-specific PPG cutoffs from training-data quantiles.
#   2. Bin each player into 4 ordinal buckets.
#   3. Fit XGBoost with multi:softprob on the bucket label.
#   4. Fit ordinal regression (cumulative-link, proportional odds) on the
#      same features.
#   5. Ensemble: geometric mean of the two probability vectors, renormalized.
#
# Note: this version uses ordinal::clm (frequentist) for fast iteration.
# Once rstanarm finishes installing, the clm fits can be swapped for
# stan_polr to recover full Bayesian posterior intervals on bucket
# probabilities. The architecture and ensemble logic are identical.
#
# Outputs:
#   output/ordinal_ensemble/
#     metrics.csv           — per-fold log-loss, accuracy, MAE-from-buckets
#     bucket_calibration.csv — observed bucket rate vs predicted
#     oos_predictions.csv   — per-player bucket probabilities + observed bucket
# ─────────────────────────────────────────────────────────────────────────────

setwd("~/Projects/R/college_nfl_model")
suppressMessages({
  library(tidyverse)
  library(tidymodels)
  library(xgboost)
  library(ordinal)
  library(rstanarm)
})

# Bayesian ordinal regression backend. "stan" = full Bayesian via rstanarm
# (slower, posterior credible intervals). "clm" = frequentist proportional
# odds via ordinal::clm (fast, point estimates + standard errors).
ORD_BACKEND <- Sys.getenv("ORD_BACKEND", unset = "clm")

source("functions/helpers.R")
source("functions/feature_specs.R")

set.seed(42)

# Pull CV harness functions for build_recipe
src11 <- readLines("11_temporal_cv.R")
fn_start    <- grep("^build_recipe <- function", src11)
load_marker <- grep("^# ── 5. Load data", src11)
eval(parse(text = src11[fn_start[1]:(load_marker - 1)]))

# ── 1. Bucket definitions ────────────────────────────────────────────────────
# Position-specific cuts based on producer-only PPG quantiles. The "bust"
# bucket is anyone with PPG = 0 (didn't qualify for an NFL season). Other
# cuts use p75 and p90 of producers — meaningful tier breaks.
#
# WR producers: p75=7.62, p90=9.75
# RB producers: p75=9.75, p90=12.54

BUCKET_LEVELS <- c("bust", "flex", "elite", "league_winner")

bucket_cuts <- list(
  WR = c(0, 7.62, 9.75),  # cuts at producer p75 and p90
  RB = c(0, 9.75, 12.54)
)

# Bucket midpoints — used to convert P(bucket) → expected PPG. For the open
# upper bucket we use a position-specific 95th-pct producer value (the top
# tier doesn't have a natural midpoint).
bucket_midpoints <- list(
  WR = c(bust = 0, flex = 4.5, elite = 8.5, league_winner = 11.5),
  RB = c(bust = 0, flex = 6.5, elite = 11.0, league_winner = 14.5)
)

assign_bucket <- function(ppg, pos) {
  cuts <- bucket_cuts[[pos]]
  out <- character(length(ppg))
  out[ppg <= cuts[1]] <- "bust"
  out[ppg >  cuts[1] & ppg <  cuts[2]] <- "flex"
  out[ppg >= cuts[2] & ppg <  cuts[3]] <- "elite"
  out[ppg >= cuts[3]] <- "league_winner"
  factor(out, levels = BUCKET_LEVELS, ordered = TRUE)
}

cat("Loading data...\n")
wr_full <- readRDS("data/wr_model_data.rds") |> attach_comp_features() |>
  mutate(bucket = assign_bucket(ppg, "WR"))
rb_full <- readRDS("data/rb_model_data.rds") |> attach_comp_features() |>
  mutate(bucket = assign_bucket(ppg, "RB"))

cat("\n=== Bucket distribution (training data) ===\n")
bind_rows(wr_full |> mutate(pos = "WR"), rb_full |> mutate(pos = "RB")) |>
  filter(has_cfb_data) |>
  count(pos, bucket) |>
  group_by(pos) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  print(n = Inf)

# ── 2. XGBoost multiclass model ──────────────────────────────────────────────

train_xgb_multiclass <- function(train_df, features, pos) {
  bf <- intersect(features, names(train_df))
  model_df <- train_df |>
    filter(has_cfb_data, if (pos == "RB") draft_year >= 2010 else TRUE) |>
    select(all_of(c("bucket", bf, "draft_year")))

  rec <- build_recipe(model_df, "bucket", pos) |>
    step_string2factor(all_nominal_predictors())  # safety
  prep_rec <- prep(rec, training = model_df)
  X_train  <- bake(prep_rec, new_data = NULL) |>
                select(-bucket, -draft_year) |> as.matrix()
  y_train  <- as.integer(model_df$bucket) - 1L  # xgb wants 0-indexed

  dm <- xgb.DMatrix(X_train, label = y_train)
  fit <- xgb.train(
    params = list(
      objective    = "multi:softprob",
      num_class    = 4,
      eta          = 0.05,
      max_depth    = 4,
      min_child_weight = 8,
      subsample    = 0.8,
      colsample_bytree = 0.8,
      eval_metric  = "mlogloss"
    ),
    data    = dm,
    nrounds = 500,
    verbose = 0
  )
  list(fit = fit, recipe = prep_rec)
}

predict_xgb_multiclass <- function(fit_obj, test_df) {
  X <- bake(fit_obj$recipe, new_data = test_df) |>
         select(-bucket, -draft_year) |> as.matrix()
  p <- predict(fit_obj$fit, xgb.DMatrix(X))
  m <- matrix(p, ncol = 4, byrow = TRUE)
  colnames(m) <- BUCKET_LEVELS
  as_tibble(m)
}

# ── 3. Ordinal regression (proportional odds, frequentist for now) ──────────
# clm is sensitive to high-correlation predictors and large feature counts.
# We restrict to a curated subset chosen for low collinearity and known
# strong signal. When rstanarm is available we'll swap in stan_polr with
# weakly-informative priors (no formula change; the ordinal::clm interface
# is intentionally compatible).

ORD_FEATURES_WR <- c("sqrt_pick", "rec_yards_final", "rec_yards_penult",
                      "rec_td_rate", "dominator_rate", "ypr",
                      "speed_score", "recruit_rating", "age",
                      "comp_weighted_ppg", "draft_year_sc")
ORD_FEATURES_RB <- c("sqrt_pick", "rush_yards_final", "rush_td_final",
                      "yards_per_touch", "speed_score", "forty",
                      "recruit_rating", "age", "comp_weighted_ppg",
                      "draft_year_sc", "rb_rec_yards")

train_ord <- function(train_df, features, pos) {
  feats <- intersect(features, names(train_df))
  d <- train_df |>
    filter(has_cfb_data, if (pos == "RB") draft_year >= 2010 else TRUE) |>
    select(all_of(c("bucket", feats))) |>
    drop_na()  # ordinal models don't handle NAs natively
  if (nrow(d) < 50) return(NULL)
  form <- as.formula(paste("bucket ~", paste(feats, collapse = " + ")))

  if (ORD_BACKEND == "stan") {
    # Bayesian proportional odds via rstanarm. Weakly informative priors,
    # 2 chains × 1000 iters (compromise between speed and posterior quality
    # for CV; 4 chains × 2000 would be standard for inference).
    tryCatch(
      stan_polr(form, data = d,
                prior = R2(0.3, "mean"),  # weakly informative R² prior
                prior_counts = dirichlet(1),
                chains = 2, iter = 1000, seed = 42, refresh = 0),
      error = function(e) { message("  stan_polr fit failed: ", conditionMessage(e)); NULL })
  } else {
    tryCatch(clm(form, data = d, link = "logit"),
             error = function(e) { message("  clm fit failed: ", conditionMessage(e)); NULL })
  }
}

predict_ord <- function(fit, test_df, features) {
  if (is.null(fit)) {
    out <- matrix(rep(c(0.5, 0.3, 0.15, 0.05), nrow(test_df)),  # uninformative prior
                  ncol = 4, byrow = TRUE)
    colnames(out) <- BUCKET_LEVELS
    return(as_tibble(out))
  }
  feats <- intersect(features, names(test_df))
  td <- test_df |> select(all_of(feats))
  # Median-impute NAs at predict time (matches XGB behavior of handling missingness)
  for (c in feats) {
    if (any(is.na(td[[c]]))) td[[c]][is.na(td[[c]])] <- median(td[[c]], na.rm = TRUE)
  }
  if (inherits(fit, "stanreg")) {
    # rstanarm's stan_polr stores cutpoints in stan_summary as "1|2", "2|3",
    # "3|4". Extract by exact name match to avoid catching unrelated rows.
    L <- posterior_linpred(fit, newdata = td)  # [draws × n_test]
    ss <- fit$stan_summary
    cutpoint_names <- c("bust|flex", "flex|elite", "elite|league_winner")
    stopifnot(all(cutpoint_names %in% rownames(ss)))
    zeta_vals <- as.numeric(ss[cutpoint_names, "mean"])
    K <- 4L
    p <- matrix(0, nrow = ncol(L), ncol = K)
    for (i in seq_len(ncol(L))) {
      lin <- L[, i]
      cumprob <- vapply(zeta_vals, function(z) mean(plogis(z - lin)), numeric(1))
      p[i, 1] <- cumprob[1]
      p[i, 2] <- cumprob[2] - cumprob[1]
      p[i, 3] <- cumprob[3] - cumprob[2]
      p[i, 4] <- 1 - cumprob[3]
    }
    colnames(p) <- BUCKET_LEVELS
    as_tibble(p)
  } else {
    p <- predict(fit, newdata = td, type = "prob")$fit
    if (is.null(colnames(p))) colnames(p) <- BUCKET_LEVELS
    as_tibble(p)
  }
}

# ── 4. Ensemble ──────────────────────────────────────────────────────────────
# Geometric mean of two probability vectors, renormalized. Penalizes
# confident-wrong (one model says "elite", other says "bust" → ensemble
# spreads probability rather than averaging in the middle).

ensemble_probs <- function(p1, p2) {
  geom <- sqrt(p1 * p2)
  geom / rowSums(geom)
}

# ── 5. Temporal CV ───────────────────────────────────────────────────────────

TEST_YEARS <- 2016:2023
MIN_TRAIN  <- 40
MIN_TEST   <- 5

oos_results <- list()
for (K in TEST_YEARS) {
  for (pos in c("WR", "RB")) {
    full <- if (pos == "WR") wr_full else rb_full
    train <- full |> filter(draft_year < K)
    test  <- full |> filter(draft_year == K, has_cfb_data)
    train_cfb <- train |> filter(has_cfb_data,
                                  if (pos == "RB") draft_year >= 2010 else TRUE)
    if (nrow(train_cfb) < MIN_TRAIN || nrow(test) < MIN_TEST) next

    cat(sprintf("── %s %d  train=%d  test=%d ──\n", pos, K, nrow(train_cfb), nrow(test)))

    dy_m <- mean(train_cfb$draft_year); dy_s <- sd(train_cfb$draft_year)
    train <- train |> mutate(draft_year_sc = (draft_year - dy_m) / dy_s)
    test  <- test  |> mutate(draft_year_sc = (draft_year - dy_m) / dy_s)

    # XGB
    feats_full <- if (pos == "WR") WR_PROD_FEATURES else RB_PROD_FEATURES
    xgb_fit <- train_xgb_multiclass(train, feats_full, pos)
    p_xgb   <- predict_xgb_multiclass(xgb_fit, test)

    # Ordinal
    ord_feats <- if (pos == "WR") ORD_FEATURES_WR else ORD_FEATURES_RB
    ord_fit   <- train_ord(train, ord_feats, pos)
    p_ord     <- predict_ord(ord_fit, test, ord_feats)

    # Ensemble
    p_ens <- ensemble_probs(as.matrix(p_xgb), as.matrix(p_ord))
    colnames(p_ens) <- BUCKET_LEVELS

    # Expected PPG via bucket-midpoint mapping
    midpts   <- bucket_midpoints[[pos]]
    exp_ppg_xgb <- as.matrix(p_xgb) %*% midpts
    exp_ppg_ord <- as.matrix(p_ord) %*% midpts
    exp_ppg_ens <- p_ens             %*% midpts

    oos_results[[paste(pos, K)]] <- tibble(
      pfr_player_name = test$pfr_player_name,
      position        = pos,
      draft_year      = test$draft_year,
      pick            = test$pick,
      ppg             = test$ppg,
      observed_bucket = as.character(test$bucket),
      p_xgb_bust      = p_xgb$bust,
      p_xgb_flex      = p_xgb$flex,
      p_xgb_elite     = p_xgb$elite,
      p_xgb_league    = p_xgb$league_winner,
      p_ord_bust      = p_ord$bust,
      p_ord_flex      = p_ord$flex,
      p_ord_elite     = p_ord$elite,
      p_ord_league    = p_ord$league_winner,
      p_ens_bust      = p_ens[, "bust"],
      p_ens_flex      = p_ens[, "flex"],
      p_ens_elite     = p_ens[, "elite"],
      p_ens_league    = p_ens[, "league_winner"],
      exp_ppg_xgb     = as.numeric(exp_ppg_xgb),
      exp_ppg_ord     = as.numeric(exp_ppg_ord),
      exp_ppg_ens     = as.numeric(exp_ppg_ens)
    )
  }
}

oos <- bind_rows(oos_results)

# ── 6. Metrics ───────────────────────────────────────────────────────────────

log_loss <- function(p_mat, observed_idx) {
  # observed_idx: 1..4 integer per row
  eps <- 1e-12
  p <- pmax(pmin(p_mat[cbind(seq_len(nrow(p_mat)), observed_idx)], 1 - eps), eps)
  -mean(log(p))
}

obs_idx <- match(oos$observed_bucket, BUCKET_LEVELS)

p_xgb_mat <- as.matrix(oos[, c("p_xgb_bust", "p_xgb_flex",
                                "p_xgb_elite", "p_xgb_league")])
p_ord_mat <- as.matrix(oos[, c("p_ord_bust", "p_ord_flex",
                                "p_ord_elite", "p_ord_league")])
p_ens_mat <- as.matrix(oos[, c("p_ens_bust", "p_ens_flex",
                                "p_ens_elite", "p_ens_league")])

# Top-1 bucket prediction
pred_xgb <- BUCKET_LEVELS[apply(p_xgb_mat, 1, which.max)]
pred_ord <- BUCKET_LEVELS[apply(p_ord_mat, 1, which.max)]
pred_ens <- BUCKET_LEVELS[apply(p_ens_mat, 1, which.max)]

metrics <- tibble(
  model = c("XGBoost multiclass", "Ordinal (clm)", "Ensemble (geom mean)"),
  log_loss = c(log_loss(p_xgb_mat, obs_idx),
                log_loss(p_ord_mat, obs_idx),
                log_loss(p_ens_mat, obs_idx)),
  accuracy = c(mean(pred_xgb == oos$observed_bucket),
                mean(pred_ord == oos$observed_bucket),
                mean(pred_ens == oos$observed_bucket)),
  mae_from_bucket = c(mean(abs(oos$ppg - oos$exp_ppg_xgb)),
                       mean(abs(oos$ppg - oos$exp_ppg_ord)),
                       mean(abs(oos$ppg - oos$exp_ppg_ens)))
)

cat("\n══ Bucket-prediction metrics (8-fold temporal CV, 2016-2023) ══\n")
print(metrics |> mutate(across(where(is.numeric), ~ round(.x, 3))))

# Per-position accuracy
cat("\n── Accuracy by position (ensemble) ──\n")
oos |> mutate(correct = pred_ens == observed_bucket) |>
  group_by(position) |>
  summarise(n = n(),
            accuracy = round(mean(correct), 3),
            top2_match = round(mean(observed_bucket %in%
              c(pred_ens, BUCKET_LEVELS[pmin(4, match(pred_ens, BUCKET_LEVELS) + 1L)],
                BUCKET_LEVELS[pmax(1, match(pred_ens, BUCKET_LEVELS) - 1L)])), 3),
            .groups = "drop") |>
  print()

# Bucket-level calibration
cat("\n── Per-bucket calibration (ensemble): predicted prob vs observed rate ──\n")
calib_rows <- list()
for (b in BUCKET_LEVELS) {
  col <- paste0("p_ens_", c("bust", "flex", "elite", "league")[match(b, BUCKET_LEVELS)])
  buckets <- cut(oos[[col]], breaks = c(0, .1, .25, .5, .75, .9, 1), include.lowest = TRUE)
  calib_rows[[b]] <- oos |>
    mutate(prob_bucket = buckets, is_obs = observed_bucket == b) |>
    group_by(prob_bucket) |>
    summarise(n = n(), pred_prob = round(mean(.data[[col]]), 3),
              obs_rate = round(mean(is_obs), 3), .groups = "drop") |>
    mutate(target = b)
}
calib <- bind_rows(calib_rows)
print(calib, n = Inf)

# Compare to current hurdle model's MAE on the same OOS set, if available
hurdle_mae <- NA
if (file.exists("output/temporal_cv/oos_predictions.csv")) {
  hurdle <- read_csv("output/temporal_cv/oos_predictions.csv", show_col_types = FALSE)
  if ("exp_ppg" %in% names(hurdle)) {
    hurdle_mae <- mean(abs(hurdle$ppg - hurdle$exp_ppg))
    cat(sprintf("\n── Comparison to current hurdle model ──\n"))
    cat(sprintf("Hurdle model MAE  : %.3f\n", hurdle_mae))
    cat(sprintf("Ensemble MAE      : %.3f\n", metrics$mae_from_bucket[3]))
    cat(sprintf("(Ensemble loses precision because it maps to 4 bucket midpoints; raw bucket distribution is the real value-add.)\n"))
  }
}

# ── 7. Save outputs ──────────────────────────────────────────────────────────

dir.create("output/ordinal_ensemble", showWarnings = FALSE, recursive = TRUE)
write_csv(metrics, "output/ordinal_ensemble/metrics.csv")
write_csv(calib,   "output/ordinal_ensemble/bucket_calibration.csv")
write_csv(oos,     "output/ordinal_ensemble/oos_predictions.csv")
cat("\nSaved: output/ordinal_ensemble/\n")

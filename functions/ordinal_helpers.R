# functions/ordinal_helpers.R
# ─────────────────────────────────────────────────────────────────────────────
# Ordinal-bucket model: classifies prospects into 5 ordered outcome tiers
#   {bust, bench, flex, elite, league_winner}
# via an XGBoost-multiclass + Bayesian-ordinal ensemble.
#
# Coexists with the continuous hurdle model in helpers.R; both score paths
# call into score_class() and append bucket probabilities to its output.
#
# Bucket definitions (from training-data producer quantiles):
#   bust          = PPG = 0           (never qualified for an NFL season)
#   bench         = 0 < PPG < p25     (qualified but barely; depth/special teams)
#   flex          = p25 ≤ PPG < p75   (lineup-flexible depth piece)
#   elite         = p75 ≤ PPG < p90   (high-end starter)
#   league_winner = PPG ≥ p90         (top-3 fantasy producer at position)
#
# Position-specific cuts because RB ceilings are higher than WR. Cuts are
# frozen here for stability across model retrains.
# ─────────────────────────────────────────────────────────────────────────────

BUCKET_LEVELS <- c("bust", "bench", "flex", "elite", "league_winner")

# Frozen cuts from 2026-05-10 training data — producer p25 / p75 / p90.
BUCKET_CUTS <- list(
  WR = c(0, 4.43, 7.62, 9.75),
  RB = c(0, 5.74, 9.75, 12.54)
)

# Midpoints used to convert P(bucket) → expected PPG. The open upper bucket
# uses position p95 producer value as a conservative point estimate.
BUCKET_MIDPOINTS <- list(
  WR = c(bust = 0, bench = 2.5, flex = 6.0, elite = 8.7, league_winner = 11.5),
  RB = c(bust = 0, bench = 3.5, flex = 7.7, elite = 11.0, league_winner = 14.5)
)

assign_bucket <- function(ppg, pos) {
  cuts <- BUCKET_CUTS[[pos]]
  out <- character(length(ppg))
  out[ppg <= cuts[1]] <- "bust"
  out[ppg >  cuts[1] & ppg <  cuts[2]] <- "bench"
  out[ppg >= cuts[2] & ppg <  cuts[3]] <- "flex"
  out[ppg >= cuts[3] & ppg <  cuts[4]] <- "elite"
  out[ppg >= cuts[4]] <- "league_winner"
  factor(out, levels = BUCKET_LEVELS, ordered = TRUE)
}

# Curated feature subset for the ordinal regression (clm). Keeping it
# lean — clm with 60+ features hits convergence issues and over-relies on
# noisy collinear predictors. The XGB side uses the full feature spec.
ORD_FEATURES_WR <- c(
  "sqrt_pick", "rec_yards_final", "rec_yards_penult",
  "rec_td_rate", "dominator_rate", "ypr",
  "speed_score", "recruit_rating", "age",
  "comp_weighted_ppg", "draft_year_sc"
)
ORD_FEATURES_RB <- c(
  "sqrt_pick", "rush_yards_final", "rush_td_final",
  "yards_per_touch", "speed_score", "forty",
  "recruit_rating", "age", "comp_weighted_ppg",
  "draft_year_sc", "rb_rec_yards"
)

# ── Train XGBoost multiclass (full feature spec) ─────────────────────────────
# Re-uses build_recipe() from 11_temporal_cv.R for era zero-fill etc.

train_xgb_bucket <- function(model_data, features, pos, build_recipe_fn) {
  bf <- intersect(features, names(model_data))
  model_df <- model_data |>
    dplyr::filter(has_cfb_data,
                  if (pos == "RB") draft_year >= 2010 else TRUE) |>
    dplyr::mutate(bucket = assign_bucket(ppg, pos)) |>
    dplyr::select(dplyr::all_of(c("bucket", bf, "draft_year")))

  rec <- build_recipe_fn(model_df, "bucket", pos)
  prep_rec <- recipes::prep(rec, training = model_df)
  X <- recipes::bake(prep_rec, new_data = NULL) |>
         dplyr::select(-bucket, -draft_year) |> as.matrix()
  y <- as.integer(model_df$bucket) - 1L

  fit <- xgboost::xgb.train(
    params = list(
      objective    = "multi:softprob",
      num_class    = length(BUCKET_LEVELS),
      eta          = 0.05,
      max_depth    = 4,
      min_child_weight = 8,
      subsample    = 0.8,
      colsample_bytree = 0.8,
      eval_metric  = "mlogloss"
    ),
    data    = xgboost::xgb.DMatrix(X, label = y),
    nrounds = 500,
    verbose = 0
  )
  list(fit = fit, recipe = prep_rec)
}

predict_xgb_bucket <- function(fit_obj, df) {
  # Cast all integer columns to double before bake. step_impute_median
  # produces double medians and can't assign them back to integer columns
  # at score time (vctrs throws "loss of precision"). Training data had no
  # NAs in those columns so the issue was masked there.
  df <- df |>
    dplyr::mutate(dplyr::across(dplyr::where(is.integer), as.numeric))
  X <- recipes::bake(fit_obj$recipe, new_data = df)
  for (col in c("bucket", "draft_year")) X[[col]] <- NULL
  X <- as.matrix(X)
  p <- predict(fit_obj$fit, xgboost::xgb.DMatrix(X))
  K <- length(BUCKET_LEVELS)
  m <- matrix(p, ncol = K, byrow = TRUE)
  colnames(m) <- BUCKET_LEVELS
  tibble::as_tibble(m)
}

# ── Train Bayesian proportional-odds regression ─────────────────────────────
# rstanarm::stan_polr fits a cumulative-link logit ("proportional odds")
# model via Stan with weakly informative priors. Compared to ordinal::clm
# (frequentist baseline) the Bayesian version provides posterior draws of
# the cutpoints + coefficients, enabling credible intervals on bucket
# probabilities — the actual reason to be Bayesian here.
#
# Tuning notes:
#   - 4 chains × 4000 iters with default 50% warmup is the rstanarm-recommended
#     setting for convergence on small problems (R̂ < 1.05 across draws).
#   - prior = R2(0.5, "mean") ≈ weakly informative ("expected R² ~ 0.5"),
#     equivalent to N(0, ~5) on standardized coefficients. More permissive
#     than the R2(0.3) we tried earlier, which over-shrunk small effects.
#   - cores = 4 runs chains in parallel; ~30-60s per fit on this data size.

train_stan_bucket <- function(model_data, features, pos,
                               chains = 4, iter = 4000, cores = 4, seed = 42) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("train_stan_bucket: rstanarm package not installed")
  }
  feats <- intersect(features, names(model_data))
  d <- model_data |>
    dplyr::filter(has_cfb_data,
                  if (pos == "RB") draft_year >= 2010 else TRUE) |>
    dplyr::mutate(bucket = assign_bucket(ppg, pos)) |>
    dplyr::select(dplyr::all_of(c("bucket", feats))) |>
    tidyr::drop_na()
  if (nrow(d) < 50) {
    warning("train_stan_bucket: insufficient training rows (", nrow(d), ")")
    return(NULL)
  }
  form <- stats::as.formula(paste("bucket ~", paste(feats, collapse = " + ")))
  message(sprintf("  stan_polr: n=%d  features=%d  chains=%d  iter=%d",
                  nrow(d), length(feats), chains, iter))
  fit <- rstanarm::stan_polr(
    form, data = d,
    prior        = rstanarm::R2(0.5, "mean"),
    prior_counts = rstanarm::dirichlet(1),
    chains       = chains,
    iter         = iter,
    cores        = cores,
    seed         = seed,
    refresh      = 0
  )
  list(
    fit       = fit,
    features  = feats,
    medians   = vapply(d[, feats, drop = FALSE], stats::median, numeric(1))
  )
}

# Backwards-compat alias — older callers used train_clm_bucket name.
train_clm_bucket <- train_stan_bucket

# Returns either:
#   - a list with $mean (tibble n×K) and $draws (array [n_draws × n × K])
#     when return_draws = TRUE — used to propagate posterior uncertainty
#     through the XGB ensemble for credible-interval extraction.
#   - a tibble (n×K of posterior means) when return_draws = FALSE — drop-in
#     replacement for the old clm interface.
predict_stan_bucket <- function(fit_obj, df, return_draws = FALSE) {
  K <- length(BUCKET_LEVELS)
  if (is.null(fit_obj)) {
    out <- matrix(rep(c(0.26, 0.17, 0.38, 0.11, 0.08), nrow(df)),
                  ncol = K, byrow = TRUE)
    colnames(out) <- BUCKET_LEVELS
    if (return_draws) {
      draws_arr <- array(rep(out, each = 1L), dim = c(1L, nrow(df), K))
      return(list(mean = tibble::as_tibble(out), draws = draws_arr))
    }
    return(tibble::as_tibble(out))
  }
  feats <- fit_obj$features
  td <- df |> dplyr::select(dplyr::all_of(feats))
  for (c in feats) {
    if (any(is.na(td[[c]]))) td[[c]][is.na(td[[c]])] <- fit_obj$medians[[c]]
  }

  # Posterior linear predictor: [n_draws × n]
  L <- rstanarm::posterior_linpred(fit_obj$fit, newdata = td)

  # Posterior cutpoints. The raw "zeta" parameter from rstan::extract is on
  # rstanarm's internal reparameterized scale (not directly comparable to
  # the linpred). The interpretable, named cutpoints — "bust|bench", etc. —
  # live in the matrix view of the stanfit and ARE on the linpred scale.
  cutpoint_names <- vapply(seq_len(K - 1), function(k)
    paste0(BUCKET_LEVELS[k], "|", BUCKET_LEVELS[k + 1]), character(1))
  draws_mat <- as.matrix(fit_obj$fit$stanfit)
  if (!all(cutpoint_names %in% colnames(draws_mat))) {
    stop("predict_stan_bucket: cutpoint columns not found in stanfit draws: ",
         paste(setdiff(cutpoint_names, colnames(draws_mat)), collapse = ", "))
  }
  zeta_draws <- draws_mat[, cutpoint_names, drop = FALSE]  # [draws × K-1]

  # Match number of posterior linpred draws to cutpoint draws (both come from
  # the same sampler, so they should be equal).
  n_draws <- min(nrow(L), nrow(zeta_draws))
  L <- L[seq_len(n_draws), , drop = FALSE]
  zeta_draws <- zeta_draws[seq_len(n_draws), , drop = FALSE]

  # Per-draw P(class). Memory: n_draws × n × K — for typical 8000 × 50 × 5
  # = 2M doubles = 16 MB. Acceptable.
  n_test <- ncol(L)
  draws_arr <- array(0, dim = c(n_draws, n_test, K))
  for (d in seq_len(n_draws)) {
    zeta_d <- zeta_draws[d, ]
    lin_d  <- L[d, ]
    cumprob <- vapply(zeta_d, function(z) plogis(z - lin_d), numeric(n_test))
    # cumprob: [n_test × (K-1)]
    if (n_test == 1) cumprob <- matrix(cumprob, nrow = 1)
    draws_arr[d, , 1] <- cumprob[, 1]
    for (k in 2:(K - 1)) draws_arr[d, , k] <- cumprob[, k] - cumprob[, k - 1]
    draws_arr[d, , K] <- 1 - cumprob[, K - 1]
  }

  # Posterior mean — what the old non-CI path returns.
  mean_p <- apply(draws_arr, c(2, 3), mean)
  colnames(mean_p) <- BUCKET_LEVELS

  if (return_draws) {
    return(list(mean = tibble::as_tibble(mean_p), draws = draws_arr))
  }
  tibble::as_tibble(mean_p)
}

# Backwards-compat alias used by score_class.
predict_clm_bucket <- predict_stan_bucket

# ── Ensemble two probability tibbles ─────────────────────────────────────────

ensemble_bucket_probs <- function(p1, p2) {
  # Geometric mean, renormalized. Penalizes confident-disagreement;
  # rewards confident-agreement.
  m1 <- as.matrix(p1); m2 <- as.matrix(p2)
  geom <- sqrt(m1 * m2)
  geom <- geom / rowSums(geom)
  tibble::as_tibble(geom)
}

# ── Convenience: full predict pipeline for one position ──────────────────────
# Returns a tibble with columns p_bust, p_flex, p_elite, p_league_winner,
# exp_ppg_bucket (bucket-midpoint expected value), and bucket_top1.

attach_bucket_predictions <- function(df, xgb_obj, clm_obj, pos,
                                       ci_level = 0.80) {
  K <- length(BUCKET_LEVELS)
  na_cols <- list(
    p_bust          = NA_real_, p_bench         = NA_real_,
    p_flex          = NA_real_, p_elite         = NA_real_,
    p_league_winner = NA_real_,
    p_bust_lo       = NA_real_, p_bench_lo      = NA_real_,
    p_flex_lo       = NA_real_, p_elite_lo      = NA_real_,
    p_league_winner_lo = NA_real_,
    p_bust_hi       = NA_real_, p_bench_hi      = NA_real_,
    p_flex_hi       = NA_real_, p_elite_hi      = NA_real_,
    p_league_winner_hi = NA_real_,
    exp_ppg_bucket  = NA_real_,
    exp_ppg_bucket_lo = NA_real_, exp_ppg_bucket_hi = NA_real_,
    bucket_top1     = NA_character_
  )
  if (is.null(xgb_obj) || is.null(clm_obj)) {
    return(df |> dplyr::mutate(!!!na_cols))
  }
  p_xgb       <- predict_xgb_bucket(xgb_obj, df)
  bayes_pred  <- predict_stan_bucket(clm_obj, df, return_draws = TRUE)

  # Per-draw ensemble: geom mean of (fixed) XGB point estimate and Bayesian
  # draw, renormalized. Posterior over the ordinal coefficients flows through
  # the ensemble — XGB stays fixed because it's not Bayesian, but the
  # ensemble's CI still captures the dominant uncertainty source for our
  # use case (the calibrated bucket probabilities).
  xgb_mat <- as.matrix(p_xgb)
  draws   <- bayes_pred$draws  # [n_draws × n × K]
  n_draws <- dim(draws)[1L]
  n_test  <- dim(draws)[2L]
  ens_arr <- array(0, dim = c(n_draws, n_test, K))
  for (d in seq_len(n_draws)) {
    g <- sqrt(xgb_mat * draws[d, , , drop = TRUE])
    if (n_test == 1) g <- matrix(g, nrow = 1)
    g <- g / rowSums(g)
    ens_arr[d, , ] <- g
  }

  ens_mean <- apply(ens_arr, c(2, 3), mean)
  alpha    <- (1 - ci_level) / 2
  ens_lo   <- apply(ens_arr, c(2, 3), stats::quantile, probs = alpha,     names = FALSE)
  ens_hi   <- apply(ens_arr, c(2, 3), stats::quantile, probs = 1 - alpha, names = FALSE)
  colnames(ens_mean) <- colnames(ens_lo) <- colnames(ens_hi) <- BUCKET_LEVELS

  midpts <- BUCKET_MIDPOINTS[[pos]]
  exp_ppg_bucket    <- as.numeric(ens_mean %*% midpts)
  # Per-draw exp_ppg_bucket → CI on the expected value
  exp_draws <- apply(ens_arr, 1, function(slice_d) as.numeric(slice_d %*% midpts))
  if (is.null(dim(exp_draws))) exp_draws <- matrix(exp_draws, nrow = 1)
  exp_ppg_bucket_lo <- apply(exp_draws, 1, stats::quantile, probs = alpha,     names = FALSE)
  exp_ppg_bucket_hi <- apply(exp_draws, 1, stats::quantile, probs = 1 - alpha, names = FALSE)
  bucket_top1       <- BUCKET_LEVELS[apply(ens_mean, 1, which.max)]

  df |> dplyr::mutate(
    p_bust             = ens_mean[, "bust"],
    p_bench            = ens_mean[, "bench"],
    p_flex             = ens_mean[, "flex"],
    p_elite            = ens_mean[, "elite"],
    p_league_winner    = ens_mean[, "league_winner"],
    p_bust_lo          = ens_lo[, "bust"],
    p_bench_lo         = ens_lo[, "bench"],
    p_flex_lo          = ens_lo[, "flex"],
    p_elite_lo         = ens_lo[, "elite"],
    p_league_winner_lo = ens_lo[, "league_winner"],
    p_bust_hi          = ens_hi[, "bust"],
    p_bench_hi         = ens_hi[, "bench"],
    p_flex_hi          = ens_hi[, "flex"],
    p_elite_hi         = ens_hi[, "elite"],
    p_league_winner_hi = ens_hi[, "league_winner"],
    exp_ppg_bucket     = exp_ppg_bucket,
    exp_ppg_bucket_lo  = exp_ppg_bucket_lo,
    exp_ppg_bucket_hi  = exp_ppg_bucket_hi,
    bucket_top1        = bucket_top1
  )
}

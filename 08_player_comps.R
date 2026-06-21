# 08_player_comps.R
# ─────────────────────────────────────────────────────────────────────────────
# Player comparison model v3 — find historical NFL comps for each prospect
#
# v3 improvements over v2:
#   1. Added profile features: dominator_rate, speed_score, recruit_rating
#   2. Per-game production rates alongside raw totals
#   3. Exponential similarity kernel (sharper differentiation of comp quality)
#   4. Optimized K (number of comps) alongside category weights and bandwidth
#   5. Producer-focused RMSE objective (discriminate WR2 from WR1, not just busts)
#   6. MAD scaling (robust to outlier producers)
#   7. Era-normalized measurables (forty, broad_jump, vertical) for cross-era comps
#
# Outputs:
#   output/player_comps.csv / .rds    — every prospect + their top comps
#   output/player_comp_summary.csv    — one row per prospect, comp summary
# ─────────────────────────────────────────────────────────────────────────────

setwd("~/Projects/R/college_nfl_model")
library(tidyverse)
library(nflreadr)

source("functions/helpers.R")

# ── 1. Load data ─────────────────────────────────────────────────────────────

wr_data <- readRDS("data/wr_model_data.rds")
rb_data <- readRDS("data/rb_model_data.rds")
qb_data <- if (file.exists("data/qb_model_data.rds")) readRDS("data/qb_model_data.rds") else NULL
te_data <- if (file.exists("data/te_model_data.rds")) readRDS("data/te_model_data.rds") else NULL
scores  <- readRDS("output/all_class_scores.rds")
# 07_score_all_classes.R only writes WR/RB. QB/TE scores live in a parallel
# rds written by 07b_score_qb_te.R; bind them in so QB/TE prospects show up
# in the prospect-side of the comp loop.
if (file.exists("output/qb_te_class_scores.rds")) {
  qb_te_scores <- readRDS("output/qb_te_class_scores.rds")
  scores <- bind_rows(scores, qb_te_scores)
}

# Combine data for recent prospects (2024-2026) not in training data
recent_combine <- load_combine() |>
  filter(pos %in% c("WR", "RB", "QB", "TE"), season >= 2024) |>
  mutate(
    name_clean = strip_suffix(clean_name(player_name)),
    height_in  = height_to_inches(ht),
    draft_year = season
  ) |>
  select(name_clean, draft_year, pos, height_in, weight = wt,
         forty, vertical, broad_jump)
cat("Loaded", nrow(recent_combine), "combine records for 2024-2026 prospects\n")

# ── 2. Feature definitions ──────────────────────────────────────────────────
# Categories: measurables, production (with per-game rates), profile, context.
# Draft capital used as pool FILTER (±40 picks), not distance feature.

WR_COMP_FEATURES <- list(
  measurables = c("height_in", "weight", "forty", "vertical", "broad_jump"),
  production  = c("rec_yards_final", "rec_final", "rec_td_final", "ypr",
                   "rec_td_rate", "rec_yards_penult", "rec_yds_yoy",
                   "rec_yards_per_game", "rec_per_game"),
  profile     = c("dominator_rate", "speed_score", "recruit_rating"),
  context     = c("age")
)

RB_COMP_FEATURES <- list(
  measurables = c("height_in", "weight", "forty", "vertical", "broad_jump"),
  production  = c("rush_yards_final", "carries_final", "rush_td_final", "ypc",
                   "rb_rec_yards", "recv_share", "scrimmage_yards",
                   "yards_per_touch", "rush_yds_yoy",
                   "rush_yards_per_game", "carries_per_game"),
  profile     = c("dominator_rate", "speed_score", "recruit_rating"),
  context     = c("age")
)

# QB: combine passing volume + efficiency + rushing upside. Mobile-QB signal
# captured via forty/speed_score and rush_yds_final.
QB_COMP_FEATURES <- list(
  measurables = c("height_in", "weight", "forty"),
  production  = c("pass_yds_final", "pass_td_final", "pass_int_final",
                   "pass_ypa_final", "pass_pct_final",
                   "pass_yds_per_game", "pass_td_per_game",
                   "pass_td_int_ratio",
                   "rush_yds_final"),
  profile     = c("speed_score", "recruit_rating", "qb_share_team"),
  context     = c("age")
)

# TE: mirrors WR_COMP_FEATURES but uses te-suffixed column names where the
# TE-specific PBP metric exists; falls back on shared WR-style stats for
# volume since rec_yards_final / rec_final / rec_td_final keep the same name.
TE_COMP_FEATURES <- list(
  measurables = c("height_in", "weight", "forty", "vertical", "broad_jump"),
  production  = c("rec_yards_final", "rec_final", "rec_td_final", "ypr_final",
                   "rec_td_rate", "rec_yards_penult", "rec_yds_yoy",
                   "rec_yards_per_game", "rec_per_game"),
  profile     = c("dominator_rate", "speed_score", "recruit_rating"),
  context     = c("age")
)

# Features that benefit from era normalization (z-score within draft_year).
# Includes production stats (change with offensive trends), measurables affected
# by timing methodology (hand→electronic ~2015), and derived profile features
# that depend on era-variant inputs. NOT era-normalized: height_in, weight,
# recruit_rating, age.
ERA_NORM_FEATURES <- c(
  # production (WR)
  "rec_yards_final", "rec_final", "rec_td_final", "ypr", "rec_td_rate",
  "rec_yards_penult", "rec_yds_yoy", "rec_yards_per_game", "rec_per_game",
  # production (RB)
  "rush_yards_final", "carries_final", "rush_td_final", "ypc",
  "rb_rec_yards", "recv_share", "scrimmage_yards", "yards_per_touch",
  "rush_yds_yoy", "rush_yards_per_game", "carries_per_game",
  # production (QB) — passing has changed dramatically; rushing modest
  "pass_yds_final", "pass_td_final", "pass_int_final", "pass_ypa_final",
  "pass_pct_final", "pass_yds_per_game", "pass_td_per_game",
  "pass_td_int_ratio", "rush_yds_final",
  # production (TE) — same names as WR for receiving, plus ypr_final
  "ypr_final",
  # measurables affected by timing changes / athlete evolution
  "forty", "broad_jump", "vertical",
  # derived profile features dependent on era-variant inputs
  "dominator_rate", "speed_score"
)

# ── 3. Era-normalize ────────────────────────────────────────────────────────
# Z-score within draft_year so cross-era comparisons are meaningful.

era_normalize <- function(df, features_to_norm = ERA_NORM_FEATURES) {
  for (feat in features_to_norm) {
    if (!feat %in% names(df)) next
    df <- df |>
      group_by(draft_year) |>
      mutate(
        !!feat := {
          vals <- .data[[feat]]
          m <- mean(vals, na.rm = TRUE)
          s <- sd(vals, na.rm = TRUE)
          if (is.na(s) || s == 0) vals - m else (vals - m) / s
        }
      ) |>
      ungroup()
  }
  df
}

# ── 4. Prepare comp pool ───────────────────────────────────────────────────

prep_comp_pool <- function(model_data, feature_list, max_year = 2023) {
  all_features <- unlist(feature_list, use.names = FALSE)

  pool <- model_data |>
    filter(has_cfb_data, draft_year <= max_year) |>
    select(pfr_player_name, position, college, draft_year, round, pick,
           ppg, made_it, avg_top2_ppg, n_qual_seasons, age,
           any_of(all_features))

  # Era-normalize production + relevant measurables/profile features
  pool <- era_normalize(pool)

  cat("  Comp pool size:", nrow(pool), "\n")
  pool
}

# ── 5. Robust Mahalanobis distance with pairwise missing-data handling ─────
# For each prospect-pool pair, only use features where BOTH have non-NA data.

compute_cov_inv <- function(mat) {
  p <- ncol(mat)

  # 1D case: just use variance
  if (p == 1) {
    v <- var(mat[, 1], na.rm = TRUE)
    if (is.na(v) || v == 0) v <- 1
    return(matrix(1 / v, 1, 1))
  }

  # Regularized inverse covariance from non-NA rows
  cov_mat <- cov(mat, use = "pairwise.complete.obs")

  # Regularize: shrink toward diagonal to handle near-singularity
  diag_cov <- diag(diag(cov_mat), nrow = p)
  lambda_shrink <- 0.1
  cov_reg <- (1 - lambda_shrink) * cov_mat + lambda_shrink * diag_cov

  # Replace any remaining NAs with diagonal values
  cov_reg[is.na(cov_reg)] <- 0
  diag(cov_reg)[diag(cov_reg) == 0] <- 1

  tryCatch(
    solve(cov_reg),
    error = function(e) {
      # Fallback: use diagonal (equivalent to standardized Euclidean)
      diag(1 / diag(cov_reg), nrow = p)
    }
  )
}

# Pre-compute per-category inverse covariance matrices
compute_category_cov_invs <- function(pool_mat, feature_list) {
  all_features <- unlist(feature_list, use.names = FALSE)
  cov_invs <- list()
  idx <- 1
  for (cat_name in names(feature_list)) {
    n_feat <- length(feature_list[[cat_name]])
    cols <- idx:(idx + n_feat - 1)
    sub_mat <- pool_mat[, cols, drop = FALSE]
    cov_invs[[cat_name]] <- compute_cov_inv(sub_mat)
    idx <- idx + n_feat
  }
  cov_invs
}

# Compute pairwise Mahalanobis distance with missing-data handling
compute_distances_mahal <- function(prospect_mat, pool_mat, feature_list,
                                    cov_invs, cat_weights) {
  all_features <- unlist(feature_list, use.names = FALSE)
  n_prospects <- nrow(prospect_mat)
  n_pool      <- nrow(pool_mat)
  dist_mat    <- matrix(NA_real_, nrow = n_prospects, ncol = n_pool)

  # Build category index mapping
  cat_indices <- list()
  idx <- 1
  for (cat_name in names(feature_list)) {
    n_feat <- length(feature_list[[cat_name]])
    cat_indices[[cat_name]] <- idx:(idx + n_feat - 1)
    idx <- idx + n_feat
  }

  for (i in seq_len(n_prospects)) {
    p_vec <- prospect_mat[i, ]

    for (j in seq_len(n_pool)) {
      pool_vec <- pool_mat[j, ]
      total_dist <- 0
      total_weight <- 0

      for (cat_name in names(feature_list)) {
        cols <- cat_indices[[cat_name]]
        p_sub <- p_vec[cols]
        q_sub <- pool_vec[cols]

        # Only use features where BOTH have real data
        valid <- !is.na(p_sub) & !is.na(q_sub)
        if (sum(valid) == 0) next

        diff <- p_sub[valid] - q_sub[valid]

        if (sum(valid) == 1) {
          # 1D: just squared difference scaled by variance
          var_inv <- cov_invs[[cat_name]][valid, valid]
          cat_dist <- as.numeric(diff * var_inv * diff)
        } else {
          # Use submatrix of inverse covariance
          sub_inv <- cov_invs[[cat_name]][valid, valid, drop = FALSE]
          cat_dist <- as.numeric(t(diff) %*% sub_inv %*% diff)
        }

        # Weight by category weight * fraction of features available
        frac_available <- sum(valid) / length(cols)
        total_dist <- total_dist + cat_weights[cat_name] * cat_dist * frac_available
        total_weight <- total_weight + cat_weights[cat_name] * frac_available
      }

      # Normalize by total weight so missing categories don't deflate distance
      dist_mat[i, j] <- if (total_weight > 0) sqrt(total_dist / total_weight) else Inf
    }
  }
  dist_mat
}

# ── 6. Robust scaling (MAD instead of SD) ──────────────────────────────────
# MAD is robust to outlier producers who inflate SD and compress typical
# differences. Preserves NAs (no imputation) for pairwise distance.

compute_scaling_params <- function(pool, features) {
  tibble(
    feature = features,
    med     = map_dbl(features, ~ median(pool[[.x]], na.rm = TRUE)),
    scale   = map_dbl(features, ~ {
      s <- mad(pool[[.x]], na.rm = TRUE)
      if (is.na(s) || s == 0) 1 else s
    })
  )
}

scale_preserving_na <- function(df, scaling_params) {
  mat <- matrix(NA_real_, nrow = nrow(df), ncol = nrow(scaling_params))
  colnames(mat) <- scaling_params$feature
  for (i in seq_len(nrow(scaling_params))) {
    feat <- scaling_params$feature[i]
    vals <- df[[feat]]
    mat[, i] <- (vals - scaling_params$med[i]) / scaling_params$scale[i]
  }
  mat
}

# ── 7. Find top-N comps (with exponential similarity kernel) ──────────────
# Exponential kernel: exp(-distance / bandwidth) gives sharper weight
# differentiation than 1/(1+d). The closest comp dominates while distant
# comps contribute minimally.

find_comps <- function(prospect_df, pool, dist_mat, n_comps = 5,
                       pick_window = 40, bandwidth = 1.0) {
  results <- vector("list", nrow(prospect_df))

  for (i in seq_len(nrow(prospect_df))) {
    p_pick <- prospect_df$pick[i]

    # Filter pool to ±pick_window (draft capital as filter, not distance)
    in_window <- which(abs(pool$pick - p_pick) <= pick_window)

    if (length(in_window) < n_comps) {
      # Widen window if too few candidates
      in_window <- order(abs(pool$pick - p_pick))[1:min(n_comps * 3, nrow(pool))]
    }

    dists <- dist_mat[i, in_window]
    top_local <- order(dists)[1:min(n_comps, length(dists))]
    top_idx <- in_window[top_local]

    comps <- pool[top_idx, ] |>
      mutate(
        similarity = round(exp(-dists[top_local] / bandwidth), 3),
        distance   = round(dists[top_local], 3),
        comp_rank  = seq_along(top_idx)
      ) |>
      select(comp_rank, comp_name = pfr_player_name, comp_college = college,
             comp_year = draft_year, comp_round = round, comp_pick = pick,
             comp_ppg = ppg, comp_raw_ppg = avg_top2_ppg,
             comp_made_it = made_it, similarity, distance)

    prospect_info <- prospect_df[i, ]
    if ("pfr_player_name" %in% names(prospect_info) && !"name" %in% names(prospect_info)) {
      prospect_info <- prospect_info |> rename(name = pfr_player_name)
    } else if ("pfr_player_name" %in% names(prospect_info) && "name" %in% names(prospect_info)) {
      prospect_info <- prospect_info |> select(-pfr_player_name)
    }
    prospect_info <- prospect_info |>
      select(any_of(c("name", "position", "college", "draft_year", "round", "pick")))

    results[[i]] <- bind_cols(
      prospect_info |> dplyr::slice(rep(1, nrow(comps))),
      comps
    )
  }

  bind_rows(results)
}

# ── 8. Weight optimization ─────────────────────────────────────────────────
# Joint optimization of category weights, K (number of comps), and
# bandwidth (exponential kernel sharpness).
# Objective: minimize RMSE on PRODUCERS ONLY (made_it==1) in the 2021-2023
# holdout. This focuses optimization on discriminating production magnitude
# rather than bust/producer classification (handled by XGBoost p_made_it).

optimize_weights <- function(model_data, feature_list, position_label,
                             pick_window = 40) {
  cat(sprintf("\n  Optimizing weights for %s...\n", position_label))

  all_features <- unlist(feature_list, use.names = FALSE)

  # Split: pool = 2002-2020, holdout = 2021-2023
  pool_data <- model_data |> filter(has_cfb_data, draft_year <= 2020)
  holdout_data <- model_data |> filter(has_cfb_data, draft_year %in% 2021:2023)

  if (nrow(holdout_data) < 10) {
    cat("    Too few holdout players, using default config\n")
    return(list(
      weights   = c(measurables = 1.0, production = 1.5, profile = 1.0, context = 0.5),
      n_comps   = 5,
      bandwidth = 1.0
    ))
  }

  # Era-normalize both using their own stats
  pool_data    <- era_normalize(pool_data)
  holdout_data <- era_normalize(holdout_data)

  # Scale (using MAD)
  scaling <- compute_scaling_params(pool_data, all_features)
  pool_mat    <- scale_preserving_na(pool_data, scaling)
  holdout_mat <- scale_preserving_na(holdout_data, scaling)

  # Pre-compute covariance inverses from pool
  cov_invs <- compute_category_cov_invs(pool_mat, feature_list)

  # Identify producers in holdout for RMSE objective
  producer_idx <- which(holdout_data$made_it == 1)
  cat(sprintf("    Holdout: %d players (%d producers)\n",
              nrow(holdout_data), length(producer_idx)))

  if (length(producer_idx) < 5) {
    cat("    Too few producers in holdout, using default config\n")
    return(list(
      weights   = c(measurables = 1.0, production = 1.5, profile = 1.0, context = 0.5),
      n_comps   = 5,
      bandwidth = 1.0
    ))
  }

  # Grid search over weight combinations
  weight_grid <- expand.grid(
    measurables = c(0.5, 1.0, 1.5),
    production  = c(1.0, 1.5, 2.0, 2.5),
    profile     = c(0.5, 1.0, 1.5),
    context     = c(0.2, 0.5, 1.0)
  )

  k_values         <- c(3, 5, 7)
  bandwidth_values <- c(0.5, 1.0, 2.0)

  best_rmse <- Inf
  best_config <- list(
    weights   = c(measurables = 1.0, production = 1.5, profile = 1.0, context = 0.5),
    n_comps   = 5,
    bandwidth = 1.0
  )

  n_combos <- nrow(weight_grid)
  cat(sprintf("    Searching %d weight combos × %d K × %d bandwidth = %d configs\n",
              n_combos, length(k_values), length(bandwidth_values),
              n_combos * length(k_values) * length(bandwidth_values)))

  for (w in seq_len(nrow(weight_grid))) {
    wts <- as.numeric(weight_grid[w, ])
    names(wts) <- names(weight_grid)

    # Compute distances (expensive — done once per weight combo)
    dist_mat <- compute_distances_mahal(holdout_mat, pool_mat, feature_list,
                                         cov_invs, wts)

    # Exclude self-matches
    for (i in seq_len(nrow(holdout_data))) {
      p_name <- clean_name(holdout_data$pfr_player_name[i])
      p_year <- holdout_data$draft_year[i]
      pool_names <- clean_name(pool_data$pfr_player_name)
      self_match <- which(pool_names == p_name & pool_data$draft_year == p_year)
      if (length(self_match) > 0) dist_mat[i, self_match] <- Inf
    }

    # Sweep K and bandwidth (cheap — reuses distance matrix)
    for (k in k_values) {
      for (bw in bandwidth_values) {
        comp_ppgs <- numeric(nrow(holdout_data))

        for (i in seq_len(nrow(holdout_data))) {
          p_pick <- holdout_data$pick[i]
          in_window <- which(abs(pool_data$pick - p_pick) <= pick_window)
          if (length(in_window) < k) {
            in_window <- order(abs(pool_data$pick - p_pick))[1:min(k * 3, nrow(pool_data))]
          }

          dists <- dist_mat[i, in_window]
          top_local <- order(dists)[1:min(k, length(dists))]
          top_idx <- in_window[top_local]

          sims <- exp(-dists[top_local] / bw)
          comp_ppgs[i] <- weighted.mean(pool_data$ppg[top_idx], sims)
        }

        # Objective: RMSE on producers only
        rmse <- sqrt(mean((comp_ppgs[producer_idx] - holdout_data$ppg[producer_idx])^2))

        if (!is.na(rmse) && rmse < best_rmse) {
          best_rmse <- rmse
          best_config <- list(weights = wts, n_comps = k, bandwidth = bw)
        }
      }
    }
  }

  cat(sprintf("    Best holdout RMSE (producers): %.3f\n", best_rmse))
  cat(sprintf("    Optimal: measurables=%.1f, production=%.1f, profile=%.1f, context=%.1f, K=%d, bandwidth=%.1f\n",
              best_config$weights["measurables"], best_config$weights["production"],
              best_config$weights["profile"], best_config$weights["context"],
              best_config$n_comps, best_config$bandwidth))
  best_config
}

# ── 9. Build prospect features from scored data ─────────────────────────────

build_prospect_features <- function(scores_df, model_data, position_filter,
                                    feature_list) {
  pos_scores <- scores_df |> filter(position == position_filter)

  hist <- model_data |>
    filter(has_cfb_data) |>
    mutate(name_clean = clean_name(pfr_player_name),
           name = pfr_player_name) |>
    select(name, name_clean, position, college, draft_year, round, pick,
           everything())

  pos_scores <- pos_scores |>
    mutate(name_clean = clean_name(name))

  # For players in training data, use rich features
  in_train <- pos_scores |>
    inner_join(hist |> select(-name, -position, -college, -round, -pick),
               by = c("name_clean", "draft_year"),
               suffix = c("", "_train"))

  # Era-normalize all relevant features
  in_train <- era_normalize(in_train)

  # For players NOT in training data (2024-2026)
  not_in_train <- pos_scores |>
    anti_join(hist, by = c("name_clean", "draft_year"))

  list(in_train = in_train, not_in_train = not_in_train)
}

# ── 10. Main pipeline ───────────────────────────────────────────────────────

run_comps <- function(model_data, feature_list, position_label, scores_df,
                      pick_window = 40, config = NULL, max_pool_year = 2023) {
  cat("\n══ Player Comps:", position_label, "══\n")

  all_features <- unlist(feature_list, use.names = FALSE)

  # Step 1: Optimize weights + K + bandwidth on holdout
  if (is.null(config)) {
    config <- optimize_weights(model_data, feature_list, position_label,
                               pick_window)
  }

  cat_weights <- config$weights
  n_comps     <- config$n_comps
  bandwidth   <- config$bandwidth

  # Step 2: Build comp pool (era-normalized). `max_pool_year` lets walk-forward
  # CV restrict the pool to strictly past prospects.
  pool <- prep_comp_pool(model_data, feature_list, max_year = max_pool_year)

  # Step 3: Scale (preserving NAs, using MAD)
  scaling <- compute_scaling_params(pool, all_features)
  pool_mat <- scale_preserving_na(pool, scaling)

  # Step 4: Pre-compute Mahalanobis covariance inverses
  cov_invs <- compute_category_cov_invs(pool_mat, feature_list)

  # Step 5: Get prospects
  prospect_split <- build_prospect_features(scores_df, model_data,
                                             position_label, feature_list)

  # --- Prospects with full features (in training data) ---
  if (nrow(prospect_split$in_train) > 0) {
    cat("  Prospects with full features:", nrow(prospect_split$in_train), "\n")
    in_train_mat <- scale_preserving_na(prospect_split$in_train, scaling)
    in_train_dist <- compute_distances_mahal(in_train_mat, pool_mat,
                                              feature_list, cov_invs,
                                              cat_weights)

    # Exclude self-matches
    for (i in seq_len(nrow(prospect_split$in_train))) {
      p_name <- clean_name(prospect_split$in_train$name[i])
      p_year <- prospect_split$in_train$draft_year[i]
      pool_names <- clean_name(pool$pfr_player_name)
      self_match <- which(pool_names == p_name & pool$draft_year == p_year)
      if (length(self_match) > 0) in_train_dist[i, self_match] <- Inf
    }

    comps_in <- find_comps(prospect_split$in_train, pool, in_train_dist,
                            n_comps, pick_window, bandwidth)
  } else {
    comps_in <- tibble()
  }

  # --- Prospects NOT in training data (2024-2026, limited features) ---
  if (nrow(prospect_split$not_in_train) > 0) {
    cat("  Prospects with limited features:", nrow(prospect_split$not_in_train), "\n")

    not_in <- prospect_split$not_in_train

    # Join combine measurables
    pos_combine <- recent_combine |> filter(pos == position_label)
    not_in <- not_in |>
      left_join(pos_combine |> select(-pos),
                by = c("name_clean", "draft_year"),
                suffix = c("", "_comb"))
    for (col in c("height_in", "weight", "forty", "vertical", "broad_jump")) {
      comb_col <- paste0(col, "_comb")
      if (comb_col %in% names(not_in)) {
        not_in[[col]] <- coalesce(not_in[[col]], not_in[[comb_col]])
        not_in[[comb_col]] <- NULL
      }
    }

    n_with_combine <- sum(!is.na(not_in$forty))
    cat("    Matched combine data:", n_with_combine, "of", nrow(not_in), "\n")

    # Ensure all feature columns exist
    for (feat in all_features) {
      if (!feat %in% names(not_in)) not_in[[feat]] <- NA_real_
    }

    # Era-normalize
    not_in <- era_normalize(not_in)

    not_in_mat <- scale_preserving_na(not_in, scaling)
    not_in_dist <- compute_distances_mahal(not_in_mat, pool_mat,
                                            feature_list, cov_invs,
                                            cat_weights)
    comps_not <- find_comps(not_in, pool, not_in_dist, n_comps,
                             pick_window, bandwidth)
  } else {
    comps_not <- tibble()
  }

  list(comps = bind_rows(comps_in, comps_not), config = config)
}

# ── 11. Run ──────────────────────────────────────────────────────────────────

# Allow 11b walk-forward CV (and other scripts) to load these helpers
# (era_normalize, prep_comp_pool, compute_*_mahal, find_comps, run_comps,
# WR/RB/QB/TE feature lists, recent_combine) without triggering the full
# weight optimization + writes below. Set `COMP_LOAD_ONLY = TRUE` in the
# calling script before `source("08_player_comps.R")`.
if (isTRUE(getOption("comp_helpers.load_only", FALSE)) ||
    isTRUE(get0("COMP_LOAD_ONLY", ifnotfound = FALSE))) {
  message("[08] COMP_LOAD_ONLY active — helpers loaded, skipping run.")
} else {
wr_result <- run_comps(wr_data, WR_COMP_FEATURES, "WR", scores)
rb_result <- run_comps(rb_data, RB_COMP_FEATURES, "RB", scores)
qb_result <- if (!is.null(qb_data)) run_comps(qb_data, QB_COMP_FEATURES, "QB", scores) else NULL
te_result <- if (!is.null(te_data)) run_comps(te_data, TE_COMP_FEATURES, "TE", scores) else NULL

all_comps <- bind_rows(
  wr_result$comps, rb_result$comps,
  if (!is.null(qb_result)) qb_result$comps else NULL,
  if (!is.null(te_result)) te_result$comps else NULL
) |>
  arrange(draft_year, position, pick, comp_rank)

# ── 12. Join model predictions for context ──────────────────────────────────

all_comps <- all_comps |>
  left_join(
    scores |> select(name, position, draft_year, exp_ppg, p_made_it),
    by = c("name", "position", "draft_year")
  )

# ── 13. Validation on 2021-2023 holdout ─────────────────────────────────────

cat("\n\n══ VALIDATION: Comp-weighted PPG vs Actual PPG (2021-2023) ══\n")

actual_ppg_lookup <- bind_rows(
  wr_data |> filter(has_cfb_data) |>
    select(pfr_player_name, draft_year, ppg, made_it, position) |>
    rename(actual_ppg_val = ppg),
  rb_data |> filter(has_cfb_data) |>
    select(pfr_player_name, draft_year, ppg, made_it, position) |>
    rename(actual_ppg_val = ppg)
) |> mutate(name = pfr_player_name) |> select(-pfr_player_name)

holdout_comps <- all_comps |>
  filter(draft_year %in% 2021:2023) |>
  left_join(actual_ppg_lookup, by = c("name", "draft_year", "position"))

holdout_summary <- holdout_comps |>
  group_by(name, position, draft_year, pick, actual_ppg_val, made_it) |>
  summarize(
    comp_weighted_ppg = weighted.mean(comp_ppg, similarity),
    comp_median_ppg   = median(comp_ppg),
    .groups = "drop"
  ) |>
  filter(!is.na(actual_ppg_val))

if (nrow(holdout_summary) > 0) {
  for (pos in c("WR", "RB")) {
    sub <- holdout_summary |> filter(position == pos)
    if (nrow(sub) < 5) next
    r_weighted <- cor(sub$comp_weighted_ppg, sub$actual_ppg_val, use = "complete.obs")
    r_median   <- cor(sub$comp_median_ppg, sub$actual_ppg_val, use = "complete.obs")
    rmse_comp  <- sqrt(mean((sub$comp_weighted_ppg - sub$actual_ppg_val)^2))

    # Also RMSE on producers only
    producers <- sub |> filter(made_it == 1)
    rmse_prod <- if (nrow(producers) >= 3) {
      sqrt(mean((producers$comp_weighted_ppg - producers$actual_ppg_val)^2))
    } else NA

    # Naive baseline: just predict PPG from log(pick) regression
    lm_fit <- lm(actual_ppg_val ~ log(pick + 1), data = sub)
    rmse_naive <- sqrt(mean(residuals(lm_fit)^2))

    cat(sprintf("\n  %s (n=%d, %d producers):\n", pos, nrow(sub), nrow(producers)))
    cat(sprintf("    Comp-weighted PPG vs Actual:  r = %.3f\n", r_weighted))
    cat(sprintf("    Comp-median PPG vs Actual:    r = %.3f\n", r_median))
    cat(sprintf("    Comp RMSE (all): %.2f  |  Comp RMSE (producers): %.2f  |  Naive: %.2f\n",
                rmse_comp, rmse_prod, rmse_naive))
  }
}

# ── 14. Pretty print ────────────────────────────────────────────────────────

print_comps <- function(comps_df, yr, pos) {
  subset <- comps_df |>
    filter(draft_year == yr, position == pos) |>
    arrange(pick, comp_rank)

  players <- unique(subset$name)
  cat(sprintf("\n══ %s %d Comps ══\n", pos, yr))
  for (p in players) {
    p_row <- subset |> filter(name == p, comp_rank == 1)
    cat(sprintf("\n  %s (%s, Rd %s #%d) — exp PPG: %.1f\n",
                p, p_row$college[1], p_row$round[1], p_row$pick[1],
                p_row$exp_ppg[1]))

    subset |>
      filter(name == p) |>
      mutate(
        outcome = case_when(
          comp_ppg < 5 ~ "BUST",
          !is.na(comp_raw_ppg) ~ sprintf("%.1f ppg", comp_raw_ppg),
          TRUE ~ sprintf("%.1f ppg*", comp_ppg)
        ),
        line = sprintf("    %d. %s (%s %d, Rd %s #%d) — %s  [sim: %.0f%%]",
                        comp_rank, comp_name, comp_college, comp_year,
                        comp_round, comp_pick, outcome, similarity * 100)
      ) |>
      pull(line) |>
      cat(sep = "\n")
    cat("\n")
  }
}

for (yr in c(2025, 2026)) {
  for (pos in c("WR", "RB")) {
    print_comps(all_comps, yr, pos)
  }
}

# ── 15. Comp summary ────────────────────────────────────────────────────────

comp_summary <- all_comps |>
  group_by(name, position, draft_year, round, pick, college, exp_ppg, p_made_it) |>
  summarize(
    comp_weighted_ppg = round(weighted.mean(comp_ppg, similarity), 2),
    comp_median_ppg   = round(median(comp_ppg), 2),
    comp_bust_rate    = round(mean(comp_ppg < 5), 2),
    comp_names        = paste(comp_name, collapse = ", "),
    n_comps           = n(),
    .groups = "drop"
  ) |>
  arrange(draft_year, position, pick)

cat("\n\n══ Comp Summary (2025-2026) ══\n")
comp_summary |>
  filter(draft_year >= 2025) |>
  group_by(draft_year, position) |>
  slice_head(n = 15) |>
  mutate(across(where(is.numeric), ~ round(.x, 2))) |>
  print(n = 60, width = 200)

# ── 16. Save ─────────────────────────────────────────────────────────────────

write_csv(all_comps, "output/player_comps.csv")
saveRDS(all_comps, "output/player_comps.rds")

write_csv(comp_summary, "output/player_comp_summary.csv")
saveRDS(comp_summary, "output/player_comp_summary.rds")

# Save learned configs for reference
cat("\n\nLearned configs:\n")
cat(sprintf("  WR: %s, K=%d, bandwidth=%.1f\n",
            paste(names(wr_result$config$weights),
                  round(wr_result$config$weights, 2), sep = "=", collapse = ", "),
            wr_result$config$n_comps, wr_result$config$bandwidth))
cat(sprintf("  RB: %s, K=%d, bandwidth=%.1f\n",
            paste(names(rb_result$config$weights),
                  round(rb_result$config$weights, 2), sep = "=", collapse = ", "),
            rb_result$config$n_comps, rb_result$config$bandwidth))

message("\nSaved: output/player_comps.csv, output/player_comp_summary.csv")
} # end COMP_LOAD_ONLY guard

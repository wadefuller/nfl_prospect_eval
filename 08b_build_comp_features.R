# 08b_build_comp_features.R
# ─────────────────────────────────────────────────────────────────────────────
# Strictly-past comp features for stacking into bust + production models.
#
# For every drafted player K (training data 2002–2023), compute:
#   comp_weighted_ppg  — similarity-weighted mean of NFL PPG for top-N comps
#   comp_bust_rate     — fraction of comps with PPG < 5
#   comp_median_ppg    — robust central tendency
#
# **Leakage prevention**: the comp pool for player K is restricted to
#   draft_year < K (and excludes K's own row by construction). This mirrors
#   the information set available at K's draft time and is invariant across
#   temporal-CV folds, so a single precompute pass is sufficient.
#
# Per-year (cohort-by-cohort) we recompute scaling and inverse covariance
# from the strictly-past pool — so even the structural (non-target-aware)
# stats are leakage-free.
#
# Outputs:
#   data/comp_features.rds  — tibble of (name_clean, draft_year, position,
#                              comp_weighted_ppg, comp_bust_rate,
#                              comp_median_ppg, n_pool_used)
# ─────────────────────────────────────────────────────────────────────────────

setwd("~/Projects/R/college_nfl_model")
library(tidyverse)

source("functions/helpers.R")

set.seed(42)

# ── Configuration ───────────────────────────────────────────────────────────
# CV-tuned by 15_comp_hp_sweep.R (2026-05-09). Sweep over a 3×3×3 grid showed
# tight neighborhoods beat wide ones: smaller n_comps + tighter pick_window
# both helped. Improvement over the previous "smoother" defaults
# (n=15, bw=2, pw=60): MAE 2.65 → 2.56 = -0.09. See output/comp_hp_sweep/.

CONFIG <- list(
  weights     = c(measurables = 1.0, production = 1.5, profile = 1.0, context = 0.5),
  n_comps     = 8,
  bandwidth   = 1.0,
  pick_window = 40,
  min_pool_size = 30
)

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

QB_COMP_FEATURES <- list(
  measurables = c("height_in", "weight", "forty"),
  production  = c("pass_yds_final", "pass_td_final", "pass_int_final",
                  "pass_ypa_final", "pass_td_int_ratio",
                  "pass_yds_per_game", "pass_yds_yoy",
                  "rush_yds_final"),
  profile     = c("speed_score", "recruit_rating"),
  context     = c("age")
)

TE_COMP_FEATURES <- list(
  measurables = c("height_in", "weight", "forty", "vertical", "broad_jump"),
  production  = c("rec_yards_final", "rec_final", "rec_td_final",
                  "rec_td_rate", "rec_yards_penult", "rec_yds_yoy",
                  "rec_yards_per_game", "rec_per_game"),
  profile     = c("dominator_rate", "speed_score", "recruit_rating"),
  context     = c("age")
)

ERA_NORM_FEATURES <- c(
  # production (WR)
  "rec_yards_final", "rec_final", "rec_td_final", "ypr", "rec_td_rate",
  "rec_yards_penult", "rec_yds_yoy", "rec_yards_per_game", "rec_per_game",
  # production (RB)
  "rush_yards_final", "carries_final", "rush_td_final", "ypc",
  "rb_rec_yards", "recv_share", "scrimmage_yards", "yards_per_touch",
  "rush_yds_yoy", "rush_yards_per_game", "carries_per_game",
  # production (QB)
  "pass_yds_final", "pass_td_final", "pass_int_final", "pass_ypa_final",
  "pass_td_int_ratio", "pass_yds_per_game", "pass_yds_yoy",
  # production (TE — shares column names with WR receiving)
  # measurables affected by timing changes / athlete evolution
  "forty", "broad_jump", "vertical",
  # derived profile features
  "dominator_rate", "speed_score"
)

# ── Helpers (extracted from 08_player_comps.R) ──────────────────────────────

era_normalize <- function(df, features_to_norm = ERA_NORM_FEATURES) {
  for (feat in features_to_norm) {
    if (!feat %in% names(df)) next
    df <- df |>
      group_by(draft_year) |>
      mutate(!!feat := {
        vals <- .data[[feat]]
        m <- mean(vals, na.rm = TRUE)
        s <- sd(vals, na.rm = TRUE)
        if (is.na(s) || s == 0) vals - m else (vals - m) / s
      }) |>
      ungroup()
  }
  df
}

compute_cov_inv <- function(mat) {
  p <- ncol(mat)
  if (p == 1) {
    v <- var(mat[, 1], na.rm = TRUE)
    if (is.na(v) || v == 0) v <- 1
    return(matrix(1 / v, 1, 1))
  }
  cov_mat <- cov(mat, use = "pairwise.complete.obs")
  diag_cov <- diag(diag(cov_mat), nrow = p)
  cov_reg <- 0.9 * cov_mat + 0.1 * diag_cov
  cov_reg[is.na(cov_reg)] <- 0
  diag(cov_reg)[diag(cov_reg) == 0] <- 1
  tryCatch(solve(cov_reg),
           error = function(e) diag(1 / diag(cov_reg), nrow = p))
}

compute_category_cov_invs <- function(pool_mat, feature_list) {
  cov_invs <- list()
  idx <- 1
  for (cat_name in names(feature_list)) {
    n_feat <- length(feature_list[[cat_name]])
    cols <- idx:(idx + n_feat - 1)
    cov_invs[[cat_name]] <- compute_cov_inv(pool_mat[, cols, drop = FALSE])
    idx <- idx + n_feat
  }
  cov_invs
}

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
    mat[, i] <- (df[[feat]] - scaling_params$med[i]) / scaling_params$scale[i]
  }
  mat
}

# Pairwise-NA Mahalanobis distance, category-weighted with availability
# normalization. Returns matrix [n_prospect × n_pool].
#
# Vectorized over (prospect × pool) for each (category × NA-pattern) cell.
# For pairwise-NA Mahalanobis, the "valid" set of features per pair depends
# on which are non-NA on both sides — that breaks pure vectorization. We
# compromise: enumerate the distinct NA-patterns of each side per category,
# then within a fixed pattern do one big matmul. Categories have ≤9 features,
# so at most 2^9 = 512 patterns per category — almost always far fewer in
# practice (combine missingness clusters; production rarely has NAs).
compute_distances_mahal <- function(prospect_mat, pool_mat, feature_list,
                                    cov_invs, cat_weights) {
  n_p <- nrow(prospect_mat)
  n_q <- nrow(pool_mat)
  total_dist   <- matrix(0, n_p, n_q)
  total_weight <- matrix(0, n_p, n_q)

  idx <- 1
  for (cat_name in names(feature_list)) {
    cat_feats <- feature_list[[cat_name]]
    n_feat    <- length(cat_feats)
    cols      <- idx:(idx + n_feat - 1)
    idx       <- idx + n_feat
    cov_inv   <- cov_invs[[cat_name]]
    cat_w     <- cat_weights[cat_name]

    p_block <- prospect_mat[, cols, drop = FALSE]
    q_block <- pool_mat[, cols, drop = FALSE]
    p_na    <- !is.na(p_block)   # n_p × n_feat
    q_na    <- !is.na(q_block)   # n_q × n_feat

    # Enumerate distinct NA patterns on each side, group rows by pattern.
    p_keys <- apply(p_na, 1L, function(r) paste0(as.integer(r), collapse = ""))
    q_keys <- apply(q_na, 1L, function(r) paste0(as.integer(r), collapse = ""))
    p_groups <- split(seq_len(n_p), p_keys)
    q_groups <- split(seq_len(n_q), q_keys)

    for (pk in names(p_groups)) {
      p_idx <- p_groups[[pk]]
      p_pat <- as.logical(as.integer(strsplit(pk, "")[[1]]))
      if (!any(p_pat)) next  # all-NA prospects in this category get 0 contribution

      for (qk in names(q_groups)) {
        q_idx <- q_groups[[qk]]
        q_pat <- as.logical(as.integer(strsplit(qk, "")[[1]]))
        valid <- p_pat & q_pat
        n_valid <- sum(valid)
        if (n_valid == 0) next

        sub_inv <- cov_inv[valid, valid, drop = FALSE]
        # diff[i,j,k] for i in p_idx, j in q_idx, k in valid
        # Compute (P_v - Q_v)' Sigma^-1 (P_v - Q_v) batched:
        #   d_ij = sum_kl (p_ik - q_jk) * S_kl * (p_il - q_jl)
        # = p_i' S p_i  -  2 p_i' S q_j  +  q_j' S q_j
        P <- p_block[p_idx, valid, drop = FALSE]
        Q <- q_block[q_idx, valid, drop = FALSE]
        # Quadratic forms:
        # rowSums((P %*% S) * P) gives p_i' S p_i  (length n_p_grp)
        PS <- P %*% sub_inv          # |p_idx| × n_valid
        QS <- Q %*% sub_inv          # |q_idx| × n_valid
        pSp <- rowSums(PS * P)       # |p_idx|
        qSq <- rowSums(QS * Q)       # |q_idx|
        pSq <- PS %*% t(Q)           # |p_idx| × |q_idx|

        cat_dist <- outer(pSp, qSq, `+`) - 2 * pSq

        frac_avail <- n_valid / n_feat
        # Numerical guard against tiny negatives from float roundoff
        cat_dist[cat_dist < 0] <- 0

        total_dist[p_idx, q_idx]   <- total_dist[p_idx, q_idx]   +
          cat_w * cat_dist * frac_avail
        total_weight[p_idx, q_idx] <- total_weight[p_idx, q_idx] +
          cat_w * frac_avail
      }
    }
  }

  out <- sqrt(total_dist / total_weight)
  out[total_weight == 0] <- Inf
  out
}

# ── Main: build comp features for one position ──────────────────────────────

build_comp_features_for_position <- function(model_data, feature_list,
                                             position_label, config = CONFIG) {
  cat(sprintf("\n══ Build comp features: %s ══\n", position_label))

  all_features <- unlist(feature_list, use.names = FALSE)

  # Era-normalize across full data (within-year z-scores, no leakage).
  data_norm <- model_data |>
    filter(has_cfb_data) |>
    era_normalize()

  cat(sprintf("  Players with CFB data: %d\n", nrow(data_norm)))

  # Pool eligibility: matured outcomes (ppg observed). For training data
  # 2002–2023, all rows have observed ppg (busts coded as 0).
  pool_full <- data_norm |> filter(!is.na(ppg))

  results <- vector("list", nrow(data_norm))

  years <- sort(unique(data_norm$draft_year))
  for (yr in years) {
    cohort_idx <- which(data_norm$draft_year == yr)
    pool_yr <- pool_full |> filter(draft_year < yr)
    n_pool <- nrow(pool_yr)

    if (n_pool < config$min_pool_size) {
      for (i in cohort_idx) {
        results[[i]] <- tibble(
          name_clean        = clean_name(data_norm$pfr_player_name[i]),
          draft_year        = data_norm$draft_year[i],
          position          = position_label,
          comp_weighted_ppg = NA_real_,
          comp_bust_rate    = NA_real_,
          comp_median_ppg   = NA_real_,
          n_pool_used       = n_pool
        )
      }
      cat(sprintf("  %d: pool=%d (skip — below min)\n", yr, n_pool))
      next
    }

    # Cohort-specific scaling and cov_invs from strictly-past pool
    scaling   <- compute_scaling_params(pool_yr, all_features)
    pool_mat  <- scale_preserving_na(pool_yr, scaling)
    cov_invs  <- compute_category_cov_invs(pool_mat, feature_list)

    # Score this cohort against the pool
    cohort_df <- data_norm[cohort_idx, ]
    cohort_mat <- scale_preserving_na(cohort_df, scaling)

    dist_mat <- compute_distances_mahal(cohort_mat, pool_mat,
                                        feature_list, cov_invs,
                                        config$weights)

    for (k in seq_along(cohort_idx)) {
      i <- cohort_idx[k]
      p_pick <- cohort_df$pick[k]

      # Pick-window filter (draft capital filter, not distance feature)
      in_window <- which(abs(pool_yr$pick - p_pick) <= config$pick_window)
      if (length(in_window) < config$n_comps) {
        in_window <- order(abs(pool_yr$pick - p_pick))[1:min(config$n_comps * 3, n_pool)]
      }

      dists <- dist_mat[k, in_window]
      top_local <- order(dists)[1:min(config$n_comps, length(dists))]
      top_idx   <- in_window[top_local]
      sims      <- exp(-dists[top_local] / config$bandwidth)
      comp_ppgs <- pool_yr$ppg[top_idx]

      results[[i]] <- tibble(
        name_clean        = clean_name(data_norm$pfr_player_name[i]),
        draft_year        = data_norm$draft_year[i],
        position          = position_label,
        comp_weighted_ppg = weighted.mean(comp_ppgs, sims),
        comp_bust_rate    = mean(comp_ppgs < 5),
        comp_median_ppg   = median(comp_ppgs),
        n_pool_used       = n_pool
      )
    }
    cat(sprintf("  %d: pool=%d, cohort=%d ✓\n", yr, n_pool, length(cohort_idx)))
  }

  # Players without CFB data get NA comp features (they get zero-imputed at
  # training time anyway, and there's no useful signal to extract).
  no_cfb_idx <- which(!seq_len(nrow(model_data)) %in%
                       which(model_data$has_cfb_data))
  no_cfb_rows <- if (length(no_cfb_idx) > 0) {
    tibble(
      name_clean        = clean_name(model_data$pfr_player_name[no_cfb_idx]),
      draft_year        = model_data$draft_year[no_cfb_idx],
      position          = position_label,
      comp_weighted_ppg = NA_real_,
      comp_bust_rate    = NA_real_,
      comp_median_ppg   = NA_real_,
      n_pool_used       = 0L
    )
  } else tibble()

  bind_rows(c(results, list(no_cfb_rows)))
}

# ── Run ──────────────────────────────────────────────────────────────────────

wr_data <- readRDS("data/wr_model_data.rds")
rb_data <- readRDS("data/rb_model_data.rds")
qb_data <- if (file.exists("data/qb_model_data.rds")) readRDS("data/qb_model_data.rds") else NULL
te_data <- if (file.exists("data/te_model_data.rds")) readRDS("data/te_model_data.rds") else NULL

wr_comps <- build_comp_features_for_position(wr_data, WR_COMP_FEATURES, "WR")
rb_comps <- build_comp_features_for_position(rb_data, RB_COMP_FEATURES, "RB")
qb_comps <- if (!is.null(qb_data))
  build_comp_features_for_position(qb_data, QB_COMP_FEATURES, "QB") else tibble()
te_comps <- if (!is.null(te_data))
  build_comp_features_for_position(te_data, TE_COMP_FEATURES, "TE") else tibble()

all_comps <- bind_rows(wr_comps, rb_comps, qb_comps, te_comps)

cat("\n── Comp feature coverage ──\n")
all_comps |>
  group_by(position) |>
  summarize(
    n          = n(),
    n_with_cmp = sum(!is.na(comp_weighted_ppg)),
    pct        = round(100 * mean(!is.na(comp_weighted_ppg)), 1),
    cmp_mean   = round(mean(comp_weighted_ppg, na.rm = TRUE), 2),
    cmp_med    = round(median(comp_weighted_ppg, na.rm = TRUE), 2),
    bust_mean  = round(mean(comp_bust_rate, na.rm = TRUE), 2)
  ) |>
  print()

# Sanity check: comp_weighted_ppg should correlate with actual ppg on
# producers (training-data leak-free correlation, since pool is strictly past).
cat("\n── Sanity: comp_weighted_ppg vs actual ppg (producers) ──\n")
sanity <- bind_rows(
  wr_data |> filter(has_cfb_data) |>
    mutate(name_clean = clean_name(pfr_player_name)) |>
    select(name_clean, draft_year, position, ppg, made_it),
  rb_data |> filter(has_cfb_data) |>
    mutate(name_clean = clean_name(pfr_player_name)) |>
    select(name_clean, draft_year, position, ppg, made_it)
) |>
  inner_join(all_comps, by = c("name_clean", "draft_year", "position"))

for (pos in c("WR", "RB")) {
  sub <- sanity |> filter(position == pos, made_it == 1, !is.na(comp_weighted_ppg))
  if (nrow(sub) >= 10) {
    r <- cor(sub$comp_weighted_ppg, sub$ppg)
    cat(sprintf("  %s producers: r = %.3f  (n=%d)\n", pos, r, nrow(sub)))
  }
}

saveRDS(all_comps, "data/comp_features.rds")
write_csv(all_comps, "data/comp_features.csv")
message("\nSaved: data/comp_features.rds  (", nrow(all_comps), " rows)")

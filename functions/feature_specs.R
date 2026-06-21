# functions/feature_specs.R
# ─────────────────────────────────────────────────────────────────────────────
# Single source of truth for model feature vectors and era-aware zero-fill
# rules. Sourced by 04_model_bust.R, 05_model_production.R, 11_temporal_cv.R,
# and 13_bust_tune.R so the deployed model, the CV evaluator, and the
# combiner-tuning replay all see identical features.
#
# Adding a feature: edit this file in one place. Removing one: same.
# ─────────────────────────────────────────────────────────────────────────────

# ── WR features ──────────────────────────────────────────────────────────────
# Shared between bust + production. Bust adds per-game rates and the
# possession-WR archetype flag (heavy + slow → limited NFL ceiling).

WR_FEATURES_BASE <- c(
  # Draft capital (sqrt_pick beats log for both bust and production magnitudes)
  "sqrt_pick", "age", "draft_year_sc", "tier",
  # Final season production
  "rec_final", "rec_yards_final", "rec_td_final", "ypr",
  # Penultimate season
  "rec_penult", "rec_yards_penult", "rec_td_penult",
  # Ante-penultimate
  "rec_yards_ante",
  # Trends & efficiency (rec_yds_yoy = chronological final - penult; can be negative)
  "rec_yds_yoy", "rec_td_rate",
  # Team context
  "teammate_rec_yards", "dominator_rate",
  # Combine athleticism
  "weight", "height_in", "forty", "vertical", "broad_jump",
  "speed_score",
  # Usage / PPA (2016+, era-zeroed via has_ppa / has_usage in build_recipe)
  "usg_pass", "usg_passing_downs", "avg_PPA_pass", "total_PPA_pass",
  # WR PBP (2014+, era-zeroed via has_wr_pbp in build_recipe).
  # aDOT and YAC/rec are not available in cfbfastR.
  "catch_rate_wr", "yards_per_target_wr", "yards_per_rec_wr",
  "explosive_rec_rate", "target_share_wr", "targets_per_game_wr",
  "epa_per_target_wr", "epa_per_play_wr_pbp",
  # Pre-college recruiting consensus
  "recruit_stars", "recruit_rating", "recruit_rank",
  # Teammate context
  "college_years", "age_relative", "n_drafted_skill", "elite_teammate",
  # Era / coverage flags
  "has_penult", "has_ppa", "has_usage", "has_wr_pbp",
  "has_recruiting", "has_combine", "has_recruit_year",
  # Did the player peak in their final college year?
  "best_season_is_final",
  # NB: breakout_age_imputed / peak_dominator_pre22 / peak_yards_pre21 /
  # n_seasons_dominant / has_breakout / dominator_age_resid / dominator_age_z
  # are built by 02g_build_breakout_features.R and attached at score time,
  # but LOFO on the 2016-2023 OOS folds showed all 7 are net-harmful
  # (peak_yards_pre21 alone costs 0.05 MAE). They're kept on the prospect
  # dataframe for inspection/diagnostics but NOT in the feature spec.
  # Comp-stack (kNN over historical NFL outcomes; strictly past pool, leakage-free).
  # comp_median_ppg dropped 2026-05-09 — LOFO showed it actively hurt OOS MAE
  # (-0.052 ALL, -0.139 RB) because it duplicates comp_weighted_ppg's signal
  # while adding noise. weighted + bust_rate together are the optimal pair.
  "comp_weighted_ppg", "comp_bust_rate", "has_comp_features",
  # Landing spot / depth chart opportunity
  "vacated_tgt_pct", "incumbent_tgt_share", "n_ret_wr_50tgt",
  "incumbent_wr1_age", "expected_depth_rank", "team_targets_prior",
  "has_landing_data",
  # Draft-capital delta: actual_pick_value - projected_pick_value (mocks).
  # Positive = drafted earlier than consensus (team belief signal).
  # Built by 02e_build_mock_draft.R + 02f_build_draft_value_chart.R.
  "draft_capital_delta", "has_mock_data"
)

WR_BUST_ONLY <- c(
  # Per-game rates (helps bust separation since volume vs efficiency matters
  # more for the made-it/bust boundary than for the production magnitude).
  "rec_yards_per_game", "rec_per_game",
  # Heavy + slow WR archetype (limited NFL ceiling).
  "is_possession_wr"
)

WR_BUST_FEATURES <- c(WR_FEATURES_BASE, WR_BUST_ONLY)
WR_PROD_FEATURES <- WR_FEATURES_BASE

# ── RB features ──────────────────────────────────────────────────────────────
# Shared between bust + production. Bust adds per-game rates.

RB_FEATURES_BASE <- c(
  "sqrt_pick", "age", "draft_year_sc", "tier",
  # Final season production (scrimmage_yards omitted — collinear with rush + rec)
  "carries_final", "rush_yards_final", "rush_td_final", "ypc",
  # Dual-threat receiving
  "rb_rec", "rb_rec_yards", "rb_rec_td",
  # Non-redundant composites
  "scrimmage_td", "yards_per_touch",
  # Penultimate / ante-penult
  "rush_yards_penult", "carries_penult",
  "rush_yards_ante",
  # Trends & efficiency
  "rush_yds_yoy", "rush_td_rate", "recv_share",
  # Team context
  "teammate_rush_yards", "dominator_rate",
  # Workload
  "total_touches",
  # Combine
  "weight", "height_in", "forty", "vertical", "broad_jump",
  "speed_score",
  # Usage / PPA (2016+, era-zeroed via has_ppa / has_usage)
  "usg_rush", "usg_passing_downs", "avg_PPA_rush", "total_PPA_rush",
  "usg_overall", "usg_pass", "avg_PPA_all", "total_PPA_all",
  # PBP Tier 1 + EPA/play (2014+, era-zeroed via has_pbp)
  "explosive_rate", "breakaway_rate",
  "target_share", "targets_per_game", "catch_rate",
  "epa_per_rush", "epa_per_play_pbp",
  # Pre-college recruiting
  "recruit_stars", "recruit_rating", "recruit_rank",
  # Teammate context
  "college_years", "age_relative", "n_drafted_skill", "elite_teammate",
  # Era / coverage flags
  "has_penult", "has_ppa", "has_usage", "has_pbp",
  "has_recruiting", "has_combine", "has_recruit_year",
  # Small-back archetype flag (rarely become feature backs in the NFL)
  "is_scat_back",
  # NB: breakout / age-adjusted dominator features were tested via LOFO and
  # found net-harmful (see WR spec note). They're attached for diagnostics
  # but not in the production feature spec.
  # Comp-stack — comp_median_ppg dropped per LOFO (see WR_FEATURES_BASE note;
  # same finding applies to RB).
  "comp_weighted_ppg", "comp_bust_rate", "has_comp_features",
  # Landing spot — RB value features dropped 2026-05-09 per LOFO. RB depth
  # charts are too volatile (injury-driven touch reshuffles) for roster-based
  # "expected workload" signals to predict NFL careers; the features added
  # net noise to RB MAE (-0.039 to -0.162 across feature ablations). The
  # has_landing_data flag is kept as a coverage indicator.
  "has_landing_data",
  # Draft-capital delta — see WR_FEATURES_BASE comment.
  "draft_capital_delta", "has_mock_data"
)

RB_BUST_ONLY <- c(
  "rush_yards_per_game", "carries_per_game", "scrimmage_yards_per_game"
)

RB_BUST_FEATURES <- c(RB_FEATURES_BASE, RB_BUST_ONLY)
RB_PROD_FEATURES <- RB_FEATURES_BASE

# ── Era-aware zero-fill rules ────────────────────────────────────────────────
# Used by build_recipe() to zero-fill era-incomplete features when their
# coverage flag is 0 — so XGBoost learns "pre-era" as a regime via the flag
# instead of being pulled toward the post-era median by step_impute_median().
#
# Each entry: list(flag = "<has_*>", features = c(...)).
# build_recipe() checks `flag %in% names(model_df)` before applying so the
# rule no-ops for stripped-down feature sets (e.g. quantile experiments).

ERA_ZEROFILL_RULES <- list(
  WR = list(
    list(flag = "has_ppa",
         features = c("avg_PPA_pass", "total_PPA_pass")),
    list(flag = "has_usage",
         features = c("usg_pass", "usg_passing_downs")),
    list(flag = "has_wr_pbp",
         features = c("catch_rate_wr", "yards_per_target_wr", "yards_per_rec_wr",
                      "explosive_rec_rate", "target_share_wr", "targets_per_game_wr",
                      "epa_per_target_wr", "epa_per_play_wr_pbp")),
    list(flag = "has_comp_features",
         features = c("comp_weighted_ppg", "comp_bust_rate"))
  ),
  RB = list(
    list(flag = "has_ppa",
         features = c("avg_PPA_rush", "total_PPA_rush",
                      "avg_PPA_all", "total_PPA_all")),
    list(flag = "has_usage",
         features = c("usg_rush", "usg_pass", "usg_overall", "usg_passing_downs")),
    list(flag = "has_pbp",
         features = c("explosive_rate", "breakaway_rate", "target_share",
                      "targets_per_game", "catch_rate", "epa_per_rush",
                      "epa_per_play_pbp")),
    list(flag = "has_comp_features",
         features = c("comp_weighted_ppg", "comp_bust_rate"))
  ),
  QB = list(
    list(flag = "has_qb_pbp",
         features = c("epa_per_dropback","epa_per_attempt","completion_pct_pbp",
                      "sack_rate","int_rate","negative_play_rate",
                      "explosive_pass_rate","late_down_epa","qb_share_team")),
    list(flag = "has_comp_features",
         features = c("comp_weighted_ppg","comp_bust_rate"))
  ),
  TE = list(
    list(flag = "has_te_pbp",
         features = c("catch_rate_te","yards_per_target_te","yards_per_rec_te",
                      "explosive_rec_rate_te","target_share_te","targets_per_game_te",
                      "epa_per_target_te","epa_per_play_te_pbp")),
    list(flag = "has_comp_features",
         features = c("comp_weighted_ppg","comp_bust_rate"))
  )
)

# ── QB feature spec ──────────────────────────────────────────────────────────
# QBs use a lighter feature set than WR/RB because the seasonal stats endpoint
# already encodes a lot of QB skill. Mobility (rushing component) is in here
# explicitly because dual-threat is increasingly important in modern NFL.

QB_FEATURES_BASE <- c(
  # Draft capital
  "sqrt_pick", "age", "draft_year_sc", "tier",
  # Passing — final season
  "pass_yds_final", "pass_td_final", "pass_int_final",
  "pass_att_final", "pass_comp_final", "pass_pct_final", "pass_ypa_final",
  "pass_td_int_ratio",
  # Penult / ante season + trend
  "pass_yds_penult", "pass_td_penult", "pass_att_penult",
  "pass_yds_ante", "pass_yds_yoy",
  # Per-game rates
  "pass_yds_per_game", "pass_td_per_game",
  # Mobility (rushing component)
  "rush_car_final", "rush_yds_final", "rush_td_final",
  "rush_yds_per_carry", "has_mobility",
  # PBP-derived efficiency (2014+ via has_qb_pbp era flag)
  "epa_per_dropback", "epa_per_attempt", "completion_pct_pbp",
  "sack_rate", "int_rate", "negative_play_rate",
  "explosive_pass_rate", "late_down_epa", "qb_share_team",
  "has_qb_pbp",
  # Comp-stack (kNN over historical NFL outcomes; strictly-past pool)
  "comp_weighted_ppg", "comp_bust_rate", "has_comp_features",
  # Combine
  "weight", "height_in", "forty", "vertical", "broad_jump",
  "speed_score",
  # Recruiting
  "recruit_stars", "recruit_rating", "recruit_rank",
  "college_years",
  # Era / coverage flags
  "has_penult", "has_recruiting", "has_combine", "has_recruit_year",
  "best_season_is_final",
  # Draft-capital delta
  "draft_capital_delta", "has_mock_data"
)
QB_BUST_FEATURES <- QB_FEATURES_BASE
QB_PROD_FEATURES <- QB_FEATURES_BASE

# ── TE feature spec ──────────────────────────────────────────────────────────
# TEs share most of the WR receiving feature space but lose the PBP /
# usage / PPA / landing pieces (we don't yet have TE-specific PBP).

TE_FEATURES_BASE <- c(
  # Draft capital
  "sqrt_pick", "age", "draft_year_sc", "tier",
  # Receiving — final season
  "rec_final", "rec_yards_final", "rec_td_final", "ypr_final",
  "rec_td_rate",
  # Penult / ante season + trend
  "rec_penult", "rec_yards_penult", "rec_td_penult",
  "rec_yards_ante", "rec_yds_yoy",
  # Per-game rates
  "rec_yards_per_game", "rec_per_game",
  # Team context
  "teammate_rec_yards", "dominator_rate",
  # PBP-derived efficiency (2014+, reuses WR PBP cache; has_te_pbp era flag)
  "catch_rate_te", "yards_per_target_te", "yards_per_rec_te",
  "explosive_rec_rate_te", "target_share_te", "targets_per_game_te",
  "epa_per_target_te", "epa_per_play_te_pbp",
  "has_te_pbp",
  # Comp-stack (kNN over historical NFL outcomes; strictly-past pool)
  "comp_weighted_ppg", "comp_bust_rate", "has_comp_features",
  # Combine + archetype flag
  "weight", "height_in", "forty", "vertical", "broad_jump",
  "speed_score", "is_move_te",
  # Recruiting
  "recruit_stars", "recruit_rating", "recruit_rank",
  "college_years",
  # Era / coverage flags
  "has_penult", "has_recruiting", "has_combine", "has_recruit_year",
  "best_season_is_final",
  # Landing spot (shared WR-side context)
  "vacated_tgt_pct", "incumbent_tgt_share", "n_ret_wr_50tgt",
  "incumbent_wr1_age", "expected_depth_rank", "team_targets_prior",
  "has_landing_data",
  # Draft-capital delta
  "draft_capital_delta", "has_mock_data"
)
TE_BUST_FEATURES <- TE_FEATURES_BASE
TE_PROD_FEATURES <- TE_FEATURES_BASE

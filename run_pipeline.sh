#!/usr/bin/env bash
# run_pipeline.sh — full rebuild from raw data to scored prospects + website.
#
# Idempotent: each cache step short-circuits if its output is already current
# (HTML caches under data/mock_html_cache, RDS caches under data/).
# Re-run any single step with `Rscript <step>.R`; the dependency chain is:
#
#   01 → targets.rds                      (NFL career outcomes; rarely changes)
#   02  + 02b/02c/02d/02e/02f → caches    (CFB stats, PBP, landing, mocks, value)
#   03 → wr/rb_model_data.rds             (merge + attach all features)
#   04 → wr/rb_bust_model.rds
#   05 → wr/rb_production_model.rds
#   07 → output/all_class_scores.rds      (scores 2021-2026)
#   11 → output/temporal_cv/*             (rolling OOS evaluation)
#   13 → output/bust_tune/*               (combiner re-tune; optional)
#
# Usage:
#   ./run_pipeline.sh                — full rebuild
#   ./run_pipeline.sh score-only     — skip retrain, just rescore + export
#   ./run_pipeline.sh data-only      — rebuild caches, no retrain
#
# Stops on the first script error so you don't silently regenerate downstream
# data from a broken upstream build.

set -euo pipefail
cd "$(dirname "$0")"

run() {
  echo
  echo "════════════════════════════════════════════════════════════════"
  echo "  ▶ $1"
  echo "════════════════════════════════════════════════════════════════"
  Rscript "$1"
}

mode="${1:-full}"

if [[ "$mode" == "data-only" || "$mode" == "full" ]]; then
  # Raw data + caches
  run 01_build_targets.R                # NFL career outcomes
  run 02_build_features.R               # CFB stats (cfbfastR raw + repaired conferences)
  run 02b_build_rb_pbp_features.R       # PBP-derived RB metrics
  run 02c_build_wr_pbp_features.R       # PBP-derived WR metrics
  run 02d_build_landing_spot_features.R # NFL roster opportunity per (team, year)
  run 02e_build_mock_draft.R            # Mock draft consensus (JackLich10 + Walt)
  run 02f_build_draft_value_chart.R     # Draft pick → career-value curve

  # Per-prospect feature merge
  run 03_merge_and_clean.R
fi

if [[ "$mode" == "full" ]]; then
  # Comp-stack features (kNN over historical NFL outcomes; strictly past pool)
  run 08b_build_comp_features.R

  # Models (~5-15 min total, dominated by xgboost grid search in 04 + 05)
  run 04_model_bust.R
  run 05_model_production.R

  # Ordinal-bucket models (XGB multiclass + clm ensemble) — coexists with
  # the continuous hurdle model and adds a P(bust|flex|elite|league_winner)
  # distribution to scored output.
  run 19_train_ordinal_models.R

  # OOS evaluation (training-tuned combiner is hardcoded in helpers.R from 13)
  run 11_temporal_cv.R
fi

if [[ "$mode" != "data-only" ]]; then
  # Two-pass score: deploy prospects need their feature data attached (via 07)
  # before 08c can compute comps for them. After 08c extends comp_features.rds,
  # 07 reruns to pick up the deploy comps in the prediction.
  run 07_score_all_classes.R
  run 08c_deploy_comps.R
  run 07_score_all_classes.R
  run export_website_data.R
  run export_inspector_data.R
fi

echo
echo "✓ Pipeline complete."

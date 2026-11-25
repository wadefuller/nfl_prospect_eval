#!/usr/bin/env bash
# run_pipeline.sh — the one entry point: raw data → models → scores → website.
#
# Every script runs from the repo root and reads/writes data/, models/ and
# output/ (all git-ignored). Run any single step the same way, e.g.
#   Rscript pipeline/5_evaluate/11_temporal_cv.R
#
# Stages (folders under pipeline/ run in order):
#   1_data      NFL outcomes + college / play-by-play / combine / mock caches
#   2_features  one training table per position, plus historical comp features
#   3_train     hit classifier, production regressor, outcome-tier models
#   4_score     score every class 2021–2026, comps, prospect write-ups
#   5_evaluate  walk-forward evaluation (the accuracy numbers in the README)
#   6_export    JSON for the website
#
# Usage:
#   ./run_pipeline.sh                — everything
#   ./run_pipeline.sh data-only      — stages 1–2 (refresh caches + training tables)
#   ./run_pipeline.sh score-only     — stages 4 + 6 with the existing models
#
# Steps are idempotent where they cache (HTML and RDS caches under data/).
# Stops on the first error so a broken upstream step can't silently
# regenerate everything downstream of it.

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
case "$mode" in full|data-only|score-only) ;; *)
  echo "usage: $0 [full|data-only|score-only]" >&2; exit 2 ;;
esac

if [[ "$mode" != "score-only" ]]; then
  # ── 1. Data ───────────────────────────────────────────────────────────────
  run pipeline/1_data/01_build_targets.R                # NFL outcomes (target)
  run pipeline/1_data/02_build_features.R               # college season stats
  run pipeline/1_data/02b_build_rb_pbp_features.R       # RB play-by-play
  run pipeline/1_data/02c_build_wr_pbp_features.R       # WR/TE play-by-play
  run pipeline/1_data/02j_build_qb_pbp_features.R       # QB play-by-play
  run pipeline/1_data/02d_build_landing_spot_features.R # NFL depth chart at landing spot
  run pipeline/1_data/02e_build_mock_draft.R            # mock-draft consensus
  run pipeline/1_data/02f_build_draft_value_chart.R     # pick → value curve
  run pipeline/1_data/02g_build_breakout_features.R     # breakout age / age-adjusted dominator
  run pipeline/1_data/02h_build_qb_features.R           # QB feature table
  run pipeline/1_data/02i_build_te_features.R           # TE feature table

  # ── 2. Features ───────────────────────────────────────────────────────────
  run pipeline/2_features/03_merge_and_clean.R          # WR/RB training tables
  run pipeline/2_features/03b_merge_qb_te.R             # QB/TE training tables
fi

if [[ "$mode" == "full" ]]; then
  run pipeline/2_features/08b_build_comp_features.R     # strictly-past comp features

  # ── 3. Train ──────────────────────────────────────────────────────────────
  run pipeline/3_train/04_model_bust.R                  # P(hit), WR/RB
  run pipeline/3_train/04b_model_bust_qb_te.R           # P(hit), QB/TE
  run pipeline/3_train/05_model_production.R            # PPG if hit, WR/RB
  run pipeline/3_train/05b_model_production_qb_te.R     # PPG if hit, QB/TE
  run pipeline/3_train/19_train_ordinal_models.R        # outcome tiers, WR/RB
  run pipeline/3_train/19b_train_ordinal_qb_te.R        # outcome tiers, QB/TE

  # ── 5. Evaluate (walk-forward; slow) ──────────────────────────────────────
  run pipeline/5_evaluate/11_temporal_cv.R              # WR/RB
  run pipeline/5_evaluate/11b_temporal_cv_qb_te.R       # QB/TE, comps refit per fold
  run pipeline/5_evaluate/20_bucket_cv.R                # outcome tiers
  run pipeline/5_evaluate/23_draft_slot_baseline.R      # draft-slot-only baseline
fi

if [[ "$mode" != "data-only" ]]; then
  # ── 4. Score ──────────────────────────────────────────────────────────────
  # Two passes: new prospects need their features attached before 08c can
  # build comps for them; the second pass picks those comps up.
  run pipeline/4_score/07_score_all_classes.R
  run pipeline/4_score/07b_score_qb_te.R
  run pipeline/4_score/08c_deploy_comps.R
  run pipeline/4_score/07_score_all_classes.R
  run pipeline/4_score/07b_score_qb_te.R
  run pipeline/4_score/08_player_comps.R                # comps shown on the site
  run pipeline/4_score/09_prospect_profiles.R           # archetypes + bullish/bearish notes

  # ── 6. Export ─────────────────────────────────────────────────────────────
  run pipeline/6_export/export_website_data.R
  run pipeline/6_export/export_inspector_data.R
  run pipeline/6_export/export_model_performance.R
fi

echo
echo "✓ Pipeline complete ($mode)."

# pipeline/

The production model. `../run_pipeline.sh` runs these stages in order. Every
script runs from the **repo root** (`Rscript pipeline/3_train/04_model_bust.R`)
and reads and writes the git-ignored `data/`, `models/` and `output/` folders.

Script numbers are stable IDs from the project's history, not a strict run
order. Within a stage, follow `run_pipeline.sh`. Files ending in `b` are the
QB/TE counterparts of the WR/RB script with the same number.

| Stage | Script | What it does | Writes |
|---|---|---|---|
| **1_data** | `01_build_targets.R` | NFL outcome: early-career half-PPR PPG, busts = 0 | `data/targets.rds` |
| | `02_build_features.R` | College season stats, recruiting, team context | `data/{wr,rb}_features_raw.rds` |
| | `02b` / `02c` / `02j` | Play-by-play aggregates for RB / WR+TE / QB (2014+) | `data/cfb_*_pbp_features.rds` |
| | `02d_build_landing_spot_features.R` | NFL depth chart at the landing spot | `data/landing_spot_features.rds` |
| | `02e_build_mock_draft.R` | Mock-draft consensus pick | `data/mock_draft_consensus.rds` |
| | `02f_build_draft_value_chart.R` | Pick → career-value curve | `data/draft_value_chart.rds` |
| | `02g_build_breakout_features.R` | Breakout age, age-adjusted dominator | `data/{wr,rb}_breakout_features.rds` |
| | `02h` / `02i` | QB / TE feature tables | `data/{qb,te}_features_raw.rds` |
| **2_features** | `03_merge_and_clean.R` / `03b_merge_qb_te.R` | Join outcomes + features into one training table per position | `data/{wr,rb,qb,te}_model_data.rds` |
| | `08b_build_comp_features.R` | Historical-comp features, drawn only from earlier classes | `data/comp_features.rds` |
| **3_train** | `04` / `04b` | Hit classifier, P(qualifying season) | `models/*_bust_model.rds` |
| | `05` / `05b` | PPG-if-hit regressor (quantile loss for RB/TE) | `models/*_production_model.rds` |
| | `19` / `19b` | Outcome-tier ensemble (XGBoost + Bayesian ordered logit) | `models/*_bucket_model.rds` |
| **4_score** | `07` / `07b` | Score every class 2021–2026 | `output/all_class_scores.*`, `output/qb_te_class_scores.*` |
| | `08c_deploy_comps.R` | Extend comp features to the newest classes | `data/comp_features.rds` |
| | `08_player_comps.R` | The comps (and "why this comp" reasons) shown on the site | `output/player_comps.*` |
| | `09_prospect_profiles.R` | Archetype labels and bullish/bearish notes | `output/prospect_profiles.csv` |
| **5_evaluate** | `11` / `11b` | Walk-forward test, 2016–2023 (QB/TE refit comps per fold) | `output/temporal_cv*/` |
| | `20_bucket_cv.R` | Walk-forward test of the outcome tiers | `output/bucket_cv/` |
| | `23_draft_slot_baseline.R` | Draft-slot-only baseline on the same folds | `output/draft_slot_baseline/` |
| **6_export** | `export_website_data.R` | Prospect board + comps JSON | `website/public/data/prospects/`, `comps/` |
| | `export_inspector_data.R` | Per-player inspector JSON | `website/public/data/inspector/` |
| | `export_model_performance.R` | Model page JSON (walk-forward metrics) | `website/public/data/model_performance.json` |

Shared code lives in `../functions/`. **Feature lists are defined only in
`functions/feature_specs.R`**, and every training, scoring and evaluation
script reads them from there.

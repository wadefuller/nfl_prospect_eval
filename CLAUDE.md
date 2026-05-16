# Draft Scout — R Pipeline

## Project overview

Draft Scout predicts NFL career performance (half-PPR fantasy PPG) for WR and RB prospects using college stats, combine athleticism, recruiting, landing spot opportunity, and mock draft consensus. It covers draft classes 2021–2026.

The R pipeline builds features, trains XGBoost models, scores all classes, and exports JSON for the React website (`website/`).

For a concise, up-to-date architecture/evaluation/feature-space shortcut, read `MODEL_CONTEXT.md` first.

---

## Pipeline execution

```bash
# Full rebuild (raw data → models → scores → website export)
./run_pipeline.sh

# Skip retrain — rescore with existing models + push to website
./run_pipeline.sh score-only

# Rebuild caches only — no retrain, no scoring
./run_pipeline.sh data-only

# Run any single step
Rscript 07_score_all_classes.R
```

**Build order (DAG):**
```
01_build_targets.R                     → data/targets.rds
02_build_features.R                    → data/wr/rb_features_raw.rds
02b_build_rb_pbp_features.R            → data/ PBP RB metrics
02c_build_wr_pbp_features.R            → data/ PBP WR metrics
02d_build_landing_spot_features.R      → data/landing_spot_features.rds
02e_build_mock_draft.R                 → data/mock_draft_consensus.rds
02f_build_draft_value_chart.R          → data/draft_value_chart.rds
03_merge_and_clean.R                   → data/wr_model_data.rds, rb_model_data.rds
04_model_bust.R                        → models/wr/rb_bust_model.rds
05_model_production.R                  → models/wr/rb_production_model.rds
07_score_all_classes.R                 → output/all_class_scores.rds
11_temporal_cv.R                       → output/temporal_cv/  (OOS eval — authoritative)
13_bust_tune.R                         → output/bust_tune/     (combiner re-tune; optional)
export_website_data.R                  → website/public/data/prospects/*.json etc.
export_inspector_data.R                → website/public/data/inspector/
```

---

## Architecture: two-stage hurdle model

The final prediction combines two XGBoost models:

```
P(made_it)  ←  bust classifier  (04_model_bust.R)
E[log(PPG) | made_it=1]  ←  production regressor  (05_model_production.R)

exp_ppg = hurdle_combine(p_made_it) × exp(log_ppg_pred)
```

**Combiner** (position-specific, tuned in `13_bust_tune.R`):
- WR: `clip(0.85 × sqrt(p_made_it))`  — power shrinkage
- RB: `clip(iso(p_made_it) − 0.05)`   — isotonic calibration + linear shift

Both models are trained with `tidymodels` + `xgboost`. Feature prep (imputation, dummying, era-aware zero-fill) lives in `build_recipe()` inside each training script.

---

## Functions: `functions/helpers.R` and `functions/feature_specs.R`

### `functions/feature_specs.R`
Single source of truth for all four feature vectors used across training and CV:
- `WR_BUST_FEATURES`, `WR_PROD_FEATURES`
- `RB_BUST_FEATURES`, `RB_PROD_FEATURES`

**Never hard-code feature lists anywhere else.** All of `04`, `05`, `11`, `13` source this file.

### `functions/helpers.R`
Key shared utilities — source at the top of any script:

| Function | What it does |
|---|---|
| `score_class(draft_year, pos)` | Scores all undrafted/2026 prospects via the full 8-step pipeline |
| `attach_base_cfb_stats()` | Fetches & joins cfbfastR stats (best-season logic, tier fix) |
| `attach_penult_features()` | Penultimate + ante-penultimate season stats |
| `attach_team_volumes()` | Team rushing/receiving context, dominator rate |
| `attach_per_game_rates()` | Per-game production rates |
| `attach_combine_features()` | Combine athleticism + speed_score |
| `attach_recruiting_features()` | 247Sports recruiting composite |
| `attach_usage_ppa_features()` | PPA efficiency + usage rates (2016+) |
| `attach_pbp_features()` | Play-by-play derived metrics (2014+) |
| `attach_landing_features()` | Landing spot depth-chart opportunity |
| `attach_draft_capital_features()` | Mock consensus → draft_capital_delta |
| `attach_comp_features()` | kNN comps from strictly-past historical pool |
| `classify_tier(conference, school)` | P4/G5/Other — uses `P4_SCHOOLS` override (cfbfastR conference field is corrupted ~50% of the time) |
| `join_cfb_stats()` | Three-pass fuzzy join: exact → stripped suffix → last-name+school |
| `pick_value(pick)` | Pick → normalized career value (0–100 exponential decay) |

**Train/score parity is enforced by construction**: `03_merge_and_clean.R` and `score_class()` call identical `attach_*` helpers.

---

## Known data quality issues

| Issue | Status | Fix |
|---|---|---|
| cfbfastR conference label corrupted ~50% of P4 team-years | Fixed | `P4_SCHOOLS` hardcoded list in `classify_tier()` |
| COVID opt-out year mismatch (Chase 2021) | Fixed | Team volumes keyed by `(team, cfb_season)` not just school |
| Pre-2010 CFB data 83–86% missing | Unresolved | WR weighted down (0.4×); RB excluded pre-2010 |
| Combine participation declining (85%→70% forty coverage) | Handled | `has_combine` flag + XGBoost native missing handling |
| Jr/Sr/II/III name mismatches in mock data | Fixed | Two-pass suffix-stripping join in `attach_draft_capital_features()` |

---

## Feature groups and era coverage

| Group | Era | Missing handled by |
|---|---|---|
| CFB raw stats (rec/rush yards, TDs, etc.) | All years | `has_cfb_data` |
| Play-by-play (PBP) — EPA, explosive rate, target share | 2014+ | `has_wr_pbp` / `has_pbp` flag; era zero-fill in recipe |
| PPA efficiency + usage rates | 2016+ | `has_ppa` / `has_usage` flag; era zero-fill in recipe |
| Recruiting composite (247Sports) | ~2005+ | `has_recruiting` flag |
| Combine athleticism | ~2000+ | `has_combine` flag |
| Landing spot opportunity | 2010+ | `has_landing_data` flag |
| Mock draft consensus | 2014+ (JackLich) / 2022+ (Walt) | `has_mock_data` flag; coverage ~38% |
| Comp-stack (kNN historical comps) | Strictly past pool only | `has_comp_features` flag |

Era flags are integers (0/1) for XGBoost native handling. **Never zero-fill PBP or PPA features directly** — that would contaminate pre-era players. Zero-fill is done via `step_mutate` inside `build_recipe()` conditional on the era flag.

---

## Model performance (temporal CV, test years 2016–2023)

Source of truth: `output/temporal_cv/metrics_summary.csv` (run `11_temporal_cv.R`)

| Split | n | MAE | Cor | Bias |
|---|---|---|---|---|
| ALL | 410 | 2.656 | 0.524 | +0.281 |
| WR | 247 | 2.452 | 0.481 | +0.517 |
| RB | 163 | 2.967 | 0.531 | −0.077 |

Bust classification accuracy OOS: **78.8%**

---

## Data sources

| Source | Package/URL | Used for |
|---|---|---|
| cfbfastR | `cfbd_stats_season_player()` | College stats (receiving, rushing, PPA, usage) |
| cfbfastR PBP | `load_pbp()` | EPA, explosive rate, target share (2014+) |
| nflreadr | `load_draft_picks()` | Actual draft slot, gsis_id |
| nflreadr | `load_combine()` | Height, weight, forty, vertical, broad jump |
| nflreadr | `load_player_stats()` | NFL career PPG outcomes (target) |
| nflreadr | `load_rosters()` | Landing spot depth-chart context |
| 247Sports | cfbfastR recruiting | Recruiting composite, stars, rank |
| JackLich10 GitHub | ESPN consensus 2014–2021 | Mock draft projected pick |
| WalterFootball | Scraped HTML | Mock draft projected pick 2022–2026 |

---

## Output files

| File | Contents |
|---|---|
| `data/wr_model_data.rds` | WR training data (all drafted players) |
| `data/rb_model_data.rds` | RB training data |
| `models/wr_bust_model.rds` | Trained WR bust classifier (tidymodels workflow) |
| `models/rb_bust_model.rds` | Trained RB bust classifier |
| `models/wr_production_model.rds` | Trained WR production regressor |
| `models/rb_production_model.rds` | Trained RB production regressor |
| `output/all_class_scores.rds` | Scored prospects 2021–2026 |
| `output/temporal_cv/metrics_summary.csv` | OOS metrics by position |
| `output/temporal_cv/oos_predictions.csv` | Per-player OOS predictions |
| `website/public/data/` | JSON consumed by the React website |

---

## Website

See `website/CLAUDE.md` for the React app details.

Quick start:
```bash
cd website && npm run dev   # → http://localhost:5173/
```

Website JSON is regenerated by:
```bash
Rscript export_website_data.R
Rscript export_inspector_data.R
```

---

## Common gotchas

- **Don't add features to `04`/`05`/`11`/`13` separately** — always add to `feature_specs.R` and let all scripts source it.
- **cfbfastR conference labels are wrong ~50% of the time** — always go through `classify_tier(conference, school)`, never use the `conference` column directly for tier.
- **`best_season_is_final` logic** — a player's "final season" is their last college year before the draft. The model picks the best qualifying season for stat features, but tracks whether that peak was also their final year.
- **Score path uses `name` column; training path uses `pfr_player_name`** — `attach_draft_capital_features()` and `attach_comp_features()` auto-detect which column is present.
- **Comp pool is strictly past** — `08b_build_comp_features.R` only uses prospects drafted before the target class. Do not use `all_class_scores.rds` directly as the comp pool.
- **Re-running `11_temporal_cv.R` is the authoritative MAE benchmark** — in-sample numbers from `07_score_all_classes.R` are optimistic.

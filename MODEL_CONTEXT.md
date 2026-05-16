# Draft Scout Model Context

Use this as the quick mental model before changing model code, score exports, or the website's model-facing UI. It summarizes the active architecture, evaluation setup, and feature space as implemented in the R pipeline.

## What The Model Predicts

Draft Scout scores drafted and draft-eligible WR/RB prospects by expected half-PPR fantasy PPG over the player's early NFL career.

The supervised target is built in `01_build_targets.R`:
- Source: `nflreadr::load_draft_picks()`, `load_rosters()`, `load_player_stats()`.
- Window: first three NFL seasons after draft.
- Qualifying season: regular-season NFL fantasy sample with enough games, then player-level early-career average.
- `made_it`: player logged at least one qualifying NFL season.
- `ppg`: shrinkage-adjusted / games-weighted half-PPR PPG, with busts coded as `0`.
- `avg_top2_ppg`: raw unshrunk reference value, kept for display/diagnostics.

The primary website score is not just the raw hurdle output. The deployed `exp_ppg` in `export_website_data.R` is:
1. continuous hurdle prediction from `score_class()`;
2. blended with the ordinal bucket expected value;
3. then blended with comp-weighted PPG when comps exist.

So, for model debugging, distinguish:
- `p_made_it`: raw bust-classifier probability.
- `p_eff`: calibrated/transformed probability used by the hurdle product.
- `exp_ppg` from `score_class()`: continuous hurdle PPG before export-time blending.
- `exp_ppg_bucket`: bucket-midpoint expected PPG from ordinal models.
- exported website `exp_ppg`: final deployed blend.

## Active Architecture

The continuous model is a two-stage hurdle model:

```text
p_made_it = XGBoost classifier(features)
log_ppg_pred = production model(features | producers only)
p_eff = position-specific transform(p_made_it)
hurdle_exp_ppg = p_eff * exp(log_ppg_pred)
```

Stage 1, `04_model_bust.R`:
- XGBoost classifier via tidymodels.
- Separate WR/RB models.
- Outcome: `made_it`.
- WR uses all available CFB rows and downweights pre-2010 rows with `importance_weights(0.4)`.
- RB uses 2010+ only due sparse older feature coverage.
- Saves isotonic calibration breakpoints (`iso_x`, `iso_y`) inside the model object.

Stage 2, `05_model_production.R`:
- Trained only on producers (`ppg > 0`) with `log(ppg)` target.
- WR: standard XGBoost regression via tidymodels.
- RB: custom XGBoost quantile model with pinball loss at `tau = 0.70`.
- RB quantile model stores its prepped recipe and raw xgboost fit.

Hurdle probability transform, in `functions/helpers.R::score_class()` and mirrored in `11_temporal_cv.R`:
- WR: `p_eff = clip(0.85 * sqrt(p_made_it))`.
- RB: `p_eff = clip(isotonic(p_made_it) - 0.05)`.
- Tuned in `13_bust_tune.R`; best combiners are saved in `output/bust_tune/best_combiners.csv`.

Ordinal bucket sidecar:
- `19_train_ordinal_models.R` trains bucket models.
- `functions/ordinal_helpers.R` defines bucket cuts, midpoints, training, and prediction helpers.
- Buckets: `bust`, `bench`, `flex`, `elite`, `league_winner`.
- XGBoost multiclass uses full production feature spec.
- Bayesian proportional-odds model (`rstanarm::stan_polr`) uses curated low-collinearity feature subsets.
- Prediction ensembles XGB and ordinal probabilities with geometric mean.
- Adds `p_bust`, `p_bench`, `p_flex`, `p_elite`, `p_league_winner`, uncertainty intervals, `bucket_top1`, and `exp_ppg_bucket`.

Export-time blend, in `export_website_data.R`:
- Hurdle/bucket blend:
  - WR: `0.30 * hurdle + 0.70 * bucket`.
  - RB: `0.20 * hurdle + 0.80 * bucket`.
- Then comp blend:
  - WR: `0.60 * blended_model + 0.40 * comp_weighted_ppg`.
  - RB: `0.35 * blended_model + 0.65 * comp_weighted_ppg`.

## Feature Pipeline

Raw college and context features are built by:
- `02_build_features.R`: CFB raw season stats, recruiting, PPA, usage, team context, best-season matching.
- `02b_build_rb_pbp_features.R`: RB PBP metrics.
- `02c_build_wr_pbp_features.R`: WR PBP metrics.
- `02d_build_landing_spot_features.R`: NFL landing/depth chart context.
- `02e_build_mock_draft.R`: mock draft consensus.
- `02f_build_draft_value_chart.R`: pick value curve.
- `08b_build_comp_features.R`: strictly-past kNN comp features for training.
- `08c_deploy_comps.R`: comp features for scored/deployed prospects.
- `03_merge_and_clean.R`: merge targets + features into `data/wr_model_data.rds` and `data/rb_model_data.rds`.

Canonical feature lists live in `functions/feature_specs.R`.

Do not add model features directly to `04_model_bust.R`, `05_model_production.R`, `11_temporal_cv.R`, or `13_bust_tune.R`. Add them to `feature_specs.R`, attach them in the feature pipeline, then update train/score parity if needed.

Important feature groups:
- Draft capital: `sqrt_pick`, draft year scaling, tier.
- College production: best eligible regular-season receiving/rushing season, TDs, efficiency, penult/ante/YoY.
- Team context: teammate production, dominator rate, teammate draft context.
- Combine: height, weight, forty, vertical, broad, speed score, position archetype flags.
- CFB PPA/usage: 2016+ coverage with era flags.
- PBP metrics: 2014+ coverage with era flags.
- Recruiting: 247Sports stars/rating/rank and recruit-year coverage.
- Landing spot: opportunity/depth chart context, especially useful for WR.
- Mock delta: actual pick value minus projected pick value.
- Comps: `comp_weighted_ppg`, `comp_bust_rate`, `has_comp_features`.

Era-aware missingness:
- PPA/usage/PBP/comp features are zero-filled only when their `has_*` flag is `0`.
- This zero-fill happens inside recipes and CV helpers so pre-era players are treated as a known regime rather than median-imputed modern players.
- Regular numeric missingness is median-imputed by recipe after era transforms.

## Train / Score Parity

Training data path:
```text
01_build_targets.R
02_build_features.R + 02b/02c/02d/02e/02f
03_merge_and_clean.R
04_model_bust.R
05_model_production.R
19_train_ordinal_models.R
```

Scoring path:
```text
07_score_all_classes.R
  -> fetch_cfb_stats()
  -> score_class()
  -> attach_* helpers in functions/helpers.R
08c_deploy_comps.R
07_score_all_classes.R again
export_website_data.R
export_inspector_data.R
```

`score_class()` is the deployed scoring core. It attaches:
1. base CFB stats;
2. penult/final trend stats;
3. team volumes;
4. per-game rates;
5. combine;
6. recruiting;
7. usage/PPA;
8. PBP;
9. landing;
10. breakout and age-adjusted dominator diagnostics;
11. comps;
12. draft-capital delta;
13. continuous hurdle predictions;
14. optional bucket predictions.

Identity joins:
- Current score path creates `.score_key`.
- `.score_key` uses `gsis_id` when available, else a stable fallback from position/year/name/school.
- Initial CFB resolution still uses name/school fallback because CFBD `athlete_id` is too sparse in local raw caches.
- After initial resolution, penult/final/ante feature joins should prefer `.score_key` to avoid repeated name-only joins.

## Evaluation

Authoritative continuous-hurdle evaluation:
- Script: `11_temporal_cv.R`.
- Rolling temporal CV: train on draft years `< K`, test on `K`, for `K = 2016:2023`.
- Rebuilds models inside each fold with fixed hyperparameters to evaluate the specification, not tuning noise.
- Outputs:
  - `output/temporal_cv/oos_predictions.csv`
  - `output/temporal_cv/metrics_summary.csv`
  - `output/temporal_cv/metrics_by_year.csv`

Current hurdle-only metrics from `output/temporal_cv/metrics_summary.csv`:

| Position | n | MAE | RMSE | Cor | Bias |
|---|---:|---:|---:|---:|---:|
| RB | 152 | 2.909 | 3.697 | 0.551 | 0.104 |
| WR | 242 | 2.445 | 3.021 | 0.480 | 0.507 |
| ALL | 394 | 2.624 | 3.298 | 0.537 | 0.352 |

Bucket CV:
- Script: `20_bucket_cv.R`.
- Outputs `output/bucket_cv/oos_predictions.csv`, `metrics.csv`, `calibration.csv`.
- Evaluates bucket log-loss/top-k accuracy and bucket-midpoint MAE.

Deployed blended model metrics:
- Exported by `export_model_performance.R` into `website/public/data/model_performance.json`.
- Uses hurdle OOS + bucket OOS + deployed blend weights.
- Current website JSON overall:
  - MAE `2.468`
  - Cor `0.595`
  - Bias `-0.233`
  - n `394`
  - WR MAE `2.361`
  - RB MAE `2.638`
  - bust accuracy `0.789`

When asked "how good is the model?", answer with deployed blend metrics unless the question specifically asks about the continuous hurdle component.

## Data Source Semantics

CFB stat source has changed over time in the repo:
- Training raw caches from `02_build_features.R` use `cfbd_stats_season_player(..., season_type = "both")` when raw caches are rebuilt.
- Per-year caches used by `fetch_cfb_stats()` may be PBP-derived regular-season caches (`data/cfb_<cat>_<year>.rds`) built to match legacy model assumptions.
- External QA against ESPN/Sports Reference often differs because public pages include bowls/playoffs while model caches may be regular-season-only.

Do not assume "final" in a field name literally means chronological final season:
- `rec_yards_final`, `rush_yards_final`, etc. are best eligible season features in much of the model.
- Chronological final-year yards are separate trend inputs (`*_actual_final`) used for YoY deltas.
- `best_season_is_final` tells whether the best production season was also the final college season.

Known quirks:
- cfbfastR conference labels are unreliable for tiering. Use `classify_tier(conference, school)` and the `P4_SCHOOLS` allowlist.
- Name resolution still matters. Examples include `Tank Dell` / `Nathaniel Dell`, `Tutu Atwell` / `Chatarius Atwell`, and `Cam Skattebo` / `Cameron Skattebo`.
- `athlete_id` exists in raw CFBD-shaped files but is too sparse to replace names fully.
- FCS and transfer players can have source disagreements and missing IDs.
- Website JSON is generated; do not hand-edit `website/public/data/*.json`.

## Commands

Full rebuild:
```bash
./run_pipeline.sh
```

Rescore/export without retraining:
```bash
./run_pipeline.sh score-only
```

Important single steps:
```bash
Rscript 07_score_all_classes.R
Rscript 08c_deploy_comps.R
Rscript export_website_data.R
Rscript export_inspector_data.R
Rscript 11_temporal_cv.R
Rscript 20_bucket_cv.R
Rscript export_model_performance.R
```

Website:
```bash
cd website
npm run dev
npm run lint
npm run build
```

## High-Risk Change Checklist

Before changing features, joins, or prediction exports:
- Check `functions/feature_specs.R`.
- Check train/score parity in `03_merge_and_clean.R` and `functions/helpers.R::score_class()`.
- If scoring features change, rerun `07_score_all_classes.R`, `08c_deploy_comps.R`, `07_score_all_classes.R`, then both exports.
- If model features change, rerun `04`, `05`, `19`, `11`, `20`, `21`, and `export_model_performance.R`.
- For UI/model-page changes, remember displayed `exp_ppg` is the final blend, not necessarily the raw hurdle prediction.
- Run `npm run lint` and `npm run build` in `website/` after frontend/export type changes.

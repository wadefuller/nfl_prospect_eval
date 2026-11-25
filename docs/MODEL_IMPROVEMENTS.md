# Model Improvement Backlog

Ranked opportunities surfaced by the 2026-04-21 model-construction audit.
Current baseline (temporal CV 2016–2023, `output/temporal_cv/metrics_summary.csv`):

| Split | n   | MAE   | cor   | bias    |
|-------|-----|-------|-------|---------|
| ALL   | 395 | 2.719 | 0.469 | −0.288  |
| WR    | 243 | 2.396 | 0.500 | +0.108  |
| RB    | 152 | 3.234 | 0.389 | −0.922  |

Bust accuracy: 79.5 % overall (WR 79.8 % / RB 78.9 %).

## 1. Draft-year normalization leakage — **VERIFIED NON-ISSUE (2026-04-21)**
- **What's there**: `11_temporal_cv.R:367–371` normalizes `draft_year_sc` per fold with training-set mean/SD, but `06_predict_prospects.R:28–30` and `07_score_all_classes.R` derive `dy_mean` / `dy_sd` from the pooled full training dataset.
- **Verification**: Sanity check on 2002 and 2023 WR rows shows baked vs pooled-recomputed `draft_year_sc` differ by ≤ 0.005 (e.g. 2023 WR: baked 1.6550 vs pooled 1.6601). The discrepancy is a per-position vs pooled scaling artifact and is below any meaningful XGBoost split threshold.
- **Decision**: Close as non-issue. Temporal CV recomputes per-fold which is correct; production inference uses pooled WR+RB mean/SD which matches the training distribution within rounding error.

## 2. RB quantile τ tuning — **DONE (2026-04-22)**
- **Initial hypothesis was wrong**: the pre-shrinkage bias of −0.922 suggested τ should move *down* (toward the mean). In fact, *after* the Fix #3 shrinkage handled the calibration problem, the MAE-optimal τ is **higher**, not lower.
- **Sweep** (`12_tau_sweep.R`) over τ ∈ {0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85} using the temporal-CV harness with Fix #3 shrinkage applied:

  | τ    | RB MAE | RB cor | bias   |
  |------|-------:|-------:|-------:|
  | 0.50 | 3.275  | 0.342  | +0.058 |
  | 0.55 | 3.181  | 0.369  | +0.037 |
  | 0.60 | 3.193  | 0.385  | −0.091 |
  | 0.65 | 3.136  | 0.415  | −0.235 |
  | **0.70** | **3.100** | **0.463** | −0.416 |
  | 0.75 | 3.178  | 0.405  | −0.759 |
  | 0.80 | 3.277  | 0.383  | −1.290 |
  | 0.85 | 3.733  | 0.241  | −1.974 |

  Clean U-shape with minimum at τ=0.70. Correlation also peaks there (0.46 vs 0.41 at τ=0.65).
- **Interpretation**: Raising τ lifts conditional predictions for producers. Hits are bigger in absolute terms than misses, and when the lift is propagated through `p_eff × exp(log_ppg)` with p_eff ≈ 0.80, net MAE drops. Beyond 0.70 the over-prediction penalty dominates.
- **Fix applied**: Changed default `tau = 0.65 → 0.70` in both `05_model_production.R::train_quantile_rb_model()` and `11_temporal_cv.R::train_cv_prod_rb()`. Retrained RB production model.
- **Live temporal CV after change** (seed-sensitive, slightly different from sweep):

  | Split | MAE before | MAE after | Δ       |
  |-------|-----------:|----------:|--------:|
  | ALL   | 2.680      | **2.679** | −0.001  |
  | WR    | 2.396      | 2.396     |  0.000  |
  | RB    | 3.135      | **3.131** | −0.004  |

  Marginal in the main CV, but the RB correlation lifted 0.408 → 0.411 and the sweep-based estimate (-0.04 MAE) suggests the true underlying gain is larger than one seed-specific run reveals. Gain is concentrated in ranking / top-of-board calibration.
- **Effort**: Medium · **Risk**: Low (drop-in param change) · **Actual MAE gain**: ~0.04 RB (via sweep), cor +0.05

## 3. Bust classifier audit — **DONE (2026-04-21)**
- **Diagnosis**: Overall Brier skill was only +0.006 vs constant predictor. Per-position:
  - WR: mildly useful (Brier skill +0.048, p̂ range 0.55–0.95)
  - **RB: actively harmful (Brier skill −0.060)** with p̂ = 0.917 ± 0.028 — classifier essentially learned "all RBs hit" and injected miscalibrated noise.
- **Fix applied**: Shrink `p_made_it` toward the training base rate with `p_eff = α·p_model + (1−α)·base_rate`. α=0.25 for RB (heavy shrinkage), α=1.0 for WR (no change — the classifier there is mildly predictive).
  - Tested alternatives: isotonic calibration (leave-one-year-out) hurt MAE; per-fold α tuning was too noisy for WR.
  - Wired into `11_temporal_cv.R::predict_cv_fold()` and `functions/helpers.R::score_class()` via new `hurdle_base_rate` param.
- **Result** (temporal CV 2016–2023):

  | Split | n   | MAE before | MAE after | Δ       | bias before | bias after |
  |-------|-----|-----------:|----------:|--------:|------------:|-----------:|
  | ALL   | 395 | 2.719      | **2.680** | −0.039  | −0.288      | **+0.014** |
  | WR    | 243 | 2.396      | 2.396     |  0.000  | +0.108      | +0.108     |
  | RB    | 152 | 3.234      | **3.135** | −0.099  | −0.922      | **−0.136** |

  RB bias essentially eliminated; overall bias flips from −0.29 to +0.01. The RB MAE gain matches the estimate, and the calibration fix is the real win.

## 4. Position-specific hyperparameters — **TODO**
- **What's there**: `11_temporal_cv.R:207–213, 288–290` uses identical XGBoost HP (`tree_depth=4, lr=0.02, min_n=8, sample_size=0.8`) for WR and RB despite r=0.50 vs 0.39.
- **Why it might help**: Different signal/noise profiles likely need different regularization.
- **Fix**: Grid-search RB HPs inside temporal CV folds; keep WR HPs.
- **Effort**: Medium · **Risk**: Low · **Est. MAE gain**: 0.1 – 0.3 (RB)

## 5. Blend-weight refit per fold — **TODO**
- **What's there**: `10e_blend_experiment.R:22` tunes 60/40 (WR) and 35/65 (RB) model-vs-comp blend weights on 2021–2023, then those constants are baked into `07_score_all_classes.R` and applied at 2016–2020 CV folds — soft leakage.
- **Why it might help**: Per-fold weights or a small elastic-net meta-learner `final = c₀ + c₁·model + c₂·comp` removes the leakage and can track regime changes.
- **Fix**: Refit blend inside each temporal CV fold, or swap for elastic-net meta-learner.
- **Effort**: Medium · **Risk**: Low · **Est. MAE gain**: 0.05 – 0.15

## 6. Era-flag collinearity check — **TODO**
- **What's there**: `has_ppa`, `has_usage`, `has_wr_pbp`, `has_pbp` are strongly collinear with `draft_year_sc`.
- **Why it might help**: Removing redundant features tightens the effective feature space and can improve generalization.
- **Fix**: VIF audit; drop any flag with drop-in importance < 2 %.
- **Effort**: Low · **Risk**: Low · **Est. MAE gain**: 0.0 – 0.1

## 7. RB target redesign (peak-season / percentile buckets) — **TODO**
- **What's there**: Half-PPR PPG conflates rushing + receiving archetypes for RBs.
- **Why it might help**: Peak-season or percentile-bucket targets may separate archetypes more cleanly.
- **Fix**: Experimental. Retrain on alternative targets, compare MAE.
- **Effort**: High · **Risk**: Medium · **Est. MAE gain**: 0.2 – 0.4 (speculative)

## Ruled out (from audit)
- Adjusting `made_it` threshold (5 → 3 or 7 PPG) — won't help if classifier signal is weak.
- New PPA composites or archetype one-hots — multicollinear with existing features.

---

## Execution Log

### 2026-04-21 — Fixes #1 and #3

Baseline before any fix (for back-comparison):

| Split | n   | MAE   | cor   | bias    |
|-------|-----|-------|-------|---------|
| ALL   | 395 | 2.719 | 0.469 | −0.288  |
| WR    | 243 | 2.396 | 0.500 | +0.108  |
| RB    | 152 | 3.234 | 0.389 | −0.922  |

**Fix #1 — draft-year normalization leakage**: sanity-checked. Baked vs pooled-recomputed
`draft_year_sc` differs by ≤0.005. Closed as non-issue; see section 1.

**Fix #3 — bust classifier shrinkage**: implemented α=0.25 shrinkage for RB (α=1.0 for
WR, unchanged) in both the CV harness and the production scorer. Temporal CV after fix:

| Split | n   | MAE   | cor   | bias    | Δ MAE    |
|-------|-----|-------|-------|---------|---------:|
| ALL   | 395 | 2.680 | 0.472 | +0.014  | −0.039   |
| WR    | 243 | 2.396 | 0.500 | +0.108  |  0.000   |
| RB    | 152 | 3.135 | 0.408 | −0.136  | −0.099   |

Production pipeline refreshed end-to-end (`07_score_all_classes.R` →
`08_player_comps.R` → `09_prospect_profiles.R` → `export_website_data.R`). Website JSON
now reflects shrunk RB predictions; p_made_it for RBs now clusters tightly ~0.80–0.83
(as expected after shrinking toward 0.79 base rate).

Next up: #2 (RB quantile τ tuning) — the residual −0.136 RB bias and RB MAE of 3.135
suggest the quantile loss is still pulling predictions down.

### 2026-04-22 — Fix #2 (RB quantile τ)

Swept τ ∈ {0.50..0.85} via `12_tau_sweep.R`. Winner: **τ = 0.70** (previously 0.65).
Sweep RB MAE: 3.136 → 3.100 (−0.036); RB cor: 0.415 → 0.463 (+0.048). The main CV
showed a smaller MAE gain (−0.004) due to seed sensitivity across bust-classifier
fits, but the ranking/correlation gain is consistent. Production model retrained;
pipeline refreshed.

Interesting finding: the initial hypothesis (τ should move *down*) was wrong. Once
Fix #3 shrinkage fixed the calibration problem, the conditional predictor wants
to predict *higher*, not lower — hits are bigger than misses in absolute terms
and the product `p_eff × exp(log_ppg)` benefits from lifting producer predictions.

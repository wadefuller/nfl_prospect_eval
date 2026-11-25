# experiments/

One-off studies that shaped the production model. The pipeline doesn't run
them. Each was run from the repo root at the time (`Rscript experiments/<file>`)
and writes to its own folder under `output/`. Some predate later feature
changes and may need small updates to run again.

| Script | Question | Outcome |
|---|---|---|
| `10_model_diagnostics.R` | Where do in-sample predictions miss? (plots) | Diagnostic only |
| `10b_quantile_experiment.R`, `10c_quantile_tuned.R` | Does a quantile loss fix RB under-projection of early-round backs? | Adopted: RB production model uses pinball loss |
| `12_tau_sweep.R` | Which quantile τ for the RB production model? | τ = 0.70 |
| `13_bust_tune.R` | How should P(hit) be turned into the hurdle multiplier? | WR power shrink, RB isotonic − 0.05 (`functions/helpers.R`) |
| `10e_blend_experiment.R` | How much weight should comps get in the final number? | Export-time model/comp blend |
| `14_comp_lofo.R` | Which comp-derived features earn their place? | Leave-one-feature-out ranking |
| `15_comp_hp_sweep.R` | Comp neighbours, bandwidth, pick window | Comp configuration in `08b` |
| `16_landing_lofo.R` | Which landing-spot features help? | Leave-one-feature-out ranking |
| `17_year_cutoff_sweep.R` | Does pre-2010 training data help or hurt? | Per-position training start year |
| `18_ordinal_ensemble.R` | Prototype of the outcome-tier ensemble | Productionised as `pipeline/3_train/19_*` |
| `21_blend_sweep.R` | Hurdle vs. tier-model blend weights | WR 0.30/0.70, RB 0.20/0.80 |
| `22_age_features_lofo.R` | Do breakout-age features add signal? | No measurable gain |

Rule of thumb: an idea graduates into `pipeline/` only if it improves the
walk-forward test in `pipeline/5_evaluate/`. In-sample gains don't count.

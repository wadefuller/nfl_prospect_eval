# export_model_performance.R
# ─────────────────────────────────────────────────────────────────────────────
# Regenerates website/public/data/model_performance.json from the latest CV
# outputs. Reports the metrics of the *deployed* model, which is the
# hurdle + bucket ensemble (weights tuned in 21_blend_sweep.R), not just
# the continuous hurdle component.
#
# Inputs:
#   output/temporal_cv/oos_predictions.csv  — hurdle OOS predictions
#   output/temporal_cv/metrics_by_year.csv  — per-year metrics (hurdle-only,
#                                              kept for trend chart)
#   output/bucket_cv/oos_predictions.csv    — bucket OOS predictions
#   output/temporal_cv_qb_te/oos_predictions.csv — QB/TE hurdle OOS + per-fold
#                                              ("honest") comps, blended as deployed
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(jsonlite)

# Hurdle CV (per-prospect predictions + per-year metrics)
hurdle <- read_csv("output/temporal_cv/oos_predictions.csv", show_col_types = FALSE)
by_year <- read_csv("output/temporal_cv/metrics_by_year.csv", show_col_types = FALSE)

# Bucket CV (per-prospect predictions from the Bayesian ensemble)
bucket <- read_csv("output/bucket_cv/oos_predictions.csv", show_col_types = FALSE)

# ── Build deployed-ensemble OOS predictions ──────────────────────────────────
# Same weights as production export_website_data.R hurdle/bucket blend.
ENSEMBLE_WEIGHTS <- list(WR = list(hurdle = 0.30, bucket = 0.70),
                         RB = list(hurdle = 0.20, bucket = 0.80))

oos <- hurdle |>
  rename(exp_ppg_hurdle = exp_ppg) |>
  inner_join(
    bucket |> select(pfr_player_name, position, draft_year, exp_ppg_bucket),
    by = c("pfr_player_name", "position", "draft_year")
  ) |>
  mutate(
    exp_ppg = case_when(
      position == "WR" ~ ENSEMBLE_WEIGHTS$WR$hurdle * exp_ppg_hurdle +
                         ENSEMBLE_WEIGHTS$WR$bucket * exp_ppg_bucket,
      position == "RB" ~ ENSEMBLE_WEIGHTS$RB$hurdle * exp_ppg_hurdle +
                         ENSEMBLE_WEIGHTS$RB$bucket * exp_ppg_bucket,
      TRUE             ~ exp_ppg_hurdle
    ),
    residual = ppg - exp_ppg
  )

# ── QB / TE deployed OOS (11b_temporal_cv_qb_te.R) ───────────────────────────
# Hurdle OOS blended with comps rebuilt inside each fold (pool < test year),
# using the same comp shares export_website_data.R applies to exp_ppg.
QBTE_COMP_SHARE <- c(QB = 0.50, TE = 0.70)
qbte <- read_csv("output/temporal_cv_qb_te/oos_predictions.csv",
                 show_col_types = FALSE) |>
  mutate(
    comp_share = unname(QBTE_COMP_SHARE[position]),
    exp_ppg = if_else(is.na(comp_weighted_ppg_honest), exp_ppg,
                      (1 - comp_share) * exp_ppg +
                        comp_share * comp_weighted_ppg_honest),
    hit       = made_it == 1,
    residual  = ppg - exp_ppg,
    round_grp = case_when(round == 1 ~ "Round 1", round == 2 ~ "Round 2",
                          TRUE ~ "Round 3+")
  )
stopifnot(all(c("QB", "TE") %in% qbte$position), !anyNA(qbte$exp_ppg),
          nrow(qbte) > 150)

keep_cols <- c("pfr_player_name", "position", "draft_year", "round", "pick",
               "made_it", "ppg", "p_made_it", "exp_ppg", "hit", "residual",
               "round_grp")
oos <- bind_rows(oos |> select(all_of(keep_cols)),
                 qbte |> select(all_of(keep_cols)))
POSITIONS <- c("QB", "RB", "WR", "TE")

mae <- function(a, p) mean(abs(a - p))
cor_f <- function(a, p) cor(a, p, use = "complete.obs")
bias  <- function(a, p) mean(p - a)

# ── Overall metrics (all positions, deployed model) ─────────────────────────
# bias = mean(predicted - actual): negative means the model predicts too low.
pos_metrics <- function(d) list(
  mae           = round(mae(d$ppg, d$exp_ppg), 3),
  cor           = round(cor_f(d$exp_ppg, d$ppg), 3),
  bias          = round(bias(d$ppg, d$exp_ppg), 3),
  bust_accuracy = round(mean((d$p_made_it >= 0.5) == (d$made_it == 1)), 3),
  n             = as.integer(nrow(d))
)
overall <- pos_metrics(oos)
for (p in POSITIONS) {
  m <- pos_metrics(filter(oos, position == p))
  names(m) <- paste0(tolower(p), "_", names(m))
  overall <- c(overall, m)
}

# ── By year ─────────────────────────────────────────────────────────────────
# Recomputed from ensembled OOS so the trend chart reflects the deployed
# model's accuracy, not the hurdle-only component.
by_year_json <- oos |>
  mutate(test_year = draft_year) |>
  group_by(test_year) |>
  summarise(
    n      = n(),
    mae    = round(mae(ppg, exp_ppg), 3),
    cor    = round(cor_f(exp_ppg, ppg), 3),
    qb_mae = round(mae(ppg[position == "QB"], exp_ppg[position == "QB"]), 3),
    rb_mae = round(mae(ppg[position == "RB"], exp_ppg[position == "RB"]), 3),
    wr_mae = round(mae(ppg[position == "WR"], exp_ppg[position == "WR"]), 3),
    te_mae = round(mae(ppg[position == "TE"], exp_ppg[position == "TE"]), 3),
    .groups = "drop"
  ) |>
  transmute(year = as.integer(test_year), n = as.integer(n),
            mae, cor, qb_mae, rb_mae, wr_mae, te_mae)

# ── By round ────────────────────────────────────────────────────────────────
build_round_group <- function(df) {
  df |>
    group_by(round_grp) |>
    summarise(
      n          = n(),
      mae        = round(mean(abs(residual)), 3),
      cor        = round(cor(exp_ppg, ppg), 3),
      hit_rate   = round(mean(hit), 3),
      avg_exp    = round(mean(exp_ppg), 2),
      avg_actual = round(mean(ppg), 2),
      .groups    = "drop"
    ) |>
    arrange(factor(round_grp, levels = c("Round 1","Round 2","Round 3","Rounds 4-7")))
}

by_round    <- build_round_group(oos)
by_round_wr <- build_round_group(filter(oos, position == "WR"))
by_round_rb <- build_round_group(filter(oos, position == "RB"))
by_round_qb <- build_round_group(filter(oos, position == "QB"))
by_round_te <- build_round_group(filter(oos, position == "TE"))

# ── Scatter ─────────────────────────────────────────────────────────────────
scatter <- oos |>
  transmute(
    name   = pfr_player_name,
    pos    = position,
    year   = as.integer(draft_year),
    round  = as.integer(round),
    pick   = as.integer(pick),
    pred   = round(exp_ppg, 2),
    actual = round(ppg, 2),
    p_hit  = round(p_made_it, 3),
    hit    = as.logical(hit)
  ) |>
  arrange(pick)

# ── Hit-rate calibration ────────────────────────────────────────────────────
calibration <- oos |>
  mutate(
    prob_bucket = cut(
      p_made_it,
      breaks = seq(0, 1, by = 0.1),
      include.lowest = TRUE,
      labels = paste0(seq(0, 90, by = 10), "-", seq(10, 100, by = 10), "%")
    )
  ) |>
  filter(!is.na(prob_bucket)) |>
  group_by(prob_bucket) |>
  summarise(
    n         = n(),
    pred_prob = round(mean(p_made_it), 3),
    obs_rate  = round(mean(hit), 3),
    .groups   = "drop"
  ) |>
  transmute(
    bucket    = as.character(prob_bucket),
    n         = as.integer(n),
    pred_prob,
    obs_rate
  )

# ── Notable residuals (top 20 by residual / position SD of actual PPG) ─────────────────────────
notable <- oos |>
  group_by(position) |>
  mutate(abs_res = abs(residual) / sd(ppg)) |>
  ungroup() |>
  slice_max(abs_res, n = 20) |>
  transmute(
    name   = pfr_player_name,
    pos    = position,
    year   = as.integer(draft_year),
    round  = as.integer(round),
    pick   = as.integer(pick),
    pred   = round(exp_ppg, 2),
    actual = round(ppg, 2),
    diff   = round(ppg - exp_ppg, 2)
  )

out <- list(
  overall    = overall,
  byYear     = by_year_json,
  byRound    = by_round,
  byRoundWR  = by_round_wr,
  byRoundRB  = by_round_rb,
  byRoundQB  = by_round_qb,
  byRoundTE  = by_round_te,
  calibration = calibration,
  scatter    = scatter,
  notable    = notable
)

write_json(out, "website/public/data/model_performance.json",
           pretty = TRUE, auto_unbox = TRUE, digits = NA)
message("Wrote: website/public/data/model_performance.json")
message(sprintf("Overall: MAE=%.3f  Cor=%.3f  N=%d  bust acc=%.1f%%",
                overall$mae, overall$cor, overall$n, 100 * overall$bust_accuracy))
for (p in tolower(POSITIONS))
  message(sprintf("  %s: n=%d MAE=%.3f Cor=%.3f Bias=%+.3f",
                  toupper(p), overall[[paste0(p, "_n")]],
                  overall[[paste0(p, "_mae")]], overall[[paste0(p, "_cor")]],
                  overall[[paste0(p, "_bias")]]))

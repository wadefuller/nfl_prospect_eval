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

mae <- function(a, p) mean(abs(a - p))
cor_f <- function(a, p) cor(a, p, use = "complete.obs")
bias  <- function(a, p) mean(p - a)

# ── Overall metrics (ensemble) ───────────────────────────────────────────────
overall <- list(
  mae              = round(mae(oos$ppg, oos$exp_ppg), 3),
  cor              = round(cor_f(oos$exp_ppg, oos$ppg), 3),
  bias             = round(bias(oos$ppg, oos$exp_ppg), 3),
  n                = as.integer(nrow(oos)),
  wr_mae           = round(mae(oos$ppg[oos$position=="WR"],
                                oos$exp_ppg[oos$position=="WR"]), 3),
  rb_mae           = round(mae(oos$ppg[oos$position=="RB"],
                                oos$exp_ppg[oos$position=="RB"]), 3),
  wr_cor           = round(cor_f(oos$exp_ppg[oos$position=="WR"],
                                  oos$ppg[oos$position=="WR"]), 3),
  rb_cor           = round(cor_f(oos$exp_ppg[oos$position=="RB"],
                                  oos$ppg[oos$position=="RB"]), 3),
  wr_bias          = round(bias(oos$ppg[oos$position=="WR"],
                                  oos$exp_ppg[oos$position=="WR"]), 3),
  rb_bias          = round(bias(oos$ppg[oos$position=="RB"],
                                  oos$exp_ppg[oos$position=="RB"]), 3),
  bust_accuracy    = round(mean((oos$p_made_it >= 0.5) == (oos$made_it == 1)), 3),
  wr_bust_accuracy = round(mean((oos$p_made_it[oos$position=="WR"] >= 0.5) ==
                                  (oos$made_it[oos$position=="WR"] == 1)), 3),
  rb_bust_accuracy = round(mean((oos$p_made_it[oos$position=="RB"] >= 0.5) ==
                                  (oos$made_it[oos$position=="RB"] == 1)), 3)
)

# ── QB / TE metrics (separate hurdle pipeline; CV via 11b_temporal_cv_qb_te.R) ─
qb_te_metrics <- tryCatch({
  m <- read_csv("output/temporal_cv_qb_te/metrics_summary.csv",
                show_col_types = FALSE)
  qb_row <- m |> dplyr::filter(position == "QB")
  te_row <- m |> dplyr::filter(position == "TE")
  list(
    qb_mae = if (nrow(qb_row)) round(qb_row$mae, 3) else NA_real_,
    qb_cor = if (nrow(qb_row)) round(qb_row$cor, 3) else NA_real_,
    qb_bias = if (nrow(qb_row)) round(qb_row$bias, 3) else NA_real_,
    qb_bust_accuracy = if (nrow(qb_row)) round(qb_row$bust_accuracy, 3) else NA_real_,
    qb_n = if (nrow(qb_row)) as.integer(qb_row$n) else 0L,
    te_mae = if (nrow(te_row)) round(te_row$mae, 3) else NA_real_,
    te_cor = if (nrow(te_row)) round(te_row$cor, 3) else NA_real_,
    te_bias = if (nrow(te_row)) round(te_row$bias, 3) else NA_real_,
    te_bust_accuracy = if (nrow(te_row)) round(te_row$bust_accuracy, 3) else NA_real_,
    te_n = if (nrow(te_row)) as.integer(te_row$n) else 0L
  )
}, error = function(e) list())
overall <- c(overall, qb_te_metrics)

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
    wr_mae = round(mae(ppg[position == "WR"], exp_ppg[position == "WR"]), 3),
    rb_mae = round(mae(ppg[position == "RB"], exp_ppg[position == "RB"]), 3),
    .groups = "drop"
  ) |>
  transmute(year = as.integer(test_year), n = as.integer(n),
            mae, cor, wr_mae, rb_mae)

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

# ── Notable residuals (top 20 by absolute residual) ─────────────────────────
notable <- oos |>
  mutate(abs_res = abs(residual)) |>
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
  calibration = calibration,
  scatter    = scatter,
  notable    = notable
)

write_json(out, "website/public/data/model_performance.json",
           pretty = TRUE, auto_unbox = TRUE, digits = NA)
message("Wrote: website/public/data/model_performance.json")
message(sprintf("Overall: MAE=%.3f  Cor=%.3f  N=%d", overall$mae, overall$cor, overall$n))
message(sprintf("  WR: MAE=%.3f  Cor=%.3f", overall$wr_mae, overall$wr_cor))
message(sprintf("  RB: MAE=%.3f  Cor=%.3f", overall$rb_mae, overall$rb_cor))
message(sprintf("Bust accuracy: %.1f%% (WR %.1f%%  RB %.1f%%)",
                100*overall$bust_accuracy, 100*overall$wr_bust_accuracy,
                100*overall$rb_bust_accuracy))

# 10e_blend_experiment.R
# Test ensemble blend: final_exp_ppg = (1-w) * model_exp_ppg + w * comp_weighted_ppg
# Tune blend weight w on 2021–2023 validation classes (fully realized targets).
# 2024–2025 partial actuals excluded from tuning (different target scale).
#
# Outputs: prints diagnostics, saves blend_weight to output/blend_weight.rds

library(tidyverse)
source("functions/helpers.R")

scores  <- read_csv("output/all_class_scores.csv", show_col_types = FALSE)
summary <- read_csv("output/player_comp_summary.csv", show_col_types = FALSE)

merged <- scores |>
  left_join(
    summary |> select(name, position, draft_year, comp_weighted_ppg, comp_bust_rate),
    by = c("name", "position", "draft_year")
  )

# Tune only on fully realized classes (3 complete seasons, shrinkage-adjusted target)
val <- merged |>
  filter(draft_year %in% 2021:2023, !is.na(actual_ppg), !is.na(comp_weighted_ppg))

cat(sprintf("Tuning on %d players across 2021–2023\n\n", nrow(val)))

# ── Sweep blend weights ───────────────────────────────────────────────────────
weights <- seq(0, 0.5, by = 0.025)

results <- map_dfr(weights, function(w) {
  blended <- val |>
    mutate(pred = (1 - w) * exp_ppg + w * comp_weighted_ppg)

  overall <- blended |>
    summarize(
      mae  = mean(abs(actual_ppg - pred)),
      rmse = sqrt(mean((actual_ppg - pred)^2)),
      bias = mean(pred - actual_ppg)
    )

  by_round <- blended |>
    mutate(round_grp = case_when(round == 1 ~ "Rd1", round == 2 ~ "Rd2", TRUE ~ "Rd3+")) |>
    group_by(round_grp) |>
    summarize(bias = mean(pred - actual_ppg), .groups = "drop") |>
    pivot_wider(names_from = round_grp, values_from = bias, names_prefix = "bias_")

  bind_cols(tibble(w = w), overall, by_round)
})

cat("── Blend weight sweep ──────────────────────────────────────────────────────\n")
results |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  print(n = nrow(results))

# ── Best weight by MAE ────────────────────────────────────────────────────────
best <- results |> slice_min(mae, n = 1)
cat(sprintf("\nBest w by MAE: %.3f  (MAE=%.3f, RMSE=%.3f, bias=%.3f)\n",
            best$w, best$mae, best$rmse, best$bias))

# ── Before / After at best weight ─────────────────────────────────────────────
w_opt <- best$w

compare <- function(df, w, label) {
  df |>
    mutate(
      pred_base    = exp_ppg,
      pred_blended = (1 - w) * exp_ppg + w * comp_weighted_ppg,
      round_grp    = case_when(round == 1 ~ "Rd1", round == 2 ~ "Rd2", TRUE ~ "Rd3+")
    ) |>
    group_by(position, round_grp) |>
    summarize(
      n            = n(),
      mae_base     = round(mean(abs(actual_ppg - pred_base)),    2),
      mae_blend    = round(mean(abs(actual_ppg - pred_blended)), 2),
      bias_base    = round(mean(pred_base    - actual_ppg),      2),
      bias_blend   = round(mean(pred_blended - actual_ppg),      2),
      .groups = "drop"
    ) |>
    arrange(position, round_grp)
}

cat("\n── Before / After by position × round (2021–2023) ─────────────────────────\n")
compare(val, w_opt, "val") |> print(n = 30)

# Also show position-level overall
cat("\n── Overall by position ─────────────────────────────────────────────────────\n")
val |>
  mutate(
    pred_base    = exp_ppg,
    pred_blended = (1 - w_opt) * exp_ppg + w_opt * comp_weighted_ppg
  ) |>
  group_by(position) |>
  summarize(
    n            = n(),
    mae_base     = round(mean(abs(actual_ppg - pred_base)),    2),
    mae_blend    = round(mean(abs(actual_ppg - pred_blended)), 2),
    rmse_base    = round(sqrt(mean((actual_ppg - pred_base)^2)),    2),
    rmse_blend   = round(sqrt(mean((actual_ppg - pred_blended)^2)), 2),
    bias_base    = round(mean(pred_base    - actual_ppg),      2),
    bias_blend   = round(mean(pred_blended - actual_ppg),      2),
    .groups      = "drop"
  ) |>
  print()

# ── Save optimal weight ───────────────────────────────────────────────────────
saveRDS(list(w = w_opt, tuned_on = "2021-2023"), "output/blend_weight.rds")
cat(sprintf("\nSaved blend weight w=%.3f to output/blend_weight.rds\n", w_opt))

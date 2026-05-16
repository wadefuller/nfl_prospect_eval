# 21_blend_sweep.R
# ─────────────────────────────────────────────────────────────────────────────
# Sweep the hurdle/bucket blend weight per position. We have two OOS sets:
#   output/temporal_cv/oos_predictions.csv     — continuous hurdle exp_ppg
#   output/bucket_cv/oos_predictions.csv       — bucket exp_ppg_bucket
#
# Both predict the same OOS player-years (2016-2023). For each candidate
# weight w ∈ [0, 1], blended_pred = w × bucket + (1-w) × hurdle. Find the
# w that minimises MAE per position.
#
# Output: output/blend_sweep/results.csv + optimal weights printed.
# ─────────────────────────────────────────────────────────────────────────────

setwd("~/Projects/R/college_nfl_model")
suppressMessages(library(tidyverse))

hurdle <- read_csv("output/temporal_cv/oos_predictions.csv", show_col_types = FALSE)
bucket <- read_csv("output/bucket_cv/oos_predictions.csv",   show_col_types = FALSE)

# Match on (name, position, draft_year). Both CVs use the same fold structure
# so the rows should align modulo column naming.
merged <- hurdle |>
  select(name = pfr_player_name, position, draft_year, ppg,
         exp_ppg_hurdle = exp_ppg) |>
  inner_join(
    bucket |> select(name = pfr_player_name, position, draft_year,
                      exp_ppg_bucket),
    by = c("name", "position", "draft_year")
  )

cat(sprintf("Matched %d / %d hurdle rows to bucket rows\n",
            nrow(merged), nrow(hurdle)))

# ── Sweep ────────────────────────────────────────────────────────────────────

weights <- seq(0, 1, by = 0.05)
mae <- function(a, p) mean(abs(a - p))

sweep_pos <- function(df, pos_filter) {
  if (pos_filter == "ALL") {
    d <- df
  } else {
    d <- df |> filter(position == pos_filter)
  }
  tibble(weight = weights) |>
    mutate(
      mae = vapply(weight, function(w)
        mae(d$ppg, w * d$exp_ppg_bucket + (1 - w) * d$exp_ppg_hurdle),
        numeric(1))
    ) |>
    mutate(position = pos_filter)
}

results <- bind_rows(
  sweep_pos(merged, "ALL"),
  sweep_pos(merged, "WR"),
  sweep_pos(merged, "RB")
)

cat("\n══ MAE vs blend weight (0 = pure hurdle, 1 = pure bucket) ══\n")
results |>
  pivot_wider(names_from = position, values_from = mae) |>
  mutate(across(c(ALL, WR, RB), ~ round(.x, 3))) |>
  print(n = Inf)

# Find best weight per position
best <- results |>
  group_by(position) |>
  slice_min(mae, n = 1, with_ties = FALSE) |>
  ungroup()

cat("\n══ Optimal weights per position ══\n")
print(best)

# Also report the "0.5 fixed" comparison
fixed <- results |> filter(weight == 0.5)
cat("\nFixed equal-weight (0.5) result:\n")
print(fixed)

dir.create("output/blend_sweep", showWarnings = FALSE, recursive = TRUE)
write_csv(results, "output/blend_sweep/results.csv")

# Summary recommendations
cat("\n── Recommended blend weights for export_website_data.R ──\n")
wr_w <- (best |> filter(position == "WR"))$weight
rb_w <- (best |> filter(position == "RB"))$weight
cat(sprintf("WR: %.2f × bucket + %.2f × hurdle  (MAE %.3f)\n",
            wr_w, 1 - wr_w, (best |> filter(position == "WR"))$mae))
cat(sprintf("RB: %.2f × bucket + %.2f × hurdle  (MAE %.3f)\n",
            rb_w, 1 - rb_w, (best |> filter(position == "RB"))$mae))

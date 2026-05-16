# 10_model_diagnostics.R
# Visual diagnostics comparing model predictions vs actual outcomes
# Draft classes 2021-2023 (complete actuals)

library(tidyverse)
library(ggplot2)

setwd("~/Projects/R/college_nfl_model")

# ── Theme ─────────────────────────────────────────────────────────────────────

theme_nfl <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background  = element_rect(fill = "#1a1a2e", color = NA),
      panel.background = element_rect(fill = "#16213e", color = NA),
      panel.grid.major = element_line(color = "#2a2a4a", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      text             = element_text(color = "#c8ccd4"),
      plot.title       = element_text(color = "#ffffff", size = 15, face = "bold", margin = margin(b = 4)),
      plot.subtitle    = element_text(color = "#8b9ab5", size = 11, margin = margin(b = 12)),
      plot.caption     = element_text(color = "#5a6a85", size = 9),
      axis.text        = element_text(color = "#8b9ab5"),
      axis.title       = element_text(color = "#c8ccd4"),
      strip.text       = element_text(color = "#ffffff", face = "bold"),
      strip.background = element_rect(fill = "#0f3460", color = NA),
      legend.background = element_rect(fill = "#1a1a2e", color = NA),
      legend.text      = element_text(color = "#c8ccd4"),
      legend.title     = element_text(color = "#ffffff"),
      plot.margin      = margin(16, 16, 12, 16)
    )
}

WIN_COLOR  <- "#22c55e"
LOSS_COLOR <- "#ef4444"
GOLD_COLOR <- "#f59e0b"
BLUE_COLOR <- "#3b82f6"
MUTED      <- "#8b9ab5"

dir.create("output/diagnostics", showWarnings = FALSE)

# ── Load data ─────────────────────────────────────────────────────────────────

scores <- read_csv("output/all_class_scores.csv", show_col_types = FALSE)

# Only classes with full actuals
actuals <- scores |>
  filter(!is.na(actual_ppg)) |>
  mutate(
    cond_ppg    = exp_ppg / p_made_it,          # conditional PPG if hit
    residual    = actual_ppg - exp_ppg,
    hit         = actual_made_it == 1,
    round_grp   = case_when(
      round == 1 ~ "Round 1",
      round == 2 ~ "Round 2",
      round >= 3 ~ "Round 3+"
    ) |> factor(levels = c("Round 1", "Round 2", "Round 3+")),
    pos_label   = paste0(position, " (n=", n(), ")")
  )

cat("Validation set:", nrow(actuals), "players |",
    sum(actuals$hit), "hits |", sum(!actuals$hit), "busts\n")

# ═══════════════════════════════════════════════════════════════════════════════
# Plot 1: Predicted vs Actual scatter (bust-adjusted exp_ppg vs actual_ppg)
# ═══════════════════════════════════════════════════════════════════════════════

p1 <- actuals |>
  ggplot(aes(x = exp_ppg, y = actual_ppg)) +
  geom_abline(slope = 1, intercept = 0, color = MUTED, linetype = "dashed", linewidth = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = GOLD_COLOR, fill = GOLD_COLOR,
              alpha = 0.15, linewidth = 1) +
  geom_point(aes(color = hit, size = round == 1), alpha = 0.75) +
  ggrepel::geom_text_repel(
    data = actuals |> filter(abs(residual) > 4 | (hit & actual_ppg > 14)),
    aes(label = str_extract(name, "\\S+$")),
    color = "#c8ccd4", size = 3, max.overlaps = 12,
    segment.color = MUTED, segment.alpha = 0.5
  ) +
  scale_color_manual(values = c("FALSE" = LOSS_COLOR, "TRUE" = WIN_COLOR),
                     labels = c("FALSE" = "Bust", "TRUE" = "Hit"),
                     name = "Outcome") +
  scale_size_manual(values = c("FALSE" = 2.5, "TRUE" = 4),
                    labels = c("FALSE" = "Rd 2+", "TRUE" = "Rd 1"),
                    name = "Draft") +
  facet_wrap(~position) +
  labs(
    title    = "Model: Predicted vs Actual PPG",
    subtitle = "Bust-adjusted Exp PPG on x-axis · dashed line = perfect calibration · 2021–2023 classes",
    x        = "Expected PPG (bust-adjusted)",
    y        = "Actual PPG",
    caption  = "Busts counted as 0 PPG in actuals"
  ) +
  theme_nfl()

ggsave("output/diagnostics/01_pred_vs_actual.png", p1, width = 11, height = 6, dpi = 150)
message("Saved: 01_pred_vs_actual.png")

# ═══════════════════════════════════════════════════════════════════════════════
# Plot 2: Hit-rate calibration — does p_made_it match observed bust rates?
# ═══════════════════════════════════════════════════════════════════════════════

calibration <- actuals |>
  mutate(prob_bucket = cut(p_made_it,
                           breaks = c(0, 0.70, 0.75, 0.80, 0.83, 0.86, 0.89, 0.92, 1.0),
                           include.lowest = TRUE)) |>
  group_by(position, prob_bucket) |>
  summarise(
    n          = n(),
    pred_prob  = mean(p_made_it),
    obs_rate   = mean(hit),
    .groups = "drop"
  ) |>
  filter(!is.na(prob_bucket))

p2 <- calibration |>
  ggplot(aes(x = pred_prob, y = obs_rate)) +
  geom_abline(slope = 1, intercept = 0, color = MUTED, linetype = "dashed", linewidth = 0.6) +
  geom_line(aes(color = position), linewidth = 1.2) +
  geom_point(aes(color = position, size = n), alpha = 0.85) +
  scale_color_manual(values = c("WR" = BLUE_COLOR, "RB" = GOLD_COLOR)) +
  scale_size_continuous(range = c(3, 9), name = "n players") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title    = "Hit-Rate Calibration",
    subtitle = "Predicted hit probability vs observed hit rate by bucket · dashed = perfect calibration",
    x        = "Predicted hit probability",
    y        = "Observed hit rate",
    color    = "Position"
  ) +
  theme_nfl()

ggsave("output/diagnostics/02_calibration.png", p2, width = 9, height = 6, dpi = 150)
message("Saved: 02_calibration.png")

# ═══════════════════════════════════════════════════════════════════════════════
# Plot 3: Distribution — Exp PPG vs Actual PPG (density overlay)
# ═══════════════════════════════════════════════════════════════════════════════

dist_data <- bind_rows(
  actuals |> select(position, value = exp_ppg)    |> mutate(type = "Expected (bust-adj.)"),
  actuals |> select(position, value = actual_ppg) |> mutate(type = "Actual")
)

p3 <- dist_data |>
  ggplot(aes(x = value, fill = type, color = type)) +
  geom_density(alpha = 0.30, linewidth = 0.8) +
  geom_vline(
    data = dist_data |> group_by(position, type) |> summarise(m = mean(value), .groups = "drop"),
    aes(xintercept = m, color = type),
    linetype = "dashed", linewidth = 0.8
  ) +
  scale_fill_manual(values  = c("Expected (bust-adj.)" = BLUE_COLOR,  "Actual" = WIN_COLOR)) +
  scale_color_manual(values = c("Expected (bust-adj.)" = BLUE_COLOR,  "Actual" = WIN_COLOR)) +
  facet_wrap(~position, scales = "free_y") +
  labs(
    title    = "Distribution: Expected vs Actual PPG",
    subtitle = "Dashed lines = group means · busts included as 0 PPG",
    x        = "PPG",
    y        = "Density",
    fill     = NULL, color = NULL
  ) +
  theme_nfl() +
  theme(legend.position = "top")

ggsave("output/diagnostics/03_distribution.png", p3, width = 11, height = 5.5, dpi = 150)
message("Saved: 03_distribution.png")

# ═══════════════════════════════════════════════════════════════════════════════
# Plot 4: Residuals by round and draft year
# ═══════════════════════════════════════════════════════════════════════════════

p4 <- actuals |>
  ggplot(aes(x = round_grp, y = residual, fill = position)) +
  geom_hline(yintercept = 0, color = MUTED, linetype = "dashed", linewidth = 0.6) +
  geom_boxplot(alpha = 0.6, outlier.color = MUTED, outlier.size = 1.5, width = 0.5) +
  geom_jitter(aes(color = position), width = 0.15, alpha = 0.5, size = 1.8) +
  scale_fill_manual(values  = c("WR" = BLUE_COLOR, "RB" = GOLD_COLOR)) +
  scale_color_manual(values = c("WR" = BLUE_COLOR, "RB" = GOLD_COLOR)) +
  facet_wrap(~position) +
  labs(
    title    = "Residuals by Draft Round",
    subtitle = "Actual − Expected PPG · positive = model underestimated · 2021–2023",
    x        = NULL,
    y        = "Residual (actual − expected PPG)",
    fill     = "Position", color = "Position"
  ) +
  theme_nfl() +
  theme(legend.position = "none")

ggsave("output/diagnostics/04_residuals_by_round.png", p4, width = 10, height = 5.5, dpi = 150)
message("Saved: 04_residuals_by_round.png")

# ═══════════════════════════════════════════════════════════════════════════════
# Plot 5: Summary stats table-style — MAE, correlation, hit-rate accuracy
# ═══════════════════════════════════════════════════════════════════════════════

summary_stats <- actuals |>
  group_by(position) |>
  summarise(
    n              = n(),
    mae            = mean(abs(residual)),
    rmse           = sqrt(mean(residual^2)),
    corr           = cor(exp_ppg, actual_ppg),
    bias           = mean(residual),
    pred_hit_rate  = mean(p_made_it),
    obs_hit_rate   = mean(hit),
    .groups = "drop"
  )

cat("\n── Model Summary Stats ──────────────────────────────────────────\n")
summary_stats |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  print()

# ── Save all plots together ───────────────────────────────────────────────────
cat("\nAll diagnostics saved to output/diagnostics/\n")

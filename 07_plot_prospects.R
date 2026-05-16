# 07_plot_prospects.R
# Compare WR and RB prospect expected PPG across the 2024, 2025, and 2026 draft classes.

library(tidyverse)

prospects <- read_csv("output/prospect_scores.csv", show_col_types = FALSE) |>
  mutate(
    draft_year = factor(draft_year, levels = c(2024, 2025, 2026)),
    class_label = case_when(
      draft_year == 2026 ~ "2026 (mock)",
      TRUE               ~ as.character(draft_year)
    ),
    class_label = factor(class_label, levels = c("2024", "2025", "2026 (mock)"))
  )

year_colors <- c("2024" = "#4e79a7", "2025" = "#f28e2b", "2026 (mock)" = "#e15759")

# ── Helper: label top N prospects per class ───────────────────────────────────
top_labels <- function(df, n = 5) {
  df |>
    group_by(class_label) |>
    slice_max(exp_ppg, n = n, with_ties = FALSE) |>
    ungroup()
}

# ── Plot 1: Distribution comparison (violin + jitter) ────────────────────────
# Shows the depth and quality spread of each class

p_dist <- prospects |>
  ggplot(aes(x = class_label, y = exp_ppg, color = class_label, fill = class_label)) +
  geom_violin(alpha = 0.15, linewidth = 0.6, trim = TRUE) +
  geom_jitter(width = 0.12, size = 2.2, alpha = 0.75, show.legend = FALSE) +
  geom_text(
    data = top_labels(prospects, n = 3),
    aes(label = str_extract(name, "^\\S+")),   # first name only to avoid clutter
    nudge_x = 0.35, size = 2.6, fontface = "bold", show.legend = FALSE
  ) +
  facet_wrap(~ position, ncol = 2, scales = "free_y",
             labeller = labeller(position = c(WR = "Wide Receivers", RB = "Running Backs"))) +
  scale_color_manual(values = year_colors) +
  scale_fill_manual(values = year_colors) +
  labs(
    title    = "NFL Prospect Expected Fantasy PPG by Draft Class",
    subtitle = "Half-PPR | Two-stage model: P(bust) × E[avg top-2 PPG of first 3 seasons]",
    x        = NULL,
    y        = "Expected PPG",
    color    = "Draft class",
    caption  = "2026 class uses mock draft pick projections. Scores reflect college production + draft capital."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text       = element_text(face = "bold", size = 13),
    legend.position  = "none",
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "grey40", size = 10),
    plot.caption     = element_text(color = "grey50", size = 8),
    panel.grid.minor = element_blank()
  )

ggsave("output/prospects_distribution.png", p_dist, width = 11, height = 7, dpi = 160)
message("Saved: output/prospects_distribution.png")

# ── Plot 2: Ranked dot plot — top 15 per class per position ──────────────────
# Shows where each 2026 prospect would slot among the recent classes

ranked_df <- prospects |>
  group_by(position) |>
  arrange(desc(exp_ppg)) |>
  mutate(overall_rank = row_number()) |>
  ungroup() |>
  group_by(position, class_label) |>
  slice_max(exp_ppg, n = 15, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    name_short = str_replace(name, "(\\S+)\\s+(\\S+).*", "\\1 \\2"),  # first + last only
    pos_label  = if_else(position == "WR", "Wide Receivers", "Running Backs")
  )

p_ranked <- ranked_df |>
  ggplot(aes(x = exp_ppg, y = reorder(name_short, exp_ppg),
             color = class_label, shape = class_label)) +
  geom_vline(xintercept = seq(4, 10, by = 1), color = "grey90", linewidth = 0.4) +
  geom_point(size = 3.5, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f", exp_ppg)),
            nudge_x = 0.18, size = 2.5, show.legend = FALSE) +
  facet_wrap(~ pos_label, scales = "free_y", ncol = 2) +
  scale_color_manual(values = year_colors, name = "Draft class") +
  scale_shape_manual(values = c("2024" = 16, "2025" = 17, "2026 (mock)" = 15),
                     name = "Draft class") +
  scale_x_continuous(limits = c(NA, 9.5)) +
  labs(
    title    = "Top 15 WR & RB Prospects Per Draft Class: Expected PPG",
    subtitle = "Half-PPR | Where would the 2026 class rank among recent draft classes?",
    x        = "Expected PPG (P(made_it) × E[top-2 PPG | made_it])",
    y        = NULL,
    caption  = "2026 class uses mock draft pick projections."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text       = element_text(face = "bold", size = 13),
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "grey40", size = 10),
    plot.caption     = element_text(color = "grey50", size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(size = 9)
  )

# need more height since combined across classes
ggsave("output/prospects_ranked.png", p_ranked, width = 14, height = 10, dpi = 160)
message("Saved: output/prospects_ranked.png")

# ── Plot 3: Class summary stats ───────────────────────────────────────────────
summary_df <- prospects |>
  group_by(position, class_label) |>
  summarise(
    n          = n(),
    median_ppg = median(exp_ppg),
    top5_avg   = mean(sort(exp_ppg, decreasing = TRUE)[1:min(5, n())]),
    max_ppg    = max(exp_ppg),
    .groups    = "drop"
  )

cat("\n── Class summary ──\n")
summary_df |> arrange(position, class_label) |> print()

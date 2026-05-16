# 08_build_pptx.R
# Generate a PowerPoint presentation with model results for 2021-2026 draft classes

library(tidyverse)
library(officer)
library(flextable)

scores <- read_csv("output/all_class_scores.csv", show_col_types = FALSE)

# ── Theme colors ──────────────────────────────────────────────────────────────
bg_dark   <- "#1a1a2e"
bg_card   <- "#16213e"
gold      <- "#c9a227"
white     <- "#ffffff"
light     <- "#cccccc"
blue_acc  <- "#4a90d9"
red_acc   <- "#e74c3c"

# ── Helper: styled flextable ─────────────────────────────────────────────────
make_table <- function(df) {
  ft <- flextable(df) |>
    fontsize(size = 9, part = "all") |>
    font(fontname = "Calibri", part = "all") |>
    color(color = white, part = "body") |>
    color(color = gold, part = "header") |>
    bg(bg = bg_card, part = "body") |>
    bg(bg = bg_dark, part = "header") |>
    bold(part = "header") |>
    align(align = "center", part = "all") |>
    align(j = 1, align = "left", part = "body") |>
    border_remove() |>
    hline(part = "header", border = fp_border(color = gold, width = 1)) |>
    hline(part = "body", border = fp_border(color = "#333355", width = 0.5)) |>
    padding(padding = 3, part = "all") |>
    autofit()
  ft
}

# ── Build presentation ────────────────────────────────────────────────────────
pptx <- read_pptx()

# Use blank layout throughout and build manually
layout_name <- "Blank"
master_name <- layout_summary(pptx)$master[layout_summary(pptx)$layout == layout_name][1]

add_dark_slide <- function(pptx) {
  pptx <- add_slide(pptx, layout = layout_name, master = master_name)
  pptx <- ph_with(pptx, block_list(
    fpar(ftext("", fp_text(font.size = 1)))
  ), location = ph_location(left = 0, top = 0, width = 10, height = 0.1))
  # Set background
  pptx <- on_slide(pptx, index = length(pptx))
  bs <- structure(list(
    type = "solid",
    color = bg_dark
  ), class = "bg_properties")
  # Can't easily set bg in officer without a template; use a full-slide rectangle instead
  pptx
}

# ══ Slide 1: Title ═══════════════════════════════════════════════════════════

pptx <- add_slide(pptx, layout = layout_name, master = master_name)

# Dark background rectangle
pptx <- ph_with(pptx,
  external_img(src = {
    tmp <- tempfile(fileext = ".png")
    png(tmp, width = 1000, height = 750, bg = bg_dark)
    par(bg = bg_dark, mar = c(0,0,0,0))
    plot.new()
    dev.off()
    tmp
  }),
  location = ph_location(left = 0, top = 0, width = 10, height = 7.5)
)

pptx <- ph_with(pptx,
  fpar(
    ftext("NFL Draft Prospects: 2021-2026", fp_text(color = white, font.size = 36, bold = TRUE, font.family = "Calibri"))
  ),
  location = ph_location(left = 0.5, top = 1.5, width = 9, height = 1)
)

pptx <- ph_with(pptx,
  fpar(
    ftext("WR & RB Class Analysis  |  College -> NFL Production Model", fp_text(color = light, font.size = 16, italic = TRUE, font.family = "Calibri"))
  ),
  location = ph_location(left = 0.5, top = 2.8, width = 9, height = 0.5)
)

pptx <- ph_with(pptx,
  fpar(
    ftext("Single-stage XGBoost model with shrinkage targets and time-based CV", fp_text(color = gold, font.size = 14, font.family = "Calibri"))
  ),
  location = ph_location(left = 0.5, top = 3.8, width = 9, height = 0.5)
)

pptx <- ph_with(pptx,
  fpar(
    ftext("Projections based on college stats, conference tier, draft capital, combine, recruiting, and PPA metrics", fp_text(color = light, font.size = 11, italic = TRUE, font.family = "Calibri"))
  ),
  location = ph_location(left = 0.5, top = 5.5, width = 9, height = 0.5)
)

# ══ Per-class slides: WR + RB top 10 tables ══════════════════════════════════

for (yr in 2021:2026) {
  year_label <- if (yr == 2026) paste0(yr, " (Mock Draft)") else as.character(yr)

  for (pos in c("WR", "RB")) {
    tbl <- scores |>
      filter(draft_year == yr, position == pos) |>
      slice_max(exp_ppg, n = 10) |>
      transmute(
        Name = name,
        College = college,
        Rd = round,
        Pick = pick,
        `P(Made It)` = paste0(round(p_made_it * 100), "%"),
        `Exp PPG` = round(exp_ppg, 2),
        `Actual PPG` = ifelse(is.na(actual_raw_ppg), "—", round(actual_raw_ppg, 1))
      )

    pptx <- add_slide(pptx, layout = layout_name, master = master_name)

    # Background
    pptx <- ph_with(pptx,
      external_img(src = {
        tmp <- tempfile(fileext = ".png")
        png(tmp, width = 1000, height = 750, bg = bg_dark)
        par(bg = bg_dark, mar = c(0,0,0,0)); plot.new(); dev.off(); tmp
      }),
      location = ph_location(left = 0, top = 0, width = 10, height = 7.5)
    )

    pptx <- ph_with(pptx,
      fpar(ftext(paste0(year_label, " ", pos, " Class - Top Prospects"),
                 fp_text(color = gold, font.size = 22, bold = TRUE, font.family = "Calibri"))),
      location = ph_location(left = 0.5, top = 0.3, width = 9, height = 0.6)
    )

    pptx <- ph_with(pptx,
      fpar(ftext("Ranked by model-projected expected fantasy PPG (half-PPR, first 3 NFL seasons)",
                 fp_text(color = light, font.size = 10, italic = TRUE, font.family = "Calibri"))),
      location = ph_location(left = 0.5, top = 0.85, width = 9, height = 0.3)
    )

    ft <- make_table(tbl)
    pptx <- ph_with(pptx, ft, location = ph_location(left = 0.4, top = 1.3, width = 9.2, height = 5))
  }
}

# ══ Cross-class comparison slide: Top WR by year ═════════════════════════════

pptx <- add_slide(pptx, layout = layout_name, master = master_name)
pptx <- ph_with(pptx,
  external_img(src = {
    tmp <- tempfile(fileext = ".png")
    png(tmp, width = 1000, height = 750, bg = bg_dark)
    par(bg = bg_dark, mar = c(0,0,0,0)); plot.new(); dev.off(); tmp
  }),
  location = ph_location(left = 0, top = 0, width = 10, height = 7.5)
)

pptx <- ph_with(pptx,
  fpar(ftext("WR Class Comparison: 2021-2026",
             fp_text(color = gold, font.size = 22, bold = TRUE, font.family = "Calibri"))),
  location = ph_location(left = 0.5, top = 0.3, width = 9, height = 0.6)
)

# Build comparison chart as ggplot, save as image
wr_top5 <- scores |>
  filter(position == "WR") |>
  group_by(draft_year) |>
  slice_max(exp_ppg, n = 5) |>
  mutate(rank = row_number(desc(exp_ppg)),
         label = paste0(name, " (", round(exp_ppg, 1), ")"),
         year_label = ifelse(draft_year == 2026, "2026*", as.character(draft_year))) |>
  ungroup()

p_wr <- ggplot(wr_top5, aes(x = exp_ppg, y = reorder(label, exp_ppg), fill = factor(draft_year))) +
  geom_col(width = 0.7, show.legend = FALSE) +
  facet_wrap(~ year_label, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("2021" = "#e74c3c", "2022" = "#e67e22", "2023" = "#f1c40f",
                                "2024" = "#2ecc71", "2025" = "#3498db", "2026" = "#9b59b6")) +
  labs(x = "Expected PPG", y = NULL) +
  theme_minimal(base_size = 9) +
  theme(
    plot.background = element_rect(fill = bg_dark, color = NA),
    panel.background = element_rect(fill = bg_card, color = NA),
    text = element_text(color = white),
    axis.text = element_text(color = light, size = 7),
    strip.text = element_text(color = gold, face = "bold", size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#333355")
  )

wr_chart <- tempfile(fileext = ".png")
ggsave(wr_chart, p_wr, width = 9, height = 5, dpi = 150, bg = bg_dark)

pptx <- ph_with(pptx, external_img(src = wr_chart),
  location = ph_location(left = 0.5, top = 1.0, width = 9, height = 5.5)
)

# ══ Cross-class comparison slide: Top RB by year ═════════════════════════════

pptx <- add_slide(pptx, layout = layout_name, master = master_name)
pptx <- ph_with(pptx,
  external_img(src = {
    tmp <- tempfile(fileext = ".png")
    png(tmp, width = 1000, height = 750, bg = bg_dark)
    par(bg = bg_dark, mar = c(0,0,0,0)); plot.new(); dev.off(); tmp
  }),
  location = ph_location(left = 0, top = 0, width = 10, height = 7.5)
)

pptx <- ph_with(pptx,
  fpar(ftext("RB Class Comparison: 2021-2026",
             fp_text(color = gold, font.size = 22, bold = TRUE, font.family = "Calibri"))),
  location = ph_location(left = 0.5, top = 0.3, width = 9, height = 0.6)
)

rb_top5 <- scores |>
  filter(position == "RB") |>
  group_by(draft_year) |>
  slice_max(exp_ppg, n = 5) |>
  mutate(rank = row_number(desc(exp_ppg)),
         label = paste0(name, " (", round(exp_ppg, 1), ")"),
         year_label = ifelse(draft_year == 2026, "2026*", as.character(draft_year))) |>
  ungroup()

p_rb <- ggplot(rb_top5, aes(x = exp_ppg, y = reorder(label, exp_ppg), fill = factor(draft_year))) +
  geom_col(width = 0.7, show.legend = FALSE) +
  facet_wrap(~ year_label, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("2021" = "#e74c3c", "2022" = "#e67e22", "2023" = "#f1c40f",
                                "2024" = "#2ecc71", "2025" = "#3498db", "2026" = "#9b59b6")) +
  labs(x = "Expected PPG", y = NULL) +
  theme_minimal(base_size = 9) +
  theme(
    plot.background = element_rect(fill = bg_dark, color = NA),
    panel.background = element_rect(fill = bg_card, color = NA),
    text = element_text(color = white),
    axis.text = element_text(color = light, size = 7),
    strip.text = element_text(color = gold, face = "bold", size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#333355")
  )

rb_chart <- tempfile(fileext = ".png")
ggsave(rb_chart, p_rb, width = 9, height = 5, dpi = 150, bg = bg_dark)

pptx <- ph_with(pptx, external_img(src = rb_chart),
  location = ph_location(left = 0.5, top = 1.0, width = 9, height = 5.5)
)

# ══ Class depth comparison slide ═════════════════════════════════════════════

pptx <- add_slide(pptx, layout = layout_name, master = master_name)
pptx <- ph_with(pptx,
  external_img(src = {
    tmp <- tempfile(fileext = ".png")
    png(tmp, width = 1000, height = 750, bg = bg_dark)
    par(bg = bg_dark, mar = c(0,0,0,0)); plot.new(); dev.off(); tmp
  }),
  location = ph_location(left = 0, top = 0, width = 10, height = 7.5)
)

pptx <- ph_with(pptx,
  fpar(ftext("Class Depth by Position: 2021-2026",
             fp_text(color = gold, font.size = 22, bold = TRUE, font.family = "Calibri"))),
  location = ph_location(left = 0.5, top = 0.3, width = 9, height = 0.6)
)

pptx <- ph_with(pptx,
  fpar(ftext("Players projected above PPG thresholds - measures class-wide talent density",
             fp_text(color = light, font.size = 10, italic = TRUE, font.family = "Calibri"))),
  location = ph_location(left = 0.5, top = 0.85, width = 9, height = 0.3)
)

# Depth table
depth_data <- scores |>
  mutate(year_label = ifelse(draft_year == 2026, "2026*", as.character(draft_year))) |>
  group_by(year_label, position) |>
  summarize(
    `>= 3 PPG` = sum(exp_ppg >= 3),
    `>= 4 PPG` = sum(exp_ppg >= 4),
    `>= 5 PPG` = sum(exp_ppg >= 5),
    `>= 6 PPG` = sum(exp_ppg >= 6),
    `Top Prospect` = name[which.max(exp_ppg)],
    `Best PPG` = round(max(exp_ppg), 2),
    .groups = "drop"
  ) |>
  rename(Year = year_label, Pos = position)

ft_depth <- make_table(depth_data)
pptx <- ph_with(pptx, ft_depth, location = ph_location(left = 0.3, top = 1.3, width = 9.4, height = 5))

# ══ Key Takeaways slide ══════════════════════════════════════════════════════

# Compute some stats for takeaways
best_wr_ever <- scores |> filter(position == "WR") |> slice_max(exp_ppg, n = 1)
best_rb_ever <- scores |> filter(position == "RB") |> slice_max(exp_ppg, n = 1)
deepest_wr <- scores |> filter(position == "WR") |> group_by(draft_year) |>
  summarize(n5 = sum(exp_ppg >= 5)) |> slice_max(n5, n = 1)
deepest_rb <- scores |> filter(position == "RB") |> group_by(draft_year) |>
  summarize(n5 = sum(exp_ppg >= 5)) |> slice_max(n5, n = 1)

pptx <- add_slide(pptx, layout = layout_name, master = master_name)
pptx <- ph_with(pptx,
  external_img(src = {
    tmp <- tempfile(fileext = ".png")
    png(tmp, width = 1000, height = 750, bg = bg_dark)
    par(bg = bg_dark, mar = c(0,0,0,0)); plot.new(); dev.off(); tmp
  }),
  location = ph_location(left = 0, top = 0, width = 10, height = 7.5)
)

pptx <- ph_with(pptx,
  fpar(ftext("Key Takeaways",
             fp_text(color = gold, font.size = 28, bold = TRUE, font.family = "Calibri"))),
  location = ph_location(left = 0.5, top = 0.4, width = 9, height = 0.6)
)

takeaway_text <- paste0(
  "Top-Projected WR (2021-2026): ", best_wr_ever$name, " (", best_wr_ever$draft_year, ") at ",
  round(best_wr_ever$exp_ppg, 2), " exp PPG\n\n",
  "Top-Projected RB (2021-2026): ", best_rb_ever$name, " (", best_rb_ever$draft_year, ") at ",
  round(best_rb_ever$exp_ppg, 2), " exp PPG\n\n",
  "Deepest WR class (>= 5 PPG): ", deepest_wr$draft_year, " with ", deepest_wr$n5, " prospects\n\n",
  "Deepest RB class (>= 5 PPG): ", deepest_rb$draft_year, " with ", deepest_rb$n5, " prospects\n\n",
  "2025 RB class is historically strong: 4 RBs above 6.0 exp PPG (Jeanty, Henderson, Hampton, Judkins)\n\n",
  "2026 WR class (mock): Makai Lemon leads at 5.82 PPG, comparable to mid-tier 2024/2025 WRs\n\n",
  "Model notes: Single-stage regression (busts=0), shrinkage-adjusted targets (k=16), ",
  "games-weighted PPG, 6-game minimum, time-based CV, missingness indicators, teammate volume."
)

pptx <- ph_with(pptx,
  fpar(ftext(takeaway_text,
             fp_text(color = white, font.size = 13, font.family = "Calibri"))),
  location = ph_location(left = 0.7, top = 1.3, width = 8.5, height = 5.5)
)

# ══ Save ═════════════════════════════════════════════════════════════════════

out_path <- "output/NFL_Draft_Prospects_2021_2026.pptx"
print(pptx, target = out_path)
message("Saved: ", out_path)

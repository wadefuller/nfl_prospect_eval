# 02f_build_draft_value_chart.R
# ─────────────────────────────────────────────────────────────────────────────
# Fits a draft pick → value curve from actual NFL career outcomes.
#
# Method (mirrors Ben Baldwin's opensourcefootball post but uses w_av instead
# of CarAV since nflreadr exposes weighted-career-AV directly):
#
#   1. Pull all draft picks 2002–2018 with non-NA `w_av` (weighted PFR
#      Approximate Value across the player's whole career; gives credit for
#      multi-year peak production rather than total counting stats).
#   2. Compute the mean w_av per pick number (1–262).
#   3. Fit a smoothed curve so we get a continuous value function. Use a
#      weighted exponential decay in log-space so the fit is monotone-decreasing
#      and well-behaved at the tail.
#         log(mean_av + 1) = β₀ + β₁·pick + β₂·pick²   (weights = n per pick)
#   4. Save (a) the closed-form coefficients and (b) a 1..262 lookup table.
#
# Output: data/draft_value_chart.rds — list(coefs, lookup, fit_summary)
#
# Usage at score time (helpers.R::pick_value):
#   v <- pick_value(c(5, 32, 100, 200))
# ─────────────────────────────────────────────────────────────────────────────

suppressMessages({
  library(tidyverse)
  library(nflreadr)
})

dir.create("output", showWarnings = FALSE)

# ── 1. Pull career-AV data ───────────────────────────────────────────────────

DRAFT_YEARS <- 2002:2018   # mature careers (≥7 yrs since draft as of 2026)

picks <- load_draft_picks() |>
  filter(season %in% DRAFT_YEARS, !is.na(pick), !is.na(w_av)) |>
  select(season, pick, w_av, position)

cat(sprintf("Picks with w_av: %d (%d–%d)\n",
            nrow(picks), min(picks$season), max(picks$season)))

# ── 2. Aggregate per pick number ─────────────────────────────────────────────

agg <- picks |>
  group_by(pick) |>
  summarise(
    n        = n(),
    mean_av  = mean(w_av),
    .groups  = "drop"
  ) |>
  arrange(pick)

cat("\nMean w_av by pick range:\n")
agg |>
  mutate(bucket = cut(pick, c(0, 10, 32, 64, 100, 150, 200, 262))) |>
  group_by(bucket) |>
  summarise(picks  = sum(n),
            mean_av = round(weighted.mean(mean_av, w = n), 2),
            .groups = "drop") |>
  print()

# ── 3. Fit smoothed curve ────────────────────────────────────────────────────

# Quadratic-in-pick fit on log-AV. Weight by n so frequently-observed picks
# (every season has pick 5; only some have pick 250) get more influence.
fit <- lm(log(mean_av + 1) ~ pick + I(pick^2), data = agg, weights = n)

cat(sprintf("\nFit: log(av+1) = %.4f + %.4e·pick + %.4e·pick²   R² = %.3f\n",
            coef(fit)[1], coef(fit)[2], coef(fit)[3],
            summary(fit)$r.squared))

# ── 4. Build 1..262 lookup ───────────────────────────────────────────────────

pick_grid <- tibble(pick = 1:262) |>
  mutate(
    log_pred = predict(fit, newdata = tibble(pick = pick)),
    value    = pmax(exp(log_pred) - 1, 0)
  )

# Normalise so pick #1 = 100 (reads more naturally than raw AV units)
scale_factor <- 100 / pick_grid$value[1]
pick_grid <- pick_grid |> mutate(value = value * scale_factor)

cat("\nResulting value chart (selected picks):\n")
print(pick_grid |> filter(pick %in% c(1, 10, 32, 50, 100, 150, 200, 262)))

# ── 5. Save ──────────────────────────────────────────────────────────────────

out <- list(
  coefs        = coef(fit),
  scale_factor = scale_factor,
  lookup       = pick_grid,
  fit_r2       = summary(fit)$r.squared,
  source       = "PFR weighted career AV (w_av), draft years 2002–2018"
)
saveRDS(out, "data/draft_value_chart.rds")

# Also a CSV for inspection
readr::write_csv(pick_grid, "data/draft_value_chart.csv")

# ── 6. Plot ──────────────────────────────────────────────────────────────────

p <- ggplot() +
  geom_point(data = agg, aes(pick, log(mean_av + 1) * scale_factor + log(scale_factor)),
             colour = "#4A5578", alpha = 0.4, size = 1) +
  geom_line(data = pick_grid, aes(pick, value), colour = "#3E8EF7", linewidth = 1) +
  scale_x_continuous(breaks = c(1, 32, 64, 100, 150, 200, 262)) +
  labs(title    = "NFL draft pick value chart",
       subtitle = sprintf("Fit to weighted career AV, draft years %d–%d (R² = %.3f)",
                          min(DRAFT_YEARS), max(DRAFT_YEARS), summary(fit)$r.squared),
       x = "Pick number",
       y = "Value (pick #1 = 100)") +
  theme_minimal()

ggsave("output/draft_value_chart.png", p, width = 8, height = 5, dpi = 120)

message("\nSaved: data/draft_value_chart.rds, data/draft_value_chart.csv, output/draft_value_chart.png")

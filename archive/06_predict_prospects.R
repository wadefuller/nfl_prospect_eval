# 06_predict_prospects.R
# ─────────────────────────────────────────────────────────────────────────────
# Quick scoring of recent draft classes (2024-2026).
# For all-class scoring + actual PPG validation, use 07_score_all_classes.R.
#
# Output: output/prospect_scores.rds / .csv
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(tidymodels)
library(xgboost)
library(cfbfastR)

source("functions/helpers.R")

readRenviron("~/.Renviron")
if (Sys.getenv("CFBD_API_KEY") == "") stop("CFBD_API_KEY not set.")

# ── 1. Load models & scaling params ──────────────────────────────────────────

wr_bust <- readRDS("models/wr_bust_model.rds")
rb_bust <- readRDS("models/rb_bust_model.rds")
wr_prod <- readRDS("models/wr_production_model.rds")
rb_prod <- readRDS("models/rb_production_model.rds")

wr_train <- readRDS("data/wr_model_data.rds")
rb_train <- readRDS("data/rb_model_data.rds")
dy_mean  <- mean(c(wr_train$draft_year, rb_train$draft_year), na.rm = TRUE)
dy_sd    <- sd(c(wr_train$draft_year,   rb_train$draft_year), na.rm = TRUE)
message(sprintf("draft_year_sc params: mean=%.1f  sd=%.2f", dy_mean, dy_sd))

# ── 2. Score each draft class ─────────────────────────────────────────────────

landing_lkp <- if (file.exists("data/landing_spot_features.rds")) {
  message("Loading landing spot features...")
  readRDS("data/landing_spot_features.rds")
} else {
  message("landing_spot_features.rds not found — run 02d first. Landing features will be NA.")
  NULL
}

score_year <- function(draft_year, draft_picks_fn) {
  message("Scoring ", draft_year, " draft class...")
  cfb <- fetch_cfb_stats(draft_year)

  draft <- draft_picks_fn(draft_year) |>
    add_teammate_context()

  wr <- score_class(filter(draft, position == "WR"), "WR",
                    wr_bust, wr_prod, cfb$recv, cfb$rush,
                    dy_mean = dy_mean, dy_sd = dy_sd,
                    landing_lkp = landing_lkp)
  rb <- score_class(filter(draft, position == "RB"), "RB",
                    rb_bust, rb_prod, cfb$recv, cfb$rush, cfb$rb_recv,
                    dy_mean = dy_mean, dy_sd = dy_sd,
                    landing_lkp = landing_lkp)
  bind_rows(wr, rb)
}

# 2024 & 2025 — actual draft picks from nflreadr
load_nfl_class <- function(yr) {
  nflreadr::load_draft_picks() |>
    filter(season == yr, position %in% c("WR", "RB")) |>
    transmute(
      name       = pfr_player_name,
      name_clean = clean_name(pfr_player_name),
      position, college,
      draft_year = season,
      round, pick, age,
      draft_team = team
    )
}

scores_2024 <- score_year(2024, load_nfl_class)
scores_2025 <- score_year(2025, load_nfl_class)

# 2026 — actual draft results; try to supplement draft_team from nflreadr
draft_2026_teams <- tryCatch(
  nflreadr::load_draft_picks() |>
    filter(season == 2026, position %in% c("WR", "RB")) |>
    transmute(name_clean = clean_name(pfr_player_name), draft_team = team),
  error = function(e) tibble(name_clean = character(), draft_team = character())
)
load_2026_class <- function(yr) {
  read_csv("data/draft_2026.csv", show_col_types = FALSE) |>
    mutate(name_clean = clean_name(name)) |>
    left_join(draft_2026_teams, by = "name_clean")
}

scores_2026 <- score_year(2026, load_2026_class)

# ── 3. Combine and display ────────────────────────────────────────────────────

all_prospects <- bind_rows(scores_2024, scores_2025, scores_2026) |>
  arrange(draft_year, position, desc(exp_ppg))

cat("\n══ Top 20 prospects by expected PPG ══\n")
all_prospects |>
  slice_max(exp_ppg, n = 20) |>
  select(name, position, draft_year, round, pick, exp_ppg, p_made_it) |>
  mutate(across(where(is.numeric), ~ round(.x, 2))) |>
  print(n = 20)

for (yr in c(2024, 2025, 2026)) {
  for (pos in c("WR", "RB")) {
    label <- sprintf("%s %d", pos, yr)
    cat(sprintf("\n── %s top 10 ──\n", label))
    all_prospects |>
      filter(draft_year == yr, position == pos) |>
      slice_max(exp_ppg, n = 10) |>
      mutate(across(where(is.numeric), ~ round(.x, 2))) |>
      print(n = 10)
  }
}

# ── 4. Save ───────────────────────────────────────────────────────────────────

saveRDS(all_prospects, "output/prospect_scores.rds")
write_csv(all_prospects, "output/prospect_scores.csv")
message("\nSaved: output/prospect_scores.rds and output/prospect_scores.csv")

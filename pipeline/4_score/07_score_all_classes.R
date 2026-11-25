# 07_score_all_classes.R
# Score all draft classes 2021-2026 with the deployed model stack:
# continuous hurdle model plus optional ordinal bucket sidecar.
# Joins actual NFL outcomes for 2021-2023 validation.
#
# Outputs: output/all_class_scores.rds / .csv

library(tidyverse)
library(tidymodels)
library(xgboost)
library(cfbfastR)
library(nflreadr)

source("functions/helpers.R")
source("functions/ordinal_helpers.R")

readRenviron("~/.Renviron")

# ── 1. Load models & scaling params ──────────────────────────────────────────

wr_bust <- readRDS("models/wr_bust_model.rds")
rb_bust <- readRDS("models/rb_bust_model.rds")
wr_prod <- readRDS("models/wr_production_model.rds")
rb_prod <- readRDS("models/rb_production_model.rds")

# Ordinal-bucket models (XGB multiclass + clm). Optional — will pass NULL
# through if either is missing, in which case score_class() emits NA bucket
# probabilities and the website hides the bucket distribution.
load_bucket_model <- function(path) {
  if (file.exists(path)) readRDS(path) else { message("  [missing] ", path); NULL }
}
wr_xgb_bucket <- load_bucket_model("models/wr_xgb_bucket_model.rds")
rb_xgb_bucket <- load_bucket_model("models/rb_xgb_bucket_model.rds")
wr_clm_bucket <- load_bucket_model("models/wr_clm_bucket_model.rds")
rb_clm_bucket <- load_bucket_model("models/rb_clm_bucket_model.rds")

wr_train <- readRDS("data/wr_model_data.rds")
rb_train <- readRDS("data/rb_model_data.rds")
dy_mean  <- mean(c(wr_train$draft_year, rb_train$draft_year), na.rm = TRUE)
dy_sd    <- sd(c(wr_train$draft_year,   rb_train$draft_year), na.rm = TRUE)

# Training base rates for hurdle-probability shrinkage (per position).
# RB bust classifier is near-useless OOS (Brier skill -0.06) — score_class()
# shrinks RB p_made_it toward this base rate with alpha=0.25. No-op for WR.
base_rate_wr <- mean(wr_train$made_it, na.rm = TRUE)
base_rate_rb <- mean(rb_train$made_it, na.rm = TRUE)
cat(sprintf("Hurdle base rates: WR=%.4f  RB=%.4f\n", base_rate_wr, base_rate_rb))

# Build lookup tables once (combine + recruiting + landing spot)
message("Loading combine and recruiting lookups...")
combine_lkp <- load_combine_lookup()
recruit_lkp <- load_recruit_lookup()
cat(sprintf("  Combine lookup: %d rows | Recruiting lookup: %d rows\n",
            nrow(combine_lkp), nrow(recruit_lkp)))

landing_lkp <- if (file.exists("data/landing_spot_features.rds")) {
  message("Loading landing spot features...")
  readRDS("data/landing_spot_features.rds")
} else {
  message("landing_spot_features.rds not found — run 02d first. Landing features will be NA.")
  NULL
}

# ── 2. Score each draft class ─────────────────────────────────────────────────

all_results <- list()

for (yr in 2021:2025) {
  message("Scoring ", yr, " draft class...")
  cfb <- fetch_cfb_stats(yr)

  draft_picks <- load_draft_picks() |>
    filter(season == yr, position %in% c("WR", "RB")) |>
    transmute(name = pfr_player_name, name_clean = clean_name(pfr_player_name),
              gsis_id,
              position, college, draft_year = season, round, pick, age,
              draft_team = team) |>
    add_teammate_context()

  wr_scores <- score_class(filter(draft_picks, position == "WR"), "WR",
                            wr_bust, wr_prod, cfb$recv, cfb$rush,
                            dy_mean = dy_mean, dy_sd = dy_sd,
                            cfb_extra = cfb, combine_lkp = combine_lkp,
                            recruit_lkp = recruit_lkp,
                            landing_lkp = landing_lkp,
                            hurdle_base_rate = base_rate_wr,
                            bucket_xgb_model = wr_xgb_bucket,
                            bucket_clm_model = wr_clm_bucket)
  rb_scores <- score_class(filter(draft_picks, position == "RB"), "RB",
                            rb_bust, rb_prod, cfb$recv, cfb$rush, cfb$rb_recv,
                            dy_mean = dy_mean, dy_sd = dy_sd,
                            cfb_extra = cfb, combine_lkp = combine_lkp,
                            recruit_lkp = recruit_lkp,
                            landing_lkp = landing_lkp,
                            hurdle_base_rate = base_rate_rb,
                            bucket_xgb_model = rb_xgb_bucket,
                            bucket_clm_model = rb_clm_bucket)
  all_results[[as.character(yr)]] <- bind_rows(wr_scores, rb_scores)
}

# 2026 actual draft — try to supplement draft_team from nflreadr
message("Scoring 2026 draft class...")
cfb_2026 <- fetch_cfb_stats(2026)
draft_2026_teams <- tryCatch(
  load_draft_picks() |>
    filter(season == 2026, position %in% c("WR", "RB")) |>
    transmute(name_clean = clean_name(pfr_player_name), draft_team = team),
  error = function(e) tibble(name_clean = character(), draft_team = character())
)
draft_2026 <- read_csv("data/draft_2026.csv", show_col_types = FALSE) |>
  mutate(name_clean = clean_name(name), gsis_id = NA_character_) |>
  left_join(draft_2026_teams, by = "name_clean") |>
  add_teammate_context()

wr_2026 <- score_class(filter(draft_2026, position == "WR"), "WR",
                        wr_bust, wr_prod, cfb_2026$recv, cfb_2026$rush,
                        dy_mean = dy_mean, dy_sd = dy_sd,
                        cfb_extra = cfb_2026, combine_lkp = combine_lkp,
                        recruit_lkp = recruit_lkp,
                        landing_lkp = landing_lkp,
                        hurdle_base_rate = base_rate_wr,
                        bucket_xgb_model = wr_xgb_bucket,
                        bucket_clm_model = wr_clm_bucket)
rb_2026 <- score_class(filter(draft_2026, position == "RB"), "RB",
                        rb_bust, rb_prod, cfb_2026$recv, cfb_2026$rush, cfb_2026$rb_recv,
                        dy_mean = dy_mean, dy_sd = dy_sd,
                        cfb_extra = cfb_2026, combine_lkp = combine_lkp,
                        recruit_lkp = recruit_lkp,
                        landing_lkp = landing_lkp,
                        hurdle_base_rate = base_rate_rb,
                        bucket_xgb_model = rb_xgb_bucket,
                        bucket_clm_model = rb_clm_bucket)
all_results[["2026"]] <- bind_rows(wr_2026, rb_2026)

# ── 3. Combine and join actual NFL production ─────────────────────────────────

all_scores <- bind_rows(all_results) |> arrange(draft_year, position, desc(exp_ppg))

# Join actual PPG from targets (covers draft classes 2002-2023)
targets <- readRDS("data/targets.rds") |>
  transmute(
    name_clean      = clean_name(pfr_player_name),
    position, draft_year,
    actual_ppg      = ppg,           # shrinkage-adjusted (matches model target)
    actual_raw_ppg  = avg_top2_ppg,  # raw unshrunk PPG (NA for busts)
    actual_made_it  = made_it,
    n_qual_seasons
  )

all_scores <- all_scores |>
  mutate(name_clean = clean_name(name)) |>
  left_join(targets, by = c("name_clean", "position", "draft_year")) |>
  select(-name_clean)

# ── 3b. Partial actuals for recent classes (2024+) not yet in targets.rds ─────
# These classes haven't completed 3 seasons yet, but we can show partial data.
# Use the same qualifying-season logic (6+ games) across whatever seasons exist.
message("Computing partial actuals for 2024+ draft classes...")

recent_draft_years <- 2024:2025

recent_draft <- load_draft_picks() |>
  filter(season %in% recent_draft_years, position %in% c("WR", "RB")) |>
  transmute(
    name_clean = clean_name(pfr_player_name),
    position, draft_year = season, gsis_id
  ) |>
  # supplement missing gsis_ids via roster data
  left_join(
    load_rosters(seasons = 2024:2025) |>
      filter(position %in% c("WR", "RB"), !is.na(gsis_id), !is.na(entry_year)) |>
      mutate(name_clean = clean_name(full_name)) |>
      distinct(name_clean, entry_year, gsis_id) |>
      rename(gsis_id_roster = gsis_id),
    by = c("name_clean", "draft_year" = "entry_year")
  ) |>
  mutate(gsis_id = coalesce(gsis_id, gsis_id_roster)) |>
  select(-gsis_id_roster) |>
  distinct(name_clean, draft_year, .keep_all = TRUE)

# Pull weekly stats for the seasons these players have played
recent_stats <- load_player_stats(seasons = 2024:2025) |>
  filter(position %in% c("WR", "RB"), season_type == "REG") |>
  group_by(player_id, season) |>
  summarize(
    games    = n(),
    half_ppr = sum((fantasy_points + fantasy_points_ppr) / 2, na.rm = TRUE),
    .groups  = "drop"
  ) |>
  mutate(half_ppr_ppg = half_ppr / games)

# Join and compute per-player averages across qualifying seasons (6+ games)
partial_actuals <- recent_draft |>
  filter(!is.na(gsis_id)) |>
  left_join(recent_stats, by = c("gsis_id" = "player_id")) |>
  filter(!is.na(season)) |>
  mutate(season_num = season - draft_year + 1) |>
  filter(season_num %in% 1:3, games >= 6) |>
  group_by(name_clean, position, draft_year) |>
  summarize(
    actual_raw_ppg  = mean(half_ppr_ppg),   # simple avg of qualifying seasons
    actual_ppg      = mean(half_ppr_ppg),   # same for display (no shrinkage)
    actual_made_it  = 1L,
    n_qual_seasons  = n(),
    .groups         = "drop"
  )

# Merge partial actuals into rows that don't already have actuals
all_scores <- all_scores |>
  mutate(name_clean = clean_name(name)) |>
  left_join(
    partial_actuals |> rename(
      actual_raw_ppg_partial = actual_raw_ppg,
      actual_ppg_partial     = actual_ppg,
      actual_made_it_partial = actual_made_it,
      n_qual_seasons_partial = n_qual_seasons
    ),
    by = c("name_clean", "position", "draft_year")
  ) |>
  mutate(
    actual_raw_ppg  = coalesce(actual_raw_ppg,  actual_raw_ppg_partial),
    actual_ppg      = coalesce(actual_ppg,      actual_ppg_partial),
    actual_made_it  = coalesce(actual_made_it,  actual_made_it_partial),
    n_qual_seasons  = coalesce(n_qual_seasons,  n_qual_seasons_partial)
  ) |>
  select(-name_clean, -actual_raw_ppg_partial, -actual_ppg_partial,
         -actual_made_it_partial, -n_qual_seasons_partial)

cat(sprintf("\nPartial actuals joined for 2024+ classes: %d players\n",
            sum(!is.na(all_scores$actual_raw_ppg) & all_scores$draft_year >= 2024)))

cat(sprintf("\nActual PPG joined: %d of %d rows have actual data\n",
            sum(!is.na(all_scores$actual_ppg)), nrow(all_scores)))

# ── 4. Print top 10 per class per position ────────────────────────────────────

for (yr in 2021:2026) {
  for (pos in c("WR", "RB")) {
    cat(sprintf("\n── %s %d top 10 ──\n", pos, yr))
    all_scores |>
      filter(draft_year == yr, position == pos) |>
      slice_max(exp_ppg, n = 10) |>
      select(name, college, round, pick, p_made_it, exp_ppg,
             actual_ppg, actual_raw_ppg) |>
      mutate(across(where(is.numeric), ~ round(.x, 2))) |>
      print(n = 10)
  }
}

# ── 5. Save ───────────────────────────────────────────────────────────────────

write_csv(all_scores, "output/all_class_scores.csv")
saveRDS(all_scores, "output/all_class_scores.rds")
message("\nSaved: output/all_class_scores.csv and output/all_class_scores.rds")

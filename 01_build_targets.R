# 01_build_targets.R
# ─────────────────────────────────────────────────────────────────────────────
# Builds NFL fantasy production targets for WR/RB draft prospects.
#
# Target: shrinkage-adjusted, games-weighted half-PPR PPG from the best 2
#         qualifying seasons (≥6 games) within a player's first 3 NFL seasons.
#
# Players with no qualifying season get ppg = 0 (enabling single-stage regression).
# Also retains the binary "made_it" flag for optional 2-stage use.
#
# Shrinkage: empirical Bayes pulls small-sample PPG toward the position mean,
#   shrunk_ppg = (total_games * raw_ppg + k * prior_mean) / (total_games + k)
#   where k = 16 (≈1 season of prior information).
#
# Training window: drafted 2002–2023 (all 3 seasons complete as of 2025 season)
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(nflreadr)

POSITIONS      <- c("WR", "RB")
DRAFT_MIN      <- 2002
DRAFT_MAX      <- 2023   # 2023 rookies completed 3rd season in 2025
MIN_GAMES      <- 6      # lowered from 8 to capture injury-shortened but productive seasons
MAX_SEASON_NUM <- 3
SHRINKAGE_K    <- 16     # equivalent of ~1 season of prior information

# ── 1. Draft picks ────────────────────────────────────────────────────────────
message("Loading draft picks...")
draft_raw <- load_draft_picks()

draft <- draft_raw |>
  filter(
    position %in% POSITIONS,
    season   %in% DRAFT_MIN:DRAFT_MAX
  ) |>
  transmute(
    pfr_player_name,
    gsis_id,
    position,
    college,
    draft_year = season,
    round,
    pick,
    age
  )

message(sprintf("  %d WR/RB picks in %d–%d", nrow(draft), DRAFT_MIN, DRAFT_MAX))

# ── 2. Supplement missing gsis_ids via roster data ────────────────────────────
message("Loading rosters to supplement missing gsis_ids...")

rosters_bridge <- load_rosters(seasons = DRAFT_MIN:2025) |>
  filter(position %in% POSITIONS, !is.na(gsis_id), !is.na(entry_year)) |>
  mutate(name_clean = str_to_lower(str_squish(full_name))) |>
  distinct(name_clean, entry_year, gsis_id)

draft <- draft |>
  mutate(name_clean = str_to_lower(str_squish(pfr_player_name))) |>
  left_join(
    rosters_bridge |> rename(gsis_id_roster = gsis_id),
    by = c("name_clean" = "name_clean", "draft_year" = "entry_year")
  ) |>
  mutate(gsis_id = coalesce(gsis_id, gsis_id_roster)) |>
  select(-name_clean, -gsis_id_roster) |>
  distinct(pfr_player_name, draft_year, .keep_all = TRUE)

n_missing <- sum(is.na(draft$gsis_id))
message(sprintf("  %d picks still missing gsis_id after roster join (will be coded ppg=0)", n_missing))

# ── 3. NFL weekly stats → season aggregates ───────────────────────────────────
message("Loading and aggregating NFL player stats (weekly → seasonal)...")

weekly_raw <- load_player_stats(seasons = DRAFT_MIN:2025)

stats <- weekly_raw |>
  filter(
    position      %in% POSITIONS,
    season_type   == "REG"
  ) |>
  group_by(player_id, season) |>
  summarize(
    games    = n(),
    half_ppr = sum((fantasy_points + fantasy_points_ppr) / 2,
                   na.rm = TRUE),
    .groups  = "drop"
  )

message(sprintf("  Seasonal rows: %d", nrow(stats)))

# ── 4. Join stats to draft picks and tag season number ──────────────────────
stats_joined <- draft |>
  filter(!is.na(gsis_id)) |>
  left_join(stats, by = c("gsis_id" = "player_id")) |>
  filter(!is.na(season)) |>
  mutate(season_num = season - draft_year + 1) |>
  filter(season_num %in% 1:MAX_SEASON_NUM)

# ── 5. Qualifying seasons (6+ games, lowered from 8) ────────────────────────
qualifying <- stats_joined |>
  filter(games >= MIN_GAMES) |>
  mutate(half_ppr_ppg = half_ppr / games) |>
  select(gsis_id, pfr_player_name, position, draft_year, season_num, games, half_ppr_ppg)

# ── 6. Games-weighted average of best 2 qualifying seasons + shrinkage ──────
production <- qualifying |>
  group_by(gsis_id, pfr_player_name, position, draft_year) |>
  slice_max(order_by = half_ppr_ppg, n = 2, with_ties = FALSE) |>
  summarize(
    raw_ppg        = mean(half_ppr_ppg),               # simple average (for reference)
    weighted_ppg   = weighted.mean(half_ppr_ppg, games), # games-weighted average
    total_top2_gms = sum(games),
    n_qual_seasons = n(),
    .groups        = "drop"
  )

# Empirical Bayes shrinkage: pull toward position mean based on sample size
# Players with many games stay close to their raw PPG; small-sample players
# get pulled toward the prior, reducing noise from 1-qualifying-season players.
production <- production |>
  group_by(position) |>
  mutate(
    prior_mean = mean(weighted_ppg),
    shrunk_ppg = (total_top2_gms * weighted_ppg + SHRINKAGE_K * prior_mean) /
                 (total_top2_gms + SHRINKAGE_K)
  ) |>
  ungroup() |>
  select(-prior_mean)

message("\nShrinkage summary:")
production |>
  group_by(position, n_qual_seasons) |>
  summarize(
    n         = n(),
    mean_raw  = round(mean(raw_ppg), 2),
    mean_wt   = round(mean(weighted_ppg), 2),
    mean_shrk = round(mean(shrunk_ppg), 2),
    .groups   = "drop"
  ) |>
  print()

# ── 7. Merge back to all drafted players; assign made_it + ppg ─────────────
targets <- draft |>
  left_join(
    production |> select(gsis_id, raw_ppg, weighted_ppg, shrunk_ppg,
                         total_top2_gms, n_qual_seasons),
    by = "gsis_id"
  ) |>
  mutate(
    made_it    = as.integer(!is.na(shrunk_ppg)),
    # Single-stage target: busts get 0, producers get shrinkage-adjusted PPG
    ppg        = coalesce(shrunk_ppg, 0),
    # Keep the old metric for reference / backward compat
    avg_top2_ppg = raw_ppg   # NA for busts
  )

# ── 8. Summary ────────────────────────────────────────────────────────────────
cat("\n")
targets |>
  group_by(position) |>
  summarize(
    n           = n(),
    made_it_n   = sum(made_it),
    made_it_pct = round(mean(made_it) * 100, 1),
    avg_ppg     = round(mean(ppg), 2),
    med_ppg     = round(median(ppg), 2),
    avg_ppg_producers = round(mean(ppg[made_it == 1]), 2),
    med_ppg_producers = round(median(ppg[made_it == 1]), 2)
  ) |>
  print()

cat("\nWR ppg quantiles (all players, busts=0):\n")
quantile(targets$ppg[targets$position == "WR"],
         probs = c(0, .1, .25, .5, .75, .9, 1)) |>
  round(2) |> print()

cat("\nRB ppg quantiles (all players, busts=0):\n")
quantile(targets$ppg[targets$position == "RB"],
         probs = c(0, .1, .25, .5, .75, .9, 1)) |>
  round(2) |> print()

cat("\nWR ppg quantiles (made_it == 1 only):\n")
quantile(targets$ppg[targets$position == "WR" & targets$made_it == 1],
         probs = c(0, .1, .25, .5, .75, .9, 1)) |>
  round(2) |> print()

cat("\nRB ppg quantiles (made_it == 1 only):\n")
quantile(targets$ppg[targets$position == "RB" & targets$made_it == 1],
         probs = c(0, .1, .25, .5, .75, .9, 1)) |>
  round(2) |> print()

# ── 9. Save ───────────────────────────────────────────────────────────────────
saveRDS(targets, "data/targets.rds")
message("\nSaved: college_nfl_model/data/targets.rds  (", nrow(targets), " rows)")

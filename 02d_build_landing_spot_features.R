# 02d_build_landing_spot_features.R
# ─────────────────────────────────────────────────────────────────────────────
# Builds NFL landing spot / depth chart opportunity features for draft prospects.
#
# For each player's draft team, characterises the opportunity by comparing
# the prior season's roster to the returning veteran roster:
#   Departing = on team in Y-1 but NOT returning as veteran in Y
#   Returning = on team in Y-1 AND on team as veteran in Y
#
# WR outputs (per draft_team × draft_year):
#   vacated_tgt_pct        — fraction of team's prior targets left by departing WRs
#   incumbent_tgt_share    — top returning WR's prior target share
#   n_ret_wr_50tgt         — count of returning WRs with ≥50 prior targets
#   incumbent_wr1_age      — age of top returning WR (as of Sep 1, draft_year)
#   expected_depth_rank_wr — estimated WR depth chart slot (n_ret_wr_50tgt + 1)
#   team_targets_prior     — team's total prior-season targets (volume ceiling)
#
# RB outputs (per draft_team × draft_year):
#   vacated_carry_pct      — fraction of team's prior carries left by departing RBs
#   incumbent_carry_share  — top returning RB's prior carry share
#   n_ret_rb_100carry      — count of returning RBs with ≥100 prior carries
#   incumbent_rb1_age      — age of top returning RB (as of Sep 1, draft_year)
#   expected_depth_rank_rb — estimated RB depth chart slot (n_ret_rb_100carry + 1)
#   team_carries_prior     — team's total prior-season carries
#
# Also: has_landing_data (1/0) — flag for players with full coverage.
#
# Note on 2026: nflreadr typically does not have mid-year rosters until
# training camp. 2026 draft picks will have NA landing features (XGBoost
# handles NA natively); has_landing_data = 0.
#
# Output: data/landing_spot_features.rds — one row per (draft_team, draft_year)
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(nflreadr)

setwd("~/Projects/R/college_nfl_model")

DRAFT_MIN  <- 2002
DRAFT_MAX  <- 2026
# We need prior-season stats, so stat seasons start one year earlier
STAT_MIN   <- DRAFT_MIN - 1   # 2001
STAT_MAX   <- 2025            # latest complete NFL season

CACHE_FILE <- "data/landing_spot_features.rds"

if (file.exists(CACHE_FILE)) {
  message("Loading cached landing spot features...")
  landing_features <- readRDS(CACHE_FILE)
  message(sprintf("  %d team-years loaded", nrow(landing_features)))
} else {

  # ── 1. Season-level player stats ─────────────────────────────────────────────
  # Weekly → season totals; team_season_totals uses weekly recent_team so
  # mid-season trades are attributed to the correct team for each week.

  message("Loading player stats (this may take a minute)...")
  weekly_raw <- load_player_stats(seasons = STAT_MIN:STAT_MAX) |>
    filter(season_type == "REG", !is.na(player_id))

  # Per-player season totals (team-agnostic — joined to roster-based team below)
  player_season_stats <- weekly_raw |>
    group_by(player_id, season) |>
    summarise(
      targets = sum(targets, na.rm = TRUE),
      carries = sum(carries, na.rm = TRUE),
      .groups = "drop"
    )

  # Per-team season totals (uses actual weekly team assignments for numerators)
  team_season_totals <- weekly_raw |>
    group_by(team, season) |>
    summarise(
      team_targets = sum(targets, na.rm = TRUE),
      team_carries = sum(carries, na.rm = TRUE),
      .groups = "drop"
    )

  # ── 2. Roster data ────────────────────────────────────────────────────────────
  message("Loading rosters...")
  roster_raw <- load_rosters(seasons = STAT_MIN:STAT_MAX) |>
    filter(position %in% c("WR", "RB"), !is.na(gsis_id)) |>
    select(season, team, gsis_id, position, entry_year, birth_date) |>
    distinct(gsis_id, season, .keep_all = TRUE)   # one row per player-season

  # ── 3. Roster transitions: did each player return to their team next year? ────
  # For player P on team T in year Y-1: returned_to_team = TRUE if P appears
  # on team T in year Y as a veteran (entry_year < Y).
  message("Building roster transition table...")

  prev_roster <- roster_raw |>
    rename(prev_season = season, prev_team = team, prev_pos = position)

  # Next-year veteran lookup (entry_year < season = they are not rookies)
  next_yr_vet <- roster_raw |>
    filter(!is.na(entry_year), entry_year < season) |>
    transmute(gsis_id, join_season = season, next_team = team)

  transitions <- prev_roster |>
    # Join each player to their status in prev_season + 1
    mutate(join_season = prev_season + 1L) |>
    left_join(next_yr_vet, by = c("gsis_id", "join_season")) |>
    mutate(
      returned_to_team = !is.na(next_team) & (next_team == prev_team)
    ) |>
    select(gsis_id, prev_team, prev_season, prev_pos, birth_date, returned_to_team) |>
    # Attach prior-season stats
    left_join(player_season_stats |> rename(prev_season = season),
              by = c("gsis_id" = "player_id", "prev_season"))

  # ── 4. WR opportunity metrics ─────────────────────────────────────────────────
  message("Computing WR opportunity metrics...")

  wr_trans <- transitions |> filter(prev_pos == "WR")

  # Age of top returning WR (by prior targets) — needs a separate slice_max pass
  wr_top_ret <- wr_trans |>
    filter(returned_to_team, !is.na(targets), !is.na(birth_date)) |>
    group_by(prev_team, prev_season) |>
    slice_max(targets, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(
      draft_year        = prev_season + 1L,
      incumbent_wr1_age = as.numeric(
        as.Date(paste0(draft_year, "-09-01")) - as.Date(birth_date)
      ) / 365.25,
      incumbent_wr1_targets = targets
    ) |>
    select(prev_team, draft_year, incumbent_wr1_age, incumbent_wr1_targets)

  wr_opp <- wr_trans |>
    group_by(draft_team = prev_team, draft_year = prev_season + 1L) |>
    summarise(
      vacated_wr_targets = sum(targets[!returned_to_team], na.rm = TRUE),
      n_ret_wr_50tgt     = sum(returned_to_team & !is.na(targets) & targets >= 50L,
                               na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(
      team_season_totals |>
        mutate(draft_year = season + 1L) |>
        select(draft_team = team, draft_year, team_targets_prior = team_targets),
      by = c("draft_team", "draft_year")
    ) |>
    left_join(wr_top_ret, by = c("draft_team" = "prev_team", "draft_year")) |>
    mutate(
      vacated_tgt_pct        = if_else(coalesce(team_targets_prior, 0L) > 0,
                                       vacated_wr_targets / team_targets_prior, NA_real_),
      incumbent_tgt_share    = if_else(!is.na(incumbent_wr1_targets) &
                                         coalesce(team_targets_prior, 0L) > 0,
                                       incumbent_wr1_targets / team_targets_prior, NA_real_),
      expected_depth_rank_wr = as.numeric(n_ret_wr_50tgt + 1L)
    ) |>
    select(draft_team, draft_year,
           vacated_tgt_pct, incumbent_tgt_share,
           n_ret_wr_50tgt, incumbent_wr1_age,
           expected_depth_rank_wr, team_targets_prior)

  # ── 5. RB opportunity metrics ─────────────────────────────────────────────────
  message("Computing RB opportunity metrics...")

  rb_trans <- transitions |> filter(prev_pos == "RB")

  rb_top_ret <- rb_trans |>
    filter(returned_to_team, !is.na(carries), !is.na(birth_date)) |>
    group_by(prev_team, prev_season) |>
    slice_max(carries, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(
      draft_year        = prev_season + 1L,
      incumbent_rb1_age = as.numeric(
        as.Date(paste0(draft_year, "-09-01")) - as.Date(birth_date)
      ) / 365.25,
      incumbent_rb1_carries = carries
    ) |>
    select(prev_team, draft_year, incumbent_rb1_age, incumbent_rb1_carries)

  rb_opp <- rb_trans |>
    group_by(draft_team = prev_team, draft_year = prev_season + 1L) |>
    summarise(
      vacated_rb_carries = sum(carries[!returned_to_team], na.rm = TRUE),
      n_ret_rb_100carry  = sum(returned_to_team & !is.na(carries) & carries >= 100L,
                               na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(
      team_season_totals |>
        mutate(draft_year = season + 1L) |>
        select(draft_team = team, draft_year, team_carries_prior = team_carries),
      by = c("draft_team", "draft_year")
    ) |>
    left_join(rb_top_ret, by = c("draft_team" = "prev_team", "draft_year")) |>
    mutate(
      vacated_carry_pct      = if_else(coalesce(team_carries_prior, 0L) > 0,
                                       vacated_rb_carries / team_carries_prior, NA_real_),
      incumbent_carry_share  = if_else(!is.na(incumbent_rb1_carries) &
                                         coalesce(team_carries_prior, 0L) > 0,
                                       incumbent_rb1_carries / team_carries_prior, NA_real_),
      expected_depth_rank_rb = as.numeric(n_ret_rb_100carry + 1L)
    ) |>
    select(draft_team, draft_year,
           vacated_carry_pct, incumbent_carry_share,
           n_ret_rb_100carry, incumbent_rb1_age,
           expected_depth_rank_rb, team_carries_prior)

  # ── 6. Combine into one row per (draft_team, draft_year) ─────────────────────
  message("Combining WR and RB opportunity metrics...")

  all_team_years <- bind_rows(
    wr_opp |> select(draft_team, draft_year),
    rb_opp |> select(draft_team, draft_year)
  ) |> distinct()

  landing_features <- all_team_years |>
    left_join(wr_opp, by = c("draft_team", "draft_year")) |>
    left_join(rb_opp, by = c("draft_team", "draft_year")) |>
    mutate(has_landing_data = 1L) |>
    filter(draft_year >= DRAFT_MIN, draft_year <= DRAFT_MAX,
           !is.na(draft_team))

  message(sprintf("  %d team-years computed (draft_years %d–%d)",
                  nrow(landing_features),
                  min(landing_features$draft_year),
                  max(landing_features$draft_year)))

  saveRDS(landing_features, CACHE_FILE)
  message("Saved: ", CACHE_FILE)
}

# ── Summary ───────────────────────────────────────────────────────────────────
cat("\nLanding spot features summary:\n")
cat(sprintf("  Team-years: %d\n", nrow(landing_features)))
cat(sprintf("  Draft years: %d–%d\n",
            min(landing_features$draft_year), max(landing_features$draft_year)))
cat(sprintf("  WR: vacated_tgt_pct median = %.2f  (N non-NA = %d)\n",
            median(landing_features$vacated_tgt_pct, na.rm = TRUE),
            sum(!is.na(landing_features$vacated_tgt_pct))))
cat(sprintf("  WR: expected_depth_rank_wr median = %.1f\n",
            median(landing_features$expected_depth_rank_wr, na.rm = TRUE)))
cat(sprintf("  RB: vacated_carry_pct median = %.2f  (N non-NA = %d)\n",
            median(landing_features$vacated_carry_pct, na.rm = TRUE),
            sum(!is.na(landing_features$vacated_carry_pct))))
cat(sprintf("  RB: expected_depth_rank_rb median = %.1f\n",
            median(landing_features$expected_depth_rank_rb, na.rm = TRUE)))

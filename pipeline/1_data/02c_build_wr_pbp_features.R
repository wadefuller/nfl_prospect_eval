# 02c_build_wr_pbp_features.R
# ─────────────────────────────────────────────────────────────────────────────
# Builds per-player-season PBP aggregates for the WR models.
#
# NOTE: cfbfastR PBP does NOT contain air_yards or yards_after_catch columns,
# so aDOT and YAC/rec (the highest-R² PFF metrics) are not buildable here.
# This script captures the subset that IS available:
#
#   catch_rate_wr           receptions / targets
#   yards_per_target_wr     receiving yards / targets
#   yards_per_rec_wr        receiving yards / receptions
#   explosive_rec_rate      share of receptions gaining >= 20 yds
#   target_share_wr         player_targets / team_targets
#   targets_per_game_wr     player_targets / games played
#   epa_per_target          mean EPA on targets
#   epa_per_play_wr_pbp     total_epa_target / targets (equivalent to above
#                           for receivers, kept for parity with RB pipeline)
#
# Garbage-time filter: abs(score_diff) <= 28. Receivers are identified via
# `position_target == "WR"` when populated (more accurate than roster lookup),
# otherwise falls back to name-matching downstream.
#
# PBP coverage in cfbfastR is reliable from ~2014+.
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(cfbfastR)

readRenviron("~/.Renviron")
options(cfbfastR.message = FALSE)

PBP_SEASONS    <- 2005:2025
CACHE_DIR      <- "data"
OUT_PATH       <- file.path(CACHE_DIR, "cfb_wr_pbp_features.rds")
PER_SEASON_DIR <- file.path(CACHE_DIR, "cfb_wr_pbp_seasonal")

if (!dir.exists(PER_SEASON_DIR)) dir.create(PER_SEASON_DIR, recursive = TRUE)

# ── Per-season aggregator ────────────────────────────────────────────────────

aggregate_season <- function(yr) {
  cache_path <- file.path(PER_SEASON_DIR, paste0(yr, ".rds"))
  if (file.exists(cache_path)) {
    message("  (cache hit) ", yr)
    return(readRDS(cache_path))
  }

  message("  loading PBP for ", yr, " ...")
  pbp <- tryCatch(
    load_cfb_pbp(yr) |> as_tibble(),
    error = function(e) {
      warning("  failed ", yr, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(pbp) || nrow(pbp) == 0) return(NULL)

  # Normalize receiver-name + yard columns. Prefer position_target == "WR" when
  # populated; otherwise keep all targets and let downstream name/position filter
  # do the work.
  pbp <- pbp |>
    mutate(
      receiver = coalesce(receiver_player_name, reception_player),
      recv_y   = coalesce(yds_receiving, yards_gained),
      pos_tgt  = if ("position_target" %in% names(pbp)) position_target else NA_character_
    ) |>
    filter(!is.na(score_diff), abs(score_diff) <= 28)

  target_plays <- pbp |>
    filter(
      coalesce(pass_attempt, 0L) == 1L,
      !is.na(receiver),
      !is.na(pos_team), pos_team != ""
    )

  # ── Re-attribute corrupted receiver names ────────────────────────────────────
  # cfbfastR returned 2025 PBP with `receiver_player_name` set to per-play
  # description strings ("#1 Z.Branch caught at 09") for ~80% of rows. Without
  # re-attribution, real players' targets get fragmented across hundreds of
  # garbage rows and their PBP-derived stats end up drastically undercounted.
  #
  # Strategy: extract (#NN F.Lastname) from garbage rows, look up the matching
  # real player on the same team in this season's PBP cleanly-named rows.
  garbage_pat <- "^#\\d+\\s+[A-Z]\\."
  is_garbage <- grepl(garbage_pat, target_plays$receiver)
  if (any(is_garbage)) {
    n_garbage <- sum(is_garbage)
    # Extract first initial + last name from garbage receiver strings.
    garb <- target_plays |> filter(is_garbage) |>
      mutate(
        garb_initial = str_extract(receiver, "(?<=#\\d{1,3}\\s)[A-Z]"),
        garb_last    = str_extract(receiver,
                                    "(?<=#\\d{1,3}\\s[A-Z]\\.)[A-Za-z'\\-]+")
      ) |>
      filter(!is.na(garb_initial), !is.na(garb_last))

    # Build (team, initial, last) → real-name lookup from the CLEAN rows.
    clean_roster <- target_plays |> filter(!is_garbage) |>
      mutate(
        first_initial = substr(receiver, 1, 1),
        last_name     = str_extract(receiver, "\\S+$")
      ) |>
      group_by(pos_team, first_initial, last_name) |>
      summarise(real_name = first(receiver), n_plays = n(),
                .groups = "drop") |>
      # Disambiguate when two players share initials+last on same team:
      # prefer the one with more clean plays (the real starter, not a backup
      # whose targets may have leaked into someone else's garbage rows).
      group_by(pos_team, first_initial, last_name) |>
      slice_max(n_plays, n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(pos_team, first_initial, last_name, real_name)

    garb_resolved <- garb |>
      left_join(clean_roster,
                by = c("pos_team", "garb_initial" = "first_initial",
                        "garb_last" = "last_name")) |>
      mutate(receiver = coalesce(real_name, receiver)) |>
      select(-garb_initial, -garb_last, -real_name)

    n_resolved <- sum(!is.na(garb_resolved$receiver) &
                       garb_resolved$receiver != target_plays$receiver[is_garbage])
    target_plays <- bind_rows(target_plays |> filter(!is_garbage), garb_resolved)
    message(sprintf("  re-attributed %d/%d garbage rows", n_resolved, n_garbage))
  }

  # Team-level target totals (denominator for target share) — across ALL
  # receivers, not just WRs.
  team_targets <- target_plays |>
    group_by(pos_team, season) |>
    summarise(team_targets = n(), .groups = "drop")

  # Per-player target aggregates. Keep all positions; the WR filter happens
  # downstream when we join by best_cfb_season to players known to be WR.
  target_agg <- target_plays |>
    group_by(player = receiver, pos_team, season) |>
    summarise(
      targets_pbp        = n(),
      receptions_pbp     = sum(coalesce(completion, 0L) == 1L, na.rm = TRUE),
      rec_yards_pbp      = sum(if_else(coalesce(completion, 0L) == 1L, recv_y, 0), na.rm = TRUE),
      explosive_rec      = sum(coalesce(completion, 0L) == 1L & recv_y >= 20, na.rm = TRUE),
      total_epa_target   = sum(EPA,  na.rm = TRUE),
      mean_epa_target    = mean(EPA, na.rm = TRUE),
      tgt_games          = n_distinct(game_id),
      share_targets_as_wr = mean(coalesce(pos_tgt, "") == "WR", na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(team_targets, by = c("pos_team", "season")) |>
    mutate(
      catch_rate_wr        = receptions_pbp / pmax(targets_pbp, 1),
      yards_per_target_wr  = rec_yards_pbp  / pmax(targets_pbp, 1),
      yards_per_rec_wr     = rec_yards_pbp  / pmax(receptions_pbp, 1),
      explosive_rec_rate   = explosive_rec  / pmax(receptions_pbp, 1),
      target_share_wr      = targets_pbp    / pmax(team_targets, 1),
      targets_per_game_wr  = targets_pbp    / pmax(tgt_games, 1),
      epa_per_target       = mean_epa_target,
      epa_per_play_wr_pbp  = total_epa_target / pmax(targets_pbp, 1)
    ) |>
    select(-mean_epa_target, -explosive_rec)

  saveRDS(target_agg, cache_path)
  message("  ✓ ", yr, " (", nrow(target_agg), " player-team rows)")
  target_agg
}

# ── Run per-season ───────────────────────────────────────────────────────────
message("Building per-season WR PBP aggregates for seasons ",
        min(PBP_SEASONS), "–", max(PBP_SEASONS))

pbp_features <- map_dfr(PBP_SEASONS, aggregate_season)

message("\nCombined PBP feature rows: ", nrow(pbp_features))
message("Seasons with data: ",
        paste(sort(unique(pbp_features$season)), collapse = ", "))

# Sanity spot-check: known WRs
spot_check <- pbp_features |>
  filter(
    (player == "Ja'Marr Chase"        & season == 2020) |
    (player == "Justin Jefferson"     & season == 2019) |
    (player == "Jaxon Smith-Njigba"   & season == 2021) |
    (player == "George Pickens"       & season == 2021) |
    (player == "DeVonta Smith"        & season == 2020) |
    (player == "Puka Nacua"           & season == 2022)
  ) |>
  select(player, pos_team, season, targets_pbp, receptions_pbp, catch_rate_wr,
         yards_per_target_wr, yards_per_rec_wr, explosive_rec_rate,
         target_share_wr, targets_per_game_wr, epa_per_target,
         share_targets_as_wr)

message("\nSpot-check (known WRs):")
print(spot_check, n = Inf)

saveRDS(pbp_features, OUT_PATH)
message("\nSaved to ", OUT_PATH)

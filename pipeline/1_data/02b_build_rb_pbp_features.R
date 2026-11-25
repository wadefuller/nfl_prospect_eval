# 02b_build_rb_pbp_features.R
# ─────────────────────────────────────────────────────────────────────────────
# Builds per-player-season aggregates from cfbfastR play-by-play for the RB
# models. These are the "Tier 1" features identified from PFF importance:
#
#   Rush-related:
#     explosive_rate          share of carries gaining >= 10 yds
#     breakaway_rate          share of carries gaining >= 15 yds
#     epa_per_rush            mean EPA on rush attempts
#
#   Receiving-related:
#     target_share            player_targets / team_targets
#     targets_per_game        player_targets / games played
#     catch_rate              receptions / targets
#
#   Combined:
#     epa_per_play_pbp        (total_epa_rush + total_epa_target) / touches
#
# PBP coverage in cfbfastR is reliable from ~2005+. Pre-2005 seasons return
# empty frames → players' final seasons pre-2005 will have NA PBP features
# (handled downstream with a has_pbp missingness flag).
#
# Garbage-time filter: abs(score_diff) <= 28 (standard CFB GT rule).
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(cfbfastR)

readRenviron("~/.Renviron")
options(cfbfastR.message = FALSE)

PBP_SEASONS <- 2005:2025
CACHE_DIR   <- "data"
OUT_PATH    <- file.path(CACHE_DIR, "cfb_rb_pbp_features.rds")
PER_SEASON_DIR <- file.path(CACHE_DIR, "cfb_rb_pbp_seasonal")

if (!dir.exists(PER_SEASON_DIR)) dir.create(PER_SEASON_DIR, recursive = TRUE)

# ── Per-season aggregator ─────────────────────────────────────────────────────
# Loads one season of PBP, aggregates rush + target stats per (player, team),
# and writes to a small per-season RDS so we don't hold all PBP in memory
# simultaneously.

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

  # Normalize name/yard/flag columns — schema drifts across seasons.
  # rusher: prefer rusher_player_name, fall back to rush_player.
  # receiver: prefer receiver_player_name, fall back to reception_player.
  pbp <- pbp |>
    mutate(
      rusher   = coalesce(rusher_player_name, rush_player),
      receiver = coalesce(receiver_player_name, reception_player),
      rush_y   = coalesce(yds_rushed,   yards_gained),
      recv_y   = coalesce(yds_receiving, yards_gained)
    ) |>
    # Garbage-time filter (standard CFB rule).
    filter(!is.na(score_diff), abs(score_diff) <= 28)

  # ── Garbage-name re-attribution ──────────────────────────────────────────────
  # cfbfastR's 2025 PBP has rusher/receiver fields populated with per-play
  # description strings ("#22 J.Love rushed to 38" or "#1 Z.Branch caught at
  # 09") for ~80% of rows. Real players' touches get fragmented across hundreds
  # of garbage rows. Re-attribute by extracting (#NN F.Lastname) and looking
  # up the matching real player on the same team from the cleanly-named rows.
  reattribute <- function(plays, name_col) {
    garbage_pat <- "^#\\d+\\s+[A-Z]\\."
    is_g <- grepl(garbage_pat, plays[[name_col]])
    if (!any(is_g)) return(plays)
    n_g <- sum(is_g)

    # Build (team, initial, last) → real-name lookup from CLEAN rows
    clean_plays <- plays |> filter(!is_g) |>
      mutate(
        first_initial = substr(.data[[name_col]], 1, 1),
        last_name     = stringr::str_extract(.data[[name_col]], "\\S+$")
      ) |>
      group_by(pos_team, first_initial, last_name) |>
      summarise(real_name = first(.data[[name_col]]), n_plays = n(),
                .groups = "drop") |>
      group_by(pos_team, first_initial, last_name) |>
      slice_max(n_plays, n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(pos_team, first_initial, last_name, real_name)

    garb <- plays |> filter(is_g) |>
      mutate(
        garb_initial = stringr::str_extract(.data[[name_col]],
                                             "(?<=#\\d{1,3}\\s)[A-Z]"),
        garb_last    = stringr::str_extract(.data[[name_col]],
                                             "(?<=#\\d{1,3}\\s[A-Z]\\.)[A-Za-z'\\-]+")
      ) |>
      left_join(clean_plays,
                by = c("pos_team", "garb_initial" = "first_initial",
                        "garb_last" = "last_name")) |>
      mutate(!!sym(name_col) := coalesce(real_name, .data[[name_col]])) |>
      select(-garb_initial, -garb_last, -real_name)

    n_resolved <- sum(garb[[name_col]] != plays[[name_col]][is_g] |
                       (!grepl(garbage_pat, garb[[name_col]])), na.rm = TRUE)
    message(sprintf("  re-attributed %d/%d %s garbage rows",
                    n_resolved, n_g, name_col))
    bind_rows(plays |> filter(!is_g), garb)
  }
  pbp <- pbp |>
    reattribute("rusher") |>
    reattribute("receiver")

  # ── Rushing aggregates ─────────────────────────────────────────────────────
  # Exclude sacks (which show up as rush == 1 in some seasons).
  rush_plays <- pbp |>
    filter(
      rush == 1,
      coalesce(play_type, "") != "Sack",
      !is.na(rusher),
      !is.na(pos_team), pos_team != "",
      !is.na(rush_y)
    )

  rush_agg <- rush_plays |>
    group_by(player = rusher, pos_team, season) |>
    summarise(
      carries           = n(),
      rush_yards_pbp    = sum(rush_y, na.rm = TRUE),
      explosive_carries = sum(rush_y >= 10, na.rm = TRUE),
      breakaway_carries = sum(rush_y >= 15, na.rm = TRUE),
      total_epa_rush    = sum(EPA,   na.rm = TRUE),
      mean_epa_rush     = mean(EPA,  na.rm = TRUE),
      rush_games        = n_distinct(game_id),
      .groups = "drop"
    ) |>
    mutate(
      ypc_pbp              = rush_yards_pbp / pmax(carries, 1),
      explosive_rate       = explosive_carries / pmax(carries, 1),
      breakaway_rate       = breakaway_carries / pmax(carries, 1),
      carries_per_game_pbp = carries / pmax(rush_games, 1),
      epa_per_rush         = mean_epa_rush
    ) |>
    select(-mean_epa_rush, -explosive_carries, -breakaway_carries)

  # ── Target / receiving aggregates ──────────────────────────────────────────
  target_plays <- pbp |>
    filter(
      coalesce(pass_attempt, 0L) == 1L,
      !is.na(receiver),
      !is.na(pos_team), pos_team != ""
    )

  target_agg <- target_plays |>
    group_by(player = receiver, pos_team, season) |>
    summarise(
      targets_pbp      = n(),
      receptions_pbp   = sum(coalesce(completion, 0L) == 1L, na.rm = TRUE),
      rec_yards_pbp    = sum(if_else(coalesce(completion, 0L) == 1L, recv_y, 0), na.rm = TRUE),
      total_epa_target = sum(EPA,  na.rm = TRUE),
      mean_epa_target  = mean(EPA, na.rm = TRUE),
      tgt_games        = n_distinct(game_id),
      .groups = "drop"
    ) |>
    mutate(
      catch_rate       = receptions_pbp / pmax(targets_pbp, 1),
      targets_per_game = targets_pbp    / pmax(tgt_games, 1),
      epa_per_target   = mean_epa_target
    ) |>
    select(-mean_epa_target)

  # Team-level target totals (denominator for target share).
  team_targets <- target_plays |>
    group_by(pos_team, season) |>
    summarise(team_targets = n(), .groups = "drop")

  target_agg <- target_agg |>
    left_join(team_targets, by = c("pos_team", "season")) |>
    mutate(target_share = targets_pbp / pmax(team_targets, 1))

  # ── Combine via full_join on (player, pos_team, season) ────────────────────
  agg <- full_join(rush_agg, target_agg, by = c("player", "pos_team", "season")) |>
    mutate(
      touches          = coalesce(carries, 0) + coalesce(targets_pbp, 0),
      total_epa_all    = coalesce(total_epa_rush, 0) + coalesce(total_epa_target, 0),
      epa_per_play_pbp = total_epa_all / pmax(touches, 1)
    )

  saveRDS(agg, cache_path)
  message("  ✓ ", yr, " (", nrow(agg), " player-team rows)")
  agg
}

# ── Run per-season ────────────────────────────────────────────────────────────
message("Building per-season PBP aggregates for seasons ", min(PBP_SEASONS), "–", max(PBP_SEASONS))

pbp_features <- map_dfr(PBP_SEASONS, aggregate_season)

message("\nCombined PBP feature rows: ", nrow(pbp_features))
message("Seasons: ", paste(sort(unique(pbp_features$season)), collapse = ", "))

# Sanity spot-check: a few known RBs
spot_check <- pbp_features |>
  filter(
    (player == "Bijan Robinson"   & season == 2022) |
    (player == "Jahmyr Gibbs"     & season == 2022) |
    (player == "Breece Hall"      & season == 2021) |
    (player == "Travis Etienne"   & season == 2020) |
    (player == "Christian McCaffrey" & season == 2015)
  ) |>
  select(player, pos_team, season, carries, rush_yards_pbp, ypc_pbp,
         explosive_rate, breakaway_rate, epa_per_rush,
         targets_pbp, target_share, targets_per_game, catch_rate,
         epa_per_play_pbp)

message("\nSpot-check (known RBs):")
print(spot_check, n = Inf)

saveRDS(pbp_features, OUT_PATH)
message("\nSaved to ", OUT_PATH)

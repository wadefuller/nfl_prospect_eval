# build_cfb_season_from_pbp.R
# PBP-backed builder: reconstructs every CFBD-API-derived table the pipeline
# needs, from cfbfastR's PBP Parquet (load_cfb_pbp) — no API calls required.
#
# PBP is hosted on GitHub (sportsdataverse/cfbfastR-data) and is not subject to
# CFBD's monthly API quota. PBP coverage starts 2014; years before that still
# require the legacy API-built caches (cfb_rushing_raw.rds, etc.).
#
# Writes to data/ (one file per year per category):
#   cfb_rushing_<year>.rds     — player, team, conference, position,
#                                rushing_car, rushing_yds, rushing_td, rushing_ypc
#   cfb_receiving_<year>.rds   — player, team, conference, position,
#                                receiving_rec, receiving_yds, receiving_td, receiving_ypr
#   cfb_usage_<year>.rds       — name, team, usg_overall, usg_rush, usg_pass,
#                                usg_passing_downs  (proxy: 3rd/4th-and-medium+)
#   cfb_ppa_<year>.rds         — name, team, {avg,total}_PPA_{pass,rush,all}
#                                (uses cfbfastR EPA as the PPA proxy)
#   cfb_team_games_<year>.rds  — team, team_games
#
# These caches are consumed by:
#   • fetch_cfb_stats() in functions/helpers.R (score-time data for prospects)
#   • 02_build_features.R (training-data builder) — as a non-API fallback
#
# Not covered (still needs CFBD API if you want to rebuild):
#   • HS recruiting stars/rankings (no PBP equivalent) — cfb_recruiting_raw.rds
#     is persisted through 2021 and covers all current training examples.

suppressPackageStartupMessages({
  library(cfbfastR)
  library(dplyr)
})

args <- commandArgs(trailingOnly = TRUE)
years <- if (length(args) > 0) as.integer(args) else c(2023, 2024)

for (yr in years) {
  message("Loading PBP for ", yr, "...")
  pbp_all <- tryCatch(as_tibble(load_cfb_pbp(yr)), error = function(e) NULL)
  if (is.null(pbp_all) || nrow(pbp_all) == 0) {
    message("  [skip] no PBP for ", yr)
    next
  }

  # Regular-season only, to match legacy cfbd_stats_season_player() semantics.
  # (That endpoint defaults to season_type = "regular"; our trained models saw
  # regular-season-only counts, so for compatibility the PBP path must too.)
  # Strict equality drops NA season_type — those are usually orphaned plays
  # without proper game metadata, not legitimate regular-season plays.
  pbp <- pbp_all |> filter(!is.na(season_type), season_type == "regular")
  message("  regular-season plays: ", nrow(pbp), " / ", nrow(pbp_all), " total")

  # Team -> conference lookup from plays that reference a pos_team
  team_conf <- pbp |>
    filter(!is.na(pos_team), pos_team != "") |>
    mutate(conf = offense_conference) |>
    filter(!is.na(conf)) |>
    distinct(team = pos_team, conference = conf) |>
    group_by(team) |> slice_head(n = 1) |> ungroup()

  # ── Rushing ────────────────────────────────────────────────────────────────
  rush <- pbp |>
    filter(rush == 1, coalesce(play_type, "") != "Sack",
           !is.na(rusher_player_name),
           !is.na(pos_team), pos_team != "") |>
    mutate(rush_y = coalesce(yds_rushed, yards_gained)) |>
    group_by(player = rusher_player_name, team = pos_team) |>
    summarise(
      rushing_car = n(),
      rushing_yds = sum(rush_y, na.rm = TRUE),
      rushing_td  = sum(coalesce(rush_td, 0L), na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      rushing_ypc = rushing_yds / pmax(rushing_car, 1),
      position    = NA_character_  # unknown from PBP; downstream filters tolerate NA
    ) |>
    left_join(team_conf, by = "team") |>
    select(player, team, conference, position,
           rushing_car, rushing_yds, rushing_td, rushing_ypc)

  out_r <- paste0("data/cfb_rushing_", yr, ".rds")
  saveRDS(rush, out_r)
  message("  ", out_r, " — ", nrow(rush), " rows")

  # ── Receiving ──────────────────────────────────────────────────────────────
  recv <- pbp |>
    filter(coalesce(pass_attempt, 0L) == 1L,
           !is.na(receiver_player_name),
           !is.na(pos_team), pos_team != "",
           coalesce(completion, 0L) == 1L) |>
    mutate(recv_y = coalesce(yds_receiving, yards_gained)) |>
    group_by(player = receiver_player_name, team = pos_team) |>
    summarise(
      receiving_rec = n(),
      receiving_yds = sum(recv_y, na.rm = TRUE),
      receiving_td  = sum(coalesce(pass_td, 0L), na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      receiving_ypr = receiving_yds / pmax(receiving_rec, 1),
      position      = NA_character_
    ) |>
    left_join(team_conf, by = "team") |>
    select(player, team, conference, position,
           receiving_rec, receiving_yds, receiving_td, receiving_ypr)

  out_w <- paste0("data/cfb_receiving_", yr, ".rds")
  saveRDS(recv, out_w)
  message("  ", out_w, " — ", nrow(recv), " rows")

  # ── Usage ──────────────────────────────────────────────────────────────────
  # Approximates cfbd_player_usage(): player share of team plays/rushes/passes.
  # CFBD's "passing_downs" definition (pass-likely situations by down+distance)
  # is non-trivial; we proxy with 3rd/4th-and-medium+ as a pass-likely filter.
  team_plays <- pbp |>
    filter(!is.na(pos_team), pos_team != "",
           coalesce(rush, 0L) == 1L | coalesce(pass_attempt, 0L) == 1L) |>
    group_by(team = pos_team) |>
    summarise(
      team_total   = n(),
      team_rushes  = sum(coalesce(rush, 0L) == 1L, na.rm = TRUE),
      team_passes  = sum(coalesce(pass_attempt, 0L) == 1L, na.rm = TRUE),
      team_pdowns  = sum(coalesce(pass_attempt, 0L) == 1L &
                         coalesce(down, 0L) >= 3L &
                         coalesce(distance, 0L) >= 4L, na.rm = TRUE),
      .groups = "drop"
    )

  # Player involvement per (player, team)
  player_rush_plays <- pbp |>
    filter(coalesce(rush, 0L) == 1L, coalesce(play_type, "") != "Sack",
           !is.na(rusher_player_name),
           !is.na(pos_team), pos_team != "") |>
    transmute(player = rusher_player_name, team = pos_team, kind = "rush",
              is_pdown = FALSE)
  player_target_plays <- pbp |>
    filter(coalesce(pass_attempt, 0L) == 1L,
           !is.na(receiver_player_name),
           !is.na(pos_team), pos_team != "") |>
    transmute(player = receiver_player_name, team = pos_team, kind = "target",
              is_pdown = coalesce(down, 0L) >= 3L & coalesce(distance, 0L) >= 4L)

  player_usage <- bind_rows(player_rush_plays, player_target_plays) |>
    group_by(player, team) |>
    summarise(
      n_rush   = sum(kind == "rush"),
      n_target = sum(kind == "target"),
      n_pdown  = sum(kind == "target" & is_pdown, na.rm = TRUE),
      .groups  = "drop"
    ) |>
    left_join(team_plays, by = "team") |>
    mutate(
      usg_overall       = (n_rush + n_target) / pmax(team_total, 1),
      usg_rush          = n_rush / pmax(team_rushes, 1),
      usg_pass          = n_target / pmax(team_passes, 1),
      usg_passing_downs = n_pdown / pmax(team_pdowns, 1)
    ) |>
    transmute(name = player, team,
              usg_overall, usg_rush, usg_pass, usg_passing_downs)

  out_u <- paste0("data/cfb_usage_", yr, ".rds")
  saveRDS(player_usage, out_u)
  message("  ", out_u, " — ", nrow(player_usage), " rows")

  # ── PPA (using EPA as proxy) ───────────────────────────────────────────────
  # CFBD's PPA is expected-points based; we use cfbfastR's EPA (the same family
  # of metric) as the closest in-house approximation. Name-matching works the
  # same downstream.
  rush_epa <- pbp |>
    filter(coalesce(rush, 0L) == 1L, coalesce(play_type, "") != "Sack",
           !is.na(rusher_player_name),
           !is.na(pos_team), pos_team != "") |>
    group_by(player = rusher_player_name, team = pos_team) |>
    summarise(
      total_PPA_rush = sum(EPA, na.rm = TRUE),
      n_rush         = sum(!is.na(EPA)),
      .groups = "drop"
    ) |>
    mutate(avg_PPA_rush = total_PPA_rush / pmax(n_rush, 1))

  pass_epa <- pbp |>
    filter(coalesce(pass_attempt, 0L) == 1L,
           !is.na(receiver_player_name),
           !is.na(pos_team), pos_team != "") |>
    group_by(player = receiver_player_name, team = pos_team) |>
    summarise(
      total_PPA_pass = sum(EPA, na.rm = TRUE),
      n_pass         = sum(!is.na(EPA)),
      .groups = "drop"
    ) |>
    mutate(avg_PPA_pass = total_PPA_pass / pmax(n_pass, 1))

  ppa <- full_join(rush_epa, pass_epa, by = c("player", "team")) |>
    mutate(
      total_PPA_rush = coalesce(total_PPA_rush, 0),
      total_PPA_pass = coalesce(total_PPA_pass, 0),
      n_rush         = coalesce(n_rush, 0L),
      n_pass         = coalesce(n_pass, 0L),
      total_PPA_all  = total_PPA_rush + total_PPA_pass,
      avg_PPA_all    = total_PPA_all / pmax(n_rush + n_pass, 1)
    ) |>
    transmute(name = player, team,
              avg_PPA_pass, avg_PPA_rush, avg_PPA_all,
              total_PPA_pass, total_PPA_rush, total_PPA_all)

  out_p <- paste0("data/cfb_ppa_", yr, ".rds")
  saveRDS(ppa, out_p)
  message("  ", out_p, " — ", nrow(ppa), " rows")

  # ── Team games (replaces cfbd_game_info) ───────────────────────────────────
  # n_distinct(game_id) per pos_team. PBP covers both halves of every game the
  # team participated in, so either side of pos_team gives the same count.
  team_games <- pbp |>
    filter(!is.na(pos_team), pos_team != "", !is.na(game_id)) |>
    distinct(team = pos_team, game_id) |>
    group_by(team) |>
    summarise(team_games = n(), .groups = "drop")

  out_g <- paste0("data/cfb_team_games_", yr, ".rds")
  saveRDS(team_games, out_g)
  message("  ", out_g, " — ", nrow(team_games), " rows")
}

message("Done.")

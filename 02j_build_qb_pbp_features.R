# 02j_build_qb_pbp_features.R
# ─────────────────────────────────────────────────────────────────────────────
# Builds per-player-season PBP aggregates for the QB models. Mirrors the
# pattern of 02b_build_rb_pbp_features.R / 02c_build_wr_pbp_features.R.
#
# Available cfbfastR PBP signals that we use:
#   passer_player_name, EPA, pass_attempt, sack, completion, play_type,
#   pos_team, season, game_id, score_diff
#
# NOT available (so we can't compute them): air_yards (aDOT), yards_after_catch,
# dropback (we derive it = pass_attempt OR sack), scramble (we approximate
# from rush plays by the passer of record on that game).
#
# Features emitted per (passer, school, season):
#   dropbacks                 pass_attempt + sack
#   epa_per_dropback          mean EPA over dropbacks (the canonical QB metric)
#   epa_per_attempt           mean EPA on pass attempts only
#   completion_pct_pbp        completions / attempts (recomputed from PBP)
#   sack_rate                 sacks / dropbacks
#   int_rate                  ints / dropbacks  (from play_type)
#   negative_play_rate        (sack + int) / dropbacks
#   late_down_epa             mean EPA on 3rd/4th down plays
#   explosive_pass_rate       share of attempts that gained ≥ 20 yds
#
# Garbage-time filter: |score_diff| <= 28 (standard CFB rule).
# QB coverage in cfbfastR PBP is reliable from ~2014+.
# ─────────────────────────────────────────────────────────────────────────────

setwd("~/Projects/R/college_nfl_model")
suppressMessages({
  library(tidyverse)
  library(cfbfastR)
})

readRenviron("~/.Renviron")
options(cfbfastR.message = FALSE)

PBP_SEASONS    <- 2005:2025
CACHE_DIR      <- "data"
OUT_PATH       <- file.path(CACHE_DIR, "cfb_qb_pbp_features.rds")
PER_SEASON_DIR <- file.path(CACHE_DIR, "cfb_qb_pbp_seasonal")
if (!dir.exists(PER_SEASON_DIR)) dir.create(PER_SEASON_DIR, recursive = TRUE)

aggregate_season <- function(yr) {
  cache_path <- file.path(PER_SEASON_DIR, paste0(yr, ".rds"))
  if (file.exists(cache_path)) {
    message("  (cache hit) ", yr)
    return(readRDS(cache_path))
  }
  message("  loading PBP for ", yr, " ...")
  pbp <- tryCatch(load_cfb_pbp(yr) |> as_tibble(),
                  error = function(e) {
                    warning("  failed ", yr, ": ", conditionMessage(e)); NULL })
  if (is.null(pbp) || nrow(pbp) == 0) return(NULL)

  # Garbage-time filter
  pbp <- pbp |> filter(!is.na(score_diff), abs(score_diff) <= 28)

  # Dropback = pass attempt OR sack. The passer of record on those plays is
  # the QB who tried to throw. Sack rows have passer_player_name populated
  # via the same naming convention. For ints we use play_type contains.
  dropbacks <- pbp |>
    filter(
      (coalesce(pass_attempt, 0L) == 1L | coalesce(sack, 0L) == 1L),
      !is.na(passer_player_name), passer_player_name != "",
      !is.na(pos_team), pos_team != ""
    ) |>
    mutate(
      is_attempt    = as.integer(coalesce(pass_attempt, 0L) == 1L),
      is_sack       = as.integer(coalesce(sack, 0L) == 1L),
      is_complete   = as.integer(coalesce(completion, 0L) == 1L & is_attempt == 1L),
      is_int        = as.integer(is_attempt == 1L &
                                  str_detect(coalesce(play_type, ""),
                                              regex("interception", ignore_case = TRUE))),
      is_explosive  = as.integer(is_complete == 1L &
                                  coalesce(yds_receiving, 0) >= 20),
      is_late_down  = as.integer(coalesce(down, 0L) >= 3L),
      epa_safe      = coalesce(EPA, 0)
    )

  team_dropbacks <- dropbacks |>
    group_by(pos_team, season) |>
    summarise(team_dropbacks = n(), .groups = "drop")

  qb_agg <- dropbacks |>
    group_by(player = passer_player_name, pos_team, season) |>
    summarise(
      qb_games           = n_distinct(game_id),
      dropbacks_pbp      = n(),
      attempts_pbp       = sum(is_attempt),
      completions_pbp    = sum(is_complete),
      sacks_pbp          = sum(is_sack),
      ints_pbp           = sum(is_int),
      explosive_pbp      = sum(is_explosive),
      total_epa          = sum(epa_safe, na.rm = TRUE),
      total_epa_att      = sum(epa_safe * is_attempt, na.rm = TRUE),
      mean_late_down_epa = mean(epa_safe[is_late_down == 1L], na.rm = TRUE),
      .groups            = "drop"
    ) |>
    left_join(team_dropbacks, by = c("pos_team", "season")) |>
    mutate(
      epa_per_dropback     = total_epa / pmax(dropbacks_pbp, 1),
      epa_per_attempt      = total_epa_att / pmax(attempts_pbp, 1),
      completion_pct_pbp   = completions_pbp / pmax(attempts_pbp, 1),
      sack_rate            = sacks_pbp / pmax(dropbacks_pbp, 1),
      int_rate             = ints_pbp / pmax(dropbacks_pbp, 1),
      negative_play_rate   = (sacks_pbp + ints_pbp) / pmax(dropbacks_pbp, 1),
      explosive_pass_rate  = explosive_pbp / pmax(completions_pbp, 1),
      late_down_epa        = mean_late_down_epa,
      qb_share_team        = dropbacks_pbp / pmax(team_dropbacks, 1)
    ) |>
    select(-mean_late_down_epa)

  saveRDS(qb_agg, cache_path)
  message("  ✓ ", yr, " (", nrow(qb_agg), " QB-team rows)")
  qb_agg
}

message("Building QB PBP seasonal aggregates...")
pbp_features <- map_dfr(PBP_SEASONS, aggregate_season) |>
  filter(!is.na(player), player != "")

cat(sprintf("\nTotal QB-team-season rows: %d\n", nrow(pbp_features)))
saveRDS(pbp_features, OUT_PATH)
cat("Saved to ", OUT_PATH, "\n")

# Sanity check — top dropback EPA last 5 years
cat("\n── Top 10 QBs by EPA/dropback (≥200 dropbacks, last 5 yrs) ──\n")
pbp_features |>
  filter(season >= 2019, dropbacks_pbp >= 200) |>
  arrange(desc(epa_per_dropback)) |> head(10) |>
  select(player, pos_team, season, dropbacks_pbp,
         epa_per_dropback, completion_pct_pbp, sack_rate) |>
  print()

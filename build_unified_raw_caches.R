# build_unified_raw_caches.R
# Produces hybrid raw caches that match the schema 02_build_features.R expects,
# with pre-2014 rows from the legacy API caches and 2014+ rows from PBP-derived
# per-year caches (built by build_cfb_season_from_pbp.R).
#
# This homogenizes the data path between training and scoring for years 2014+:
# both will use cfbfastR PBP-derived counts (slight ~1% drift from CFBD's stat
# service, but no API quota dependency, and EPA-as-PPA proxy is internally
# consistent with score-time data).
#
# Pre-existing cfb_*_raw.rds files are backed up to cfb_*_raw_api.rds before
# being overwritten — so this is reversible.
#
# Outputs (overwrites in place):
#   data/cfb_rushing_raw.rds
#   data/cfb_receiving_raw.rds
#   data/cfb_usage_raw.rds
#   data/cfb_ppa_raw.rds
#   data/cfb_team_games_raw.rds

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tibble)
})

PBP_YEARS <- 2014:2025
DATA_DIR  <- "data"

backup <- function(path) {
  if (file.exists(path)) {
    bak <- sub("\\.rds$", "_api.rds", path)
    if (!file.exists(bak)) {
      file.copy(path, bak)
      message("  [backup] ", path, " → ", bak)
    } else {
      message("  [backup] already exists: ", bak)
    }
  }
}

# Back up all originals first (so subsequent reads pull from the backup, not
# the file we're about to overwrite)
for (kind in c("rushing", "receiving", "usage", "ppa", "team_games")) {
  backup(file.path(DATA_DIR, paste0("cfb_", kind, "_raw.rds")))
}

# ── Rushing ──────────────────────────────────────────────────────────────────
message("=== rushing ===")
api_rush <- readRDS(file.path(DATA_DIR, "cfb_rushing_raw_api.rds")) |>
  as_tibble() |>
  filter(cfb_season < min(PBP_YEARS))

pbp_rush <- map_dfr(PBP_YEARS, function(yr) {
  f <- file.path(DATA_DIR, sprintf("cfb_rushing_%d.rds", yr))
  if (!file.exists(f)) { message("  [skip] no ", f); return(tibble()) }
  readRDS(f) |> mutate(cfb_season = yr, year = yr)
})

# pad PBP to API schema with NA columns
api_cols <- names(readRDS(file.path(DATA_DIR, "cfb_rushing_raw_api.rds")))
unified_rush <- bind_rows(api_rush, pbp_rush)
for (c in setdiff(api_cols, names(unified_rush))) unified_rush[[c]] <- NA

backup(file.path(DATA_DIR, "cfb_rushing_raw.rds"))
saveRDS(unified_rush, file.path(DATA_DIR, "cfb_rushing_raw.rds"))
message(sprintf("  rushing: %d API (pre-%d) + %d PBP (%d-%d) = %d rows",
                nrow(api_rush), min(PBP_YEARS), nrow(pbp_rush),
                min(PBP_YEARS), max(PBP_YEARS), nrow(unified_rush)))

# ── Receiving ────────────────────────────────────────────────────────────────
message("=== receiving ===")
api_recv <- readRDS(file.path(DATA_DIR, "cfb_receiving_raw_api.rds")) |>
  as_tibble() |>
  filter(cfb_season < min(PBP_YEARS))

pbp_recv <- map_dfr(PBP_YEARS, function(yr) {
  f <- file.path(DATA_DIR, sprintf("cfb_receiving_%d.rds", yr))
  if (!file.exists(f)) { message("  [skip] no ", f); return(tibble()) }
  readRDS(f) |> mutate(cfb_season = yr, year = yr)
})

api_cols <- names(readRDS(file.path(DATA_DIR, "cfb_receiving_raw_api.rds")))
unified_recv <- bind_rows(api_recv, pbp_recv)
for (c in setdiff(api_cols, names(unified_recv))) unified_recv[[c]] <- NA

backup(file.path(DATA_DIR, "cfb_receiving_raw.rds"))
saveRDS(unified_recv, file.path(DATA_DIR, "cfb_receiving_raw.rds"))
message(sprintf("  receiving: %d API + %d PBP = %d rows",
                nrow(api_recv), nrow(pbp_recv), nrow(unified_recv)))

# ── Usage ────────────────────────────────────────────────────────────────────
message("=== usage ===")
api_usage <- readRDS(file.path(DATA_DIR, "cfb_usage_raw_api.rds")) |>
  as_tibble() |>
  filter(cfb_season < min(PBP_YEARS))

pbp_usage <- map_dfr(PBP_YEARS, function(yr) {
  f <- file.path(DATA_DIR, sprintf("cfb_usage_%d.rds", yr))
  if (!file.exists(f)) { message("  [skip] no ", f); return(tibble()) }
  readRDS(f) |> mutate(cfb_season = yr, season = yr)
})

api_cols <- names(readRDS(file.path(DATA_DIR, "cfb_usage_raw_api.rds")))
unified_usage <- bind_rows(api_usage, pbp_usage)
for (c in setdiff(api_cols, names(unified_usage))) unified_usage[[c]] <- NA

backup(file.path(DATA_DIR, "cfb_usage_raw.rds"))
saveRDS(unified_usage, file.path(DATA_DIR, "cfb_usage_raw.rds"))
message(sprintf("  usage: %d API + %d PBP = %d rows",
                nrow(api_usage), nrow(pbp_usage), nrow(unified_usage)))

# ── PPA ──────────────────────────────────────────────────────────────────────
message("=== ppa ===")
api_ppa <- readRDS(file.path(DATA_DIR, "cfb_ppa_raw_api.rds")) |>
  as_tibble() |>
  filter(cfb_season < min(PBP_YEARS))

pbp_ppa <- map_dfr(PBP_YEARS, function(yr) {
  f <- file.path(DATA_DIR, sprintf("cfb_ppa_%d.rds", yr))
  if (!file.exists(f)) { message("  [skip] no ", f); return(tibble()) }
  readRDS(f) |> mutate(cfb_season = yr, season = yr)
})

api_cols <- names(readRDS(file.path(DATA_DIR, "cfb_ppa_raw_api.rds")))
unified_ppa <- bind_rows(api_ppa, pbp_ppa)
for (c in setdiff(api_cols, names(unified_ppa))) unified_ppa[[c]] <- NA

backup(file.path(DATA_DIR, "cfb_ppa_raw.rds"))
saveRDS(unified_ppa, file.path(DATA_DIR, "cfb_ppa_raw.rds"))
message(sprintf("  ppa: %d API + %d PBP = %d rows",
                nrow(api_ppa), nrow(pbp_ppa), nrow(unified_ppa)))

# ── Team games ───────────────────────────────────────────────────────────────
message("=== team_games ===")
api_tg <- readRDS(file.path(DATA_DIR, "cfb_team_games_raw_api.rds")) |>
  as_tibble() |>
  filter(cfb_season < min(PBP_YEARS))

# Legacy API cache shape: one row per (cfb_season, team, game). PBP shape:
# already-aggregated. Normalize to "long" (one row per game) so downstream
# group_by(cfb_season, team) |> summarise(team_games = n()) still works.
# For PBP we replicate `team_games` rows per team to mimic the old shape.
pbp_tg_long <- map_dfr(PBP_YEARS, function(yr) {
  f <- file.path(DATA_DIR, sprintf("cfb_team_games_%d.rds", yr))
  if (!file.exists(f)) { message("  [skip] no ", f); return(tibble()) }
  agg <- readRDS(f)
  # expand: each team contributes team_games rows of (cfb_season, team)
  tibble(cfb_season = yr,
         team       = rep(agg$team, agg$team_games))
})

unified_tg <- bind_rows(api_tg, pbp_tg_long)
backup(file.path(DATA_DIR, "cfb_team_games_raw.rds"))
saveRDS(unified_tg, file.path(DATA_DIR, "cfb_team_games_raw.rds"))
message(sprintf("  team_games: %d API + %d PBP = %d rows",
                nrow(api_tg), nrow(pbp_tg_long), nrow(unified_tg)))

message("\nDone. Run:  Rscript 02_build_features.R")

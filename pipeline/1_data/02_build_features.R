# 02_build_features.R
# ─────────────────────────────────────────────────────────────────────────────
# Builds the college-stat feature matrix for WR/RB NFL draft prospects.
#
# Sources:
#   - cfbfastR: player-level season stats (receiving, rushing)
#   - nflreadr: draft picks (round, pick, age, college)
#
# Strategy:
#   Primary features = player's FINAL college season (draft_year - 1).
#   Secondary features = penultimate season (draft_year - 2) for volume/trend.
#   If only one college season is found, penultimate features are NA.
#
# College season range fetched: 2001–2023
#   → covers final seasons for players drafted 2002–2024
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(cfbfastR)    # install: install.packages("cfbfastR")

source("functions/helpers.R")  # classify_tier (P4_SCHOOLS override) lives here

# ── API key ───────────────────────────────────────────────────────────────────
# cfbfastR 2.x: set key via environment variable (no register_cfbd() in this version).
# One-time persistent setup — run this once in your R console:
#   writeLines('CFBD_API_KEY=YOUR_KEY', con = "~/.Renviron")
#   readRenviron("~/.Renviron")
# Or for the current session only: Sys.setenv(CFBD_API_KEY = "YOUR_KEY")
readRenviron("~/.Renviron")
if (Sys.getenv("CFBD_API_KEY") == "") {
  stop("CFBD_API_KEY not set.\n",
       "Run in R console: Sys.setenv(CFBD_API_KEY = 'YOUR_KEY')\n",
       "For persistence:  writeLines('CFBD_API_KEY=YOUR_KEY', '~/.Renviron')")
}
message("CFBD API key loaded ✓")

COLLEGE_SEASONS <- 2001:2025  # extends through 2025 for 2026 prospects' final season
CACHE_DIR       <- "data"

# ── Helpers ───────────────────────────────────────────────────────────────────

clean_name <- function(x) {
  x |>
    str_to_lower() |>
    str_remove_all("[^a-z ]") |>   # strip punctuation / accents
    str_squish()
}

strip_suffix <- function(x) {
  str_remove(x, "\\s+(jr|sr|ii|iii|iv|v)$")
}

normalize_school <- function(x) {
  x |>
    str_to_lower() |>
    str_remove_all("\\.") |>
    str_replace_all("\\bst\\b", "state") |>
    str_replace_all("\\s*\\([^)]+\\)", "") |>
    str_replace_all("ala-", "alabama ") |>
    str_replace_all("la-", "louisiana ") |>
    str_squish()
}

# Conference → school tier — see functions/helpers.R::classify_tier()

# ── 1. Fetch / cache college receiving stats ──────────────────────────────────
# season_type = "both" — fetches regular season + postseason (bowl/CFP/CCG)
# combined. Using "regular" only systematically undercounts top prospects who
# played in conference championships, bowls, or playoffs (e.g. Egbuka 2024:
# 60/743/9 regular vs 81/1011/10 with postseason; Hunter 2024: 88/1091/14 vs
# 96/1258/15 with bowl). The model trains on "best season" so undercount =
# undertrain on the players we care about most.
recv_cache <- file.path(CACHE_DIR, "cfb_receiving_raw.rds")

if (file.exists(recv_cache)) {
  message("Loading cached receiving stats...")
  recv_raw <- readRDS(recv_cache) |> as_tibble()
} else {
  message("Fetching receiving stats from cfbfastR (", length(COLLEGE_SEASONS), " seasons)...")
  recv_raw <- map_dfr(COLLEGE_SEASONS, function(yr) {
    tryCatch({
      message("  year: ", yr)
      cfbd_stats_season_player(year = yr, season_type = "both", category = "receiving") |>
        mutate(cfb_season = yr)
    }, error = function(e) {
      warning("  Failed for year ", yr, ": ", conditionMessage(e))
      tibble()
    })
  })
  saveRDS(recv_raw, recv_cache)
}

# ── 2. Fetch / cache college rushing stats ────────────────────────────────────
rush_cache <- file.path(CACHE_DIR, "cfb_rushing_raw.rds")

if (file.exists(rush_cache)) {
  message("Loading cached rushing stats...")
  rush_raw <- readRDS(rush_cache) |> as_tibble()
} else {
  message("Fetching rushing stats from cfbfastR...")
  rush_raw <- map_dfr(COLLEGE_SEASONS, function(yr) {
    tryCatch({
      message("  year: ", yr)
      cfbd_stats_season_player(year = yr, season_type = "both", category = "rushing") |>
        mutate(cfb_season = yr)
    }, error = function(e) {
      warning("  Failed for year ", yr, ": ", conditionMessage(e))
      tibble()
    })
  })
  saveRDS(rush_raw, rush_cache)
}

# ── 2b. Fetch / cache college passing stats (QB features) ────────────────────
# Same season_type = "both" comment from the receiving block applies here.
pass_cache <- file.path(CACHE_DIR, "cfb_passing_raw.rds")

if (file.exists(pass_cache)) {
  message("Loading cached passing stats...")
  pass_raw <- readRDS(pass_cache) |> as_tibble()
} else {
  message("Fetching passing stats from cfbfastR (", length(COLLEGE_SEASONS), " seasons)...")
  pass_raw <- map_dfr(COLLEGE_SEASONS, function(yr) {
    tryCatch({
      message("  year: ", yr)
      cfbd_stats_season_player(year = yr, season_type = "both", category = "passing") |>
        mutate(cfb_season = yr)
    }, error = function(e) {
      warning("  Failed for year ", yr, ": ", conditionMessage(e))
      tibble()
    })
  })
  saveRDS(pass_raw, pass_cache)
}

# ── 3. Fetch / cache recruiting ratings ──────────────────────────────────────
# cfbd_recruiting_player(year) returns HS recruiting class data (247Sports composite):
#   stars (1-5), rating (0-1), national ranking
# Year here = year player enrolled in college.
# Draft class 2005-2024 → recruits enrolled ~2001-2021.

RECRUIT_SEASONS <- 2000:2024  # extended for 2026+ classes (entrants enrolled 2022+)

recruit_cache <- file.path(CACHE_DIR, "cfb_recruiting_raw.rds")

if (file.exists(recruit_cache)) {
  message("Loading cached recruiting data...")
  recruit_raw <- readRDS(recruit_cache)
} else {
  message("Fetching recruiting data from cfbfastR (", length(RECRUIT_SEASONS), " seasons)...")
  recruit_raw <- map_dfr(RECRUIT_SEASONS, function(yr) {
    tryCatch({
      message("  year: ", yr)
      cfbd_recruiting_player(year = yr) |>
        mutate(recruit_year = yr)
    }, error = function(e) {
      warning("  Failed for year ", yr, ": ", conditionMessage(e))
      tibble()
    })
  })
  saveRDS(recruit_raw, recruit_cache)
}

# Build name-keyed lookup (ALL positions — many NFL WR/RBs were recruited as
# CB, S, QB, ATH, etc. and later converted; the old position filter excluded
# ~51 matchable players).
#
# Important: 247Sports' `committed_to` is NA for ~30% of recruits (uncommitted
# at signing day, transfers, etc.). Joining on (name + school) hard-misses
# every prospect with NA committed_to even when the name uniquely identifies
# them — Zay Flowers, Rashee Rice, Tank Dell, DJ Chark, etc. all fail this
# way. We instead keep one row per name (highest rating) and let the join
# match on name alone; downstream `college_years` validity (draft_year -
# recruit_year ∈ [2,6]) catches false-positive name collisions.
recruit_lookup <- recruit_raw |>
  filter(recruit_type == "HighSchool") |>
  mutate(name_clean = strip_suffix(clean_name(name))) |>
  select(name_clean, committed_to, position, stars, rating, ranking, recruit_year) |>
  filter(!is.na(rating)) |>
  filter(recruit_year >= 2000) |>
  # Pick the highest-rated entry per name. Ties (e.g., two unrelated 3-stars
  # named "Anthony Miller" in different years) get broken by `rating`, with
  # the downstream college_years sanity filter pruning the wrong-year picks.
  group_by(name_clean) |>
  slice_max(rating, n = 1, with_ties = FALSE) |>
  ungroup() |>
  rename(recruit_stars = stars, recruit_rating = rating, recruit_rank = ranking)

# Same lookup for both WR and RB — name + college_years sanity filter is enough
recv_recruit <- recruit_lookup
rush_recruit <- recruit_lookup

# ── 4. Fetch / cache usage + PPA metrics ─────────────────────────────────────
# cfbd_player_usage: pass/rush usage rates per player (available 2016+)
# cfbd_metrics_ppa_players_season: avg & total PPA per player (available 2016+)
# Pre-2016 seasons return errors gracefully → NAs imputed at model time.

usage_cache <- file.path(CACHE_DIR, "cfb_usage_raw.rds")

if (file.exists(usage_cache)) {
  message("Loading cached usage data...")
  usage_raw <- readRDS(usage_cache)
} else {
  message("Fetching player usage from cfbfastR...")
  usage_raw <- map_dfr(COLLEGE_SEASONS, function(yr) {
    tryCatch(
      cfbd_player_usage(year = yr) |> mutate(cfb_season = yr),
      error = function(e) tibble()
    )
  })
  saveRDS(usage_raw, usage_cache)
}

ppa_cache <- file.path(CACHE_DIR, "cfb_ppa_raw.rds")

if (file.exists(ppa_cache)) {
  message("Loading cached PPA data...")
  ppa_raw <- readRDS(ppa_cache)
} else {
  message("Fetching player PPA from cfbfastR...")
  ppa_raw <- map_dfr(COLLEGE_SEASONS, function(yr) {
    tryCatch(
      cfbd_metrics_ppa_players_season(year = yr, threshold = 0.1) |>
        mutate(cfb_season = yr),
      error = function(e) tibble()
    )
  })
  saveRDS(ppa_raw, ppa_cache)
}

# Join-ready lookups keyed by name_clean + school + cfb_season
usage_lookup <- usage_raw |>
  mutate(name_clean = clean_name(name)) |>
  select(cfb_season, name_clean, school = team,
         usg_pass, usg_rush, usg_passing_downs, usg_overall) |>
  group_by(cfb_season, name_clean, school) |>
  slice_max(usg_overall, n = 1, with_ties = FALSE) |>
  ungroup()

ppa_lookup <- ppa_raw |>
  mutate(name_clean = clean_name(name)) |>
  select(cfb_season, name_clean, school = team,
         avg_PPA_pass, avg_PPA_rush, avg_PPA_all,
         total_PPA_pass, total_PPA_rush, total_PPA_all) |>
  group_by(cfb_season, name_clean, school) |>
  slice_max(total_PPA_all, n = 1, with_ties = FALSE) |>
  ungroup()

# ── 5. Standardise column names ───────────────────────────────────────────────
# cfbfastR 2.x returns a wide table with prefixed columns (receiving_*, rushing_*)
# and the same schema for both the "receiving" and "rushing" category endpoints.

normalise_recv <- function(df) {
  df |>
    transmute(
      cfb_season,
      athlete_name = player,
      school       = team,
      conference,
      position,
      rec        = receiving_rec,
      rec_yards  = receiving_yds,
      rec_td     = receiving_td,
      rec_avg    = receiving_ypr,   # yards per reception
      rec_long   = receiving_long
    )
}

normalise_rush <- function(df) {
  df |>
    transmute(
      cfb_season,
      athlete_name = player,
      school       = team,
      conference,
      position,
      carries    = rushing_car,
      rush_yards = rushing_yds,
      rush_td    = rushing_td,
      rush_avg   = rushing_ypc,     # yards per carry
      rush_long  = rushing_long,
      # Also capture receiving stats for dual-threat RBs
      rb_rec       = receiving_rec,
      rb_rec_yards = receiving_yds,
      rb_rec_td    = receiving_td
    )
}

recv <- normalise_recv(recv_raw) |>
  mutate(name_clean = clean_name(athlete_name))

rush <- normalise_rush(rush_raw) |>
  mutate(name_clean = clean_name(athlete_name))

# ── 4. Load draft picks ───────────────────────────────────────────────────────
# Re-read from targets script output (already filtered to WR/RB 2005-2023)
targets <- readRDS("data/targets.rds")

# Known nickname overrides: NFL name → cfbfastR name (clean form).
# These players are listed under a different name in cfbfastR than in the
# NFL draft records — automatic matching cannot resolve them.
NICKNAME_OVERRIDES <- tribble(
  ~nfl_clean,       ~cfb_clean,
  "tank dell",       "nathaniel dell",
  "tutu atwell",     "chatarius atwell"
)

draft <- targets |>
  select(gsis_id, pfr_player_name, position, college, draft_year, round, pick, age) |>
  mutate(
    # Strip suffixes (Jr., Sr., II, etc.) before cleaning so they don't corrupt
    # the last-name key used in the fallback join pass.
    name_clean              = clean_name(strip_suffix(pfr_player_name)),
    college_clean           = str_to_lower(str_squish(college)),
    final_cfb_season        = draft_year - 1,
    penult_cfb_season       = draft_year - 2,
    ante_penult_cfb_season  = draft_year - 3   # 3rd-to-last college season
  ) |>
  # Apply nickname overrides: replace name_clean where NFL name ≠ cfbfastR name
  left_join(NICKNAME_OVERRIDES, by = c("name_clean" = "nfl_clean")) |>
  mutate(name_clean = coalesce(cfb_clean, name_clean)) |>
  select(-cfb_clean)

# ── Team-level volume (for dominator rate) ────────────────────────────────────
# Sum all receiving/rushing yards by team+season → denominator for share calcs
team_recv_vol <- recv_raw |>
  group_by(cfb_season, team) |>
  summarize(team_rec_yards = sum(receiving_yds, na.rm = TRUE), .groups = "drop")

team_rush_vol <- rush_raw |>
  group_by(cfb_season, team) |>
  summarize(team_rush_yards = sum(rushing_yds, na.rm = TRUE), .groups = "drop")

# ── Team games played (regular season) ───────────────────────────────────────
# cfbd_stats_season_player has no games-played column; derive from game schedule.
# Used to compute per-game receiving rates (rec_yards_per_game, rec_per_game).
#
# Sources, in preference order:
#   1. Aggregated raw cache (cfb_team_games_raw.rds) — historical API fetch
#   2. Per-year PBP-derived caches (cfb_team_games_<yr>.rds) — non-API
#   3. load_cfb_pbp(yr) → n_distinct(game_id) per team — non-API
#   4. cfbd_game_info() live — API, last resort

games_cache <- file.path(CACHE_DIR, "cfb_team_games_raw.rds")

if (file.exists(games_cache)) {
  message("Loading cached team games data...")
  team_games_raw <- readRDS(games_cache)
} else {
  message("Building team games from PBP (non-API)...")
  team_games_raw <- map_dfr(COLLEGE_SEASONS, function(yr) {
    # Per-year PBP cache first
    per_yr_cache <- file.path(CACHE_DIR, sprintf("cfb_team_games_%d.rds", yr))
    if (file.exists(per_yr_cache)) {
      message("  year: ", yr, " (per-year cache)")
      out <- readRDS(per_yr_cache)
      if (!is.null(out) && nrow(out) > 0) return(mutate(out, cfb_season = yr))
    }
    # Fresh PBP pull
    tryCatch({
      message("  year: ", yr, " (PBP)")
      pbp <- as_tibble(load_cfb_pbp(yr))
      pbp |>
        filter(!is.na(pos_team), pos_team != "", !is.na(game_id)) |>
        distinct(team = pos_team, game_id) |>
        group_by(team) |>
        summarise(team_games = n(), .groups = "drop") |>
        mutate(cfb_season = yr)
    }, error = function(e1) {
      # Last-resort API fall-through (for pre-2014 where PBP doesn't exist)
      tryCatch({
        message("  year: ", yr, " (API fallback)")
        gi <- cfbd_game_info(year = yr, season_type = "both") |>
          filter(completed == TRUE)
        bind_rows(
          gi |> transmute(cfb_season = yr, team = home_team),
          gi |> transmute(cfb_season = yr, team = away_team)
        ) |>
          group_by(cfb_season, team) |> summarise(team_games = n(), .groups = "drop")
      }, error = function(e2) tibble())
    })
  })
  saveRDS(team_games_raw, games_cache)
}

# team_games_raw may arrive in two shapes depending on source:
#   • Legacy API cache: one row per (cfb_season, team, game)
#   • New PBP path:     already aggregated (cfb_season, team, team_games)
# Normalize to the aggregated shape.
team_games <- if ("team_games" %in% names(team_games_raw)) {
  team_games_raw |>
    group_by(cfb_season, team) |>
    summarise(team_games = sum(team_games), .groups = "drop")
} else {
  team_games_raw |>
    group_by(cfb_season, team) |>
    summarise(team_games = n(), .groups = "drop")
}

# ── Teammate draft context ────────────────────────────────────────────────────
# Load ALL draft picks (not just WR/RB) for teammate counting.
# n_drafted_skill: how many WR/RB/TE from same college were drafted in same year (excl self)
# elite_teammate: binary flag if any SAME-POSITION teammate was drafted in rounds 1-2

all_draft_picks <- nflreadr::load_draft_picks() |>
  filter(season %in% 2002:2025) |>
  transmute(
    pfr_player_name,
    position,
    college,
    draft_year = season,
    round
  )

# n_drafted_skill: count of skill-position teammates from same school+year, minus self
skill_positions <- c("WR", "RB", "TE")
skill_drafted_counts <- all_draft_picks |>
  filter(position %in% skill_positions) |>
  group_by(college, draft_year) |>
  mutate(n_skill_total = n()) |>
  ungroup() |>
  transmute(pfr_player_name, college, draft_year,
            n_drafted_skill = n_skill_total - 1L)

# elite_teammate: did any OTHER same-position player from same school+year go in rounds 1-2?
elite_teammate_flag <- all_draft_picks |>
  filter(round <= 2) |>
  select(college, draft_year, position, elite_name = pfr_player_name) |>
  inner_join(
    all_draft_picks |> select(pfr_player_name, college, draft_year, position),
    by = c("college", "draft_year", "position"),
    relationship = "many-to-many"
  ) |>
  filter(elite_name != pfr_player_name) |>
  distinct(pfr_player_name, college, draft_year, position) |>
  mutate(elite_teammate = 1L)

message(sprintf("Teammate context: %d players with skill-position teammates, %d with elite same-pos teammate",
                sum(skill_drafted_counts$n_drafted_skill > 0),
                nrow(elite_teammate_flag)))

# ── 5. Match college stats to draft picks ────────────────────────────────────
# Join on cleaned name + cfb_season. Season is the most reliable anchor.
# Multiple schools in one season would be ambiguous; we take the row with most yards.

match_recv <- function(draft_df, recv_df, season_col,
                       cfb_positions = c("WR", "TE", "ATH", "APB")) {
  by_vec <- c("name_clean" = "name_clean",
              setNames("cfb_season", season_col))
  recv_prepped <- recv_df |>
    filter(position %in% cfb_positions | is.na(position)) |>
    group_by(name_clean, cfb_season) |>
    slice_max(rec_yards, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(name_clean, cfb_season, school, conference, rec, rec_yards, rec_td, rec_avg) |>
    mutate(school_norm = normalize_school(school))

  draft_df |>
    mutate(college_norm = normalize_school(college)) |>
    left_join(recv_prepped, by = by_vec, relationship = "many-to-many") |>
    # Prefer school match, then most yards
    mutate(school_match = as.integer(school_norm == college_norm)) |>
    group_by(gsis_id) |>
    slice_max(order_by = tibble(school_match, coalesce(rec_yards, -Inf)),
              n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(-school_norm, -college_norm, -school_match)
}

match_rush <- function(draft_df, rush_df, season_col,
                       cfb_positions = c("RB", "FB", "ATH", "APB")) {
  by_vec <- c("name_clean" = "name_clean",
              setNames("cfb_season", season_col))
  rush_prepped <- rush_df |>
    filter(position %in% cfb_positions | is.na(position)) |>
    group_by(name_clean, cfb_season) |>
    slice_max(rush_yards, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(name_clean, cfb_season, school, conference, carries, rush_yards, rush_td, rush_avg) |>
    mutate(school_norm = normalize_school(school))

  draft_df |>
    mutate(college_norm = normalize_school(college)) |>
    left_join(rush_prepped, by = by_vec, relationship = "many-to-many") |>
    mutate(school_match = as.integer(school_norm == college_norm)) |>
    group_by(gsis_id) |>
    slice_max(order_by = tibble(school_match, coalesce(rush_yards, -Inf)),
              n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(-school_norm, -college_norm, -school_match)
}

# ── Best-season matchers ──────────────────────────────────────────────────────
# Instead of anchoring to the final college season, find the season with peak
# production across final + penultimate + ante-penultimate. This helps players
# who had a career year as a sophomore (e.g. opted-out seniors) and helps the
# model capture peak ceiling rather than just the most-recent data point.
# Adds two columns to the output: best_cfb_season and best_season_is_final.

match_recv_best <- function(draft_df, recv_df,
                             cfb_positions = c("WR", "TE", "ATH", "APB",
                                               "RB", "QB", "SLOT", "PRO", "?")) {
  recv_filtered <- recv_df |>
    filter(position %in% cfb_positions | is.na(position)) |>
    group_by(name_clean, cfb_season) |>
    slice_max(rec_yards, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(name_clean, cfb_season, school, conference, rec, rec_yards, rec_td, rec_avg) |>
    mutate(school_norm = normalize_school(school))

  best <- draft_df |>
    select(gsis_id, name_clean, college,
           final_cfb_season, penult_cfb_season, ante_penult_cfb_season) |>
    mutate(college_norm = normalize_school(college)) |>
    pivot_longer(
      c(final_cfb_season, penult_cfb_season, ante_penult_cfb_season),
      names_to = "season_type", values_to = "cfb_season"
    ) |>
    filter(!is.na(cfb_season)) |>
    left_join(recv_filtered, by = c("name_clean", "cfb_season"),
              relationship = "many-to-many") |>
    # Prefer rows where school matches draft college; break ties by yards
    mutate(school_match = as.integer(school_norm == college_norm)) |>
    group_by(gsis_id) |>
    slice_max(order_by = tibble(school_match, coalesce(rec_yards, -Inf)),
              n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(best_season_is_final = as.integer(season_type == "final_cfb_season")) |>
    select(gsis_id, best_cfb_season = cfb_season, best_season_is_final,
           school, conference, rec, rec_yards, rec_td, rec_avg)

  draft_df |>
    left_join(best, by = "gsis_id") |>
    group_by(gsis_id) |>
    slice(1) |>
    ungroup()
}

match_rush_best <- function(draft_df, rush_df,
                             cfb_positions = c("RB", "FB", "ATH", "APB")) {
  rush_filtered <- rush_df |>
    filter(position %in% cfb_positions | is.na(position)) |>
    group_by(name_clean, cfb_season) |>
    slice_max(rush_yards, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(name_clean, cfb_season, school, conference, carries, rush_yards, rush_td, rush_avg) |>
    mutate(school_norm = normalize_school(school))

  best <- draft_df |>
    select(gsis_id, name_clean, college,
           final_cfb_season, penult_cfb_season, ante_penult_cfb_season) |>
    mutate(college_norm = normalize_school(college)) |>
    pivot_longer(
      c(final_cfb_season, penult_cfb_season, ante_penult_cfb_season),
      names_to = "season_type", values_to = "cfb_season"
    ) |>
    filter(!is.na(cfb_season)) |>
    left_join(rush_filtered, by = c("name_clean", "cfb_season"),
              relationship = "many-to-many") |>
    mutate(school_match = as.integer(school_norm == college_norm)) |>
    group_by(gsis_id) |>
    slice_max(order_by = tibble(school_match, coalesce(rush_yards, -Inf)),
              n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(best_season_is_final = as.integer(season_type == "final_cfb_season")) |>
    select(gsis_id, best_cfb_season = cfb_season, best_season_is_final,
           school, conference, carries, rush_yards, rush_td, rush_avg)

  draft_df |>
    left_join(best, by = "gsis_id") |>
    group_by(gsis_id) |>
    slice(1) |>
    ungroup()
}

# ── 6. WR features ────────────────────────────────────────────────────────────
# Final season receiving + penultimate season receiving
wr_draft <- filter(draft, position == "WR")

wr_final <- wr_draft |>
  match_recv_best(recv) |>
  rename_with(~ paste0(.x, "_final"), any_of(c("school", "conference", "rec", "rec_yards", "rec_td", "rec_avg")))

# Actual final-season stats (chronological, for YoY trend feature)
wr_actual_final <- wr_draft |>
  match_recv(recv, "final_cfb_season") |>
  select(gsis_id, rec_yards_actual_final = rec_yards)

wr_penult <- wr_draft |>
  match_recv(recv, "penult_cfb_season") |>
  select(gsis_id, rec_penult = rec, rec_yards_penult = rec_yards, rec_td_penult = rec_td)

wr_ante_penult <- wr_draft |>
  match_recv(recv, "ante_penult_cfb_season") |>
  select(gsis_id, rec_yards_ante = rec_yards)

wr_features_raw <- wr_final |>
  left_join(wr_actual_final, by = "gsis_id") |>
  left_join(wr_penult,       by = "gsis_id") |>
  left_join(wr_ante_penult,  by = "gsis_id") |>
  # Dominator rate: player's share of team receiving volume in their best season
  left_join(
    team_recv_vol |> rename(school_final = team),
    by = c("best_cfb_season" = "cfb_season", "school_final")
  ) |>
  # Games played: regular-season games for player's team in their best season
  left_join(
    team_games |> rename(school_final = team),
    by = c("best_cfb_season" = "cfb_season", "school_final")
  ) |>
  # Usage rates — join on best season (available 2016+ seasons; NAs elsewhere)
  left_join(
    usage_lookup |> rename(school_final = school),
    by = c("name_clean", "best_cfb_season" = "cfb_season", "school_final")
  ) |>
  # PPA efficiency metrics — join on best season (available 2016+ seasons)
  left_join(
    ppa_lookup |> rename(school_final = school),
    by = c("name_clean", "best_cfb_season" = "cfb_season", "school_final")
  ) |>
  # Recruiting rating: join on suffix-stripped name only. School constraint
  # was hard-dropping ~30% of prospects whose recruit cfb_school is NA in
  # 247's data (Zay Flowers, Rashee Rice, etc.). Downstream college_years
  # sanity filter rejects wrong-year name collisions.
  mutate(name_clean_base = strip_suffix(name_clean)) |>
  left_join(
    recv_recruit |> select(-committed_to),
    by = c("name_clean_base" = "name_clean")
  ) |>
  mutate(
    tier               = classify_tier(conference_final, school = coalesce(school_final, college)),
    ypr                = rec_avg_final,
    has_penult         = as.integer(!is.na(rec_yards_penult)),
    # YoY uses chronological final-year vs penult-year (can be negative = declining)
    rec_yds_yoy        = rec_yards_actual_final - rec_yards_penult,   # NA when either missing
    teammate_rec_yards  = pmax(team_rec_yards - rec_yards_final, 0),  # team volume minus player's own
    rec_td_rate         = rec_td_final / pmax(rec_final, 1),
    rec_yards_per_game  = rec_yards_final / team_games,   # NA when team_games unavailable
    rec_per_game        = rec_final       / team_games
  ) |>
  # Age-adjusted production & teammate context
  mutate(
    college_years_raw = draft_year - recruit_year,
    # Out-of-range college_years signals a wrong-player name match. We must
    # clear ALL recruit features (rating, stars, rank, year) — not just
    # college_years — otherwise the polluted rating leaks into the model.
    .recruit_valid    = !is.na(college_years_raw) &
                         college_years_raw >= 2 &
                         college_years_raw <= 6,
    college_years     = if_else(.recruit_valid, college_years_raw, NA_real_),
    recruit_rating    = if_else(.recruit_valid, recruit_rating, NA_real_),
    recruit_stars     = if_else(.recruit_valid, recruit_stars,  NA_integer_),
    recruit_rank      = if_else(.recruit_valid, recruit_rank,   NA_integer_),
    recruit_year      = if_else(.recruit_valid, recruit_year,   NA_integer_)
  ) |>
  select(-college_years_raw, -.recruit_valid) |>
  group_by(draft_year) |>
  mutate(age_relative = age - mean(age, na.rm = TRUE)) |>
  ungroup() |>
  left_join(skill_drafted_counts,
            by = c("pfr_player_name", "college", "draft_year")) |>
  left_join(elite_teammate_flag |> filter(position == "WR") |> select(-position),
            by = c("pfr_player_name", "college", "draft_year")) |>
  mutate(
    n_drafted_skill = coalesce(n_drafted_skill, 0L),
    elite_teammate  = coalesce(elite_teammate, 0L)
  ) |>
  select(-name_clean_base, -rec_yards_actual_final)

# ── 6b. WR PBP features (catch rate, YPT, explosive rec, target share, EPA) ──
# Built by 02c_build_wr_pbp_features.R. Joins on (name_clean, school_norm,
# best_cfb_season). PBP reliable from 2014+ → pre-2014 final-seasons will be NA
# and handled downstream via the has_wr_pbp missingness flag.
#
# NOTE: cfbfastR PBP does NOT contain air_yards or yards_after_catch, so aDOT
# and YAC/rec (the highest-R² PFF metrics) are not available here.

wr_pbp_path <- file.path(CACHE_DIR, "cfb_wr_pbp_features.rds")

if (file.exists(wr_pbp_path)) {
  message("Joining WR PBP features from ", wr_pbp_path)
  wr_pbp_lookup <- readRDS(wr_pbp_path) |>
    mutate(
      name_clean  = clean_name(player),
      school_norm = normalize_school(pos_team)
    ) |>
    # After school normalization, two raw rows can collapse to the same
    # (name, school_norm, season) key — keep the one with more volume.
    group_by(name_clean, school_norm, season) |>
    slice_max(targets_pbp, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(
      name_clean, school_norm, cfb_season = season,
      targets_pbp_wr        = targets_pbp,
      receptions_pbp_wr     = receptions_pbp,
      rec_yards_pbp_wr      = rec_yards_pbp,
      catch_rate_wr,
      yards_per_target_wr,
      yards_per_rec_wr,
      explosive_rec_rate,
      target_share_wr,
      targets_per_game_wr,
      epa_per_target_wr     = epa_per_target,
      epa_per_play_wr_pbp,
      share_targets_as_wr
    )

  wr_features_raw <- wr_features_raw |>
    mutate(school_norm_tmp = normalize_school(school_final)) |>
    left_join(
      wr_pbp_lookup,
      by = c("name_clean", "best_cfb_season" = "cfb_season",
             "school_norm_tmp" = "school_norm"),
      relationship = "many-to-one"
    ) |>
    select(-school_norm_tmp)

  matched_wr <- sum(!is.na(wr_features_raw$targets_pbp_wr))
  message(sprintf("WR PBP match: %d / %d WRs have PBP features (%.1f%%)",
                  matched_wr, nrow(wr_features_raw),
                  100 * matched_wr / nrow(wr_features_raw)))

  # Coverage on the post-2014 cohort (where PBP actually exists)
  modern_wr <- wr_features_raw |> filter(best_cfb_season >= 2014)
  matched_modern <- sum(!is.na(modern_wr$targets_pbp_wr))
  message(sprintf("  on 2014+ cohort: %d / %d (%.1f%%)",
                  matched_modern, nrow(modern_wr),
                  100 * matched_modern / pmax(nrow(modern_wr), 1)))
} else {
  warning("WR PBP cache not found — run 02c_build_wr_pbp_features.R first. ",
          "WR models will train without PBP features.")
  wr_features_raw <- wr_features_raw |>
    mutate(
      targets_pbp_wr = NA_real_, receptions_pbp_wr = NA_real_,
      rec_yards_pbp_wr = NA_real_,
      catch_rate_wr = NA_real_, yards_per_target_wr = NA_real_,
      yards_per_rec_wr = NA_real_, explosive_rec_rate = NA_real_,
      target_share_wr = NA_real_, targets_per_game_wr = NA_real_,
      epa_per_target_wr = NA_real_, epa_per_play_wr_pbp = NA_real_,
      share_targets_as_wr = NA_real_
    )
}

# ── 7. RB features ────────────────────────────────────────────────────────────
rb_draft <- filter(draft, position == "RB")

rb_rush_final <- rb_draft |>
  match_rush_best(rush) |>
  rename_with(~ paste0(.x, "_final"), any_of(c("school", "conference", "carries", "rush_yards", "rush_td", "rush_avg")))

# RB receiving: use RB/FB positions (not the default WR/TE filter!)
rb_recv_final <- rb_draft |>
  match_recv(recv, "final_cfb_season",
             cfb_positions = c("RB", "FB", "ATH", "APB")) |>
  select(gsis_id, rb_rec = rec, rb_rec_yards = rec_yards, rb_rec_td = rec_td)

# Actual final-season rushing stats (chronological, for YoY trend feature)
rb_actual_final <- rb_draft |>
  match_rush(rush, "final_cfb_season") |>
  select(gsis_id, rush_yards_actual_final = rush_yards)

rb_rush_penult <- rb_draft |>
  match_rush(rush, "penult_cfb_season") |>
  select(gsis_id, rush_yards_penult = rush_yards, carries_penult = carries,
         rush_td_penult = rush_td)

rb_rush_ante_penult <- rb_draft |>
  match_rush(rush, "ante_penult_cfb_season") |>
  select(gsis_id, rush_yards_ante = rush_yards)

rb_features_raw <- rb_rush_final |>
  left_join(rb_recv_final,        by = "gsis_id") |>
  left_join(rb_actual_final,      by = "gsis_id") |>
  left_join(rb_rush_penult,       by = "gsis_id") |>
  left_join(rb_rush_ante_penult,  by = "gsis_id") |>
  # Dominator rate: player's share of team rushing volume in best season
  left_join(
    team_rush_vol |> rename(school_final = team),
    by = c("best_cfb_season" = "cfb_season", "school_final")
  ) |>
  # Games played: regular-season games for player's team in their best season
  left_join(
    team_games |> rename(school_final = team),
    by = c("best_cfb_season" = "cfb_season", "school_final")
  ) |>
  # Usage rates (available 2016+)
  left_join(
    usage_lookup |> rename(school_final = school),
    by = c("name_clean", "best_cfb_season" = "cfb_season", "school_final")
  ) |>
  # PPA efficiency metrics (available 2016+)
  left_join(
    ppa_lookup |> rename(school_final = school),
    by = c("name_clean", "best_cfb_season" = "cfb_season", "school_final")
  ) |>
  # Recruiting rating — name-only join (see WR-side comment for rationale)
  mutate(name_clean_base = strip_suffix(name_clean)) |>
  left_join(
    rush_recruit |> select(-committed_to),
    by = c("name_clean_base" = "name_clean")
  ) |>
  mutate(
    tier                 = classify_tier(conference_final, school = coalesce(school_final, college)),
    ypc                  = rush_avg_final,
    has_penult           = as.integer(!is.na(rush_yards_penult)),
    # YoY uses chronological final-year vs penult-year (can be negative = declining)
    rush_yds_yoy         = rush_yards_actual_final - rush_yards_penult,   # NA when either missing
    recv_share           = coalesce(rb_rec_yards, 0) /
                           (rush_yards_final + coalesce(rb_rec_yards, 0) + .001),
    teammate_rush_yards  = pmax(team_rush_yards - rush_yards_final, 0),
    rush_td_rate         = rush_td_final / pmax(carries_final, 1),
    total_touches        = carries_final + coalesce(rb_rec, 0),
    # Composite scrimmage features (rush + receiving)
    scrimmage_yards      = rush_yards_final + coalesce(rb_rec_yards, 0),
    scrimmage_td         = rush_td_final + coalesce(rb_rec_td, 0),
    yards_per_touch      = scrimmage_yards / pmax(total_touches, 1),
    # Per-game rates (normalise for team schedule length)
    rush_yards_per_game  = rush_yards_final / team_games,
    carries_per_game     = carries_final    / team_games,
    scrimmage_yards_per_game = scrimmage_yards / team_games
  ) |>
  # Age-adjusted production & teammate context. Same logic as WR side: clear
  # ALL recruit features when college_years sanity check fails, so wrong-player
  # name matches don't leak rating/stars/rank into the training data.
  mutate(
    college_years_raw = draft_year - recruit_year,
    .recruit_valid    = !is.na(college_years_raw) &
                         college_years_raw >= 2 &
                         college_years_raw <= 6,
    college_years     = if_else(.recruit_valid, college_years_raw, NA_real_),
    recruit_rating    = if_else(.recruit_valid, recruit_rating, NA_real_),
    recruit_stars     = if_else(.recruit_valid, recruit_stars,  NA_integer_),
    recruit_rank      = if_else(.recruit_valid, recruit_rank,   NA_integer_),
    recruit_year      = if_else(.recruit_valid, recruit_year,   NA_integer_)
  ) |>
  select(-college_years_raw, -.recruit_valid) |>
  group_by(draft_year) |>
  mutate(age_relative = age - mean(age, na.rm = TRUE)) |>
  ungroup() |>
  left_join(skill_drafted_counts,
            by = c("pfr_player_name", "college", "draft_year")) |>
  left_join(elite_teammate_flag |> filter(position == "RB") |> select(-position),
            by = c("pfr_player_name", "college", "draft_year")) |>
  mutate(
    n_drafted_skill = coalesce(n_drafted_skill, 0L),
    elite_teammate  = coalesce(elite_teammate, 0L)
  ) |>
  select(-name_clean_base, -rush_yards_actual_final)

# ── 7b. RB PBP features (Tier 1 + EPA/play) ───────────────────────────────────
# Built by 02b_build_rb_pbp_features.R. Joins on (name_clean, school_norm, season).
# PBP data is only reliable from 2005+ → pre-2005 final-seasons will be NA.

pbp_path <- file.path(CACHE_DIR, "cfb_rb_pbp_features.rds")

if (file.exists(pbp_path)) {
  message("Joining PBP features from ", pbp_path)
  pbp_lookup <- readRDS(pbp_path) |>
    mutate(
      name_clean  = clean_name(player),
      school_norm = normalize_school(pos_team)
    ) |>
    select(
      name_clean, school_norm, cfb_season = season,
      # Rushing PBP
      carries_pbp = carries, rush_yards_pbp, ypc_pbp,
      carries_per_game_pbp, explosive_rate, breakaway_rate,
      epa_per_rush, total_epa_rush,
      # Receiving PBP
      targets_pbp, receptions_pbp, rec_yards_pbp,
      catch_rate, targets_per_game, target_share,
      epa_per_target, total_epa_target,
      # Combined
      epa_per_play_pbp, touches_pbp = touches
    )

  rb_features_raw <- rb_features_raw |>
    mutate(school_norm_tmp = normalize_school(school_final)) |>
    left_join(
      pbp_lookup,
      by = c("name_clean", "best_cfb_season" = "cfb_season",
             "school_norm_tmp" = "school_norm"),
      relationship = "many-to-one"
    ) |>
    select(-school_norm_tmp)

  matched <- sum(!is.na(rb_features_raw$carries_pbp) | !is.na(rb_features_raw$targets_pbp))
  message(sprintf("PBP match: %d / %d RBs have PBP features (%.1f%%)",
                  matched, nrow(rb_features_raw),
                  100 * matched / nrow(rb_features_raw)))
} else {
  warning("PBP cache not found — run 02b_build_rb_pbp_features.R first. ",
          "RB models will train without PBP features.")
  # Add empty columns so downstream scripts don't break.
  rb_features_raw <- rb_features_raw |>
    mutate(
      carries_pbp = NA_real_, rush_yards_pbp = NA_real_, ypc_pbp = NA_real_,
      carries_per_game_pbp = NA_real_,
      explosive_rate = NA_real_, breakaway_rate = NA_real_,
      epa_per_rush = NA_real_, total_epa_rush = NA_real_,
      targets_pbp = NA_real_, receptions_pbp = NA_real_, rec_yards_pbp = NA_real_,
      catch_rate = NA_real_, targets_per_game = NA_real_, target_share = NA_real_,
      epa_per_target = NA_real_, total_epa_target = NA_real_,
      epa_per_play_pbp = NA_real_, touches_pbp = NA_real_
    )
}

# ── 8. Save ───────────────────────────────────────────────────────────────────
saveRDS(wr_features_raw, "data/wr_features_raw.rds")
saveRDS(rb_features_raw, "data/rb_features_raw.rds")

message(sprintf("WR feature rows: %d  (matched final-season stats: %d)",
                nrow(wr_features_raw),
                sum(!is.na(wr_features_raw$rec_yards_final))))

message(sprintf("RB feature rows: %d  (matched final-season stats: %d)",
                nrow(rb_features_raw),
                sum(!is.na(rb_features_raw$rush_yards_final))))

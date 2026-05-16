#!/usr/bin/env Rscript
# inspect_player.R
# Small CLI tool to inspect ALL input data the model sees for a given player.
#
# Usage:
#   Rscript inspect_player.R "Ja'Marr Chase"
#   Rscript inspect_player.R "Chase" --year 2021
#   Rscript inspect_player.R "concepcion" --pos WR
#   Rscript inspect_player.R --list 2026           # list all players in a year
#   Rscript inspect_player.R --list WR 2025        # list WRs in a year
#
# Sources (merged by player name):
#   data/wr_model_data.rds   — training set 2002–2023 (hit targets + all features)
#   data/rb_model_data.rds   — training set 2002–2023
#   output/all_class_scores.rds — scored predictions 2021–2026 (model outputs)
#
# Output: grouped feature dump (identity / outcome / production /
#   advanced / combine / recruiting / era-flags / model predictions).

suppressPackageStartupMessages({
  library(tidyverse)
})

# ── Helpers ──────────────────────────────────────────────────────────────────

clean <- function(x) {
  x |>
    tolower() |>
    iconv(to = "ASCII//TRANSLIT") |>
    gsub("[^a-z0-9 ]", "", x = _) |>
    gsub("\\s+", " ", x = _) |>
    trimws()
}

fmt <- function(x) {
  if (is.null(x) || length(x) == 0) return("—")
  if (is.na(x)) return("NA")
  if (is.logical(x)) return(if (x) "TRUE" else "FALSE")
  if (is.numeric(x)) {
    if (x == round(x) && abs(x) < 1e6) return(format(x, big.mark = ","))
    return(sprintf("%.3f", x))
  }
  as.character(x)
}

kv <- function(label, val, width = 28) {
  cat(sprintf("  %-*s %s\n", width, paste0(label, ":"), fmt(val)))
}

section <- function(title) {
  cat(sprintf("\n── %s %s\n", title,
              strrep("─", max(0, 72 - nchar(title) - 4))))
}

# Print a grouped set of fields; skip any field absent from the row
group <- function(row, fields) {
  for (f in fields) {
    if (f %in% names(row)) kv(f, row[[f]])
  }
}

# ── Argument parsing ─────────────────────────────────────────────────────────

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0 || any(args %in% c("-h", "--help"))) {
  cat("Usage:\n")
  cat("  Rscript inspect_player.R \"<name>\" [--pos WR|RB] [--year YYYY]\n")
  cat("  Rscript inspect_player.R --list [WR|RB] <year>\n")
  quit(save = "no", status = 0)
}

pos_filter  <- NULL
year_filter <- NULL
do_list     <- FALSE
list_pos    <- NULL
list_year   <- NULL

# Pull flags first
if ("--list" %in% args) {
  do_list <- TRUE
  idx <- which(args == "--list")
  rest <- args[-idx]
  pos_candidates <- rest[rest %in% c("WR", "RB", "wr", "rb")]
  yr_candidates  <- suppressWarnings(as.integer(rest))
  yr_candidates  <- yr_candidates[!is.na(yr_candidates) & yr_candidates > 1900]
  list_pos  <- if (length(pos_candidates)) toupper(pos_candidates[1]) else NULL
  list_year <- if (length(yr_candidates))  yr_candidates[1] else NULL
} else {
  # Strip flag pairs
  if ("--pos" %in% args) {
    i <- which(args == "--pos")
    pos_filter <- toupper(args[i + 1])
    args <- args[-c(i, i + 1)]
  }
  if ("--year" %in% args) {
    i <- which(args == "--year")
    year_filter <- as.integer(args[i + 1])
    args <- args[-c(i, i + 1)]
  }
  if (length(args) == 0) {
    stop("Need a name after flags.")
  }
  query <- paste(args, collapse = " ")
}

# ── Load sources ─────────────────────────────────────────────────────────────

normalize <- function(df) {
  if ("round" %in% names(df)) df$round <- suppressWarnings(as.integer(as.character(df$round)))
  if ("pick"  %in% names(df)) df$pick  <- suppressWarnings(as.integer(as.character(df$pick)))
  df
}

wr_train <- readRDS("data/wr_model_data.rds") |>
  mutate(position = "WR", name = pfr_player_name,
         name_clean = clean(pfr_player_name), source = "training") |>
  normalize()
rb_train <- readRDS("data/rb_model_data.rds") |>
  mutate(position = "RB", name = pfr_player_name,
         name_clean = clean(pfr_player_name), source = "training") |>
  normalize()
scores <- tryCatch(
  readRDS("output/all_class_scores.rds") |>
    mutate(name_clean = clean(name), source = "scored") |>
    normalize(),
  error = function(e) NULL)

# ── --list mode ──────────────────────────────────────────────────────────────

if (do_list) {
  src <- if (!is.null(list_year) && list_year >= 2024 && !is.null(scores)) {
    scores
  } else {
    bind_rows(
      wr_train |> transmute(name, position, draft_year, round, pick, college, ppg),
      rb_train |> transmute(name, position, draft_year, round, pick, college, ppg)
    )
  }
  out <- src
  if (!is.null(list_year)) out <- out |> filter(draft_year == list_year)
  if (!is.null(list_pos))  out <- out |> filter(position == list_pos)

  cols <- intersect(c("name", "position", "college", "round", "pick",
                      "p_made_it", "exp_ppg", "ppg"), names(out))
  cat(sprintf("\n%d players matching: pos=%s year=%s\n\n",
              nrow(out), list_pos %||% "any", list_year %||% "any"))
  out |> arrange(pick) |> select(all_of(cols)) |> print(n = 200)
  quit(save = "no", status = 0)
}

# ── Search ───────────────────────────────────────────────────────────────────

q_clean <- clean(query)

search_in <- function(df) {
  hits <- df |> filter(grepl(q_clean, name_clean, fixed = TRUE))
  if (!is.null(pos_filter))  hits <- hits |> filter(position == pos_filter)
  if (!is.null(year_filter)) hits <- hits |> filter(draft_year == year_filter)
  hits
}

train_hits  <- bind_rows(search_in(wr_train), search_in(rb_train))
scored_hits <- if (!is.null(scores)) search_in(scores) else tibble()

n_hits <- nrow(train_hits) + nrow(scored_hits)

if (n_hits == 0) {
  cat("No matches. Try --list <year> to browse.\n")
  # Fuzzy suggestions
  all_names <- unique(c(wr_train$name, rb_train$name,
                        if (!is.null(scores)) scores$name))
  d <- adist(query, all_names, ignore.case = TRUE)[1, ]
  top <- all_names[order(d)][1:5]
  cat("Did you mean:\n")
  for (n in top) cat("  -", n, "\n")
  quit(save = "no", status = 1)
}

# Merge: training row wins for feature columns, but overlay model predictions
# + actuals from the scored row when present (training row has no p_made_it).
overlay_cols <- c("p_made_it", "exp_ppg",
                  "actual_ppg", "actual_raw_ppg", "actual_made_it")

merge_pair <- function(tr_row, sc_row) {
  for (c in overlay_cols) {
    if (c %in% names(sc_row) && !is.null(sc_row[[c]]) && !is.na(sc_row[[c]])) {
      tr_row[[c]] <- sc_row[[c]]
    }
  }
  tr_row
}

# Build master list keyed on (name_clean, position, draft_year)
train_keys  <- train_hits  |> mutate(k = paste(name_clean, position, draft_year))
scored_keys <- scored_hits |> mutate(k = paste(name_clean, position, draft_year))

merged_rows <- list()
for (k in unique(c(train_keys$k, scored_keys$k))) {
  tr <- train_keys  |> filter(.data$k == !!k)
  sc <- scored_keys |> filter(.data$k == !!k)
  if (nrow(tr) > 0) {
    merged_rows[[k]] <- if (nrow(sc) > 0) merge_pair(tr[1, ], sc[1, ]) else tr[1, ]
  } else {
    merged_rows[[k]] <- sc[1, ]
  }
}
combined <- bind_rows(merged_rows) |> select(-any_of("k"))

if (nrow(combined) > 1) {
  cat(sprintf("\n%d matches — pass --year / --pos to narrow, or be more specific:\n\n",
              nrow(combined)))
  combined |>
    mutate(yr = draft_year) |>
    select(name, position, yr, college, round, pick, source) |>
    arrange(yr) |>
    print(n = 30)
  quit(save = "no", status = 0)
}

row <- as.list(combined[1, ])

# ── Render ───────────────────────────────────────────────────────────────────

cat(sprintf("\n╔%s╗\n", strrep("═", 72)))
cat(sprintf("║ %-71s║\n", paste0(row$name, "  (",
                                  row$position, " · ",
                                  row$college %||% "—", " · ",
                                  row$draft_year, ")")))
cat(sprintf("╚%s╝\n", strrep("═", 72)))
cat(sprintf("  source: %s\n", row$source))

section("Identity / Draft")
group(row, c("pfr_player_name", "gsis_id", "position", "college", "tier",
             "draft_year", "round", "pick", "log_pick", "sqrt_pick", "age",
             "age_relative", "college_years", "n_drafted_skill",
             "elite_teammate"))

section("Outcome / Target (training only)")
group(row, c("made_it", "ppg", "raw_ppg", "weighted_ppg", "shrunk_ppg",
             "avg_top2_ppg", "n_qual_seasons", "total_top2_gms"))
# From scored-hits side
group(row, c("actual_ppg", "actual_raw_ppg", "actual_made_it"))

section("Model Predictions (scored only)")
group(row, c("p_made_it", "exp_ppg"))

if (row$position == "WR") {
  section("WR — Final-Season Production")
  group(row, c("rec_final", "rec_yards_final", "rec_td_final", "ypr",
               "rec_yards_per_game", "rec_per_game",
               "rec_td_rate", "dominator_rate", "best_season_is_final",
               "is_possession_wr"))

  section("WR — Penult / Ante / YoY")
  group(row, c("rec_penult", "rec_yards_penult", "rec_td_penult",
               "rec_yards_ante", "rec_yds_yoy", "has_penult"))

  section("WR — PBP (2014+)")
  group(row, c("catch_rate_wr", "yards_per_target_wr", "yards_per_rec_wr",
               "explosive_rec_rate", "target_share_wr", "targets_per_game_wr",
               "epa_per_target_wr", "epa_per_play_wr_pbp", "has_wr_pbp"))

  section("WR — PPA / Usage")
  group(row, c("usg_pass", "usg_passing_downs",
               "avg_PPA_pass", "total_PPA_pass",
               "has_ppa", "has_usage"))

  section("Context")
  group(row, c("teammate_rec_yards", "age_adj_yards"))

} else {
  section("RB — Final-Season Production")
  group(row, c("carries_final", "rush_yards_final", "rush_td_final", "ypc",
               "rb_rec", "rb_rec_yards", "rb_rec_td",
               "scrimmage_yards", "scrimmage_td", "yards_per_touch",
               "total_touches", "rush_yards_per_game", "carries_per_game",
               "scrimmage_yards_per_game",
               "rush_td_rate", "recv_share", "dominator_rate",
               "best_season_is_final", "is_scat_back"))

  section("RB — Penult / Ante / YoY")
  group(row, c("rush_yards_penult", "carries_penult", "rush_yards_ante",
               "rush_yds_yoy", "has_penult"))

  section("RB — PBP (2014+)")
  group(row, c("explosive_rate", "breakaway_rate",
               "target_share", "targets_per_game", "catch_rate",
               "epa_per_rush", "epa_per_play_pbp",
               "carries_per_game_pbp", "ypc_pbp", "has_pbp"))

  section("RB — PPA / Usage")
  group(row, c("usg_rush", "usg_passing_downs", "usg_overall", "usg_pass",
               "avg_PPA_rush", "total_PPA_rush",
               "avg_PPA_all", "total_PPA_all",
               "has_ppa", "has_usage"))

  section("Context")
  group(row, c("teammate_rush_yards"))
}

section("Combine")
group(row, c("height_in", "weight", "forty", "vertical", "broad_jump",
             "speed_score", "has_combine"))

section("Recruiting")
group(row, c("recruit_stars", "recruit_rating", "recruit_rank",
             "has_recruiting", "has_recruit_year"))

section("Data-Availability Flags")
flag_cols <- grep("^has_", names(row), value = TRUE)
group(row, flag_cols)

cat("\n")

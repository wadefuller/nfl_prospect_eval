# 02g_build_breakout_features.R
# ─────────────────────────────────────────────────────────────────────────────
# Per-prospect "breakout" features. Fantasy literature (notably Hayden Winks,
# JJ Zachariason, PFF) has consistently found that the AGE at which a player
# first hit a meaningful production threshold predicts NFL outcome above and
# beyond final-year volume. Younger breakouts → higher NFL success rate.
#
# This script computes, for every drafted WR/RB plus the upcoming class:
#   breakout_age           — youngest age at which a player crossed a
#                            position-specific threshold. NA if never.
#   peak_dominator_pre22   — max single-season team-share before age 22.
#   peak_yards_pre21       — max single-season yards (rec_yds or rush_yds)
#                            before age 21.
#   n_seasons_dominant     — count of seasons where the player was a team
#                            primary (dominator >= 0.20 WR / 0.25 RB).
#
# Thresholds (position-specific):
#   WR breakout: receiving dominator >= 0.20 in a single season
#   RB breakout: rushing  dominator >= 0.25 in a single season
#
# Age in cfb_season Y = draft_age − (draft_year − Y)
# Since cfbfastR's `year` reflects the fall semester (game year), this gives
# the player's age at the START of that college season (close enough for a
# breakout-age feature with quarter-year granularity).
#
# Outputs:
#   data/wr_breakout_features.rds
#   data/rb_breakout_features.rds
# ─────────────────────────────────────────────────────────────────────────────

setwd("~/Projects/R/college_nfl_model")
suppressMessages({
  library(tidyverse)
  library(nflreadr)
})
source("functions/helpers.R")

# Thresholds (frozen — same numbers as Winks/JJZ public work)
WR_DOMINATOR_BREAKOUT <- 0.20
RB_DOMINATOR_BREAKOUT <- 0.25
PRE22_AGE_CUTOFF      <- 22   # "before age 22" → age < 22
PRE21_AGE_CUTOFF      <- 21

# ── Pull draft-age data for every prospect we'll need (training + deploy) ────

draft_picks <- load_draft_picks() |>
  filter(position %in% c("WR", "RB")) |>
  transmute(
    name_clean = clean_name(pfr_player_name),
    name       = pfr_player_name,
    position,
    draft_year = season,
    draft_age  = age,
    college
  )

draft_2026 <- if (file.exists("data/draft_2026.csv")) {
  read_csv("data/draft_2026.csv", show_col_types = FALSE) |>
    filter(position %in% c("WR", "RB")) |>
    transmute(
      name_clean = clean_name(name),
      name,
      position,
      draft_year,
      draft_age  = age,
      college
    )
} else tibble()

draft_all <- bind_rows(draft_picks, draft_2026) |>
  distinct(name_clean, position, draft_year, .keep_all = TRUE)

cat(sprintf("Draft-age lookup: %d prospects (%d WR / %d RB)\n",
            nrow(draft_all),
            sum(draft_all$position == "WR"),
            sum(draft_all$position == "RB")))

# ── Raw season stats ────────────────────────────────────────────────────────

recv_raw <- readRDS("data/cfb_receiving_raw.rds") |> as_tibble()
rush_raw <- readRDS("data/cfb_rushing_raw.rds") |> as_tibble()

# ── Build per-(player, season) records with age + dominator ─────────────────

build_dominator_table <- function(stats_raw, draft_pos, kind = c("recv", "rush")) {
  kind <- match.arg(kind)
  yds_col  <- if (kind == "recv") "receiving_yds" else "rushing_yds"
  player_col <- "player"

  # Team totals per (team, cfb_season)
  team_totals <- stats_raw |>
    group_by(team, cfb_season) |>
    summarise(team_yards = sum(.data[[yds_col]], na.rm = TRUE), .groups = "drop")

  # Per-player rows (one per season they played, possibly multiple schools
  # for transfers; we keep all rows so a transfer can break out at school A
  # and still be matched at school B later).
  stats <- stats_raw |>
    filter(!str_detect(.data[[player_col]], "^#")) |>  # drop garbage rows
    transmute(
      name_clean = canonical_cfb_name(clean_name(.data[[player_col]])),
      school     = team,
      cfb_season = as.integer(cfb_season),
      yards      = coalesce(.data[[yds_col]], 0)
    ) |>
    # Suffix-strip on player side so "Brian Thomas Jr." entries merge with
    # the draft-side "brian thomas" key (which is also strip-suffixed below).
    mutate(name_clean = strip_suffix(name_clean)) |>
    # Within (name, school, season) keep highest yards (de-dupe + collapse
    # the suffix-stripped + original-named rows).
    group_by(name_clean, school, cfb_season) |>
    summarise(yards = max(yards, na.rm = TRUE), .groups = "drop") |>
    left_join(team_totals, by = c("school" = "team", "cfb_season")) |>
    mutate(
      dominator = if_else(coalesce(team_yards, 0) > 0,
                          yards / team_yards, NA_real_)
    )
  stats
}

cat("Building dominator tables...\n")
wr_seasons <- build_dominator_table(recv_raw, "WR", "recv")
rb_seasons <- build_dominator_table(rush_raw, "RB", "rush")

# ── Compute breakout features per (name, draft_year, position) ──────────────
# Strategy: for each prospect (key = name_clean × position × draft_year), pull
# all of their cfb seasons that occurred BEFORE draft_year. Compute age in
# each season from draft_age. Then summarise.

compute_breakout <- function(prospects, seasons, dom_threshold,
                              yards_threshold_pre21,
                              breakout_col_prefix = NULL) {
  prospects |>
    mutate(name_clean = strip_suffix(name_clean)) |>
    left_join(seasons, by = "name_clean", relationship = "many-to-many") |>
    filter(!is.na(cfb_season), cfb_season < draft_year) |>
    mutate(age_in_season = draft_age - (draft_year - cfb_season)) |>
    # Drop implausible ages (handles bad recruit-year matches / FCS namesakes)
    filter(age_in_season >= 17, age_in_season <= 25) |>
    group_by(name_clean, position, draft_year, draft_age) |>
    summarise(
      n_seasons          = n(),
      breakout_age       = {
        bk <- age_in_season[dominator >= dom_threshold & !is.na(dominator)]
        if (length(bk) == 0) NA_real_ else min(bk, na.rm = TRUE)
      },
      peak_dominator_pre22 = {
        v <- dominator[age_in_season < PRE22_AGE_CUTOFF & !is.na(dominator)]
        if (length(v) == 0) NA_real_ else max(v, na.rm = TRUE)
      },
      peak_yards_pre21    = {
        v <- yards[age_in_season < PRE21_AGE_CUTOFF & !is.na(yards)]
        if (length(v) == 0) NA_real_ else max(v, na.rm = TRUE)
      },
      n_seasons_dominant  = sum(dominator >= dom_threshold, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      # Encode "never broke out" as 23.0 (just past the pre-22 cutoff) so
      # XGBoost doesn't need to handle NA differently from "old breakout".
      # has_breakout flag preserves the missingness signal.
      has_breakout         = as.integer(!is.na(breakout_age)),
      breakout_age_imputed = if_else(is.na(breakout_age), 23.0, breakout_age)
    )
}

cat("Computing WR breakout features...\n")
wr_breakout <- compute_breakout(
  filter(draft_all, position == "WR"),
  wr_seasons,
  dom_threshold = WR_DOMINATOR_BREAKOUT,
  yards_threshold_pre21 = 800
)

cat("Computing RB breakout features...\n")
rb_breakout <- compute_breakout(
  filter(draft_all, position == "RB"),
  rb_seasons,
  dom_threshold = RB_DOMINATOR_BREAKOUT,
  yards_threshold_pre21 = 1000
)

# ── Coverage report ────────────────────────────────────────────────────────

report <- function(label, df) {
  cat(sprintf("\n── %s ──\n", label))
  cat(sprintf("  Rows                  : %d\n", nrow(df)))
  cat(sprintf("  has_breakout (any)    : %d (%.0f%%)\n",
              sum(df$has_breakout, na.rm = TRUE),
              100 * mean(df$has_breakout, na.rm = TRUE)))
  cat(sprintf("  breakout_age median   : %.1f\n",
              median(df$breakout_age, na.rm = TRUE)))
  cat(sprintf("  peak_dominator median : %.2f\n",
              median(df$peak_dominator_pre22, na.rm = TRUE)))
  cat(sprintf("  peak_yards_pre21 mdn  : %.0f\n",
              median(df$peak_yards_pre21, na.rm = TRUE)))
}

report("WR", wr_breakout)
report("RB", rb_breakout)

# ── Save ────────────────────────────────────────────────────────────────────

saveRDS(wr_breakout, "data/wr_breakout_features.rds")
saveRDS(rb_breakout, "data/rb_breakout_features.rds")
cat("\nSaved: data/wr_breakout_features.rds + data/rb_breakout_features.rds\n")

# ── Age-conditioned dominator curve ─────────────────────────────────────────
# For each integer age in [18, 24], compute the mean and SD of *final-season*
# dominator across all drafted players at that age. This is the population
# benchmark we subtract at attach time to get an age-adjusted residual.
# Frozen from the full training pool; deploy prospects re-use the same curve.

cat("\nFitting age dominator curves...\n")

build_age_curve <- function(prospects, seasons) {
  # Use the player's final-school best dominator as the "snapshot" dominator
  # the model already sees as `dominator_rate`. We approximate by their
  # highest single-season dominator from the seasons table (ignoring which
  # specific season — the model joins on best-season anyway).
  prospects |>
    mutate(name_clean = strip_suffix(name_clean)) |>
    left_join(seasons, by = "name_clean", relationship = "many-to-many") |>
    filter(!is.na(cfb_season), cfb_season < draft_year, !is.na(dominator)) |>
    group_by(name_clean, position, draft_year, draft_age) |>
    summarise(best_dom = max(dominator, na.rm = TRUE), .groups = "drop") |>
    mutate(age_bucket = pmax(18L, pmin(24L, as.integer(round(draft_age))))) |>
    group_by(age_bucket) |>
    summarise(
      n     = n(),
      mean  = mean(best_dom, na.rm = TRUE),
      sd    = sd(best_dom,   na.rm = TRUE),
      .groups = "drop"
    )
}

wr_curve_df <- build_age_curve(filter(draft_all, position == "WR"), wr_seasons)
rb_curve_df <- build_age_curve(filter(draft_all, position == "RB"), rb_seasons)

cat("\nWR age-dominator curve:\n"); print(wr_curve_df)
cat("\nRB age-dominator curve:\n"); print(rb_curve_df)

# Store as named vectors keyed by age (character keys to handle string lookup)
make_curve_obj <- function(d) {
  list(
    mean = setNames(d$mean, as.character(d$age_bucket)),
    sd   = setNames(d$sd,   as.character(d$age_bucket)),
    n    = setNames(d$n,    as.character(d$age_bucket))
  )
}
saveRDS(make_curve_obj(wr_curve_df), "data/wr_age_dom_curve.rds")
saveRDS(make_curve_obj(rb_curve_df), "data/rb_age_dom_curve.rds")
cat("\nSaved: data/wr_age_dom_curve.rds + data/rb_age_dom_curve.rds\n")

# ── Quick sanity check on known prospects ──────────────────────────────────

cat("\n── Sanity check (known prospects) ──\n")
checks <- list(
  c("WR", "Ja'Marr Chase", 2021),
  c("WR", "Justin Jefferson", 2020),
  c("WR", "Marvin Harrison Jr.", 2024),
  c("WR", "Travis Hunter", 2025),
  c("RB", "Saquon Barkley", 2018),
  c("RB", "Ashton Jeanty", 2025),
  c("RB", "Jeremiyah Love", 2026),
  c("RB", "Bijan Robinson", 2023)
)
for (c_ in checks) {
  tbl <- if (c_[1] == "WR") wr_breakout else rb_breakout
  nm <- strip_suffix(clean_name(c_[2]))
  hit <- tbl |> filter(name_clean == nm, draft_year == as.integer(c_[3]))
  if (nrow(hit) == 0) {
    cat(sprintf("  %-22s %d: NOT FOUND\n", c_[2], as.integer(c_[3])))
  } else {
    r <- hit[1, ]
    cat(sprintf("  %-22s %d: breakout_age=%-4s peak_dom=%.2f peak_yds_pre21=%-5.0f n_dom=%d\n",
                c_[2], as.integer(c_[3]),
                ifelse(is.na(r$breakout_age), "—", sprintf("%.1f", r$breakout_age)),
                r$peak_dominator_pre22 %||% NA_real_,
                r$peak_yards_pre21 %||% NA_real_,
                r$n_seasons_dominant))
  }
}

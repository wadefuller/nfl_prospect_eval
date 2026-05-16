# 09_prospect_profiles.R
# ─────────────────────────────────────────────────────────────────────────────
# Generates prospect score, archetype, and scouting blurb for each prospect.
#
# Prospect Score (0-100): composite of model predictions + comp outcomes
#   - 40% p_made_it (scaled to 0-100)
#   - 35% exp_ppg (percentile among all prospects in same position)
#   - 15% comp_weighted_ppg (percentile)
#   - 10% (1 - comp_bust_rate) * 100
#
# Archetype: rule-based classification from measurables + production profile
#   WR: Alpha, Deep Threat, Possession, Slot, Big-Play, Contested Catch, Raw Athlete
#   RB: Workhorse, Speed Back, Power Back, Receiving Back, All-Purpose
#
# Blurb: programmatic scouting report from data signals
#
# Outputs:
#   output/prospect_profiles.csv — score, archetype, blurb per prospect
# ─────────────────────────────────────────────────────────────────────────────

setwd("~/Projects/R/college_nfl_model")
library(tidyverse)
library(nflreadr)

source("functions/helpers.R")

# ── 1. Load data ─────────────────────────────────────────────────────────────

scores  <- read_csv("output/all_class_scores.csv", show_col_types = FALSE)
summary <- read_csv("output/player_comp_summary.csv", show_col_types = FALSE)
comps   <- read_csv("output/player_comps.csv", show_col_types = FALSE)

wr_train <- readRDS("data/wr_model_data.rds")
rb_train <- readRDS("data/rb_model_data.rds")

combine_all <- load_combine() |>
  filter(pos %in% c("WR", "RB")) |>
  mutate(
    name_clean = strip_suffix(clean_name(player_name)),
    height_in  = height_to_inches(ht),
    draft_year = season
  ) |>
  select(name_clean, draft_year, pos, height_in, weight = wt,
         forty, vertical, broad_jump)

# Also get combine from training data (already has it)
train_combine_wr <- wr_train |>
  mutate(name_clean = strip_suffix(clean_name(pfr_player_name))) |>
  select(name_clean, draft_year, height_in, weight, forty, vertical, broad_jump,
         rec_yards_final, rec_final, rec_td_final, ypr, rec_td_rate,
         rec_yards_penult, recruit_stars, recruit_rating, age,
         usg_pass)

train_combine_rb <- rb_train |>
  mutate(name_clean = strip_suffix(clean_name(pfr_player_name))) |>
  select(name_clean, draft_year, height_in, weight, forty, vertical, broad_jump,
         rush_yards_final, carries_final, rush_td_final, ypc,
         rb_rec, rb_rec_yards, recv_share, yards_per_touch,
         scrimmage_yards, recruit_stars, recruit_rating, age,
         usg_rush)

# Get cfb stats for all prospects
wr_features_raw <- readRDS("data/wr_features_raw.rds")
rb_features_raw <- readRDS("data/rb_features_raw.rds")

# ── 2. Build unified prospect table with all features ────────────────────────

# Merge scores + comp summary
merged <- scores |>
  left_join(
    summary |> select(name, position, draft_year,
                       comp_weighted_ppg, comp_median_ppg,
                       comp_bust_rate, comp_names),
    by = c("name", "position", "draft_year")
  )

# For each prospect, find their measurables
# First check training data, then combine data
get_measurables <- function(name, pos, year) {
  nc <- strip_suffix(clean_name(name))

  if (pos == "WR") {
    row <- train_combine_wr |>
      filter(name_clean == nc, draft_year == year)
    if (nrow(row) == 0) {
      row <- train_combine_wr |> filter(name_clean == nc) |> slice(1)
    }
  } else {
    row <- train_combine_rb |>
      filter(name_clean == nc, draft_year == year)
    if (nrow(row) == 0) {
      row <- train_combine_rb |> filter(name_clean == nc) |> slice(1)
    }
  }

  # If still not found, try combine data
  if (nrow(row) == 0) {
    row <- combine_all |>
      filter(name_clean == nc, draft_year == year)
    if (nrow(row) == 0) {
      row <- combine_all |> filter(name_clean == nc) |> slice(1)
    }
  }

  if (nrow(row) == 0) return(NULL)
  row[1, ]
}

# Build feature table for all prospects
prospect_features <- merged |>
  rowwise() |>
  mutate(
    measurables = list(get_measurables(name, position, draft_year))
  ) |>
  ungroup()

# Extract measurables columns
prospect_features <- prospect_features |>
  mutate(
    height_in  = map_dbl(measurables, ~ if (!is.null(.x) && "height_in" %in% names(.x)) as.numeric(.x$height_in) else NA_real_),
    weight     = map_dbl(measurables, ~ if (!is.null(.x) && "weight" %in% names(.x)) as.numeric(.x$weight) else NA_real_),
    forty      = map_dbl(measurables, ~ if (!is.null(.x) && "forty" %in% names(.x)) as.numeric(.x$forty) else NA_real_),
    vertical   = map_dbl(measurables, ~ if (!is.null(.x) && "vertical" %in% names(.x)) as.numeric(.x$vertical) else NA_real_),
    broad_jump = map_dbl(measurables, ~ if (!is.null(.x) && "broad_jump" %in% names(.x)) as.numeric(.x$broad_jump) else NA_real_)
  )

# Get additional WR/RB production features not in scores
prospect_features <- prospect_features |>
  mutate(
    ypr = map_dbl(measurables, ~ if (!is.null(.x) && "ypr" %in% names(.x)) as.numeric(.x$ypr) else NA_real_),
    rec_td_rate = map_dbl(measurables, ~ if (!is.null(.x) && "rec_td_rate" %in% names(.x)) as.numeric(.x$rec_td_rate) else NA_real_),
    recruit_stars = map_dbl(measurables, ~ if (!is.null(.x) && "recruit_stars" %in% names(.x)) as.numeric(.x$recruit_stars) else NA_real_),
    # RB-specific
    ypc = map_dbl(measurables, ~ if (!is.null(.x) && "ypc" %in% names(.x)) as.numeric(.x$ypc) else NA_real_),
    recv_share = map_dbl(measurables, ~ if (!is.null(.x) && "recv_share" %in% names(.x)) as.numeric(.x$recv_share) else NA_real_),
    yards_per_touch = map_dbl(measurables, ~ if (!is.null(.x) && "yards_per_touch" %in% names(.x)) as.numeric(.x$yards_per_touch) else NA_real_),
    rb_rec = map_dbl(measurables, ~ if (!is.null(.x) && "rb_rec" %in% names(.x)) as.numeric(.x$rb_rec) else NA_real_),
    rb_rec_yards = map_dbl(measurables, ~ if (!is.null(.x) && "rb_rec_yards" %in% names(.x)) as.numeric(.x$rb_rec_yards) else NA_real_)
  ) |>
  select(-measurables)

# For prospects not in training data, get production from cfb features
# WR
wr_cfb <- wr_features_raw |>
  mutate(name_clean = strip_suffix(clean_name(pfr_player_name))) |>
  select(name_clean, draft_year, ypr_cfb = ypr, rec_td_rate_cfb = rec_td_rate,
         rec_yards_penult, recruit_stars_cfb = recruit_stars)

prospect_features <- prospect_features |>
  mutate(name_clean = strip_suffix(clean_name(name))) |>
  left_join(wr_cfb, by = c("name_clean", "draft_year")) |>
  mutate(
    ypr = coalesce(ypr, ypr_cfb),
    rec_td_rate = coalesce(rec_td_rate, rec_td_rate_cfb),
    recruit_stars = coalesce(recruit_stars, recruit_stars_cfb)
  ) |>
  select(-ypr_cfb, -rec_td_rate_cfb, -recruit_stars_cfb, -name_clean)

# RB
rb_cfb <- rb_features_raw |>
  mutate(name_clean = strip_suffix(clean_name(pfr_player_name))) |>
  select(name_clean, draft_year, ypc_cfb = ypc,
         recv_share_cfb = recv_share, yards_per_touch_cfb = yards_per_touch,
         rb_rec_cfb = rb_rec, rb_rec_yards_cfb = rb_rec_yards,
         recruit_stars_cfb2 = recruit_stars)

prospect_features <- prospect_features |>
  mutate(name_clean = strip_suffix(clean_name(name))) |>
  left_join(rb_cfb, by = c("name_clean", "draft_year")) |>
  mutate(
    ypc = coalesce(ypc, ypc_cfb),
    recv_share = coalesce(recv_share, recv_share_cfb),
    yards_per_touch = coalesce(yards_per_touch, yards_per_touch_cfb),
    rb_rec = coalesce(rb_rec, rb_rec_cfb),
    rb_rec_yards = coalesce(rb_rec_yards, rb_rec_yards_cfb),
    recruit_stars = coalesce(recruit_stars, recruit_stars_cfb2)
  ) |>
  select(-ends_with("_cfb"), -ends_with("_cfb2"), -name_clean)

cat("Prospect features built:", nrow(prospect_features), "prospects\n")
cat("  With height:", sum(!is.na(prospect_features$height_in)), "\n")
cat("  With forty:", sum(!is.na(prospect_features$forty)), "\n")
cat("  With ypr (WR):", sum(!is.na(prospect_features$ypr) & prospect_features$position == "WR"), "\n")
cat("  With ypc (RB):", sum(!is.na(prospect_features$ypc) & prospect_features$position == "RB"), "\n")

# ── 3. Prospect Score (0-100) ────────────────────────────────────────────────
# Composite: model upside + ceiling estimate + comp validation.
#
# Recalibrated 2026-05-10 against the 5-bucket Bayesian ensemble. On the
# 2014-2023 cohort with mature outcomes, signal correlations to actual NFL
# PPG are:
#   upside (p_elite + p_league_winner)   r = +0.663  ← strongest
#   exp_ppg (the blended hurdle+bucket)  r = +0.647
#   p_league_winner                      r = +0.608
#   comp_weighted_ppg                    r = +0.553
#   bust_safety (1 - p_bust)             r = +0.521
#   p_made_it (hurdle classifier)        r = +0.495  ← weakest
#
# The old formula put 40% weight on hit_score (derived from p_made_it),
# which is the LEAST predictive signal. The new formula leads with the
# strongest signal (upside), keeps the now-blended exp_ppg, and falls back
# on the comp stack for second-opinion validation.
#
# For prospects with no bucket data (e.g. very old training rows from
# before the bucket model existed), we fall back to the hurdle-based score.

compute_prospect_score <- function(df) {
  df |>
    group_by(position) |>
    mutate(
      # Component 1 — Upside: how much probability does the model put on the
      # top-two outcome tiers (elite + league_winner)?
      upside_score   = (coalesce(p_elite, 0) + coalesce(p_league_winner, 0)) * 100,
      upside_pctile  = percent_rank(upside_score) * 100,

      # Component 2 — Expected PPG percentile (now uses the ensemble exp_ppg
      # which already blends hurdle + bucket).
      ppg_pctile     = percent_rank(exp_ppg) * 100,

      # Component 3 — Bust safety: complement of bucket bust probability.
      bust_safety    = (1 - coalesce(p_bust, 0.3)) * 100,

      # Component 4 — Comp-stack percentile (second-opinion via kNN over
      # historical NFL outcomes).
      comp_pctile    = percent_rank(coalesce(comp_weighted_ppg, 0)) * 100,

      # Component 5 — Comp bust safety (inverse bust rate from kNN comps).
      comp_safety    = (1 - coalesce(comp_bust_rate, 0.5)) * 100,

      # Hurdle hit_score retained as fallback for rows missing bucket data.
      hit_score_fb   = pmin(100, pmax(0, (p_made_it - 0.5) / 0.5 * 100)),

      prospect_score = round(
        if_else(
          !is.na(p_elite) & !is.na(p_league_winner) & !is.na(p_bust),
          # New formula — bucket-aware
          0.35 * upside_pctile +
          0.30 * ppg_pctile +
          0.15 * bust_safety +
          0.10 * comp_pctile +
          0.10 * comp_safety,
          # Fallback for prospects without bucket data
          0.40 * hit_score_fb +
          0.35 * ppg_pctile +
          0.15 * comp_pctile +
          0.10 * comp_safety
        )
      )
    ) |>
    ungroup()
}

prospect_features <- compute_prospect_score(prospect_features)

cat("\nProspect score distribution:\n")
prospect_features |>
  group_by(position) |>
  summarize(
    min = min(prospect_score),
    p25 = quantile(prospect_score, 0.25),
    median = median(prospect_score),
    p75 = quantile(prospect_score, 0.75),
    max = max(prospect_score)
  ) |> print()

# ── 4. Archetype Classification ──────────────────────────────────────────────

# WR archetypes based on measurables + production profile
classify_wr_archetype <- function(height_in, weight, forty, ypr,
                                  rec_yards_final, rec_final, rec_td_rate,
                                  pick) {
  tall <- !is.na(height_in) && height_in >= 74       # 6'2"+
  big  <- !is.na(height_in) && height_in >= 73       # 6'1"+
  small <- !is.na(height_in) && height_in <= 71      # 5'11" or shorter
  heavy <- !is.na(weight) && weight >= 215
  fast <- !is.na(forty) && forty <= 4.42
  very_fast <- !is.na(forty) && forty <= 4.38
  big_play <- !is.na(ypr) && ypr >= 16.5
  deep_play <- !is.na(ypr) && ypr >= 18
  volume <- !is.na(rec_final) && rec_final >= 70
  high_td <- !is.na(rec_td_rate) && rec_td_rate >= 0.12
  big_producer <- !is.na(rec_yards_final) && rec_yards_final >= 1100
  solid_producer <- !is.na(rec_yards_final) && rec_yards_final >= 800
  elite_capital <- !is.na(pick) && pick <= 15

  # ── Tier 1: clear archetype from measurables + production
  if (tall && fast && big_producer) return("Alpha WR")
  if (big && big_producer && high_td && elite_capital) return("Alpha WR")
  if (tall && heavy && big_play) return("Contested Catch")
  if (tall && heavy && solid_producer) return("Contested Catch")
  if (very_fast && deep_play) return("Deep Threat")
  if (fast && big_play && !volume) return("Deep Threat")
  if (small && volume) return("Slot WR")

  # ── Tier 2: production-driven (when measurables missing or average)
  if (fast && big_producer) return("Speed WR")
  if (very_fast && solid_producer) return("Speed WR")
  if (volume && !big_play) return("Route Runner")
  if (big_producer && high_td) return("High-Volume WR")
  if (big_producer) return("High-Volume WR")

  # ── Tier 3: measurables-driven fallbacks
  if (big_play) return("Big-Play WR")
  if (very_fast) return("Speed WR")
  if (fast) return("Speed WR")
  if (tall && heavy) return("Contested Catch")
  if (tall) return("Contested Catch")

  # ── Tier 4: production-only fallbacks (no combine data)
  if (solid_producer && !is.na(ypr) && ypr >= 15) return("Big-Play WR")
  if (solid_producer && volume) return("Route Runner")
  if (solid_producer) return("High-Volume WR")
  if (volume) return("Route Runner")
  if (!is.na(ypr) && ypr >= 17) return("Big-Play WR")
  if (high_td) return("Big-Play WR")

  # Fallback based on draft capital
  if (elite_capital) return("Alpha WR")
  if (!is.na(pick) && pick <= 40) return("High-Upside WR")
  "Developmental WR"
}

# RB archetypes
classify_rb_archetype <- function(height_in, weight, forty, ypc,
                                  rush_yards_final, carries_final,
                                  recv_share, rb_rec_yards, pick) {
  heavy <- !is.na(weight) && weight >= 220
  big <- !is.na(weight) && weight >= 210
  # 4.42 is a meaningful speed threshold for RBs; 4.45 is merely average NFL-caliber
  fast <- !is.na(forty) && forty <= 4.42
  very_fast <- !is.na(forty) && forty <= 4.38
  pass_catcher <- !is.na(recv_share) && recv_share >= 0.25
  decent_receiver <- (!is.na(rb_rec_yards) && rb_rec_yards >= 200) ||
                     (!is.na(recv_share) && recv_share >= 0.15)
  high_volume <- !is.na(carries_final) && carries_final >= 200
  moderate_volume <- !is.na(carries_final) && carries_final >= 150
  explosive <- !is.na(ypc) && ypc >= 6.0
  very_explosive <- !is.na(ypc) && ypc >= 7.0
  big_producer <- !is.na(rush_yards_final) && rush_yards_final >= 1200
  solid_producer <- !is.na(rush_yards_final) && rush_yards_final >= 900

  # ── Tier 1: clear archetype
  # Three-Down Back: proven rusher who can also catch — no pick gate (that's draft value, not archetype)
  if (solid_producer && pass_catcher && (big_producer || decent_receiver)) return("Three-Down Back")
  if (heavy && high_volume && big_producer) return("Workhorse")
  if (very_fast && explosive && big_producer) return("Explosive Back")
  if (very_fast && explosive) return("Speed Back")
  if (heavy && big_producer) return("Power Back")

  # ── Tier 2: production-driven
  if (big_producer && decent_receiver) return("All-Purpose Back")
  if (big_producer && high_volume) return("Workhorse")
  if (fast && big_producer) return("Speed Back")
  if (pass_catcher && solid_producer) return("All-Purpose Back")
  if (pass_catcher) return("Receiving Back")
  if (explosive && big_producer) return("Big-Play Back")

  # ── Tier 3: fallbacks
  if (heavy && moderate_volume) return("Power Back")
  if (fast && solid_producer) return("Speed Back")
  # Speed alone without production is upside, not a proven archetype
  if (fast && decent_receiver) return("All-Purpose Back")
  if (big_producer) return("Workhorse")
  if (solid_producer && moderate_volume) return("Workhorse")
  if (solid_producer) return("Workhorse")
  if (very_explosive) return("Big-Play Back")
  if (decent_receiver) return("All-Purpose Back")
  if (high_volume) return("Workhorse")

  # Final: draft capital signals value even without a clear production signature
  if (!is.na(pick) && pick <= 64) return("High-Upside Back")
  "Developmental Back"
}

# Apply archetypes
prospect_features <- prospect_features |>
  mutate(
    archetype = case_when(
      position == "WR" ~ pmap_chr(
        list(height_in, weight, forty, ypr,
             rec_yards_final, rec_final, rec_td_rate, pick),
        classify_wr_archetype
      ),
      position == "RB" ~ pmap_chr(
        list(height_in, weight, forty, ypc,
             rush_yards_final, carries_final,
             recv_share, rb_rec_yards, pick),
        classify_rb_archetype
      )
    )
  )

cat("\nArchetype distribution:\n")
prospect_features |> count(position, archetype) |> print(n = 30)

# ── 5. Scouting Blurb Generation ─────────────────────────────────────────────
# Single function handles both WR and RB; position-specific branches are
# limited to production stats, efficiency metrics, and measurable thresholds.

generate_blurb <- function(name, college, pick, round, tier,
                           p_made_it, exp_ppg,
                           comp_weighted_ppg, comp_bust_rate,
                           recruit_stars, comp_names,
                           prospect_score, archetype, actual_ppg,
                           position,
                           # WR-specific
                           rec_yards_final = NA, ypr = NA, rec_td_rate = NA,
                           height_in = NA,
                           # RB-specific
                           rush_yards_final = NA, ypc = NA,
                           recv_share = NA, rb_rec_yards = NA,
                           # shared measurables
                           weight = NA, forty = NA) {
  bulls <- c()
  bears <- c()

  # Draft capital
  if (!is.na(pick) && pick <= 15) {
    bulls <- c(bulls, sprintf("top-15 pick (Rd %d, #%d)", round, pick))
  } else if (!is.na(pick) && pick <= 32) {
    if (position == "WR") {
      bulls <- c(bulls, sprintf("first-round capital (Rd %d, #%d)", round, pick))
    } else if (pick <= 40) {
      bulls <- c(bulls, sprintf("strong draft capital (Rd %d, #%d)", round, pick))
    }
  } else {
    late_threshold <- if (position == "WR") 60 else 80
    late_label     <- if (position == "WR") "late draft capital limits upside ceiling" else "late draft capital limits opportunity"
    if (!is.na(pick) && pick >= late_threshold) bears <- c(bears, late_label)
  }

  # Position-specific production + efficiency
  if (position == "WR") {
    if (!is.na(rec_yards_final) && rec_yards_final >= 1200) {
      bulls <- c(bulls, sprintf("elite %d-yard college season", as.integer(rec_yards_final)))
    } else if (!is.na(rec_yards_final) && rec_yards_final >= 800) {
      bulls <- c(bulls, sprintf("strong %d-yard college season", as.integer(rec_yards_final)))
    } else if (!is.na(rec_yards_final) && rec_yards_final > 0 && rec_yards_final < 600) {
      bears <- c(bears, sprintf("modest college production (%d rec yards)", as.integer(rec_yards_final)))
    }
    if (!is.na(ypr) && ypr >= 17) {
      bulls <- c(bulls, sprintf("explosive %.1f yards per reception", ypr))
    } else if (!is.na(ypr) && ypr < 12) {
      bears <- c(bears, sprintf("low %.1f YPR suggests limited big-play ability", ypr))
    }
    if (!is.na(rec_td_rate) && rec_td_rate >= 0.15) {
      bulls <- c(bulls, "elite touchdown rate in college")
    }
  } else {
    if (!is.na(rush_yards_final) && rush_yards_final >= 1400) {
      bulls <- c(bulls, sprintf("dominant %d-yard rushing season", as.integer(rush_yards_final)))
    } else if (!is.na(rush_yards_final) && rush_yards_final >= 1000) {
      bulls <- c(bulls, sprintf("strong %d-yard rushing season", as.integer(rush_yards_final)))
    } else if (!is.na(rush_yards_final) && rush_yards_final < 700) {
      bears <- c(bears, sprintf("limited college rushing production (%d yards)", as.integer(rush_yards_final)))
    }
    if (!is.na(ypc) && ypc >= 6.5) {
      bulls <- c(bulls, sprintf("explosive %.1f YPC", ypc))
    } else if (!is.na(ypc) && ypc < 5.0) {
      bears <- c(bears, sprintf("below-average %.1f YPC", ypc))
    }
    if (!is.na(recv_share) && recv_share >= 0.25) {
      bulls <- c(bulls, "proven pass-catching ability")
    } else if (!is.na(rb_rec_yards) && rb_rec_yards >= 300) {
      bulls <- c(bulls, sprintf("%d receiving yards show versatility", as.integer(rb_rec_yards)))
    }
  }

  # Measurables (position-specific thresholds)
  if (position == "WR") {
    if (!is.na(height_in) && !is.na(forty)) {
      if (height_in >= 74 && forty <= 4.45) {
        bulls <- c(bulls, sprintf("rare size-speed combo (%d\", %.2fs)", height_in, forty))
      } else if (forty <= 4.38) {
        bulls <- c(bulls, sprintf("elite speed (%.2fs forty)", forty))
      } else if (height_in >= 75) {
        bulls <- c(bulls, sprintf("imposing %d\" frame", height_in))
      }
    } else if (!is.na(forty) && forty <= 4.38) {
      bulls <- c(bulls, sprintf("elite speed (%.2fs forty)", forty))
    } else if (!is.na(height_in) && height_in >= 75) {
      bulls <- c(bulls, sprintf("imposing %d\" frame", height_in))
    }
    if (!is.na(forty) && forty >= 4.58) bears <- c(bears, sprintf("concerning %.2fs forty", forty))
  } else {
    if (!is.na(weight) && !is.na(forty)) {
      if (weight >= 220 && forty <= 4.45) {
        bulls <- c(bulls, sprintf("rare size-speed combo (%d lbs, %.2fs)", as.integer(weight), forty))
      } else if (forty <= 4.40) {
        bulls <- c(bulls, sprintf("elite speed (%.2fs forty)", forty))
      }
    } else if (!is.na(forty) && forty <= 4.40) {
      bulls <- c(bulls, sprintf("elite speed (%.2fs forty)", forty))
    }
    if (!is.na(forty) && forty >= 4.58) bears <- c(bears, sprintf("slow %.2fs forty", forty))
  }

  # Recruiting pedigree (shared)
  if (!is.na(recruit_stars) && recruit_stars >= 5) {
    bulls <- c(bulls, "5-star recruit pedigree")
  } else if (!is.na(recruit_stars) && recruit_stars >= 4) {
    bulls <- c(bulls, "4-star recruit pedigree")
  }

  # Conference (shared)
  if (!is.na(tier) && tier == "G5") bears <- c(bears, "G5 competition level")

  # Model signals (shared)
  if (!is.na(p_made_it) && p_made_it >= 0.90) {
    bulls <- c(bulls, sprintf("%.0f%% model hit probability", p_made_it * 100))
  } else if (!is.na(p_made_it) && p_made_it >= 0.85) {
    bulls <- c(bulls, sprintf("high model confidence (%.0f%%)", p_made_it * 100))
  } else if (!is.na(p_made_it) && p_made_it < 0.80) {
    bears <- c(bears, sprintf("only %.0f%% model hit probability", p_made_it * 100))
  }

  # Comp signals (position-specific PPG thresholds)
  if (!is.na(comp_bust_rate) && comp_bust_rate >= 0.4) {
    bears <- c(bears, sprintf("%.0f%% of comps busted", comp_bust_rate * 100))
  } else if (!is.na(comp_bust_rate) && comp_bust_rate == 0) {
    bulls <- c(bulls, "zero busts among comps")
  }
  comp_high <- if (position == "WR") 10 else 12
  comp_low  <- if (position == "WR")  5 else  6
  if (!is.na(comp_weighted_ppg) && comp_weighted_ppg >= comp_high) {
    bulls <- c(bulls, sprintf("comps averaged %.1f PPG", comp_weighted_ppg))
  } else if (!is.na(comp_weighted_ppg) && comp_weighted_ppg < comp_low) {
    bears <- c(bears, sprintf("comps averaged only %.1f PPG", comp_weighted_ppg))
  }

  # ── Fallback: ensure at least one point in each category ─────────────────
  if (length(bulls) == 0) {
    if (!is.na(comp_weighted_ppg) && comp_weighted_ppg >= 7) {
      bulls <- c(bulls, sprintf("comps averaged %.1f PPG", comp_weighted_ppg))
    } else if (!is.na(p_made_it) && p_made_it >= 0.80) {
      bulls <- c(bulls, sprintf("%.0f%% model hit probability", p_made_it * 100))
    } else if (!is.na(pick)) {
      bulls <- c(bulls, sprintf("Rd %d pick", round))
    }
  }
  if (length(bears) == 0) {
    if (!is.na(comp_weighted_ppg) && comp_weighted_ppg < 7 && !is.na(comp_weighted_ppg)) {
      bears <- c(bears, sprintf("comps averaged only %.1f PPG", comp_weighted_ppg))
    } else if (!is.na(p_made_it) && p_made_it < 0.85) {
      bears <- c(bears, sprintf("only %.0f%% model hit probability", p_made_it * 100))
    } else if (!is.na(pick) && pick > 32) {
      bears <- c(bears, sprintf("Rd %d capital limits opportunity", round))
    }
  }

  # Capitalise first letter of each point, trim to top 3
  cap_first <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  bulls <- cap_first(bulls[1:min(3, length(bulls))])
  bears <- cap_first(bears[1:min(3, length(bears))])

  list(bullish = bulls, bearish = bears)
}

# Apply blurb generation — returns list(bullish, bearish) per player
blurb_results <- pmap(
  list(name = prospect_features$name,
       college = prospect_features$college,
       pick = prospect_features$pick,
       round = prospect_features$round,
       tier = prospect_features$tier,
       p_made_it = prospect_features$p_made_it,
       exp_ppg = prospect_features$exp_ppg,
       comp_weighted_ppg = prospect_features$comp_weighted_ppg,
       comp_bust_rate = prospect_features$comp_bust_rate,
       recruit_stars = prospect_features$recruit_stars,
       comp_names = prospect_features$comp_names,
       prospect_score = prospect_features$prospect_score,
       archetype = prospect_features$archetype,
       actual_ppg = prospect_features$actual_ppg,
       position = prospect_features$position,
       rec_yards_final = prospect_features$rec_yards_final,
       ypr = prospect_features$ypr,
       rec_td_rate = prospect_features$rec_td_rate,
       height_in = prospect_features$height_in,
       rush_yards_final = prospect_features$rush_yards_final,
       ypc = prospect_features$ypc,
       recv_share = prospect_features$recv_share,
       rb_rec_yards = prospect_features$rb_rec_yards,
       weight = prospect_features$weight,
       forty = prospect_features$forty),
  generate_blurb
)

prospect_features <- prospect_features |>
  mutate(
    bullish = map(blurb_results, "bullish"),
    bearish = map(blurb_results, "bearish")
  )

# ── 6. Output ────────────────────────────────────────────────────────────────

output <- prospect_features |>
  mutate(
    # Flat string for CSV (semicolon-separated, label prefix)
    blurb = paste(
      if_else(lengths(bullish) > 0, paste0("Bullish: ", map_chr(bullish, ~ paste(.x, collapse = "; "))), ""),
      if_else(lengths(bearish) > 0, paste0("Bearish: ", map_chr(bearish, ~ paste(.x, collapse = "; "))), ""),
      sep = " "
    ) |> str_squish()
  ) |>
  select(name, position, draft_year, round, pick, college, tier,
         prospect_score, archetype, blurb, bullish, bearish,
         height_in, weight, forty,
         p_made_it, exp_ppg, comp_weighted_ppg, comp_bust_rate)

# CSV gets the flat blurb string only (list cols don't write to CSV cleanly)
output |> select(-bullish, -bearish) |>
  write_csv("output/prospect_profiles.csv")
cat("\nSaved: output/prospect_profiles.csv\n")

# Print some examples
cat("\n══ 2026 WR Profiles ══\n")
output |> filter(draft_year == 2026, position == "WR") |>
  select(name, prospect_score, archetype, blurb) |>
  print(n = 15, width = 200)

cat("\n══ 2026 RB Profiles ══\n")
output |> filter(draft_year == 2026, position == "RB") |>
  select(name, prospect_score, archetype, blurb) |>
  print(n = 10, width = 200)

cat("\n══ 2023 Top Prospects (validation) ══\n")
output |> filter(draft_year == 2023) |>
  arrange(desc(prospect_score)) |>
  select(name, position, prospect_score, archetype, blurb) |>
  head(10) |>
  print(n = 10, width = 200)

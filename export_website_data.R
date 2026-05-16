# export_website_data.R
# Converts model output CSVs into JSON files for the prospect profile website.
# Run after any model update to refresh the website data.
#
# Outputs:
#   website/public/data/meta.json
#   website/public/data/prospects/{year}.json
#   website/public/data/comps/{slug}.json

setwd("~/Projects/R/college_nfl_model")
library(tidyverse)
library(jsonlite)
library(nflreadr)
library(cfbfastR)

scores   <- read_csv("output/all_class_scores.csv", show_col_types = FALSE)
comps    <- read_csv("output/player_comps.csv", show_col_types = FALSE)
summary  <- read_csv("output/player_comp_summary.csv", show_col_types = FALSE)
profiles_raw <- read_csv("output/prospect_profiles.csv", show_col_types = FALSE)

# ── Percentile references (training distribution, has_cfb_data == 1) ─────────
# Used to rank each prospect's college-production stats against historical
# drafted players at the same position.
wr_train_ref <- readRDS("data/wr_model_data.rds") |> filter(has_cfb_data == 1)
rb_train_ref <- readRDS("data/rb_model_data.rds") |> filter(has_cfb_data == 1)

make_pct <- function(ref_vec) {
  ref_vec <- ref_vec[is.finite(ref_vec)]
  if (length(ref_vec) == 0L) return(function(x) rep(NA_real_, length(x)))
  fn <- stats::ecdf(ref_vec)
  function(x) {
    out <- rep(NA_real_, length(x))
    ok  <- is.finite(x)
    out[ok] <- round(fn(x[ok]) * 100)
    out
  }
}

# WR stats exposed in the UI (order matters — drives card layout)
wr_stat_cols <- c("rec_yards_final", "rec_final", "rec_td_final",
                  "rec_yards_per_game", "ypr", "rec_td_rate", "dominator_rate",
                  "catch_rate_wr", "target_share_wr", "yards_per_target_wr",
                  "epa_per_target_wr", "explosive_rec_rate")
rb_stat_cols <- c("rush_yards_final", "carries_final", "rush_td_final",
                  "rush_yards_per_game", "ypc", "rush_td_rate",
                  "rb_rec_yards", "rb_rec", "rb_rec_td",
                  "epa_per_rush", "explosive_rate", "breakaway_rate",
                  "target_share", "catch_rate")

wr_pct_fns <- setNames(lapply(wr_stat_cols, function(c) make_pct(wr_train_ref[[c]])), wr_stat_cols)
rb_pct_fns <- setNames(lapply(rb_stat_cols, function(c) make_pct(rb_train_ref[[c]])), rb_stat_cols)

# Re-derive bullish/bearish lists from the flat blurb string so we can export
# them as proper JSON arrays. Format: "Bullish: X; Y. Bearish: A; B."
# Use lookahead for next label so internal periods (e.g. "4.36s") don't cut the match.
parse_points <- function(blurb, label) {
  if (is.na(blurb) || blurb == "") return(list())
  pattern <- paste0("(?i)", label, ":\\s*(.*?)(?=\\s*(?:Bullish|Bearish):|$)")
  m <- regmatches(blurb, regexpr(pattern, blurb, perl = TRUE))
  if (length(m) == 0) return(list())
  body <- trimws(sub(paste0("(?i)", label, ":\\s*"), "", m, perl = TRUE))
  body <- trimws(sub("\\.?$", "", body))
  if (body == "") return(list())
  as.list(trimws(strsplit(body, ";")[[1]]))
}

profiles <- profiles_raw |>
  mutate(
    bullish = map(blurb, ~ parse_points(.x, "Bullish")),
    bearish = map(blurb, ~ parse_points(.x, "Bearish"))
  )

# ── Helper: make URL-safe slug ───────────────────────────────────────────────
make_slug <- function(name, year) {
  slug <- name |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "-") |>
    str_remove("^-|-$")
  paste0(slug, "-", year)
}

# ── Headshot lookups ─────────────────────────────────────────────────────────
# Two sources:
#   nflreadr::load_players()         — NFL.com CDN, covers 2021-2025 draft picks
#   cfbfastR::load_cfb_rosters()     — ESPN CDN, covers 2026 prospects (still in college)
#
# Matching: clean names (lowercase, letters only) + position + draft_year.
# For CFB rosters we also match on college/team to reduce false positives.

clean_nm <- function(x) {
  x |>
    str_to_lower() |>
    str_remove("\\s+(jr|sr|ii|iii|iv|v)\\.?$") |>   # strip name suffix before cleaning
    str_remove_all("[^a-z ]") |>                      # keep only letters + spaces
    str_squish()
}

# NFL headshots: drafted players already in the league (2021-2025).
# Match on name + draft_year only — NOT position, because some players are
# listed at a different NFL position than their college/model position
# (e.g. Travis Hunter: WR in model, CB in nflreadr).
nfl_hs_lkp <- tryCatch({
  load_players() |>
    filter(!is.na(headshot), !is.na(draft_year)) |>
    transmute(
      nm_clean     = clean_nm(display_name),
      draft_year   = as.integer(draft_year),
      headshot_url = headshot
    ) |>
    group_by(nm_clean, draft_year) |>
    dplyr::slice(1) |>
    ungroup()
}, error = function(e) {
  message("  [headshots] nflreadr load_players failed: ", conditionMessage(e))
  tibble(nm_clean = character(), draft_year = integer(), headshot_url = character())
})
cat(sprintf("  NFL headshot lookup: %d entries\n", nrow(nfl_hs_lkp)))

# CFB headshots: 2026 prospects (2025 season) + fallback for any 2025 misses (2024 season).
# Match on name + position + college (team) to minimise false positives.
# Also emit a last-name-only frame so we can fall back for nicknamed players
# (e.g. "Kevin Concepcion" in our data vs "KC Concepcion" in cfbfastR).
load_cfb_hs <- function(seasons) {
  tryCatch({
    load_cfb_rosters(seasons = seasons) |>
      filter(position %in% c("WR", "RB"), !is.na(headshot_url)) |>
      transmute(
        nm_clean     = clean_nm(paste(first_name, last_name)),
        last_clean   = clean_nm(last_name),
        position,
        college      = team,
        headshot_url
      )
  }, error = function(e) {
    message("  [headshots] load_cfb_rosters(", seasons, ") failed: ", conditionMessage(e))
    tibble(nm_clean = character(), last_clean = character(),
           position = character(), college = character(),
           headshot_url = character())
  })
}
cfb_hs_raw <- bind_rows(load_cfb_hs(2025), load_cfb_hs(2024))

cfb_hs_lkp <- cfb_hs_raw |>
  group_by(nm_clean, position, college) |>
  dplyr::slice(1) |>
  ungroup() |>
  select(nm_clean, position, college, headshot_url)
cat(sprintf("  CFB headshot lookup: %d entries (2024+2025 seasons)\n", nrow(cfb_hs_lkp)))

# Nickname fallback: keyed by last_name + position + college. Only keep rows
# where that combination is unique in the roster, so we never match the wrong
# player when two people share a surname at the same school.
cfb_hs_lastname_lkp <- cfb_hs_raw |>
  distinct(last_clean, position, college, headshot_url) |>
  add_count(last_clean, position, college, name = "n_candidates") |>
  filter(n_candidates == 1) |>
  select(last_clean, position, college, headshot_url)
cat(sprintf("  CFB last-name fallback: %d unique entries\n", nrow(cfb_hs_lastname_lkp)))

# Manual headshot overrides: e.g. brand-new draft picks not yet in NFL load_players()
# nor cfbfastR rosters. data/draft_2026.csv carries ESPN-sourced URLs from the
# 2026 draft pull, keyed by name + position + college.
manual_hs_lkp <- tryCatch({
  read_csv("data/draft_2026.csv", show_col_types = FALSE) |>
    filter(!is.na(headshot_url), nzchar(headshot_url)) |>
    transmute(
      nm_clean     = clean_nm(name),
      position,
      college,
      headshot_url
    ) |>
    distinct(nm_clean, position, college, .keep_all = TRUE)
}, error = function(e) {
  message("  [headshots] manual override CSV missing/unreadable: ", conditionMessage(e))
  tibble(nm_clean = character(), position = character(),
         college = character(), headshot_url = character())
})
cat(sprintf("  Manual headshot overrides: %d entries\n", nrow(manual_hs_lkp)))

# ── Create output directories ────────────────────────────────────────────────
dir.create("website/public/data/prospects", recursive = TRUE, showWarnings = FALSE)
dir.create("website/public/data/comps", recursive = TRUE, showWarnings = FALSE)

# ── Meta ─────────────────────────────────────────────────────────────────────
meta <- list(
  availableYears = sort(unique(scores$draft_year)),
  positions      = sort(unique(scores$position)),
  lastUpdated    = as.character(Sys.Date()),
  totalProspects = nrow(scores)
)
write_json(meta, "website/public/data/meta.json", auto_unbox = TRUE, pretty = TRUE)

# ── Merge scores + comp summary ──────────────────────────────────────────────
merged <- scores |>
  left_join(
    summary |> select(name, position, draft_year,
                       comp_weighted_ppg, comp_median_ppg,
                       comp_bust_rate, comp_names),
    by = c("name", "position", "draft_year")
  ) |>
  left_join(
    # height_in/weight/forty come from scores (score_class output) — don't
    # re-import from profiles or we'd get .x/.y suffixes.
    profiles |> select(name, position, draft_year,
                        prospect_score, archetype, blurb,
                        bullish, bearish),
    by = c("name", "position", "draft_year")
  ) |>
  mutate(
    id = make_slug(name, draft_year),
    # ── Hurdle / bucket ensemble (position-specific) ────────────────────────
    # Tuned on 8-fold rolling temporal CV via 21_blend_sweep.R (2026-05-10).
    # The bucket model's exp_ppg_bucket has lower OOS MAE alone, but a small
    # blend with the continuous hurdle wins overall — the two models have
    # different failure modes and ensemble away some residual.
    #   WR: 0.70 × bucket + 0.30 × hurdle → MAE 2.36 (vs 2.45 hurdle / 2.37 bucket)
    #   RB: 0.80 × bucket + 0.20 × hurdle → MAE 2.64 (vs 2.91 / 2.66)
    # NA-safe: if exp_ppg_bucket is missing (no bucket models loaded), we
    # fall back to the raw hurdle prediction.
    exp_ppg = case_when(
      is.na(exp_ppg_bucket) ~ exp_ppg,
      position == "WR" ~ 0.30 * exp_ppg + 0.70 * exp_ppg_bucket,
      position == "RB" ~ 0.20 * exp_ppg + 0.80 * exp_ppg_bucket,
      TRUE ~ exp_ppg
    ),
    # ── Comp ensemble blend (position-specific weights) ─────────────────────
    # Blend ensemble prediction with comp-weighted PPG. Weights tuned on
    # 2021-2023 validation; comps dominate for RB where the model is
    # ceiling-capped, less weight for WR where model signal is stronger.
    # Falls back to raw exp_ppg when comp data is unavailable.
    exp_ppg = case_when(
      is.na(comp_weighted_ppg) ~ exp_ppg,
      position == "WR" ~ 0.60 * exp_ppg + 0.40 * comp_weighted_ppg,
      position == "RB" ~ 0.35 * exp_ppg + 0.65 * comp_weighted_ppg,
      TRUE ~ exp_ppg
    ),
    # Null out display stats for players with no CFB data (opted out, FCS, etc.)
    across(
      any_of(c(wr_stat_cols, rb_stat_cols)),
      ~ ifelse(has_cfb_data == 1L, .x, NA_real_)
    ),
    # Clean up NAs for JSON (null instead of "NA")
    across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)),
    nm_clean   = clean_nm(name),
    last_clean = clean_nm(word(name, -1))
  ) |>
  # Join NFL headshots (2021-2025 draft picks; no position key — see note above)
  left_join(
    nfl_hs_lkp,
    by = c("nm_clean", "draft_year")
  ) |>
  rename(nfl_hs = headshot_url) |>
  # Join CFB headshots (2026 prospects — college roster match on name + position + college)
  left_join(
    cfb_hs_lkp,
    by = c("nm_clean", "position", "college")
  ) |>
  rename(cfb_hs = headshot_url) |>
  # Nickname fallback: last name + position + college, only when unique
  left_join(
    cfb_hs_lastname_lkp,
    by = c("last_clean", "position", "college")
  ) |>
  rename(cfb_hs_last = headshot_url) |>
  # Manual overrides (e.g. ESPN headshots for fresh 2026 draftees)
  left_join(
    manual_hs_lkp,
    by = c("nm_clean", "position", "college")
  ) |>
  rename(manual_hs = headshot_url) |>
  # Prefer NFL headshot; fall back to CFB full-name; CFB last-name; manual override
  mutate(
    headshot_url = coalesce(nfl_hs, cfb_hs, cfb_hs_last, manual_hs)
  ) |>
  select(-nm_clean, -last_clean, -nfl_hs, -cfb_hs, -cfb_hs_last, -manual_hs)

# ── Compute percentile columns (by position, using training CDF) ─────────────
# Adds one `<col>_pct` integer (0-100) per stat in wr_stat_cols / rb_stat_cols.
# Percentile is NA when the raw stat is NA (e.g. pre-2010 CFB sparsity).
for (c in wr_stat_cols) {
  new_col <- paste0(c, "_pct")
  vals <- ifelse(merged$position == "WR", merged[[c]], NA_real_)
  merged[[new_col]] <- wr_pct_fns[[c]](vals)
}
for (c in rb_stat_cols) {
  new_col <- paste0(c, "_pct")
  vals <- ifelse(merged$position == "RB", merged[[c]], NA_real_)
  merged[[new_col]] <- rb_pct_fns[[c]](vals)
}

# ── Per-year prospect JSON ───────────────────────────────────────────────────
for (yr in unique(merged$draft_year)) {
  yr_data <- merged |>
    filter(draft_year == yr) |>
    arrange(pick) |>
    mutate(
      comp_names_list = str_split(comp_names, ", ")
    ) |>
    select(
      id, name, position, college, tier, round, pick,
      p_made_it, exp_ppg,
      # Ordinal-bucket distribution (XGB+clm ensemble) — coexists with
      # the continuous exp_ppg. Lets the UI show a stacked bar of outcome
      # tiers per prospect.
      any_of(c("p_bust", "p_bench", "p_flex", "p_elite", "p_league_winner",
               "p_bust_lo", "p_bench_lo", "p_flex_lo", "p_elite_lo", "p_league_winner_lo",
               "p_bust_hi", "p_bench_hi", "p_flex_hi", "p_elite_hi", "p_league_winner_hi",
               "exp_ppg_bucket", "exp_ppg_bucket_lo", "exp_ppg_bucket_hi",
               "bucket_top1")),
      prospect_score, archetype, blurb, bullish, bearish,
      headshot_url,
      height_in, weight, forty,
      # Full WR stat set + percentiles
      any_of(wr_stat_cols), any_of(paste0(wr_stat_cols, "_pct")),
      # Full RB stat set + percentiles
      any_of(rb_stat_cols), any_of(paste0(rb_stat_cols, "_pct")),
      # PBP availability flags (so UI can tell "no PBP" from "bad PBP")
      any_of(c("has_wr_pbp", "has_pbp", "has_cfb_data")),
      # Draft-capital delta (mock vs actual)
      any_of(c("proj_pick", "actual_pick_value", "proj_pick_value",
               "draft_capital_delta", "has_mock_data")),
      actual_ppg, actual_raw_ppg, actual_made_it, n_qual_seasons,
      comp_weighted_ppg, comp_median_ppg, comp_bust_rate,
      comp_names = comp_names_list
    )

  out <- list(
    draftYear = yr,
    prospects = yr_data |>
      purrr::transpose() |>
      lapply(function(row) {
        # Convert NA to NULL for clean JSON
        lapply(row, function(x) if (length(x) == 1 && is.na(x)) NULL else x)
      })
  )

  write_json(out, sprintf("website/public/data/prospects/%d.json", yr),
             auto_unbox = TRUE, pretty = TRUE, null = "null")
  cat(sprintf("  Wrote %d prospects for %d\n", nrow(yr_data), yr))
}

# ── Per-prospect comp JSON ───────────────────────────────────────────────────
for (i in seq_len(nrow(merged))) {
  row <- merged[i, ]
  slug <- row$id

  prospect_comps <- comps |>
    filter(name == row$name, position == row$position, draft_year == row$draft_year) |>
    arrange(comp_rank) |>
    select(
      rank = comp_rank,
      name = comp_name,
      college = comp_college,
      year = comp_year,
      round = comp_round,
      pick = comp_pick,
      ppg = comp_ppg,
      rawPpg = comp_raw_ppg,
      madeIt = comp_made_it,
      similarity
    ) |>
    mutate(
      across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)),
      madeIt = ppg >= 5   # bust = under 5 PPG (shrinkage-adjusted)
    )

  comp_out <- list(
    prospectId = slug,
    comps = prospect_comps |>
      purrr::transpose() |>
      lapply(function(row) lapply(row, function(x) if (length(x) == 1 && is.na(x)) NULL else x))
  )

  write_json(comp_out, sprintf("website/public/data/comps/%s.json", slug),
             auto_unbox = TRUE, pretty = TRUE, null = "null")
}

cat(sprintf("\nExported %d prospect files and %d comp files\n",
            length(unique(merged$draft_year)), nrow(merged)))
message("Done: website/public/data/")

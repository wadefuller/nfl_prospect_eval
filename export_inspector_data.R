#!/usr/bin/env Rscript
# export_inspector_data.R
# Exports per-player feature inspector data for the React website.
# Filters to draft_year >= 2021. Mirrors the layout the Shiny app
# (inspect_app.R) used to render, but as static JSON the SPA can fetch.
#
# Outputs:
#   website/public/data/inspector/index.json
#   website/public/data/inspector/players/{id}.json

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(jsonlite); library(tibble)
})

setwd("~/Projects/R/college_nfl_model")

MIN_YEAR <- 2021
OUT_DIR  <- "website/public/data/inspector"
PLAYERS_DIR <- file.path(OUT_DIR, "players")
dir.create(PLAYERS_DIR, recursive = TRUE, showWarnings = FALSE)

clean_name <- function(x) {
  x |> tolower() |> iconv(to = "ASCII//TRANSLIT") |>
    gsub("[^a-z0-9 ]", "", x = _) |>
    gsub("\\s+", " ", x = _) |> trimws()
}
make_slug <- function(name, year) {
  paste0(name |> str_to_lower() |> str_replace_all("[^a-z0-9]+", "-") |>
           str_remove("^-|-$"), "-", year)
}
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

normalize_row <- function(df) {
  if ("round" %in% names(df)) df$round <- suppressWarnings(as.integer(as.character(df$round)))
  if ("pick"  %in% names(df)) df$pick  <- suppressWarnings(as.integer(as.character(df$pick)))
  df
}

# ── Load & build master (training + scored, mirroring inspect_app.R) ─────────
wr_train <- readRDS("data/wr_model_data.rds") |>
  mutate(position = "WR", name = pfr_player_name,
         name_clean = clean_name(pfr_player_name), source = "training") |>
  normalize_row()
rb_train <- readRDS("data/rb_model_data.rds") |>
  mutate(position = "RB", name = pfr_player_name,
         name_clean = clean_name(pfr_player_name), source = "training") |>
  normalize_row()
qb_train <- if (file.exists("data/qb_model_data.rds")) {
  readRDS("data/qb_model_data.rds") |>
    mutate(position = "QB", name = pfr_player_name,
           name_clean = clean_name(pfr_player_name), source = "training") |>
    normalize_row()
} else tibble()
te_train <- if (file.exists("data/te_model_data.rds")) {
  readRDS("data/te_model_data.rds") |>
    mutate(position = "TE", name = pfr_player_name,
           name_clean = clean_name(pfr_player_name), source = "training") |>
    normalize_row()
} else tibble()

scores <- tryCatch(
  readRDS("output/all_class_scores.rds") |>
    mutate(name_clean = clean_name(name), source = "scored") |>
    normalize_row(),
  error = function(e) NULL)

overlay_cols <- c("p_made_it", "exp_ppg", "actual_ppg", "actual_raw_ppg",
                  "actual_made_it",
                  # 5-bucket Bayesian ensemble outputs
                  "p_bust", "p_bench", "p_flex", "p_elite", "p_league_winner",
                  "p_bust_lo", "p_bench_lo", "p_flex_lo", "p_elite_lo",
                  "p_league_winner_lo",
                  "p_bust_hi", "p_bench_hi", "p_flex_hi", "p_elite_hi",
                  "p_league_winner_hi",
                  "exp_ppg_bucket", "exp_ppg_bucket_lo", "exp_ppg_bucket_hi",
                  "bucket_top1")

build_master <- function() {
  train_all <- bind_rows(wr_train, rb_train, qb_train, te_train) |>
    mutate(k = paste(name_clean, position, draft_year))
  sc <- scores
  if (!is.null(sc)) sc <- sc |> mutate(k = paste(name_clean, position, draft_year))

  tr_with_overlay <- train_all
  if (!is.null(sc)) {
    sc_keep <- sc |> select(k, any_of(overlay_cols))
    tr_with_overlay <- tr_with_overlay |>
      left_join(sc_keep, by = "k", suffix = c(".tr", ".sc"))
    for (c in overlay_cols) {
      tr_col <- paste0(c, ".tr"); sc_col <- paste0(c, ".sc")
      if (sc_col %in% names(tr_with_overlay)) {
        tr_with_overlay[[c]] <- dplyr::coalesce(
          tr_with_overlay[[sc_col]],
          if (tr_col %in% names(tr_with_overlay))
            tr_with_overlay[[tr_col]] else NA_real_)
        tr_with_overlay[[tr_col]] <- NULL
        tr_with_overlay[[sc_col]] <- NULL
      }
    }
  }
  sc_only <- if (!is.null(sc)) sc |> filter(!k %in% train_all$k) else tibble()
  bind_rows(tr_with_overlay, sc_only) |>
    arrange(draft_year, position, pick) |>
    select(-any_of("k"))
}

master_full <- build_master()
master <- master_full |> filter(draft_year >= MIN_YEAR)

# ── Headshot URLs ────────────────────────────────────────────────────────────
# Reuse the curated headshot lookups already exported by export_website_data.R.
# Each year file under website/public/data/prospects/ has a `headshot_url` per
# prospect, keyed by `id` (slug). We also pull the FINAL displayed values
# (post-hurdle/bucket ensemble, post-comp blend, plus the recalibrated
# prospect_score) so the inspector header matches the prospect cards.
PROSPECTS_DIR <- "website/public/data/prospects"

prospect_json_lkp <- tryCatch({
  files <- list.files(PROSPECTS_DIR, pattern = "\\.json$", full.names = TRUE)
  cols_to_keep <- c("id", "headshot_url", "exp_ppg", "prospect_score",
                    "p_made_it", "p_bust", "p_bench", "p_flex", "p_elite",
                    "p_league_winner", "p_bust_lo", "p_bench_lo", "p_flex_lo",
                    "p_elite_lo", "p_league_winner_lo", "p_bust_hi",
                    "p_bench_hi", "p_flex_hi", "p_elite_hi",
                    "p_league_winner_hi", "exp_ppg_bucket",
                    "exp_ppg_bucket_lo", "exp_ppg_bucket_hi", "bucket_top1")
  map_dfr(files, function(f) {
    yr <- jsonlite::fromJSON(f, simplifyVector = TRUE)
    if (!is.data.frame(yr$prospects)) return(tibble())
    p <- yr$prospects
    keep <- intersect(cols_to_keep, names(p))
    as_tibble(p[, keep, drop = FALSE]) |>
      rename(slug = id) |>
      mutate(slug = as.character(slug))
  })
}, error = function(e) {
  message("  [json overlay] couldn't read prospects JSONs: ", conditionMessage(e))
  tibble(slug = character())
})

headshot_lkp <- prospect_json_lkp |>
  select(any_of(c("slug", "headshot_url"))) |>
  filter(!is.na(headshot_url), headshot_url != "")

cat(sprintf("Loaded %d prospect JSON rows (%d with headshots).\n",
            nrow(prospect_json_lkp), nrow(headshot_lkp)))

cat(sprintf("Loaded %d players (WR=%d, RB=%d). Years %d-%d.\n",
            nrow(master), sum(master$position == "WR"),
            sum(master$position == "RB"),
            min(master$draft_year), max(master$draft_year)))

# ── Feature groups (mirrors inspect_app.R) ───────────────────────────────────
FEATURE_GROUPS <- list(
  WR = list(
    "Final Season"        = c("rec_final","rec_yards_final","rec_td_final","ypr",
                              "rec_yards_per_game","rec_per_game","rec_td_rate","dominator_rate"),
    "Penult / Ante / YoY" = c("rec_penult","rec_yards_penult","rec_td_penult",
                              "rec_yards_ante","rec_yds_yoy"),
    "PBP"                 = c("catch_rate_wr","yards_per_target_wr","yards_per_rec_wr",
                              "explosive_rec_rate","target_share_wr","targets_per_game_wr",
                              "epa_per_target_wr","epa_per_play_wr_pbp"),
    "PPA / Usage"         = c("usg_pass","usg_passing_downs","avg_PPA_pass","total_PPA_pass"),
    "Combine"             = c("height_in","weight","forty","vertical","broad_jump","speed_score"),
    "Recruiting"          = c("recruit_stars","recruit_rating","recruit_rank"),
    "Context"             = c("age","age_relative","college_years","teammate_rec_yards",
                              "n_drafted_skill","elite_teammate")
  ),
  RB = list(
    "Final Season"        = c("carries_final","rush_yards_final","rush_td_final","ypc",
                              "rb_rec","rb_rec_yards","rb_rec_td","scrimmage_yards","scrimmage_td",
                              "yards_per_touch","total_touches","rush_yards_per_game","carries_per_game",
                              "scrimmage_yards_per_game","rush_td_rate","recv_share","dominator_rate"),
    "Penult / Ante / YoY" = c("rush_yards_penult","carries_penult","rush_td_penult",
                              "rush_yards_ante","rush_yds_yoy"),
    "PBP"                 = c("explosive_rate","breakaway_rate","target_share","targets_per_game",
                              "catch_rate","epa_per_rush","epa_per_play_pbp","carries_per_game_pbp"),
    "PPA / Usage"         = c("usg_rush","usg_passing_downs","usg_overall","usg_pass",
                              "avg_PPA_rush","total_PPA_rush","avg_PPA_all","total_PPA_all"),
    "Combine"             = c("height_in","weight","forty","vertical","broad_jump","speed_score"),
    "Recruiting"          = c("recruit_stars","recruit_rating","recruit_rank"),
    "Context"             = c("age","age_relative","college_years","teammate_rush_yards",
                              "n_drafted_skill","elite_teammate")
  ),
  QB = list(
    "Final Season"        = c("pass_yds_final","pass_td_final","pass_int_final",
                              "pass_att_final","pass_comp_final","pass_pct_final","pass_ypa_final",
                              "pass_td_int_ratio","pass_yds_per_game","pass_td_per_game"),
    "Penult / Ante / YoY" = c("pass_yds_penult","pass_td_penult","pass_att_penult",
                              "pass_yds_ante","pass_yds_yoy"),
    "Mobility"            = c("rush_car_final","rush_yds_final","rush_td_final",
                              "rush_yds_per_carry","has_mobility"),
    "Combine"             = c("height_in","weight","forty","vertical","broad_jump","speed_score"),
    "Recruiting"          = c("recruit_stars","recruit_rating","recruit_rank"),
    "Context"             = c("age","college_years")
  ),
  TE = list(
    "Final Season"        = c("rec_final","rec_yards_final","rec_td_final","ypr_final",
                              "rec_td_rate","dominator_rate","rec_yards_per_game","rec_per_game"),
    "Penult / Ante / YoY" = c("rec_penult","rec_yards_penult","rec_td_penult",
                              "rec_yards_ante","rec_yds_yoy"),
    "Combine"             = c("height_in","weight","forty","vertical","broad_jump","speed_score","is_move_te"),
    "Recruiting"          = c("recruit_stars","recruit_rating","recruit_rank"),
    "Context"             = c("age","college_years","teammate_rec_yards")
  )
)

WR_ONLY_COLS <- c("rec_final","rec_yards_final","rec_td_final","rec_penult","rec_yards_penult",
  "rec_td_penult","rec_yards_ante","rec_yds_yoy","ypr","rec_yards_per_game","rec_per_game",
  "rec_td_rate","catch_rate_wr","yards_per_target_wr","yards_per_rec_wr","explosive_rec_rate",
  "target_share_wr","targets_per_game_wr","epa_per_target_wr","epa_per_play_wr_pbp","has_wr_pbp",
  "usg_pass","avg_PPA_pass","total_PPA_pass","teammate_rec_yards","is_possession_wr")

RB_ONLY_COLS <- c("carries_final","rush_yards_final","rush_td_final","carries_penult",
  "rush_yards_penult","rush_td_penult","rush_yards_ante","rush_yds_yoy","ypc","rush_td_rate",
  "rush_yards_per_game","carries_per_game","scrimmage_yards_per_game","rb_rec","rb_rec_yards",
  "rb_rec_td","scrimmage_yards","scrimmage_td","yards_per_touch","total_touches","recv_share",
  "explosive_rate","breakaway_rate","target_share","targets_per_game","catch_rate","epa_per_rush",
  "epa_per_play_pbp","carries_per_game_pbp","has_pbp","usg_rush","usg_overall","avg_PPA_rush",
  "total_PPA_rush","avg_PPA_all","total_PPA_all","teammate_rush_yards","is_scat_back")

METRIC_DESC <- list(
  rec_final = "Receptions in best college season (peak, not necessarily senior year)",
  rec_yards_final = "Receiving yards in best college season",
  rec_td_final = "Receiving touchdowns in best college season",
  ypr = "Yards per reception in best season",
  rec_yards_per_game = "Receiving yards / team games played",
  rec_per_game = "Receptions / team games played",
  rec_td_rate = "TDs per reception (TD efficiency)",
  dominator_rate = "Player share of team scrimmage yards in best season — proxy for offensive centrality",
  rec_penult = "Receptions in chronological year-before-draft season",
  rec_yards_penult = "Receiving yards in year-before-draft season",
  rec_td_penult = "Receiving TDs in year-before-draft season",
  rec_yards_ante = "Receiving yards two seasons before draft (3rd-to-last college year)",
  rec_yds_yoy = "Year-over-year change: actual final yards − penult yards. Negative = declining trajectory",
  catch_rate_wr = "Receptions / targets (PBP-derived)",
  yards_per_target_wr = "Receiving yards / target (efficiency on opportunities)",
  yards_per_rec_wr = "Receiving yards per completed catch (PBP)",
  explosive_rec_rate = "Share of receptions of 15+ yards (big-play rate)",
  target_share_wr = "Player targets / team total targets",
  targets_per_game_wr = "Targets per game played (volume signal)",
  epa_per_target_wr = "Mean EPA on plays where this player was targeted",
  epa_per_play_wr_pbp = "Mean EPA per touch (target+rush) — overall efficiency",
  usg_pass = "Player share of team passing-down plays",
  avg_PPA_pass = "Mean Predicted Points Added per pass play (passing efficiency)",
  total_PPA_pass = "Total PPA accumulated on pass plays (volume × efficiency)",
  carries_final = "Carries in best college season",
  rush_yards_final = "Rushing yards in best season",
  rush_td_final = "Rushing TDs in best season",
  ypc = "Yards per carry in best season",
  rb_rec = "Receptions in same season as best rushing year (RB pass-catching)",
  rb_rec_yards = "Receiving yards in best rushing season",
  rb_rec_td = "Receiving TDs in best rushing season",
  scrimmage_yards = "Rush yards + receiving yards in best season",
  scrimmage_td = "Rush TDs + receiving TDs in best season",
  yards_per_touch = "Scrimmage yards / total touches (carries+receptions)",
  total_touches = "Carries + receptions in best season",
  rush_yards_per_game = "Rush yards / team games played",
  carries_per_game = "Carries / team games played",
  scrimmage_yards_per_game = "Scrimmage yards / team games played",
  rush_td_rate = "Rush TDs per carry (TD efficiency)",
  recv_share = "Receiving yards / scrimmage yards (pass-game involvement)",
  carries_penult = "Carries in year-before-draft season",
  rush_yards_penult = "Rush yards in year-before-draft season",
  rush_td_penult = "Rush TDs in year-before-draft season",
  rush_yards_ante = "Rush yards two seasons before draft (3rd-to-last college year)",
  rush_yds_yoy = "Year-over-year change: actual final yards − penult yards. Negative = declining",
  explosive_rate = "Share of carries gaining 10+ yards",
  breakaway_rate = "Share of carries gaining 15+ yards",
  target_share = "Player targets / team total targets (RB receiving role)",
  targets_per_game = "Targets per game played",
  catch_rate = "Receptions / targets (RB-side)",
  epa_per_rush = "Mean EPA per rush attempt (run-game efficiency)",
  epa_per_play_pbp = "Mean EPA per touch (rush+target) — overall efficiency",
  carries_per_game_pbp = "Carries / games played, PBP-derived (handles partial seasons better)",
  usg_rush = "Player share of team rushing plays",
  usg_passing_downs = "Share of team pass-likely plays (3rd/4th-and-medium+)",
  usg_overall = "Player share of all team plays (rush + pass)",
  avg_PPA_rush = "Mean PPA per rush play (rushing efficiency)",
  total_PPA_rush = "Total PPA on rush plays (volume × efficiency)",
  avg_PPA_all = "Mean PPA across all plays (overall efficiency)",
  total_PPA_all = "Total PPA across all plays",
  height_in = "Height in inches",
  weight = "Listed weight in pounds",
  forty = "40-yard dash time, seconds (lower = faster)",
  vertical = "Vertical jump in inches",
  broad_jump = "Broad jump in inches",
  speed_score = "Bill Barnwell speed score: weight × 200 / forty^4 — size-adjusted speed",
  recruit_stars = "Star rating (2-5) from HS recruiting profile",
  recruit_rating = "Composite rating (0-1 scale)",
  recruit_rank = "National rank at position (1 = best). Lower is better",
  age = "Player age at time of draft",
  age_relative = "Age vs same-position class mean (negative = younger than peers)",
  college_years = "Years between recruit class and draft year (early-declarer signal)",
  teammate_rec_yards = "Highest single-season rec yards by another skill teammate at same school",
  teammate_rush_yards = "Highest single-season rush yards by another skill teammate at same school",
  n_drafted_skill = "Count of other WR/RB/TE drafted from same school in same class",
  elite_teammate = "1 if a same-position teammate from same class went in Rd 1-2"
)

# ── Cohort percentile fns (training only, has_cfb_data filter mirrors app) ───
cohort_pool <- list(
  WR = wr_train |> filter(has_cfb_data),
  RB = rb_train |> filter(has_cfb_data),
  QB = if (nrow(qb_train) > 0) qb_train |> filter(has_cfb_data) else tibble(),
  TE = if (nrow(te_train) > 0) te_train |> filter(has_cfb_data) else tibble()
)

pct_of <- function(pos, col, val) {
  if (is.null(val) || !is.finite(val)) return(NA_real_)
  cohort <- cohort_pool[[pos]][[col]]
  if (is.null(cohort)) return(NA_real_)
  cohort <- cohort[is.finite(cohort)]
  if (length(cohort) < 5) return(NA_real_)
  mean(cohort <= val)
}

# ── Per-player slug + payload ────────────────────────────────────────────────
master <- master |> mutate(slug = make_slug(name, draft_year))

# Overlay headshot + final displayed values from the prospect JSONs. The
# JSON-side `exp_ppg` is post-ensemble (hurdle + bucket + comp), so the
# inspector header now matches what users see on prospect cards. Bucket
# fields + prospect_score also come from here.
overlay_from_json <- intersect(
  c("headshot_url", "exp_ppg", "prospect_score", "p_made_it",
    "p_bust", "p_bench", "p_flex", "p_elite", "p_league_winner",
    "p_bust_lo", "p_bench_lo", "p_flex_lo", "p_elite_lo", "p_league_winner_lo",
    "p_bust_hi", "p_bench_hi", "p_flex_hi", "p_elite_hi", "p_league_winner_hi",
    "exp_ppg_bucket", "exp_ppg_bucket_lo", "exp_ppg_bucket_hi", "bucket_top1"),
  names(prospect_json_lkp))

# Drop columns from master that we'll overlay (so the JSON wins cleanly).
for (c in overlay_from_json) {
  if (c %in% names(master)) master[[c]] <- NULL
}
master <- master |>
  left_join(prospect_json_lkp |> select(slug, all_of(overlay_from_json)),
            by = "slug")

fmt_value <- function(v) {
  if (is.null(v) || length(v) == 0) return(NA_character_)
  if (is.logical(v)) v <- as.integer(v)
  if (is.na(v)) return(NA_character_)
  if (!is.numeric(v)) return(as.character(v))
  if (abs(v - round(v)) < 1e-9 && abs(v) < 1e6) format(round(v), big.mark = ",")
  else if (abs(v) < 1) sprintf("%.3f", v)
  else sprintf("%.2f", v)
}

build_player_payload <- function(r) {
  pos <- r$position
  groups <- FEATURE_GROUPS[[pos]]

  group_payload <- map(names(groups), function(grp) {
    feats <- groups[[grp]]
    rows <- compact(map(feats, function(feat) {
      v <- r[[feat]]
      if (is.null(v) || length(v) == 0) return(NULL)
      if (is.logical(v)) v <- as.integer(v)
      if (!is.numeric(v)) return(NULL)
      pct <- pct_of(pos, feat, v)
      list(
        feat       = feat,
        desc       = METRIC_DESC[[feat]] %||% "",
        value      = if (is.na(v)) NA else as.numeric(v),
        valueDisplay = fmt_value(v) %||% "NA",
        percentile = if (is.na(pct)) NA else as.integer(round(pct * 100))
      )
    }))
    if (length(rows) == 0) return(NULL)
    list(label = grp, rows = rows)
  }) |> compact()

  # Production trajectory triplets
  if (pos == "WR") {
    prod_meta <- list(
      list(metric = "Rec yds", ante = "rec_yards_ante", penult = "rec_yards_penult", final = "rec_yards_final"),
      list(metric = "Rec",     ante = NA,               penult = "rec_penult",       final = "rec_final"),
      list(metric = "Rec TD",  ante = NA,               penult = "rec_td_penult",    final = "rec_td_final")
    )
  } else {
    prod_meta <- list(
      list(metric = "Rush yds", ante = "rush_yards_ante", penult = "rush_yards_penult", final = "rush_yards_final"),
      list(metric = "Carries",  ante = NA,                penult = "carries_penult",    final = "carries_final"),
      list(metric = "Rush TD",  ante = NA,                penult = "rush_td_penult",    final = "rush_td_final")
    )
  }
  production <- map(prod_meta, function(m) {
    pull_v <- function(col) {
      if (is.na(col)) return(NA_real_)
      v <- r[[col]]
      if (is.null(v) || length(v) == 0) NA_real_ else as.numeric(v)
    }
    list(metric = m$metric,
         ante   = pull_v(m$ante),
         penult = pull_v(m$penult),
         final  = pull_v(m$final))
  })

  # Combine block (value + percentile)
  combine_metrics <- c("height_in","weight","forty","vertical","broad_jump","speed_score")
  combine <- map(combine_metrics, function(c) {
    v <- r[[c]]
    if (is.null(v) || length(v) == 0) v <- NA_real_
    pct <- pct_of(pos, c, v)
    list(metric = c,
         value      = if (is.na(v)) NA else as.numeric(v),
         valueDisplay = fmt_value(v) %||% "NA",
         percentile = if (is.na(pct)) NA else as.integer(round(pct * 100)))
  })

  # Raw data dump (alphabetical, position-filtered)
  drop_cols <- c("name_clean", "source", "slug",
                 if (pos == "WR") RB_ONLY_COLS else WR_ONLY_COLS)
  raw_long <- r |>
    select(-any_of(drop_cols)) |>
    pivot_longer(everything(), names_to = "field", values_to = "value",
                 values_transform = list(value = as.character)) |>
    arrange(field)
  raw <- map2(raw_long$field, raw_long$value, function(f, v) {
    list(field = f, value = if (is.na(v)) NA else as.character(v))
  })

  list(
    id        = r$slug,
    name      = r$name,
    position  = pos,
    college   = r$college %||% NA_character_,
    draft_year= as.integer(r$draft_year),
    round     = if (is.na(r$round)) NA else as.integer(r$round),
    pick      = if (is.na(r$pick))  NA else as.integer(r$pick),
    tier      = r$tier %||% NA_character_,
    headshot_url = r$headshot_url %||% NA_character_,
    summary   = list(
      p_made_it      = r$p_made_it %||% NA_real_,
      exp_ppg        = r$exp_ppg   %||% NA_real_,
      prospect_score = if (is.null(r$prospect_score) || is.na(r$prospect_score))
                         NA_integer_ else as.integer(r$prospect_score),
      actual_ppg     = r$actual_ppg %||% r$ppg %||% NA_real_,
      made_it        = if (!is.null(r$made_it) && !is.na(r$made_it))
                         as.integer(r$made_it)
                       else if (!is.null(r$actual_made_it) && !is.na(r$actual_made_it))
                         as.integer(r$actual_made_it)
                       else NA_integer_,
      # 5-bucket Bayesian ensemble (post-XGB+stan_polr geom-mean + 80% CI)
      bucket = if (!is.null(r$p_bust) && !is.na(r$p_bust)) list(
        top1 = r$bucket_top1 %||% NA_character_,
        means = list(
          bust          = r$p_bust          %||% NA_real_,
          bench         = r$p_bench         %||% NA_real_,
          flex          = r$p_flex          %||% NA_real_,
          elite         = r$p_elite         %||% NA_real_,
          league_winner = r$p_league_winner %||% NA_real_
        ),
        lo = list(
          bust          = r$p_bust_lo          %||% NA_real_,
          bench         = r$p_bench_lo         %||% NA_real_,
          flex          = r$p_flex_lo          %||% NA_real_,
          elite         = r$p_elite_lo         %||% NA_real_,
          league_winner = r$p_league_winner_lo %||% NA_real_
        ),
        hi = list(
          bust          = r$p_bust_hi          %||% NA_real_,
          bench         = r$p_bench_hi         %||% NA_real_,
          flex          = r$p_flex_hi          %||% NA_real_,
          elite         = r$p_elite_hi         %||% NA_real_,
          league_winner = r$p_league_winner_hi %||% NA_real_
        ),
        exp_ppg_bucket    = r$exp_ppg_bucket    %||% NA_real_,
        exp_ppg_bucket_lo = r$exp_ppg_bucket_lo %||% NA_real_,
        exp_ppg_bucket_hi = r$exp_ppg_bucket_hi %||% NA_real_
      ) else NULL
    ),
    groups     = group_payload,
    production = production,
    combine    = combine,
    raw        = raw
  )
}

# ── Write per-player files ───────────────────────────────────────────────────
cat("Writing per-player JSONs to", PLAYERS_DIR, "...\n")
n <- nrow(master)
for (i in seq_len(n)) {
  r <- master[i, ]
  payload <- build_player_payload(r)
  out <- file.path(PLAYERS_DIR, paste0(r$slug, ".json"))
  write_json(payload, out, auto_unbox = TRUE, na = "null", null = "null")
  if (i %% 50 == 0) cat(sprintf("  %d / %d\n", i, n))
}

# ── Write index.json ─────────────────────────────────────────────────────────
index <- master |>
  mutate(actual_combined = coalesce(actual_ppg, ppg)) |>
  transmute(
    id        = slug,
    name      = name,
    position  = position,
    college   = college %||% NA_character_,
    draft_year = as.integer(draft_year),
    round     = as.integer(round),
    pick      = as.integer(pick),
    tier      = tier %||% NA_character_,
    headshot_url = headshot_url,
    p_made_it = p_made_it,
    exp_ppg   = exp_ppg,
    actual_ppg = actual_combined,
    made_it   = as.integer(coalesce(made_it, actual_made_it))
  ) |>
  arrange(desc(coalesce(exp_ppg, -Inf)), draft_year, pick)

meta <- list(
  lastUpdated   = format(Sys.Date()),
  minDraftYear  = MIN_YEAR,
  maxDraftYear  = max(index$draft_year),
  positions     = sort(unique(index$position)),
  total         = nrow(index),
  players       = index
)

write_json(meta, file.path(OUT_DIR, "index.json"),
           auto_unbox = TRUE, na = "null", null = "null")

cat(sprintf("\nDone. Wrote %d player files + index.json to %s\n", n, OUT_DIR))

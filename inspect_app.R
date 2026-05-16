#!/usr/bin/env Rscript
# inspect_app.R
# Visual inspector for every player in training + scoring data.
#
# Run with:   Rscript inspect_app.R
# Then open:  http://127.0.0.1:8787/
#
# Features:
#   • Filter by position + draft class, fuzzy-search by name
#   • Header card: identity, draft capital, model prediction, actual outcome
#   • Percentile strip: every numeric feature as a bar showing where this
#     player sits in the same-position training distribution
#   • Production trajectory: final / penult / ante season stats
#   • Combine radar: physical measurables vs position cohort
#   • Raw feature table: every column the model sees

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
})

# ── Data load (once at startup) ──────────────────────────────────────────────

clean_name <- function(x) {
  x |>
    tolower() |>
    iconv(to = "ASCII//TRANSLIT") |>
    gsub("[^a-z0-9 ]", "", x = _) |>
    gsub("\\s+", " ", x = _) |>
    trimws()
}

normalize_row <- function(df) {
  if ("round" %in% names(df)) df$round <- suppressWarnings(as.integer(as.character(df$round)))
  if ("pick"  %in% names(df)) df$pick  <- suppressWarnings(as.integer(as.character(df$pick)))
  df
}

wr_train <- readRDS("data/wr_model_data.rds") |>
  mutate(position = "WR", name = pfr_player_name,
         name_clean = clean_name(pfr_player_name), source = "training") |>
  normalize_row()
rb_train <- readRDS("data/rb_model_data.rds") |>
  mutate(position = "RB", name = pfr_player_name,
         name_clean = clean_name(pfr_player_name), source = "training") |>
  normalize_row()

scores <- tryCatch(
  readRDS("output/all_class_scores.rds") |>
    mutate(name_clean = clean_name(name), source = "scored") |>
    normalize_row(),
  error = function(e) NULL
)

overlay_cols <- c("p_made_it", "exp_ppg",
                  "actual_ppg", "actual_raw_ppg", "actual_made_it")

# Build master: training rows primary, overlay model outputs from scored
build_master <- function() {
  train_all <- bind_rows(wr_train, rb_train) |>
    mutate(k = paste(name_clean, position, draft_year))
  sc <- scores
  if (!is.null(sc)) sc <- sc |> mutate(k = paste(name_clean, position, draft_year))

  # Training rows (with overlay)
  tr_with_overlay <- train_all
  if (!is.null(sc)) {
    sc_keep <- sc |> select(k, any_of(overlay_cols))
    tr_with_overlay <- tr_with_overlay |>
      left_join(sc_keep, by = "k", suffix = c(".tr", ".sc"))
    # Prefer .sc value where present
    for (c in overlay_cols) {
      tr_col <- paste0(c, ".tr")
      sc_col <- paste0(c, ".sc")
      if (sc_col %in% names(tr_with_overlay)) {
        tr_with_overlay[[c]] <- dplyr::coalesce(
          tr_with_overlay[[sc_col]],
          if (tr_col %in% names(tr_with_overlay)) tr_with_overlay[[tr_col]] else NA_real_
        )
        tr_with_overlay[[tr_col]] <- NULL
        tr_with_overlay[[sc_col]] <- NULL
      }
    }
  }

  # Scored-only rows (2024+ prospects)
  sc_only <- if (!is.null(sc)) {
    sc |> filter(!k %in% train_all$k)
  } else tibble()

  bind_rows(tr_with_overlay, sc_only) |>
    arrange(draft_year, position, pick) |>
    select(-any_of("k"))
}

master <- build_master()

cat(sprintf("Loaded %d players (WR=%d, RB=%d). Years %d-%d.\n",
            nrow(master),
            sum(master$position == "WR"),
            sum(master$position == "RB"),
            min(master$draft_year), max(master$draft_year)))

# Feature groupings for the app
FEATURE_GROUPS <- list(
  WR = list(
    "Final Season"       = c("rec_final", "rec_yards_final", "rec_td_final",
                             "ypr", "rec_yards_per_game", "rec_per_game",
                             "rec_td_rate", "dominator_rate"),
    "Penult / Ante / YoY"= c("rec_penult", "rec_yards_penult", "rec_td_penult",
                             "rec_yards_ante", "rec_yds_yoy"),
    "PBP"                = c("catch_rate_wr", "yards_per_target_wr",
                             "yards_per_rec_wr", "explosive_rec_rate",
                             "target_share_wr", "targets_per_game_wr",
                             "epa_per_target_wr", "epa_per_play_wr_pbp"),
    "PPA / Usage"        = c("usg_pass", "usg_passing_downs",
                             "avg_PPA_pass", "total_PPA_pass"),
    "Combine"            = c("height_in", "weight", "forty", "vertical",
                             "broad_jump", "speed_score"),
    "Recruiting"         = c("recruit_stars", "recruit_rating", "recruit_rank"),
    "Context"            = c("age", "age_relative", "college_years",
                             "teammate_rec_yards", "age_adj_yards",
                             "n_drafted_skill", "elite_teammate")
  ),
  RB = list(
    "Final Season"       = c("carries_final", "rush_yards_final",
                             "rush_td_final", "ypc", "rb_rec", "rb_rec_yards",
                             "rb_rec_td", "scrimmage_yards", "scrimmage_td",
                             "yards_per_touch", "total_touches",
                             "rush_yards_per_game", "carries_per_game",
                             "scrimmage_yards_per_game",
                             "rush_td_rate", "recv_share", "dominator_rate"),
    "Penult / Ante / YoY"= c("rush_yards_penult", "carries_penult",
                             "rush_td_penult",
                             "rush_yards_ante", "rush_yds_yoy"),
    "PBP"                = c("explosive_rate", "breakaway_rate",
                             "target_share", "targets_per_game", "catch_rate",
                             "epa_per_rush", "epa_per_play_pbp",
                             "carries_per_game_pbp"),
    "PPA / Usage"        = c("usg_rush", "usg_passing_downs", "usg_overall",
                             "usg_pass", "avg_PPA_rush", "total_PPA_rush",
                             "avg_PPA_all", "total_PPA_all"),
    "Combine"            = c("height_in", "weight", "forty", "vertical",
                             "broad_jump", "speed_score"),
    "Recruiting"         = c("recruit_stars", "recruit_rating", "recruit_rank"),
    "Context"            = c("age", "age_relative", "college_years",
                             "teammate_rush_yards", "n_drafted_skill",
                             "elite_teammate")
  )
)

# Precompute position-specific training cohorts for percentile refs.
# Use training data only (full outcomes) so percentiles are stable.
cohort_pool <- list(
  WR = wr_train |> filter(has_cfb_data),
  RB = rb_train |> filter(has_cfb_data)
)

# Position-specific columns. Listed once here so the Raw-data tab and any
# other consumer drop fields that aren't meaningful for the current player's
# position (e.g. WR-only `rec_final` on an RB row, RB-only `carries_final` on
# a WR row). Anything not in either list is treated as shared.
WR_ONLY_COLS <- c(
  # Final / penult / ante receiving counts
  "rec_final", "rec_yards_final", "rec_td_final",
  "rec_penult", "rec_yards_penult", "rec_td_penult",
  "rec_yards_ante", "rec_yds_yoy",
  # WR-side efficiency / per-game
  "ypr", "rec_yards_per_game", "rec_per_game", "rec_td_rate",
  # WR PBP suite
  "catch_rate_wr", "yards_per_target_wr", "yards_per_rec_wr",
  "explosive_rec_rate", "target_share_wr", "targets_per_game_wr",
  "epa_per_target_wr", "epa_per_play_wr_pbp", "has_wr_pbp",
  # WR PPA / usage / context
  "usg_pass", "avg_PPA_pass", "total_PPA_pass",
  "teammate_rec_yards", "is_possession_wr"
)
RB_ONLY_COLS <- c(
  # Final / penult / ante rushing counts
  "carries_final", "rush_yards_final", "rush_td_final",
  "carries_penult", "rush_yards_penult", "rush_td_penult",
  "rush_yards_ante", "rush_yds_yoy",
  # RB-side efficiency / per-game
  "ypc", "rush_td_rate", "rush_yards_per_game", "carries_per_game",
  "scrimmage_yards_per_game",
  # RB receiving (the RB pipeline's receiving-as-second-skill columns)
  "rb_rec", "rb_rec_yards", "rb_rec_td",
  "scrimmage_yards", "scrimmage_td", "yards_per_touch", "total_touches",
  "recv_share",
  # RB PBP suite
  "explosive_rate", "breakaway_rate", "target_share", "targets_per_game",
  "catch_rate", "epa_per_rush", "epa_per_play_pbp",
  "carries_per_game_pbp", "has_pbp",
  # RB PPA / usage / context
  "usg_rush", "usg_overall", "avg_PPA_rush", "total_PPA_rush",
  "avg_PPA_all", "total_PPA_all",
  "teammate_rush_yards", "is_scat_back"
)

# ── Metric dictionary ────────────────────────────────────────────────────────
# Short human-readable descriptions for every column shown in the percentile
# strip. Used to render a one-line caption under each metric label so the user
# doesn't have to remember what e.g. `epa_per_play_pbp` means.
METRIC_DESC <- list(
  # ── WR Final-season production ───────────────────────────────────────────
  rec_final              = "Receptions in best college season (peak, not necessarily senior year)",
  rec_yards_final        = "Receiving yards in best college season",
  rec_td_final           = "Receiving touchdowns in best college season",
  ypr                    = "Yards per reception in best season",
  rec_yards_per_game     = "Receiving yards / team games played",
  rec_per_game           = "Receptions / team games played",
  rec_td_rate            = "TDs per reception (TD efficiency)",
  dominator_rate         = "Player share of team scrimmage yards in best season — proxy for offensive centrality",
  # ── WR Penult / ante / YoY ───────────────────────────────────────────────
  rec_penult             = "Receptions in chronological year-before-draft season",
  rec_yards_penult       = "Receiving yards in year-before-draft season",
  rec_td_penult          = "Receiving TDs in year-before-draft season",
  rec_yards_ante         = "Receiving yards two seasons before draft (3rd-to-last college year)",
  rec_yds_yoy            = "Year-over-year change: actual final yards − penult yards. Negative = declining trajectory",
  # ── WR PBP (cfbfastR play-by-play, regular season only) ──────────────────
  catch_rate_wr          = "Receptions / targets (PBP-derived)",
  yards_per_target_wr    = "Receiving yards / target (efficiency on opportunities)",
  yards_per_rec_wr       = "Receiving yards per completed catch (PBP)",
  explosive_rec_rate     = "Share of receptions of 15+ yards (big-play rate)",
  target_share_wr        = "Player targets / team total targets",
  targets_per_game_wr    = "Targets per game played (volume signal)",
  epa_per_target_wr      = "Mean EPA on plays where this player was targeted",
  epa_per_play_wr_pbp    = "Mean EPA per touch (target+rush) — overall efficiency",
  # ── WR PPA / Usage (CFBD-equivalent, EPA-as-PPA proxy 2014+) ─────────────
  usg_pass               = "Player share of team passing-down plays",
  avg_PPA_pass           = "Mean Predicted Points Added per pass play (passing efficiency)",
  total_PPA_pass         = "Total PPA accumulated on pass plays (volume × efficiency)",
  # ── RB Final-season production ───────────────────────────────────────────
  carries_final          = "Carries in best college season",
  rush_yards_final       = "Rushing yards in best season",
  rush_td_final          = "Rushing TDs in best season",
  ypc                    = "Yards per carry in best season",
  rb_rec                 = "Receptions in same season as best rushing year (RB pass-catching)",
  rb_rec_yards           = "Receiving yards in best rushing season",
  rb_rec_td              = "Receiving TDs in best rushing season",
  scrimmage_yards        = "Rush yards + receiving yards in best season",
  scrimmage_td           = "Rush TDs + receiving TDs in best season",
  yards_per_touch        = "Scrimmage yards / total touches (carries+receptions)",
  total_touches          = "Carries + receptions in best season",
  rush_yards_per_game    = "Rush yards / team games played",
  carries_per_game       = "Carries / team games played",
  scrimmage_yards_per_game = "Scrimmage yards / team games played",
  rush_td_rate           = "Rush TDs per carry (TD efficiency)",
  recv_share             = "Receiving yards / scrimmage yards (pass-game involvement)",
  # ── RB Penult / ante / YoY ───────────────────────────────────────────────
  carries_penult         = "Carries in year-before-draft season",
  rush_yards_penult      = "Rush yards in year-before-draft season",
  rush_td_penult         = "Rush TDs in year-before-draft season",
  rush_yards_ante        = "Rush yards two seasons before draft (3rd-to-last college year)",
  rush_yds_yoy           = "Year-over-year change: actual final yards − penult yards. Negative = declining",
  # ── RB PBP ───────────────────────────────────────────────────────────────
  explosive_rate         = "Share of carries gaining 10+ yards",
  breakaway_rate         = "Share of carries gaining 15+ yards",
  target_share           = "Player targets / team total targets (RB receiving role)",
  targets_per_game       = "Targets per game played",
  catch_rate             = "Receptions / targets (RB-side)",
  epa_per_rush           = "Mean EPA per rush attempt (run-game efficiency)",
  epa_per_play_pbp       = "Mean EPA per touch (rush+target) — overall efficiency",
  carries_per_game_pbp   = "Carries / games played, PBP-derived (handles partial seasons better)",
  # ── RB PPA / Usage ───────────────────────────────────────────────────────
  usg_rush               = "Player share of team rushing plays",
  usg_passing_downs      = "Share of team pass-likely plays (3rd/4th-and-medium+)",
  usg_overall            = "Player share of all team plays (rush + pass)",
  avg_PPA_rush           = "Mean PPA per rush play (rushing efficiency)",
  total_PPA_rush         = "Total PPA on rush plays (volume × efficiency)",
  avg_PPA_all            = "Mean PPA across all plays (overall efficiency)",
  total_PPA_all          = "Total PPA across all plays",
  # ── Combine (NFL Combine + nflreadr) ─────────────────────────────────────
  height_in              = "Height in inches",
  weight                 = "Listed weight in pounds",
  forty                  = "40-yard dash time, seconds (lower = faster)",
  vertical               = "Vertical jump in inches",
  broad_jump             = "Broad jump in inches",
  speed_score            = "Bill Barnwell speed score: weight × 200 / forty^4 — size-adjusted speed",
  # ── Recruiting (247Sports composite, via cfbfastR) ───────────────────────
  recruit_stars          = "Star rating (2-5) from HS recruiting profile",
  recruit_rating         = "Composite rating (0-1 scale)",
  recruit_rank           = "National rank at position (1 = best). Lower is better",
  # ── Context / biographical ───────────────────────────────────────────────
  age                    = "Player age at time of draft",
  age_relative           = "Age vs same-position class mean (negative = younger than peers)",
  college_years          = "Years between recruit class and draft year (early-declarer signal)",
  teammate_rec_yards     = "Highest single-season rec yards by another skill teammate at same school",
  teammate_rush_yards    = "Highest single-season rush yards by another skill teammate at same school",
  age_adj_yards          = "Production yards adjusted for age — captures young breakouts",
  n_drafted_skill        = "Count of other WR/RB/TE drafted from same school in same class",
  elite_teammate         = "1 if a same-position teammate from same class went in Rd 1-2"
)

pct_of <- function(pos, col, val) {
  if (is.null(val) || !is.finite(val)) return(NA_real_)
  cohort <- cohort_pool[[pos]][[col]]
  if (is.null(cohort)) return(NA_real_)
  cohort <- cohort[is.finite(cohort)]
  if (length(cohort) < 5) return(NA_real_)
  mean(cohort <= val)
}

# Colours (match website-ish dark theme) ──────────────────────────────────────
PAL <- list(
  bg      = "#0B1226",
  surface = "#141B33",
  text    = "#F0F4FF",
  muted   = "#8A9AC0",
  accent  = "#3E8EF7",
  good    = "#2DD4A0",
  warn    = "#F5A623",
  bad     = "#F75757"
)

pct_color <- function(p) {
  if (is.na(p)) return(PAL$muted)
  if (p >= 0.80) return(PAL$good)
  if (p >= 0.60) return(PAL$accent)
  if (p >= 0.40) return(PAL$warn)
  PAL$bad
}

# ── UI ───────────────────────────────────────────────────────────────────────

theme_app <- bs_theme(
  version = 5,
  bg = PAL$bg, fg = PAL$text,
  primary = PAL$accent, secondary = PAL$muted,
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

ui <- page_sidebar(
  theme = theme_app,
  title = "Prospect Inspector",
  sidebar = sidebar(
    width = 280, bg = PAL$surface,
    selectInput("pos", "Position",
                choices = c("All", "WR", "RB"), selected = "All"),
    sliderInput("yr", "Draft class",
                min = min(master$draft_year), max = max(master$draft_year),
                value = c(min(master$draft_year), max(master$draft_year)),
                step = 1, sep = ""),
    checkboxInput("hits_only", "Hits only (made_it == 1)", FALSE),
    hr(),
    selectInput("player", "Player",
                choices = NULL, selected = NULL, selectize = TRUE),
    hr(),
    div(style = sprintf("color:%s;font-size:12px;", PAL$muted),
        HTML(paste0(
          "<b>Percentile colors</b><br>",
          "<span style='color:", PAL$good,  "'>■</span> &ge;80&nbsp;&nbsp;",
          "<span style='color:", PAL$accent, "'>■</span> 60–80&nbsp;&nbsp;",
          "<span style='color:", PAL$warn,  "'>■</span> 40–60&nbsp;&nbsp;",
          "<span style='color:", PAL$bad,   "'>■</span> &lt;40"
        )))
  ),

  # Header card
  uiOutput("header"),

  navset_card_tab(
    nav_panel("Percentile strip",
              uiOutput("pct_strip")),
    nav_panel("Production",
              plotOutput("prod_plot", height = "330px")),
    nav_panel("Combine",
              plotOutput("combine_plot", height = "330px")),
    nav_panel("Raw data",
              DTOutput("raw_tbl"))
  ),
  tags$style(HTML(sprintf("
    body { background:%s; }
    .bslib-card, .card { background:%s !important; border:1px solid rgba(255,255,255,0.05); }
    .nav-tabs .nav-link { color:%s; }
    .nav-tabs .nav-link.active { color:%s; border-color:rgba(255,255,255,0.05) rgba(255,255,255,0.05) %s; background:%s; }
    .pct-row { display:flex; align-items:flex-start; gap:10px; padding:6px 0; font-family:ui-monospace,monospace; font-size:12.5px; border-bottom:1px solid rgba(255,255,255,0.03); }
    .pct-label-wrap { flex:0 0 260px; display:flex; flex-direction:column; gap:2px; }
    .pct-label { color:%s; font-weight:600; }
    .pct-desc { color:%s; font-size:10.5px; font-family:Inter,system-ui,sans-serif; line-height:1.3; opacity:0.75; font-weight:400; letter-spacing:0; }
    .pct-val { flex:0 0 70px; text-align:right; color:%s; padding-top:2px; }
    .pct-bar { flex:1; height:8px; background:rgba(255,255,255,0.08); border-radius:4px; position:relative; overflow:hidden; margin-top:6px; }
    .pct-bar-fill { position:absolute; top:0; left:0; bottom:0; border-radius:4px; }
    .pct-bar-tick { position:absolute; top:-2px; bottom:-2px; width:2px; background:%s; border-radius:1px; }
    .pct-pct { flex:0 0 48px; text-align:right; font-weight:600; padding-top:2px; }
    h3.group-title { color:%s; font-size:13px; font-weight:600; text-transform:uppercase;
                     letter-spacing:1px; margin:18px 0 6px; border-bottom:1px solid rgba(255,255,255,0.06); padding-bottom:4px; }
  ",
  PAL$bg, PAL$surface,
  PAL$muted, PAL$text, PAL$surface, PAL$surface,
  PAL$muted, PAL$muted, PAL$text, PAL$text, PAL$accent)))
)

# ── Server ───────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Filtered player list
  filtered <- reactive({
    df <- master
    if (input$pos != "All") df <- df |> filter(position == input$pos)
    df <- df |> filter(draft_year >= input$yr[1], draft_year <= input$yr[2])
    if (isTRUE(input$hits_only)) df <- df |> filter(made_it == 1L)
    df |> arrange(draft_year, position, pick)
  })

  observe({
    df <- filtered()
    choices <- if (nrow(df) == 0) c("no matches" = "") else {
      setNames(
        paste(df$name_clean, df$position, df$draft_year, sep = "|"),
        sprintf("%s — %s %d (Rd %s #%s) %s",
                df$name, df$position, df$draft_year,
                coalesce(as.character(df$round), "?"),
                coalesce(as.character(df$pick),  "?"),
                df$college %||% "")
      )
    }
    updateSelectInput(session, "player",
                      choices = choices,
                      selected = choices[1])
  })

  current <- reactive({
    req(input$player)
    if (input$player == "") return(NULL)
    parts <- strsplit(input$player, "|", fixed = TRUE)[[1]]
    nc <- parts[1]; pos <- parts[2]; yr <- as.integer(parts[3])
    master |> filter(name_clean == nc, position == pos, draft_year == yr) |>
      slice(1)
  })

  # ── Header card ────────────────────────────────────────────────────────────
  output$header <- renderUI({
    r <- current(); if (is.null(r) || nrow(r) == 0) return(NULL)

    fmt_n <- function(x, d = 2) if (is.na(x)) "—" else formatC(x, digits = d, format = "f")
    fmt_i <- function(x) if (is.na(x)) "—" else format(round(x), big.mark = ",")

    p_hit  <- r$p_made_it %||% NA
    exp_v  <- r$exp_ppg   %||% NA
    act_v  <- r$actual_ppg %||% r$ppg
    made   <- r$made_it    %||% r$actual_made_it

    pill <- function(label, value, color) {
      div(style = sprintf(
        "background:%s22;color:%s;padding:6px 14px;border-radius:8px;
         font-family:ui-monospace,monospace;font-weight:700;font-size:15px;
         display:inline-flex;flex-direction:column;gap:2px;min-width:90px;",
        color, color),
        tags$div(label, style = sprintf("font-size:10px;color:%s;font-weight:500;letter-spacing:1px;text-transform:uppercase;", PAL$muted)),
        tags$div(value))
    }

    wrap_card <- function(...) {
      div(style = sprintf("background:%s;border:1px solid rgba(255,255,255,0.05);
                           border-radius:10px;padding:18px 22px;margin-bottom:14px;",
                          PAL$surface), ...)
    }

    wrap_card(
      div(style = "display:flex;justify-content:space-between;align-items:center;gap:20px;flex-wrap:wrap;",
          div(
            tags$h3(style = sprintf("color:%s;margin:0 0 4px;font-size:22px;font-weight:700;", PAL$text),
                    r$name),
            tags$div(style = sprintf("color:%s;font-size:13px;", PAL$muted),
                     sprintf("%s · %s · %d · Rd %s #%s · %s",
                             r$position, r$college %||% "?", r$draft_year,
                             fmt_i(r$round), fmt_i(r$pick),
                             r$tier %||% "?"))
          ),
          div(style = "display:flex;gap:10px;flex-wrap:wrap;",
              pill("P(hit)",  fmt_n(p_hit, 3),  PAL$accent),
              pill("Exp PPG", fmt_n(exp_v, 2),  PAL$warn),
              pill("Actual",  fmt_n(act_v, 2),
                   if (is.na(act_v)) PAL$muted
                   else if (!is.na(exp_v) && act_v >= exp_v) PAL$good else PAL$bad),
              pill("made_it", if (is.na(made)) "—" else as.character(made),
                   if (is.na(made)) PAL$muted
                   else if (made == 1) PAL$good else PAL$bad)
          )
      )
    )
  })

  # ── Percentile strip ───────────────────────────────────────────────────────
  output$pct_strip <- renderUI({
    r <- current(); if (is.null(r) || nrow(r) == 0) return(NULL)
    pos <- r$position
    groups <- FEATURE_GROUPS[[pos]]

    render_row <- function(feat) {
      v <- r[[feat]]
      if (is.null(v) || length(v) == 0) return(NULL)
      if (is.logical(v)) v <- as.integer(v)
      if (!is.numeric(v)) return(NULL)
      pct <- pct_of(pos, feat, v)
      col <- pct_color(pct)
      fill_w <- if (is.na(pct)) 0 else round(pct * 100, 1)
      tick   <- fill_w
      desc   <- METRIC_DESC[[feat]] %||% ""
      tags$div(class = "pct-row",
        tags$div(class = "pct-label-wrap",
          tags$div(class = "pct-label", feat),
          if (nzchar(desc)) tags$div(class = "pct-desc", desc)
        ),
        tags$div(class = "pct-val",
                 if (is.na(v)) "NA"
                 else if (v == round(v) && abs(v) < 1e6) format(v, big.mark = ",")
                 else sprintf("%.2f", v)),
        tags$div(class = "pct-bar",
          tags$div(class = "pct-bar-fill",
                   style = sprintf("width:%.1f%%;background:%s;", fill_w, col)),
          tags$div(class = "pct-bar-tick",
                   style = sprintf("left:%.1f%%;", tick))),
        tags$div(class = "pct-pct", style = sprintf("color:%s;", col),
                 if (is.na(pct)) "—" else sprintf("p%02d", round(pct * 100)))
      )
    }

    tagList(
      lapply(names(groups), function(grp) {
        rows <- lapply(groups[[grp]], render_row)
        rows <- rows[!sapply(rows, is.null)]
        if (length(rows) == 0) return(NULL)
        tagList(tags$h3(class = "group-title", grp), rows)
      })
    )
  })

  # ── Production trajectory plot ─────────────────────────────────────────────
  output$prod_plot <- renderPlot({
    r <- current(); if (is.null(r) || nrow(r) == 0) return(NULL)
    pos <- r$position

    cols_meta <- if (pos == "WR") list(
      list(metric = "Rec yds", ante = "rec_yards_ante",
           penult = "rec_yards_penult", final = "rec_yards_final"),
      list(metric = "Rec",     ante = NA,
           penult = "rec_penult", final = "rec_final"),
      list(metric = "Rec TD",  ante = NA,
           penult = "rec_td_penult", final = "rec_td_final")
    ) else list(
      list(metric = "Rush yds",  ante = "rush_yards_ante",
           penult = "rush_yards_penult", final = "rush_yards_final"),
      list(metric = "Carries",   ante = NA,
           penult = "carries_penult", final = "carries_final"),
      list(metric = "Rush TD",   ante = NA,
           penult = "rush_td_penult", final = "rush_td_final")
    )

    rows <- map_dfr(cols_meta, function(m) {
      tibble(
        metric = m$metric,
        season = c("Ante", "Penult", "Final"),
        value  = c(
          if (is.na(m$ante))   NA_real_ else r[[m$ante]]   %||% NA_real_,
          if (is.na(m$penult)) NA_real_ else r[[m$penult]] %||% NA_real_,
          if (is.na(m$final))  NA_real_ else r[[m$final]]  %||% NA_real_
        )
      )
    }) |> mutate(season = factor(season, levels = c("Ante", "Penult", "Final")),
                 metric = factor(metric, levels = unique(metric)))

    ggplot(rows, aes(x = season, y = value, fill = season)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = ifelse(is.na(value), "", format(round(value), big.mark = ","))),
                vjust = -0.4, color = PAL$text, size = 3.5) +
      facet_wrap(~metric, scales = "free_y") +
      scale_fill_manual(values = c("Ante" = PAL$muted,
                                   "Penult" = PAL$accent,
                                   "Final" = PAL$good)) +
      labs(x = NULL, y = NULL,
           title = "Season-by-season production (chronological)",
           subtitle = "Ante = 2 years before draft · Penult = year before draft · Final = draft-year college season") +
      theme_minimal(base_size = 12) +
      theme(
        plot.background   = element_rect(fill = PAL$bg, color = NA),
        panel.background  = element_rect(fill = PAL$surface, color = NA),
        panel.grid.major  = element_line(color = "#1E2540"),
        panel.grid.minor  = element_blank(),
        text              = element_text(color = PAL$text),
        axis.text         = element_text(color = PAL$muted),
        strip.text        = element_text(color = PAL$text, face = "bold"),
        strip.background  = element_rect(fill = PAL$bg, color = NA),
        plot.title        = element_text(color = PAL$text, face = "bold"),
        plot.subtitle     = element_text(color = PAL$muted, size = 10),
        legend.position   = "none"
      )
  }, bg = PAL$bg)

  # ── Combine vs cohort plot ────────────────────────────────────────────────
  output$combine_plot <- renderPlot({
    r <- current(); if (is.null(r) || nrow(r) == 0) return(NULL)
    pos <- r$position
    metrics <- c("height_in", "weight", "forty", "vertical", "broad_jump", "speed_score")

    cohort <- cohort_pool[[pos]] |> select(all_of(metrics)) |>
      pivot_longer(everything(), names_to = "metric", values_to = "value") |>
      filter(is.finite(value))

    player_row <- tibble(
      metric = metrics,
      value  = map_dbl(metrics, ~ { v <- r[[.x]]; if (is.null(v)) NA_real_ else as.numeric(v) })
    )

    metric_order <- metrics
    cohort$metric      <- factor(cohort$metric,      levels = metric_order)
    player_row$metric  <- factor(player_row$metric,  levels = metric_order)

    ggplot(cohort, aes(x = value)) +
      geom_density(fill = PAL$accent, color = NA, alpha = 0.35) +
      geom_vline(data = player_row,
                 aes(xintercept = value),
                 color = PAL$good, linewidth = 1.2, na.rm = TRUE) +
      geom_text(data = player_row,
                aes(x = value, y = 0,
                    label = ifelse(is.na(value), "—",
                                   ifelse(metric == "forty",
                                          sprintf("%.2f", value),
                                          sprintf("%.1f", value)))),
                color = PAL$good, vjust = -1.2, hjust = -0.1,
                size = 3.7, fontface = "bold", na.rm = TRUE) +
      facet_wrap(~metric, scales = "free", nrow = 2) +
      labs(x = NULL, y = NULL,
           title = sprintf("Combine measurables vs drafted %s cohort", pos),
           subtitle = "Green line = this player · shaded area = training-set distribution") +
      theme_minimal(base_size = 11) +
      theme(
        plot.background   = element_rect(fill = PAL$bg, color = NA),
        panel.background  = element_rect(fill = PAL$surface, color = NA),
        panel.grid.major  = element_line(color = "#1E2540"),
        panel.grid.minor  = element_blank(),
        text              = element_text(color = PAL$text),
        axis.text         = element_text(color = PAL$muted, size = 8.5),
        strip.text        = element_text(color = PAL$text, face = "bold"),
        strip.background  = element_rect(fill = PAL$bg, color = NA),
        plot.title        = element_text(color = PAL$text, face = "bold"),
        plot.subtitle     = element_text(color = PAL$muted, size = 10)
      )
  }, bg = PAL$bg)

  # ── Raw data table ─────────────────────────────────────────────────────────
  output$raw_tbl <- renderDT({
    r <- current(); if (is.null(r) || nrow(r) == 0) return(NULL)
    # Drop columns that aren't meaningful for this player's position.
    drop_cols <- c("name_clean", "source",
                   if (r$position == "WR") RB_ONLY_COLS else WR_ONLY_COLS)
    long <- r |>
      select(-any_of(drop_cols)) |>
      pivot_longer(everything(), names_to = "field", values_to = "value",
                   values_transform = list(value = as.character)) |>
      arrange(field)
    datatable(long, rownames = FALSE, options = list(pageLength = 25,
                                                     dom = "tip",
                                                     order = list()),
              style = "bootstrap5") |>
      formatStyle(columns = c("field","value"), color = PAL$text,
                  backgroundColor = PAL$surface)
  })
}

# ── Launch ───────────────────────────────────────────────────────────────────
shinyApp(ui, server, options = list(port = 8787, host = "127.0.0.1", launch.browser = FALSE))

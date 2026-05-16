# functions/helpers.R
# ─────────────────────────────────────────────────────────────────────────────
# Shared utilities for the prospect model pipeline.
# Source this file at the top of any script that needs these helpers.
# No library() calls here — callers load their own packages.
# ─────────────────────────────────────────────────────────────────────────────

# ── Name utilities ────────────────────────────────────────────────────────────

clean_name <- function(x) x |> str_to_lower() |> str_remove_all("[^a-z ]") |> str_squish()

# Canonicalize CFB stat names that appear under different formal/nickname
# variants across seasons. This keeps cross-season features such as penult/YoY
# attached to the same draft prospect without changing display names.
# Add new mappings (cfbfastR_name → draft_name, both lowercase, suffix-stripped)
# whenever you find a prospect whose stats won't join because the formal name
# in the player-stats endpoint differs from the draft name.
canonical_cfb_name <- function(x) {
  dplyr::recode(x,
    # Nickname / formal-name pairs
    "cameron skattebo"   = "cam skattebo",
    "michael washington" = "mike washington",
    .default = x
  )
}

strip_suffix <- function(x) str_remove(x, "\\s+(jr|sr|ii|iii|iv|v)$")

# NFL team code aliases. nflreadr / cfbfastR / PFR / ESPN each use slightly
# different code sets — landing-spot feature data was built with 2-letter
# nflreadr codes (KC, GB, LV) while load_draft_picks returns 3-letter PFR
# codes (KAN, GNB, LVR). normalize_team_code maps everything to the nflreadr
# 2-letter convention used by the landing-spot cache. Both sides of the join
# must call this before comparing.
normalize_team_code <- function(x) {
  pfr_to_nflreadr <- c(
    "KAN" = "KC",  "GNB" = "GB",  "LVR" = "LV",  "LAR" = "LA",
    "NWE" = "NE",  "NOR" = "NO",  "TAM" = "TB",  "SFO" = "SF",
    "ARZ" = "ARI", "BLT" = "BAL", "CLV" = "CLE", "HST" = "HOU",
    "OAK" = "LV",  "STL" = "LA",  "SD"  = "LAC", "SL"  = "LA"
  )
  ifelse(x %in% names(pfr_to_nflreadr), pfr_to_nflreadr[x], x)
}

add_stripped_names <- function(df, yards_col) {
  stripped <- df |>
    mutate(name_stripped = strip_suffix(name_clean)) |>
    filter(name_stripped != name_clean) |>
    mutate(name_clean = name_stripped) |>
    select(-name_stripped)
  # When the source df has a school column, group by (name, school) so transfer
  # players keep one row per school (e.g. Zachariah Branch USC 2024 + Georgia
  # 2025 both kept). Without this, the function collapsed to one row per name
  # and silently dropped the prospect's final-school row whenever an earlier
  # school had a higher yards_col value — score-side join (which keys on
  # name+final_school) then missed the match.
  group_cols <- c("name_clean",
                  intersect(c("school", "school_norm", "pos_team"), names(df)))
  bind_rows(df, stripped) |>
    group_by(across(all_of(group_cols))) |>
    slice_max({{ yards_col }}, n = 1, with_ties = FALSE) |>
    ungroup()
}

ensure_score_key <- function(df) {
  if (".score_key" %in% names(df)) return(df)
  gsis <- if ("gsis_id" %in% names(df)) df$gsis_id else rep(NA_character_, nrow(df))
  df |>
    mutate(
      .score_key = if_else(
        !is.na(gsis) & gsis != "",
        paste0("gsis:", gsis),
        paste("fallback", position, draft_year, name_clean, normalize_school(college), sep = "|")
      )
    )
}

key_cfb_table <- function(draft_df, stats_df) {
  if (is.null(stats_df) || nrow(stats_df) == 0 || ".score_key" %in% names(stats_df)) {
    return(stats_df)
  }
  if (!all(c("name_clean", "school") %in% names(stats_df))) {
    return(stats_df)
  }
  keyed_draft <- draft_df |>
    ensure_score_key() |>
    select(.score_key, name_clean, college)

  join_cfb_stats(keyed_draft, stats_df,
                 school_col_draft = "college", school_col_stats = "school") |>
    group_by(.score_key) |>
    dplyr::slice(1) |>
    ungroup()
}

key_cfb_extra <- function(draft_df, cfb_extra) {
  if (is.null(cfb_extra)) return(cfb_extra)
  for (nm in c("recv_actual_final", "recv_penult", "rush_actual_final", "rush_penult")) {
    if (nm %in% names(cfb_extra)) {
      cfb_extra[[nm]] <- key_cfb_table(draft_df, cfb_extra[[nm]])
    }
  }
  cfb_extra
}

join_feature_lookup <- function(df, lookup, cols) {
  join_key <- if (!is.null(lookup) && ".score_key" %in% names(df) && ".score_key" %in% names(lookup)) {
    ".score_key"
  } else {
    "name_clean"
  }
  lookup_small <- lookup |>
    select(any_of(c(join_key, cols))) |>
    distinct(across(all_of(join_key)), .keep_all = TRUE)
  left_join(df, lookup_small, by = join_key)
}

height_to_inches <- function(ht) {
  ft  <- as.integer(str_extract(ht, "^\\d+"))
  ins <- as.integer(str_extract(ht, "(?<=-)\\d+$"))
  ft * 12L + ins
}

# ── Draft tier ────────────────────────────────────────────────────────────────
# cfbfastR's `conference` field is severely corrupted: entire team-years get
# scrambled to wrong conferences. Examples observed in the data:
#   - LSU 2019 → "Sun Belt"      (actually SEC)
#   - Alabama 2019 → "ACC"       (actually SEC)
#   - Louisiana → "SEC"          (actually Sun Belt — G5)
#   - Western Michigan → "Big Ten" (actually MAC — G5)
#   - Boise State → "ACC"        (actually MWC — G5)
#   - Samford (FCS) → "SEC"
#   - North Dakota State (FCS) → "FBS Independents"
#   - UConn → "Big East"         (actually Independent)
#
# Because the conference field cannot be trusted for P4/G5/Other classification,
# we use an explicit school-name allowlist for P4 and ONLY fall back to the
# conference field for G5 vs Other. Conference-based P4 fallback is removed
# entirely — too many false positives.
#
# Realignment only moves teams between P4 conferences (P4 → P4 = no change),
# so this list is stable across the model's training era.

P4_SCHOOLS <- c(
  # SEC (current 16 + historic)
  "Alabama", "Arkansas", "Auburn", "Florida", "Georgia", "Kentucky", "LSU",
  "Mississippi State", "Missouri", "Ole Miss", "Oklahoma", "South Carolina",
  "Tennessee", "Texas", "Texas A&M", "Vanderbilt",
  # Big Ten (current 18)
  "Illinois", "Indiana", "Iowa", "Maryland", "Michigan", "Michigan State",
  "Minnesota", "Nebraska", "Northwestern", "Ohio State", "Oregon",
  "Penn State", "Purdue", "Rutgers", "UCLA", "USC", "Washington", "Wisconsin",
  # Big 12 (current 16 + historic)
  "Arizona", "Arizona State", "Baylor", "BYU", "Cincinnati", "Colorado",
  "Houston", "Iowa State", "Kansas", "Kansas State", "Oklahoma State",
  "TCU", "Texas Tech", "UCF", "Utah", "West Virginia",
  # ACC (current 17 + historic)
  "Boston College", "California", "Clemson", "Duke", "Florida State",
  "Georgia Tech", "Louisville", "Miami", "NC State", "North Carolina",
  "Pittsburgh", "SMU", "Stanford", "Syracuse", "Virginia", "Virginia Tech",
  "Wake Forest",
  # Pac historic (Oregon State / Wash State left after 2024 collapse)
  "Oregon State", "Washington State",
  # FBS Independent traditionally treated P4
  "Notre Dame",
  # ── Name variants seen in nflreadr / cfbfastR / draft picks ────────────────
  # "St." abbreviation form
  "Arizona St.", "Florida St.", "Iowa St.", "Kansas St.", "Michigan St.",
  "Mississippi St.", "Ohio St.", "Oklahoma St.", "Oregon St.", "Penn St.",
  "Washington St.",
  # Other variants
  "Mississippi",     # Ole Miss listed as "Mississippi" in some sources
  "Boston Col.",     # Boston College
  "Miami (FL)",      # disambiguates from Miami (OH) — MAC
  "Pitt"             # Pittsburgh
)

classify_tier <- function(conference, school = NULL) {
  g5      <- c("American Athletic", "Mountain West", "Sun Belt",
                "Mid-American", "Conference USA", "Western Athletic")
  school_is_p4 <- if (is.null(school)) rep(FALSE, length(conference))
                  else                  school %in% P4_SCHOOLS
  # P4: school-list ONLY. cfbfastR's conference field is too corrupted to
  # use as a P4 fallback (Louisiana → "SEC", Boise State → "ACC", etc.).
  # G5/Other: conference field is reliable enough for G5 detection.
  case_when(
    school_is_p4            ~ "P4",
    conference %in% g5      ~ "G5",
    TRUE                    ~ "Other"
  )
}

# ── Feature attachers ─────────────────────────────────────────────────────────
# Both training and scoring paths go through these so the deployed model and
# the per-class scoring see identical feature vectors. Each attacher is
# NA-safe: missing lookups → NA columns + a 0/1 has_* flag, never an error.

# Comp-stack: kNN over historical NFL outcomes (built by 08b_build_comp_features.R).
# Adds comp_weighted_ppg, comp_bust_rate, comp_median_ppg, has_comp_features.
# name_col auto-detects from the dataframe (training data has pfr_player_name;
# scoring data has name) — pass an explicit value to override.

attach_comp_features <- function(df, comp_features_path = "data/comp_features.rds",
                                  name_col = NULL) {
  if (is.null(name_col)) {
    name_col <- if ("pfr_player_name" %in% names(df)) "pfr_player_name"
                else if ("name" %in% names(df))      "name"
                else stop("attach_comp_features: no name column ",
                          "(looked for `pfr_player_name`, `name`)")
  }
  if (!file.exists(comp_features_path)) {
    warning("comp_features.rds not found — adding empty comp features")
    return(df |> mutate(
      comp_weighted_ppg = NA_real_,
      comp_bust_rate    = NA_real_,
      comp_median_ppg   = NA_real_,
      has_comp_features = 0L
    ))
  }
  comps <- readRDS(comp_features_path)
  df |>
    mutate(.name_clean_join = clean_name(.data[[name_col]])) |>
    left_join(
      comps |> select(name_clean, draft_year, position,
                      comp_weighted_ppg, comp_bust_rate, comp_median_ppg),
      by = c(".name_clean_join" = "name_clean", "draft_year", "position")
    ) |>
    mutate(has_comp_features = as.integer(!is.na(comp_weighted_ppg))) |>
    select(-.name_clean_join)
}

# Landing spot: per-(draft_team, draft_year) opportunity (built by
# 02d_build_landing_spot_features.R). Joined identically at training and
# scoring time.
#
# `df` must already carry a `draft_team` column. If it doesn't and a
# `draft_teams_lkp` is provided (e.g. at training time, pulling teams from
# nflreadr load_draft_picks), we backfill it via (gsis_id, draft_year).
#
# When landing_lkp is NULL (no roster data yet, e.g. for the upcoming
# draft class before training-camp rosters are released), all landing
# columns get NA and has_landing_data = 0L. XGBoost handles NA natively.

attach_landing_features <- function(df, position, landing_lkp = NULL,
                                     draft_teams_lkp = NULL) {
  pos <- toupper(position)
  if (!pos %in% c("WR", "RB")) stop("attach_landing_features: position must be WR or RB")

  # Optional draft_team backfill (training path uses gsis_id; scoring path
  # already carries draft_team from the draft pick load).
  if (!is.null(draft_teams_lkp) && !"draft_team" %in% names(df)) {
    df <- df |> left_join(draft_teams_lkp, by = c("gsis_id", "draft_year"))
  }

  has_lkp <- !is.null(landing_lkp) && "draft_team" %in% names(df)

  if (has_lkp) {
    # Normalize team codes on both sides — landing data uses nflreadr 2-letter
    # convention (KC, GB), draft picks use PFR 3-letter (KAN, GNB).
    df <- df |> mutate(.draft_team_norm = normalize_team_code(draft_team))
    landing_lkp <- landing_lkp |>
      mutate(.draft_team_norm = normalize_team_code(draft_team))
    if (pos == "WR") {
      df <- df |>
        left_join(
          landing_lkp |> select(.draft_team_norm, draft_year,
                                vacated_tgt_pct, incumbent_tgt_share,
                                n_ret_wr_50tgt, incumbent_wr1_age,
                                expected_depth_rank_wr, team_targets_prior,
                                has_landing_data),
          by = c(".draft_team_norm", "draft_year")
        ) |>
        rename(expected_depth_rank = expected_depth_rank_wr)
    } else {
      df <- df |>
        left_join(
          landing_lkp |> select(.draft_team_norm, draft_year,
                                vacated_carry_pct, incumbent_carry_share,
                                n_ret_rb_100carry, incumbent_rb1_age,
                                expected_depth_rank_rb, team_carries_prior,
                                has_landing_data),
          by = c(".draft_team_norm", "draft_year")
        ) |>
        rename(expected_depth_rank = expected_depth_rank_rb)
    }
    df |> mutate(has_landing_data = coalesce(has_landing_data, 0L)) |>
      select(-.draft_team_norm)
  } else {
    # Position-specific NA fill (only emits the columns the model actually uses)
    if (pos == "WR") {
      df |> mutate(
        vacated_tgt_pct     = NA_real_, incumbent_tgt_share = NA_real_,
        n_ret_wr_50tgt      = NA_real_, incumbent_wr1_age   = NA_real_,
        team_targets_prior  = NA_real_, expected_depth_rank = NA_real_,
        has_landing_data    = 0L
      )
    } else {
      df |> mutate(
        vacated_carry_pct   = NA_real_, incumbent_carry_share = NA_real_,
        n_ret_rb_100carry   = NA_real_, incumbent_rb1_age     = NA_real_,
        team_carries_prior  = NA_real_, expected_depth_rank   = NA_real_,
        has_landing_data    = 0L
      )
    }
  }
}

# Draft pick value chart (fitted by 02f_build_draft_value_chart.R from PFR
# weighted-career-AV, draft years 2002–2018). Returns the value of a pick
# number on a 0–100 scale (pick #1 = 100). Vectorised; out-of-range picks
# are clamped to [1, max_pick] before lookup.
pick_value <- function(picks, chart_path = "data/draft_value_chart.rds") {
  if (!file.exists(chart_path)) {
    warning("draft_value_chart.rds not found — pick_value() returning NA")
    return(rep(NA_real_, length(picks)))
  }
  chart <- readRDS(chart_path)
  lookup <- chart$lookup$value
  max_p  <- length(lookup)
  idx    <- pmin(pmax(round(picks), 1L), max_p)
  out    <- rep(NA_real_, length(picks))
  ok     <- !is.na(picks)
  out[ok] <- lookup[idx[ok]]
  out
}

# Breakout features (pre-computed by 02g_build_breakout_features.R from the
# full year-by-year stats cache). Adds:
#   breakout_age           youngest age at first dominator-threshold season
#   breakout_age_imputed   = breakout_age, with NA → 23 (XGBoost prefers a
#                            single numeric over a flag+NA split)
#   peak_dominator_pre22   max single-season team-share before age 22
#   peak_yards_pre21       max single-season yards before age 21
#   n_seasons_dominant     count of seasons at/above dominator threshold
#   has_breakout           1/0 coverage flag
#
# Joins on (suffix-stripped name_clean, draft_year, position). NA-safe — if
# the precomputed cache is missing we fill with NA + has_breakout=0.

# Age-conditioned dominator residual. The unadjusted dominator_rate isn't
# age-aware — a 25% target share at age 19 is materially more impressive than
# the same number at age 23. We model the expected dominator as a smooth
# function of age (loess fit on training data) and store the per-age mean +
# sd as a frozen lookup. The residual feature gives XGBoost a clean age-
# adjusted signal without re-fitting at score time.
#
# age_dom_curve: list of two named vectors ($mean, $sd) keyed by integer age.

attach_age_adj_dominator <- function(df, pos, curve_path = NULL) {
  if (is.null(curve_path)) {
    curve_path <- if (pos == "WR") "data/wr_age_dom_curve.rds"
                  else            "data/rb_age_dom_curve.rds"
  }
  if (!file.exists(curve_path)) {
    return(df |> dplyr::mutate(dominator_age_z     = NA_real_,
                                dominator_age_resid = NA_real_))
  }
  curve <- readRDS(curve_path)
  lookup_mean <- function(a) curve$mean[as.character(pmax(18, pmin(24, round(a))))]
  lookup_sd   <- function(a) curve$sd  [as.character(pmax(18, pmin(24, round(a))))]
  df |>
    dplyr::mutate(
      .exp_dom = ifelse(is.na(age), NA_real_, as.numeric(lookup_mean(age))),
      .sd_dom  = ifelse(is.na(age), NA_real_, as.numeric(lookup_sd(age))),
      dominator_age_resid = dominator_rate - .exp_dom,
      dominator_age_z     = dplyr::if_else(.sd_dom > 0,
                                            (dominator_rate - .exp_dom) / .sd_dom,
                                            NA_real_)
    ) |>
    dplyr::select(-.exp_dom, -.sd_dom)
}

attach_breakout_features <- function(df, pos,
                                      wr_path = "data/wr_breakout_features.rds",
                                      rb_path = "data/rb_breakout_features.rds") {
  na_cols <- list(
    breakout_age         = NA_real_,
    breakout_age_imputed = 23.0,
    peak_dominator_pre22 = NA_real_,
    peak_yards_pre21     = NA_real_,
    n_seasons_dominant   = NA_integer_,
    has_breakout         = 0L
  )
  path <- if (pos == "WR") wr_path else rb_path
  if (!file.exists(path)) {
    return(df |> dplyr::mutate(!!!na_cols))
  }
  brk <- readRDS(path) |>
    dplyr::select(name_clean, draft_year, position,
                  breakout_age, breakout_age_imputed,
                  peak_dominator_pre22, peak_yards_pre21,
                  n_seasons_dominant, has_breakout)
  # Source the join key — score path has `name`, training path has `pfr_player_name`.
  if (!"name_clean" %in% names(df)) {
    src_name <- if ("name" %in% names(df)) "name"
                else if ("pfr_player_name" %in% names(df)) "pfr_player_name"
                else stop("attach_breakout_features: no name column")
    df <- df |> dplyr::mutate(name_clean = clean_name(.data[[src_name]]))
  }
  df |>
    dplyr::mutate(.nc_match = strip_suffix(name_clean)) |>
    dplyr::left_join(brk, by = c(".nc_match" = "name_clean", "draft_year", "position")) |>
    dplyr::mutate(
      breakout_age_imputed = dplyr::coalesce(breakout_age_imputed, 23.0),
      has_breakout         = dplyr::coalesce(has_breakout, 0L)
    ) |>
    dplyr::select(-.nc_match)
}

# Mock-draft consensus → projected pick + draft-capital-delta features.
# Joins by (name_clean, draft_year). Players without mock coverage (early
# rounds where Walt/ESPN don't go that deep) get NA + has_mock_data = 0L.
#
# proj_pick           — projected overall pick from mock consensus
# proj_pick_value     — pick_value(proj_pick)
# actual_pick_value   — pick_value(pick)  (draft-day capital)
# draft_capital_delta — actual − projected. Positive ⇒ team drafted EARLIER
#                       than mocks (bullish "team belief"). Negative ⇒ slid.
# has_mock_data       — 1/0 coverage flag (XGBoost-friendly)
attach_draft_capital_features <- function(df, mock_path = "data/mock_draft_consensus.rds") {
  if (!file.exists(mock_path)) {
    warning("mock_draft_consensus.rds not found — adding empty draft-capital features")
    return(df |> mutate(
      proj_pick           = NA_real_,
      proj_pick_value     = NA_real_,
      actual_pick_value   = NA_real_,
      draft_capital_delta = NA_real_,
      has_mock_data       = 0L
    ))
  }
  # Source the join key — score path has `name`, training path has `pfr_player_name`.
  if (!"name_clean" %in% names(df)) {
    src_name <- if      ("name"            %in% names(df)) "name"
                else if ("pfr_player_name" %in% names(df)) "pfr_player_name"
                else stop("attach_draft_capital_features: no name column ",
                          "(looked for `name_clean`, `name`, `pfr_player_name`)")
    df <- df |> mutate(name_clean = clean_name(.data[[src_name]]))
  }
  # Two-pass join to absorb Jr/Sr/II/III mismatches (e.g. Walt has
  # "Brian Thomas Jr.", nflreadr has "Brian Thomas"). Same pattern used by
  # join_cfb_stats fallback. Pass 1 = exact name_clean; Pass 2 fills in the
  # still-NA rows by stripping suffixes on both sides.
  mocks <- readRDS(mock_path) |>
    select(name_clean, draft_year, position, proj_pick = projected_pick) |>
    mutate(name_clean_stripped = strip_suffix(name_clean)) |>
    distinct(name_clean, draft_year, .keep_all = TRUE)

  mocks_stripped <- mocks |>
    distinct(name_clean_stripped, draft_year, .keep_all = TRUE) |>
    select(name_clean_stripped, draft_year,
           proj_pick_strip = proj_pick)

  df |>
    left_join(mocks |> select(name_clean, draft_year, proj_pick),
              by = c("name_clean", "draft_year")) |>
    mutate(.name_strip = strip_suffix(name_clean)) |>
    left_join(mocks_stripped,
              by = c(".name_strip" = "name_clean_stripped", "draft_year")) |>
    mutate(
      proj_pick           = coalesce(proj_pick, proj_pick_strip),
      proj_pick_value     = pick_value(proj_pick),
      actual_pick_value   = pick_value(pick),
      draft_capital_delta = actual_pick_value - proj_pick_value,
      has_mock_data       = as.integer(!is.na(proj_pick))
    ) |>
    select(-any_of(c("proj_pick_strip", ".name_strip")))
}

# ── Teammate context ──────────────────────────────────────────────────────────

add_teammate_context <- function(draft_df) {
  draft_df <- draft_df |>
    group_by(position, draft_year) |>
    mutate(age_relative = age - mean(age, na.rm = TRUE)) |>
    ungroup() |>
    mutate(age_relative = coalesce(age_relative, 0))

  skill_counts <- draft_df |>
    filter(position %in% c("WR", "RB", "TE")) |>
    group_by(college, draft_year) |>
    mutate(n_skill_total = n()) |>
    ungroup() |>
    transmute(name_clean, college, draft_year,
              n_drafted_skill = n_skill_total - 1L)

  elite_picks <- draft_df |>
    filter(round <= 2) |>
    select(college, draft_year, position, elite_name = name_clean)

  elite_flags <- draft_df |>
    select(name_clean, college, draft_year, position) |>
    inner_join(elite_picks, by = c("college", "draft_year", "position"),
               relationship = "many-to-many") |>
    filter(elite_name != name_clean) |>
    distinct(name_clean, college, draft_year, position) |>
    mutate(elite_teammate = 1L)

  draft_df |>
    left_join(skill_counts, by = c("name_clean", "college", "draft_year")) |>
    left_join(elite_flags |> select(name_clean, college, draft_year, elite_teammate),
              by = c("name_clean", "college", "draft_year")) |>
    mutate(
      n_drafted_skill  = coalesce(n_drafted_skill, 0L),
      elite_teammate   = coalesce(elite_teammate, 0L),
      college_years    = NA_real_,
      has_recruit_year = 0L
    )
}

# ── School name normalizer ────────────────────────────────────────────────────
# Normalises college names so nflreadr and cfbfastR formats can be compared.
# E.g. "St. " → "State ", "(FL)" removal, "Ala-" prefix fix, etc.

normalize_school <- function(x) {
  x |>
    str_to_lower() |>
    str_remove_all("\\.") |>                         # "St." → "St"
    str_replace_all("\\bst\\b", "state") |>          # "St" → "State"
    str_replace_all("\\s*\\([^)]+\\)", "") |>        # remove "(FL)", "(OH)" etc.
    str_replace_all("ala-", "alabama ") |>           # "Ala-Birmingham" → "Alabama Birmingham"
    str_replace_all("la-", "louisiana ") |>          # "La-Monroe" → "Louisiana Monroe"
    str_squish()
}

# ── Two-pass CFB stats joiner ─────────────────────────────────────────────────
# Pass 1: exact name_clean match.
# Pass 2: for still-unmatched rows, try last-name + normalized-school match.
# This handles nickname/format differences (Daniel/Dan, "Jr." suffix variants,
# hyphenated names) while keeping the school constraint to avoid false positives.

join_cfb_stats <- function(draft_df, stats_df, by_col = "name_clean",
                            school_col_draft = "college",
                            school_col_stats = "school") {
  # Pre-dedupe: if a name_clean has multiple stats rows (e.g. a Power-5
  # prospect and an FCS player share a cleaned name, or the suffix-stripped
  # variant of "Brian Thomas Jr." collides with a different "Brian Thomas"),
  # the bare name join below fans the prospect out into one row per stat
  # entry. We collapse stats_df to a single best row per name_clean by
  # preferring (a) matching school via school_norm against ANY draft row's
  # college, then (b) highest yards. Multi-school transfer cases are still
  # handled by the per-school dedup inside process_*_year upstream.
  stat_cols <- setdiff(names(stats_df), c(by_col, school_col_stats, "conference"))
  first_stat <- stat_cols[1]
  draft_schools_norm <- unique(normalize_school(draft_df[[school_col_draft]]))
  stats_dedup <- stats_df |>
    mutate(.school_norm = normalize_school(!!sym(school_col_stats)),
           .school_match = .school_norm %in% draft_schools_norm) |>
    group_by(!!sym(by_col)) |>
    arrange(desc(.school_match), desc(!!sym(first_stat))) |>
    slice_head(n = 1) |>
    ungroup() |>
    select(-.school_norm, -.school_match)

  # Pass 1 — exact name match (keep school column for downstream joins)
  matched <- draft_df |>
    left_join(stats_dedup,
              by = setNames(by_col, by_col))

  # Identify rows that didn't get stats (all key stat columns NA)
  unmatched_idx <- which(is.na(matched[[first_stat]]))

  if (length(unmatched_idx) == 0) return(matched)

  # Pass 2 — last name + normalized school. Strip suffix BEFORE extracting
  # last name so "mike washington jr" → "washington" (not "jr"). Suffix
  # mismatch between draft data ("Mike Washington Jr.") and cfbfastR
  # ("Mike Washington") was silently dropping these prospects.
  stats_fallback <- stats_df |>
    mutate(
      last_name_clean  = str_extract(strip_suffix(!!sym(by_col)), "\\S+$"),
      school_norm      = normalize_school(!!sym(school_col_stats))
    )

  draft_fallback <- draft_df[unmatched_idx, ] |>
    mutate(
      last_name_clean = str_extract(strip_suffix(!!sym(by_col)), "\\S+$"),
      school_norm     = normalize_school(!!sym(school_col_draft))
    )

  # Pass 2 join on last name + school (keep school column for downstream joins)
  resolved <- draft_fallback |>
    left_join(
      stats_fallback |> select(-any_of(by_col)),
      by = c("last_name_clean", "school_norm"),
      relationship = "many-to-many"
    ) |>
    select(-last_name_clean, -school_norm) |>
    # If the fallback join matched a prospect to multiple stats rows (multiple
    # players sharing last name + school in PBP-derived caches), keep the row
    # with the most production. first_stat is the leading raw count column.
    group_by(across(all_of(c(by_col, school_col_draft)))) |>
    slice_max(!!sym(first_stat), n = 1, with_ties = FALSE) |>
    ungroup()

  newly_matched <- resolved |>
    filter(!is.na(!!sym(first_stat)))

  if (nrow(newly_matched) > 0) {
    message(sprintf(
      "  [join_cfb_stats] Resolved %d prospect(s) via last-name+school fallback: %s",
      nrow(newly_matched),
      paste(newly_matched[[by_col]], collapse = ", ")
    ))
  }

  still_missing <- resolved |>
    filter(is.na(!!sym(first_stat))) |>
    pull(by_col)

  if (length(still_missing) > 0) {
    message(sprintf(
      "  [join_cfb_stats] %d prospect(s) unresolved (FCS/no CFB data or true missing): %s",
      length(still_missing),
      paste(still_missing, collapse = ", ")
    ))
  }

  # Patch resolved rows back into the matched data frame
  matched[unmatched_idx, ] <- resolved
  matched
}

# ── CFB stats fetcher ─────────────────────────────────────────────────────────
# Requires cfbfastR to be loaded in the calling script.
#
# Note: the position filter for recv is intentionally REMOVED. The cfbfastR
# roster position field reflects what a player is listed as, not what they
# actually do (e.g. converted QBs, DBs who play WR). Keeping the filter caused
# ~76 legitimate WR prospects to get zero stats. The rush and rb_recv filters
# are kept because those are functional (only RBs/FBs accumulate rushing yards).

fetch_cfb_stats <- function(draft_year) {
  cfb_year <- draft_year - 1

  # Safe wrapper — cfbfastR returns an empty tibble / crashes on rate limits or
  # when the monthly API quota is exhausted. Always return a tibble with the
  # expected `player` column so downstream transmute() calls don't fail with
  # "object 'player' not found".
  #
  # Order of preference:
  #   1. Per-season local cache at data/cfb_<cat>_<year>.rds
  #      (built from PBP by build_cfb_season_from_pbp.R — needed when API quota
  #       is exhausted for recent seasons like 2024)
  #   2. Live cfbd_stats_season_player() API call
  #   3. Empty skeleton tibble (warn, don't crash)
  safe_season_player <- function(yr, cat) {
    empty <- tibble::tibble(
      player = character(), team = character(), conference = character(),
      position = character(),
      receiving_rec = integer(), receiving_yds = integer(),
      receiving_td = integer(), receiving_ypr = numeric(),
      rushing_car = integer(), rushing_yds = integer(),
      rushing_td = integer(), rushing_ypc = numeric()
    )
    cache_path <- sprintf("data/cfb_%s_%d.rds", cat, yr)
    if (file.exists(cache_path)) {
      message("    [cache] ", cache_path)
      out <- tryCatch(readRDS(cache_path), error = function(e) NULL)
      if (!is.null(out) && is.data.frame(out) && nrow(out) > 0 && "player" %in% names(out)) {
        return(out)
      }
    }
    out <- tryCatch(
      # season_type = "both" → regular + postseason. Critical for top prospects:
      # without it Egbuka 2024 reads 60/743/9 (regular) instead of 81/1011/10.
      cfbd_stats_season_player(year = yr, category = cat, season_type = "both"),
      error = function(e) { message("    [warn] fetch ", cat, " ", yr, " failed: ", conditionMessage(e)); NULL }
    )
    if (is.null(out) || !is.data.frame(out) || nrow(out) == 0 || !"player" %in% names(out)) {
      message("    [warn] empty/invalid response for ", cat, " ", yr, "; using empty tibble")
      return(empty)
    }
    # Cache the successful live fetch for future runs
    tryCatch(saveRDS(out, cache_path), error = function(e) NULL)
    out
  }

  message("  Fetching cfbfastR stats for ", cfb_year, "...")
  recv_raw <- safe_season_player(cfb_year, "receiving")
  rush_raw <- safe_season_player(cfb_year, "rushing")

  # Penultimate season: for best-season selection AND penult features
  message("  Fetching cfbfastR stats for ", cfb_year - 1L, " (penultimate)...")
  recv_raw_prev <- safe_season_player(cfb_year - 1L, "receiving")
  rush_raw_prev <- safe_season_player(cfb_year - 1L, "rushing")

  process_recv_year <- function(raw, season_year) {
    raw |>
      transmute(name_clean = canonical_cfb_name(clean_name(player)), school = team, conference,
                rec = receiving_rec, rec_yards = receiving_yds,
                rec_td = receiving_td, ypr = receiving_ypr,
                cfb_season = season_year,
                is_final_season = (season_year == cfb_year)) |>
      group_by(name_clean) |> slice_max(rec_yards, n = 1, with_ties = FALSE) |> ungroup()
  }

  # WR receiving: combine final + penult, keep best year per player.
  recv <- bind_rows(
    process_recv_year(recv_raw,      cfb_year),
    process_recv_year(recv_raw_prev, cfb_year - 1L)
  ) |>
    group_by(name_clean) |>
    slice_max(rec_yards, n = 1, with_ties = FALSE) |>
    ungroup() |>
    add_stripped_names(rec_yards)

  # WR actual final-year receiving (chronological, for YoY — separate from best-season)
  recv_actual_final <- recv_raw |>
    transmute(name_clean = canonical_cfb_name(clean_name(player)),
              school = team,
              rec_yards_actual_final = receiving_yds) |>
    group_by(name_clean) |> slice_max(rec_yards_actual_final, n = 1, with_ties = FALSE) |> ungroup() |>
    add_stripped_names(rec_yards_actual_final)

  # WR penultimate receiving (always from penult year, for trend features)
  recv_penult <- recv_raw_prev |>
    transmute(name_clean = canonical_cfb_name(clean_name(player)),
              school = team,
              rec_penult = receiving_rec, rec_yards_penult = receiving_yds,
              rec_td_penult = receiving_td) |>
    group_by(name_clean) |> slice_max(rec_yards_penult, n = 1, with_ties = FALSE) |> ungroup() |>
    add_stripped_names(rec_yards_penult)

  rb_recv <- recv_raw |>
    filter(position %in% c("RB", "FB", "ATH", "APB") | is.na(position)) |>
    transmute(name_clean = canonical_cfb_name(clean_name(player)),
              school = team,
              rb_rec = receiving_rec, rb_rec_yards = receiving_yds,
              rb_rec_td = receiving_td) |>
    group_by(name_clean) |> slice_max(rb_rec_yards, n = 1, with_ties = FALSE) |> ungroup() |>
    add_stripped_names(rb_rec_yards)

  # RB rushing: combine final + penult, keep best year per player (mirrors WR recv logic)
  process_rush_year <- function(raw, season_year) {
    raw |>
      filter(position %in% c("RB", "FB", "ATH", "APB") | is.na(position)) |>
      transmute(name_clean = canonical_cfb_name(clean_name(player)), school = team, conference,
                carries = rushing_car, rush_yards = rushing_yds,
                rush_td = rushing_td, ypc = rushing_ypc,
                cfb_season = season_year,
                is_final_season = (season_year == cfb_year)) |>
      group_by(name_clean) |> slice_max(rush_yards, n = 1, with_ties = FALSE) |> ungroup()
  }

  rush <- bind_rows(
    process_rush_year(rush_raw,      cfb_year),
    process_rush_year(rush_raw_prev, cfb_year - 1L)
  ) |>
    group_by(name_clean) |>
    slice_max(rush_yards, n = 1, with_ties = FALSE) |>
    ungroup() |>
    add_stripped_names(rush_yards)

  # RB actual final-year rushing (chronological, for YoY)
  rush_actual_final <- rush_raw |>
    filter(position %in% c("RB", "FB", "ATH", "APB") | is.na(position)) |>
    transmute(name_clean = canonical_cfb_name(clean_name(player)),
              school = team,
              rush_yards_actual_final = rushing_yds) |>
    group_by(name_clean) |> slice_max(rush_yards_actual_final, n = 1, with_ties = FALSE) |> ungroup() |>
    add_stripped_names(rush_yards_actual_final)

  # RB penultimate rushing
  rush_penult <- rush_raw_prev |>
    filter(position %in% c("RB", "FB", "ATH", "APB") | is.na(position)) |>
    transmute(name_clean = canonical_cfb_name(clean_name(player)),
              school = team,
              rush_yards_penult = rushing_yds, carries_penult = rushing_car,
              rush_td_penult = rushing_td) |>
    group_by(name_clean) |> slice_max(rush_yards_penult, n = 1, with_ties = FALSE) |> ungroup() |>
    add_stripped_names(rush_yards_penult)

  # Team-level volumes (for dominator rate) — indexed by (team, cfb_season) so
  # downstream joins can match on the player's actual best-stats year. Critical
  # for COVID opt-outs (e.g. Ja'Marr Chase 2020) where the prospect's stats
  # come from cfb_year - 1 but team-level cfb_year data refers to a roster
  # they weren't on. Mirrors training-pipeline behavior in 02_build_features.R.
  team_recv_vol <- bind_rows(
    recv_raw      |> mutate(cfb_season = cfb_year),
    recv_raw_prev |> mutate(cfb_season = cfb_year - 1L)
  ) |>
    group_by(team, cfb_season) |>
    summarize(team_rec_yards = sum(receiving_yds, na.rm = TRUE), .groups = "drop")
  team_rush_vol <- bind_rows(
    rush_raw      |> mutate(cfb_season = cfb_year),
    rush_raw_prev |> mutate(cfb_season = cfb_year - 1L)
  ) |>
    group_by(team, cfb_season) |>
    summarize(team_rush_yards = sum(rushing_yds, na.rm = TRUE), .groups = "drop")

  # Cache-aware wrapper — checks data/cfb_<kind>_<year>.rds (built from PBP by
  # build_cfb_season_from_pbp.R) before hitting the API. Same fallback chain
  # as safe_season_player() above.
  safe_endpoint <- function(yr, kind, live_fn) {
    cache_path <- sprintf("data/cfb_%s_%d.rds", kind, yr)
    if (file.exists(cache_path)) {
      message("    [cache] ", cache_path)
      out <- tryCatch(readRDS(cache_path), error = function(e) NULL)
      if (!is.null(out) && is.data.frame(out) && nrow(out) > 0) return(out)
    }
    out <- tryCatch(live_fn(yr),
                    error = function(e) { message("    [warn] ", kind, " ", yr, " failed: ", conditionMessage(e)); NULL })
    if (!is.null(out) && is.data.frame(out) && nrow(out) > 0) {
      tryCatch(saveRDS(out, cache_path), error = function(e) NULL)
    }
    out
  }

  # Team games — fetched for BOTH cfb_year and cfb_year - 1 so per-game rates
  # use the schedule from the player's actual stats year. (See team_recv_vol
  # comment above for the COVID opt-out case this fixes.)
  fetch_team_games_year <- function(yr) {
    tg <- safe_endpoint(yr, "team_games", function(y) {
      gi <- cfbd_game_info(year = y, season_type = "regular") |>
        filter(completed == TRUE)
      bind_rows(
        gi |> transmute(team = home_team),
        gi |> transmute(team = away_team)
      ) |>
        group_by(team) |> summarize(team_games = n(), .groups = "drop")
    })
    if (is.null(tg)) tibble(team = character(), team_games = integer()) else tg
  }
  team_games <- tryCatch({
    bind_rows(
      fetch_team_games_year(cfb_year)      |> mutate(cfb_season = cfb_year),
      fetch_team_games_year(cfb_year - 1L) |> mutate(cfb_season = cfb_year - 1L)
    )
  }, error = function(e) tibble(team = character(),
                                 cfb_season = integer(),
                                 team_games = integer()))

  # Usage rates (available 2016+)
  usage <- tryCatch({
    raw <- safe_endpoint(cfb_year, "usage", function(y) cfbd_player_usage(year = y))
    if (is.null(raw) || nrow(raw) == 0) stop("no usage data")
    raw |>
      mutate(name_clean = clean_name(name)) |>
      select(name_clean, school = team,
             usg_pass, usg_rush, usg_passing_downs, usg_overall) |>
      group_by(name_clean) |>
      slice_max(usg_overall, n = 1, with_ties = FALSE) |> ungroup() |>
      add_stripped_names(usg_overall)
  }, error = function(e) NULL)

  # PPA metrics (available 2016+)
  ppa <- tryCatch({
    raw <- safe_endpoint(cfb_year, "ppa",
                         function(y) cfbd_metrics_ppa_players_season(year = y, threshold = 0.1))
    if (is.null(raw) || nrow(raw) == 0) stop("no ppa data")
    raw |>
      mutate(name_clean = clean_name(name)) |>
      select(name_clean, school = team,
             avg_PPA_pass, avg_PPA_rush, avg_PPA_all,
             total_PPA_pass, total_PPA_rush, total_PPA_all) |>
      group_by(name_clean) |>
      slice_max(total_PPA_all, n = 1, with_ties = FALSE) |> ungroup() |>
      add_stripped_names(total_PPA_all)
  }, error = function(e) NULL)

  # PBP features (2014+) — pulled from cached aggregate built by 02b.
  # Filter to the player's best CFB season (final or penult) so the join picks
  # up the right year. The school join happens downstream via school_norm.
  pbp <- tryCatch({
    pbp_all <- readRDS("data/cfb_rb_pbp_features.rds")
    pbp_all |>
      dplyr::filter(season %in% c(cfb_year, cfb_year - 1L)) |>
      dplyr::mutate(
        name_clean  = clean_name(player),
        school_norm = normalize_school(pos_team)
      ) |>
      dplyr::select(
        name_clean, school_norm, season,
        explosive_rate, breakaway_rate, target_share, targets_per_game,
        catch_rate, epa_per_rush, epa_per_play_pbp, carries_per_game_pbp
      ) |>
      dplyr::group_by(name_clean, school_norm) |>
      # If player has both final + penult rows, prefer final (larger sample).
      dplyr::slice_max(season, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      add_stripped_names(epa_per_play_pbp)
  }, error = function(e) NULL)

  # WR PBP features (2014+) — pulled from cached aggregate built by 02c.
  # aDOT and YAC/rec are NOT available (cfbfastR has no air_yards/YAC columns).
  pbp_wr <- tryCatch({
    pbp_all <- readRDS("data/cfb_wr_pbp_features.rds")
    pbp_all |>
      dplyr::filter(season %in% c(cfb_year, cfb_year - 1L)) |>
      dplyr::mutate(
        name_clean  = clean_name(player),
        school_norm = normalize_school(pos_team)
      ) |>
      # Dedupe on (name, school_norm, season) — keep highest-volume row
      dplyr::group_by(name_clean, school_norm, season) |>
      dplyr::slice_max(targets_pbp, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::select(
        name_clean, school_norm, season,
        catch_rate_wr, yards_per_target_wr, yards_per_rec_wr,
        explosive_rec_rate, target_share_wr, targets_per_game_wr,
        epa_per_target_wr = epa_per_target, epa_per_play_wr_pbp
      ) |>
      dplyr::group_by(name_clean, school_norm) |>
      dplyr::slice_max(season, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      add_stripped_names(epa_per_play_wr_pbp)
  }, error = function(e) NULL)

  list(recv = recv, rush = rush, rb_recv = rb_recv,
       recv_actual_final = recv_actual_final, recv_penult = recv_penult,
       rush_actual_final = rush_actual_final, rush_penult = rush_penult,
       team_recv_vol = team_recv_vol, team_rush_vol = team_rush_vol,
       team_games = team_games, usage = usage, ppa = ppa,
       pbp = pbp, pbp_wr = pbp_wr)
}

# ── Combine + recruiting loaders ─────────────────────────────────────────────
# These use nflreadr (combine) and cached cfbfastR data (recruiting).
# Called once at scoring time to build lookup tables.

load_combine_lookup <- function() {
  comb <- load_combine() |>
    filter(pos %in% c("WR", "RB")) |>
    mutate(
      name_clean = strip_suffix(clean_name(player_name)),
      height_in  = height_to_inches(ht),
      # Normalize school for fuzzy matching against `college` field on prospects.
      school_norm = normalize_school(school)
    ) |>
    select(name_clean, draft_year = season, pos, school_norm,
           height_in, weight = wt, forty, vertical, broad_jump)
  # Deduplicate (player can appear at multiple positions; same (name, year)
  # in the strict-match pool, or same (name, school) in the NA-draft-year pool).
  comb |>
    group_by(name_clean, draft_year, school_norm) |>
    dplyr::slice(1) |>
    ungroup()
}

# Two-pass combine join with NA-draft-year fallback.
# nflreadr's combine table leaves draft_year = NA for players whose draft
# hasn't happened yet (entire 2026 class until April) or who went undrafted.
# Pass 1: strict join on (name_clean, draft_year)
# Pass 2: for prospects still missing forty, fall back to NA-draft-year combine
#         entries matched by (name_clean, normalized_school). The school check
#         disambiguates name collisions (e.g. two "Chris Bell" entries — one
#         at Norfolk State, one at Louisville).
join_combine_two_pass <- function(df, combine_full, college_col = "college") {
  combine_cols <- c("height_in", "weight", "forty", "vertical", "broad_jump")
  # Strip Jr/Sr/II/III suffix on the prospect side. The combine lookup already
  # has stripped names (load_combine_lookup applies strip_suffix). Score path
  # builds name_clean as plain clean_name() without stripping, so without this
  # step "mike washington jr" never matches the lookup's "mike washington".
  df <- df |> mutate(.nc_match = strip_suffix(name_clean))

  # Pass 1: strict (stripped name + draft_year)
  strict <- combine_full |> filter(!is.na(draft_year)) |>
    select(-school_norm) |>
    distinct(name_clean, draft_year, .keep_all = TRUE)
  out <- df |> left_join(strict, by = c(".nc_match" = "name_clean", "draft_year"))

  # Pass 2: name + school match against NA-draft-year combine rows
  loose <- combine_full |> filter(is.na(draft_year)) |>
    select(-draft_year) |>
    distinct(name_clean, school_norm, .keep_all = TRUE)
  out <- out |>
    mutate(.school_norm = normalize_school(.data[[college_col]])) |>
    left_join(loose |> rename_with(~ paste0(.x, ".loose"),
                                    .cols = all_of(combine_cols)),
              by = c(".nc_match" = "name_clean", ".school_norm" = "school_norm"))
  # Coalesce strict → loose for each combine column
  for (c in combine_cols) {
    loose_c <- paste0(c, ".loose")
    out[[c]] <- coalesce(out[[c]], out[[loose_c]])
  }
  out |> select(-any_of(c(".nc_match", ".school_norm", paste0(combine_cols, ".loose"))))
}

load_recruit_lookup <- function() {
  recruit_cache <- "data/cfb_recruiting_raw.rds"
  if (!file.exists(recruit_cache)) {
    message("  [load_recruit_lookup] No cached recruiting data; returning empty lookup")
    return(tibble(name_clean = character(), cfb_school = character(),
                  recruit_stars = numeric(), recruit_rating = numeric(),
                  recruit_rank = numeric(), recruit_year = numeric()))
  }
  recruit_raw <- readRDS(recruit_cache)

  # No position filter — many NFL WR/RBs were recruited as CB, S, QB, ATH, etc.
  # The school constraint in downstream joins prevents false positives.
  recruit_raw |>
    filter(recruit_type == "HighSchool") |>
    mutate(
      name_clean = strip_suffix(clean_name(name)),
      cfb_school = committed_to
    ) |>
    filter(!is.na(rating)) |>
    select(name_clean, cfb_school, recruit_stars = stars,
           recruit_rating = rating, recruit_rank = ranking, recruit_year) |>
    group_by(name_clean, cfb_school) |>
    dplyr::slice_max(recruit_rating, n = 1, with_ties = FALSE) |>
    ungroup()
}

# ── score_class helper pipeline ───────────────────────────────────────────────
# Each step is a small attacher: takes (df, pos, ...lookups), joins one
# feature group, and falls back to NA + 0/1 has_* flag when the lookup is
# missing. score_class() chains them; tests can call any one in isolation.

# Step 1: base CFB stats join + initial mutates (raw counts → *_final, has_cfb_data, etc.)
attach_base_cfb_stats <- function(draft_df, pos, recv_stats, rush_stats,
                                   rb_recv_stats = NULL, dy_mean, dy_sd) {
  draft_df <- ensure_score_key(draft_df)
  if (pos == "WR") {
    join_cfb_stats(draft_df, recv_stats,
                   school_col_draft = "college", school_col_stats = "school") |>
      mutate(
        tier                 = factor(classify_tier(conference, school = college), levels = c("P4", "G5", "Other")),
        log_pick             = log(pick + 1),
        sqrt_pick            = sqrt(pick),
        draft_year_sc        = (draft_year - dy_mean) / dy_sd,
        rec_final            = coalesce(rec, 0),
        rec_yards_final      = coalesce(rec_yards, 0),
        rec_td_final         = coalesce(rec_td, 0),
        ypr                  = coalesce(ypr, NA_real_),
        rec_td_rate          = rec_td_final / pmax(rec_final, 1),
        has_cfb_data         = !is.na(rec_yards),
        best_season_is_final = as.integer(coalesce(is_final_season, TRUE)),
        rec_yards_ante       = NA_real_
      ) |>
      select(-any_of("is_final_season"))
  } else {
    rb_recv_join_key <- if (!is.null(rb_recv_stats) &&
                            ".score_key" %in% names(draft_df) &&
                            ".score_key" %in% names(rb_recv_stats)) ".score_key" else "name_clean"
    join_cfb_stats(draft_df, rush_stats,
                   school_col_draft = "college", school_col_stats = "school") |>
      left_join(
        rb_recv_stats |>
          select(any_of(c(rb_recv_join_key, "rb_rec", "rb_rec_yards", "rb_rec_td"))) |>
          distinct(across(all_of(rb_recv_join_key)), .keep_all = TRUE),
        by = rb_recv_join_key
      ) |>
      mutate(
        tier             = factor(classify_tier(conference, school = college), levels = c("P4", "G5", "Other")),
        log_pick         = log(pick + 1),
        sqrt_pick        = sqrt(pick),
        draft_year_sc    = (draft_year - dy_mean) / dy_sd,
        carries_final    = coalesce(carries, 0),
        rush_yards_final = coalesce(rush_yards, 0),
        rush_td_final    = coalesce(rush_td, 0),
        ypc              = coalesce(ypc, NA_real_),
        rb_rec           = coalesce(rb_rec, 0),
        rb_rec_yards     = coalesce(rb_rec_yards, 0),
        rb_rec_td        = coalesce(rb_rec_td, 0),
        recv_share       = rb_rec_yards / (rush_yards_final + rb_rec_yards + .001),
        rush_td_rate     = rush_td_final / pmax(carries_final, 1),
        total_touches    = carries_final + rb_rec,
        scrimmage_td     = rush_td_final + rb_rec_td,
        scrimmage_yards  = rush_yards_final + rb_rec_yards,
        yards_per_touch  = scrimmage_yards / pmax(total_touches, 1),
        has_cfb_data     = !is.na(rush_yards),
        rush_yards_ante  = NA_real_
      )
  }
}

# Step 2: penultimate season + chronological YoY (rec_yds_yoy / rush_yds_yoy can go negative)
attach_penult_features <- function(df, pos, cfb_extra) {
  if (pos == "WR") {
    af <- cfb_extra$recv_actual_final; pn <- cfb_extra$recv_penult
    df <- if (!is.null(af))
      df |> join_feature_lookup(af, "rec_yards_actual_final")
    else df |> mutate(rec_yards_actual_final = NA_real_)
    if (!is.null(pn)) {
      df |>
        join_feature_lookup(pn, c("rec_penult", "rec_yards_penult", "rec_td_penult")) |>
        mutate(
          rec_penult       = coalesce(rec_penult, 0L),
          rec_yards_penult = coalesce(rec_yards_penult, 0L),
          rec_td_penult    = coalesce(rec_td_penult, 0L),
          has_penult       = as.integer(rec_yards_penult > 0),
          rec_yds_yoy      = if_else(has_penult == 1L & !is.na(rec_yards_actual_final),
                                      rec_yards_actual_final - rec_yards_penult, NA_real_)
        )
    } else {
      df |> mutate(rec_penult = 0L, rec_yards_penult = 0L, rec_td_penult = 0L,
                    has_penult = 0L, rec_yds_yoy = NA_real_)
    }
  } else {
    af <- cfb_extra$rush_actual_final; pn <- cfb_extra$rush_penult
    df <- if (!is.null(af))
      df |> join_feature_lookup(af, "rush_yards_actual_final")
    else df |> mutate(rush_yards_actual_final = NA_real_)
    if (!is.null(pn)) {
      df |>
        join_feature_lookup(pn, c("rush_yards_penult", "carries_penult", "rush_td_penult")) |>
        mutate(
          rush_yards_penult = as.numeric(coalesce(rush_yards_penult, 0)),
          carries_penult    = as.numeric(coalesce(carries_penult, 0)),
          rush_td_penult    = as.numeric(coalesce(rush_td_penult, 0)),
          has_penult        = as.integer(rush_yards_penult > 0),
          rush_yds_yoy      = if_else(has_penult == 1L & !is.na(rush_yards_actual_final),
                                       rush_yards_actual_final - rush_yards_penult, NA_real_)
        )
    } else {
      df |> mutate(rush_yards_penult = 0, carries_penult = 0, rush_td_penult = 0,
                    has_penult = 0L, rush_yds_yoy = NA_real_)
    }
  }
}

# Step 3: team volume → dominator rate + teammate volume
# Joins by (school, cfb_season) so team-level numerators come from the same
# year as the player's stats, not a fixed cfb_year. Without this, COVID
# opt-outs (Chase, Sewell) get garbage dominator/teammate values.
attach_team_volumes <- function(df, pos, cfb_extra) {
  if (pos == "WR") {
    vol <- cfb_extra$team_recv_vol
    if (is.null(vol))
      return(df |> mutate(teammate_rec_yards = NA_real_, dominator_rate = NA_real_))
    df |>
      left_join(vol, by = c("school" = "team", "cfb_season")) |>
      mutate(
        teammate_rec_yards = pmax(coalesce(team_rec_yards, 0) - rec_yards_final, 0),
        dominator_rate     = if_else(!is.na(team_rec_yards) & team_rec_yards > 0,
                                     rec_yards_final / team_rec_yards, NA_real_)
      ) |>
      select(-any_of("team_rec_yards"))
  } else {
    vol <- cfb_extra$team_rush_vol
    if (is.null(vol))
      return(df |> mutate(teammate_rush_yards = NA_real_, dominator_rate = NA_real_))
    df |>
      left_join(vol, by = c("school" = "team", "cfb_season")) |>
      mutate(
        teammate_rush_yards = pmax(coalesce(team_rush_yards, 0) - rush_yards_final, 0),
        dominator_rate      = if_else(!is.na(team_rush_yards) & team_rush_yards > 0,
                                      rush_yards_final / team_rush_yards, NA_real_)
      ) |>
      select(-any_of("team_rush_yards"))
  }
}

# Step 4: per-game rates — joins (school, cfb_season) so the divisor matches
# the actual schedule the player's stats came from (e.g. LSU 2019 = 13 games,
# LSU 2020 = 9 games. Chase's 1498 yds came from 2019 → divide by 13, not 9.)
attach_per_game_rates <- function(df, pos, cfb_extra) {
  has_games <- !is.null(cfb_extra$team_games) && nrow(cfb_extra$team_games) > 0
  if (pos == "WR") {
    if (!has_games)
      return(df |> mutate(rec_yards_per_game = NA_real_, rec_per_game = NA_real_))
    df |>
      left_join(cfb_extra$team_games, by = c("school" = "team", "cfb_season")) |>
      mutate(
        rec_yards_per_game = if_else(!is.na(team_games), rec_yards_final / team_games, NA_real_),
        rec_per_game       = if_else(!is.na(team_games), rec_final / team_games, NA_real_)
      ) |>
      select(-any_of("team_games"))
  } else {
    if (!has_games)
      return(df |> mutate(rush_yards_per_game = NA_real_, carries_per_game = NA_real_,
                          scrimmage_yards_per_game = NA_real_))
    df |>
      left_join(cfb_extra$team_games, by = c("school" = "team", "cfb_season")) |>
      mutate(
        rush_yards_per_game      = if_else(!is.na(team_games), rush_yards_final / team_games, NA_real_),
        carries_per_game         = if_else(!is.na(team_games), carries_final / team_games, NA_real_),
        scrimmage_yards_per_game = if_else(!is.na(team_games), scrimmage_yards / team_games, NA_real_)
      ) |>
      select(-any_of("team_games"))
  }
}

# Step 5: combine — speed_score (weight-adjusted forty) + position-specific archetype flag
attach_combine_features <- function(df, pos, combine_lkp) {
  if (is.null(combine_lkp)) {
    cols <- list(weight = NA_real_, height_in = NA_real_, forty = NA_real_,
                 vertical = NA_real_, broad_jump = NA_real_,
                 has_combine = 0L, speed_score = NA_real_)
    cols[[if (pos == "WR") "is_possession_wr" else "is_scat_back"]] <- 0L
    return(df |> mutate(!!!cols))
  }
  # Two-pass match: strict (name+year) for drafted players, fallback to
  # (name+school) for the NA-draft-year cohort (recent combines not yet
  # backfilled, plus historical UDFAs we generally won't hit anyway).
  df <- df |>
    join_combine_two_pass(combine_lkp |> filter(pos == !!pos) |> select(-pos)) |>
    mutate(
      has_combine = as.integer(!is.na(forty)),
      speed_score = if_else(!is.na(weight) & !is.na(forty),
                             (weight * 200) / (forty^4), NA_real_)
    )
  if (pos == "WR") {
    # Heavy + slow archetype = limited NFL ceiling
    df |> mutate(is_possession_wr = as.integer(coalesce(weight > 215 & forty > 4.50, FALSE)))
  } else {
    # Sub-195 lb backs rarely become feature RBs in the NFL
    df |> mutate(is_scat_back = as.integer(coalesce(weight < 195, FALSE)))
  }
}

# Step 6: recruiting (247Sports) — same logic both positions
attach_recruiting_features <- function(df, recruit_lkp) {
  if (is.null(recruit_lkp)) {
    return(df |> mutate(
      recruit_stars    = NA_real_, recruit_rating = NA_real_, recruit_rank = NA_real_,
      has_recruiting   = 0L, has_recruit_year = 0L, college_years = NA_real_
    ))
  }
  # The recruit lookup pre-strips Jr/Sr/II/III suffixes (load_recruit_lookup uses
  # strip_suffix). The score-side `name_clean` keeps the suffix (clean_name only),
  # so without this strip we miss Marvin Harrison Jr., Mike Washington Jr., etc.
  df |>
    mutate(.nc_match = strip_suffix(name_clean)) |>
    left_join(
      recruit_lkp |> select(name_clean, cfb_school, recruit_stars,
                            recruit_rating, recruit_rank, recruit_year),
      by = c(".nc_match" = "name_clean"),
      relationship = "many-to-many"
    ) |>
    group_by(name, draft_year) |>
    dplyr::slice_max(coalesce(recruit_rating, -Inf), n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(-.nc_match) |>
    mutate(
      has_recruiting   = as.integer(!is.na(recruit_rating)),
      has_recruit_year = as.integer(!is.na(recruit_year)),
      college_years    = if_else(!is.na(recruit_year),
                                  as.numeric(draft_year - recruit_year), NA_real_),
      college_years    = if_else(!is.na(college_years) & college_years >= 2 & college_years <= 6,
                                  college_years, NA_real_)
    )
}

# Step 7: usage + PPA (2016+ — feature spec era-zeroes via has_usage / has_ppa)
attach_usage_ppa_features <- function(df, pos, cfb_extra) {
  usage_cols <- if (pos == "WR") c("usg_pass", "usg_passing_downs")
                else            c("usg_rush", "usg_pass", "usg_passing_downs", "usg_overall")
  ppa_cols   <- if (pos == "WR") c("avg_PPA_pass", "total_PPA_pass")
                else            c("avg_PPA_rush", "total_PPA_rush",
                                  "avg_PPA_all",  "total_PPA_all")
  usage_check <- usage_cols[1]
  ppa_check   <- ppa_cols[1]

  # The usage/ppa tables can have multiple rows per name_clean — different
  # schools (e.g. FCS namesake collisions), suffix variants, or parsing
  # noise from cfbfastR. Joining by name_clean alone fans the prospect out
  # into one row per match. We dedupe each table to a single row per
  # name_clean, preferring a school match against the prospect's college.
  dedupe_by_name <- function(tbl, value_check) {
    if (!"school" %in% names(tbl)) return(tbl)
    draft_schools_norm <- unique(normalize_school(df[["college"]]))
    tbl |>
      mutate(.school_norm = normalize_school(school),
             .school_match = .school_norm %in% draft_schools_norm) |>
      group_by(name_clean) |>
      arrange(desc(.school_match), desc(!is.na(.data[[value_check]]))) |>
      slice_head(n = 1) |>
      ungroup() |>
      select(-.school_norm, -.school_match)
  }

  df <- if (!is.null(cfb_extra$usage)) {
    usage_tbl <- dedupe_by_name(cfb_extra$usage, usage_check)
    df |>
      left_join(usage_tbl |> select(name_clean, all_of(usage_cols)),
                by = "name_clean", suffix = c("", "_u")) |>
      mutate(has_usage = as.integer(!is.na(.data[[usage_check]])))
  } else {
    df |> mutate(!!!setNames(rep(list(NA_real_), length(usage_cols)), usage_cols),
                  has_usage = 0L)
  }
  if (!is.null(cfb_extra$ppa)) {
    ppa_tbl <- dedupe_by_name(cfb_extra$ppa, ppa_check)
    df |>
      left_join(ppa_tbl |> select(name_clean, all_of(ppa_cols)),
                by = "name_clean", suffix = c("", "_ppa")) |>
      mutate(has_ppa = as.integer(!is.na(.data[[ppa_check]])))
  } else {
    df |> mutate(!!!setNames(rep(list(NA_real_), length(ppa_cols)), ppa_cols),
                  has_ppa = 0L)
  }
}

# Step 8: PBP (2014+) — different slot + column lists per position. Joins
# on (name_clean, school_norm) since cfbfastR PBP names sometimes diverge
# from cfbd_stats_season_player names — school anchor prevents collisions.
attach_pbp_features <- function(df, pos, cfb_extra) {
  if (pos == "WR") {
    slot       <- cfb_extra$pbp_wr
    pbp_cols   <- c("catch_rate_wr", "yards_per_target_wr", "yards_per_rec_wr",
                    "explosive_rec_rate", "target_share_wr", "targets_per_game_wr",
                    "epa_per_target_wr", "epa_per_play_wr_pbp")
    flag_name  <- "has_wr_pbp"
    check_cols <- c("catch_rate_wr", "target_share_wr")
  } else {
    slot       <- cfb_extra$pbp
    pbp_cols   <- c("explosive_rate", "breakaway_rate", "target_share",
                    "targets_per_game", "catch_rate", "epa_per_rush",
                    "epa_per_play_pbp", "carries_per_game_pbp")
    flag_name  <- "has_pbp"
    check_cols <- c("explosive_rate", "target_share")
  }
  if (is.null(slot)) {
    return(df |> mutate(
      !!!setNames(rep(list(NA_real_), length(pbp_cols)), pbp_cols),
      !!flag_name := 0L
    ))
  }
  df |>
    mutate(school_norm_tmp = normalize_school(school)) |>
    left_join(
      slot |> select(name_clean, school_norm, all_of(pbp_cols)),
      by = c("name_clean", "school_norm_tmp" = "school_norm"),
      relationship = "many-to-one"
    ) |>
    select(-school_norm_tmp) |>
    mutate(!!flag_name := as.integer(!is.na(.data[[check_cols[1]]]) |
                                      !is.na(.data[[check_cols[2]]])))
}

# ── Score one draft class ─────────────────────────────────────────────────────
# Requires tidymodels to be loaded in the calling script.
# dy_mean / dy_sd: draft_year scaling params computed from training data.
# cfb_extra: additional data from fetch_cfb_stats() (penult, team vols, usage, ppa)
# combine_lkp / recruit_lkp: preloaded lookups (built once, passed to all calls)

score_class <- function(draft_df, position, bust_model, prod_model,
                        recv_stats, rush_stats, rb_recv_stats = NULL,
                        dy_mean, dy_sd,
                        cfb_extra = NULL,
                        combine_lkp = NULL, recruit_lkp = NULL,
                        landing_lkp = NULL,
                        hurdle_base_rate = NULL,
                        bucket_xgb_model = NULL, bucket_clm_model = NULL) {
  # hurdle_base_rate: training-set P(made_it). Used to shrink the bust
  # classifier toward the base rate — the classifier is near-useless for RBs
  # (OOS Brier skill ~-0.06) so we blend toward the prior.
  pos <- str_to_upper(position)

  # ── Feature pipeline: same shape WR/RB; each step is NA-safe so missing
  #    lookups produce flagged-NA columns rather than failures ──
  draft_df <- ensure_score_key(draft_df)
  cfb_extra <- key_cfb_extra(draft_df, cfb_extra)
  if (pos == "RB" && !is.null(rb_recv_stats)) {
    rb_recv_stats <- key_cfb_table(draft_df, rb_recv_stats)
  }

  df <- attach_base_cfb_stats(draft_df, pos, recv_stats, rush_stats,
                              rb_recv_stats, dy_mean, dy_sd) |>
    attach_penult_features(pos, cfb_extra)         |>
    attach_team_volumes(pos, cfb_extra)             |>
    attach_per_game_rates(pos, cfb_extra)           |>
    attach_combine_features(pos, combine_lkp)       |>
    attach_recruiting_features(recruit_lkp)         |>
    attach_usage_ppa_features(pos, cfb_extra)       |>
    attach_pbp_features(pos, cfb_extra)             |>
    attach_landing_features(pos, landing_lkp = landing_lkp) |>
    attach_breakout_features(pos) |>
    attach_age_adj_dominator(pos) |>
    attach_comp_features() |>
    attach_draft_capital_features()

  bust_prob    <- predict(bust_model$fit, df, type = "prob") |> pull(.pred_made_it)
  # Quantile models (e.g. RB τ=0.65) carry a baked recipe + raw xgb model.
  # Standard models use a tidymodels workflow fit directly.
  if (isTRUE(prod_model$type == "quantile")) {
    df_baked     <- bake(prod_model$recipe, new_data = df)
    X_score      <- as.matrix(df_baked |> select(-any_of("log_ppg")))
    log_ppg_pred <- predict(prod_model$fit, xgb.DMatrix(X_score))
  } else {
    log_ppg_pred <- predict(prod_model$fit, df) |> pull(.pred)
  }

  # Hurdle model: E[PPG] = P(producer) * E[PPG | producer]
  # prod_model is trained on log(ppg) for producers only, so back-transform with exp()
  #
  # ── Hurdle-probability combiner (tuned via 13_bust_tune.R, 2026-04-28) ────
  #
  # Sweep over (identity / shrink-to-base / linear / power_shrink / isotonic /
  # iso_shrink / iso_linear) per position, with leave-one-fold-out validation
  # over 2016–2023 OOS predictions. Winners are stable across all 8 folds:
  #
  #   WR: power_shrink(alpha=0.85, gamma=0.5)   p_eff = clip(0.85 * sqrt(p))
  #       LOFO MAE 2.39 (vs identity 2.39 — neutral)
  #   RB: iso_linear(alpha=1.0, beta=-0.05)     p_eff = clip(iso(p) - 0.05)
  #       LOFO MAE 2.89 (vs shrink α=0.25 baseline 3.10 — Δ=-0.21)
  #
  # See output/bust_tune/lofo_results.csv for per-fold winners. Combined OOS
  # MAE drops 2.67 → 2.59.
  clip01 <- function(x) pmax(0, pmin(1, x))
  iso_apply <- function(p, model) {
    if (is.null(model$iso_x) || is.null(model$iso_y)) return(p)
    approx(model$iso_x, model$iso_y,
           xout = pmax(pmin(p, max(model$iso_x)), min(model$iso_x)),
           rule = 2, ties = "ordered")$y
  }
  p_eff <- if (pos == "RB") {
    clip01(iso_apply(bust_prob, bust_model) - 0.05)
  } else {
    clip01(0.85 * sqrt(bust_prob))
  }

  df_pred <- df |>
    mutate(
      p_made_it = bust_prob,                # raw classifier for diagnostics
      p_eff     = p_eff,
      exp_ppg   = pmax(p_eff * exp(log_ppg_pred), 0)
    )

  # Ordinal-bucket model (XGB multiclass + clm ensemble) — coexists with the
  # continuous hurdle prediction and adds a distribution over outcome
  # buckets {bust, flex, elite, league_winner}. NA-safe when models aren't
  # supplied (e.g. score_class called from CV harness).
  if (!is.null(bucket_xgb_model) && !is.null(bucket_clm_model)) {
    df_pred <- attach_bucket_predictions(df_pred, bucket_xgb_model,
                                          bucket_clm_model, pos)
  } else {
    df_pred <- df_pred |> mutate(
      p_bust             = NA_real_, p_bench            = NA_real_,
      p_flex             = NA_real_, p_elite            = NA_real_,
      p_league_winner    = NA_real_,
      p_bust_lo          = NA_real_, p_bench_lo         = NA_real_,
      p_flex_lo          = NA_real_, p_elite_lo         = NA_real_,
      p_league_winner_lo = NA_real_,
      p_bust_hi          = NA_real_, p_bench_hi         = NA_real_,
      p_flex_hi          = NA_real_, p_elite_hi         = NA_real_,
      p_league_winner_hi = NA_real_,
      exp_ppg_bucket     = NA_real_,
      exp_ppg_bucket_lo  = NA_real_, exp_ppg_bucket_hi  = NA_real_,
      bucket_top1        = NA_character_
    )
  }

  df_pred |>
    select(name, position, draft_year, round, pick, college, tier,
           p_made_it, p_eff, exp_ppg,
           p_bust, p_bench, p_flex, p_elite, p_league_winner,
           p_bust_lo, p_bench_lo, p_flex_lo, p_elite_lo, p_league_winner_lo,
           p_bust_hi, p_bench_hi, p_flex_hi, p_elite_hi, p_league_winner_hi,
           exp_ppg_bucket, exp_ppg_bucket_lo, exp_ppg_bucket_hi, bucket_top1,
           any_of(c(
             # Volume — raw counts (both positions emit what's applicable)
             "rec_final", "rec_yards_final", "rec_td_final",
             "carries_final", "rush_yards_final", "rush_td_final",
             "rb_rec", "rb_rec_yards", "rb_rec_td",
             "scrimmage_yards", "scrimmage_td", "total_touches",
             # Penult / Ante / YoY (all positions)
             "rec_penult", "rec_yards_penult", "rec_td_penult",
             "rec_yards_ante", "rec_yds_yoy",
             "carries_penult", "rush_yards_penult", "rush_td_penult",
             "rush_yards_ante", "rush_yds_yoy",
             "has_penult",
             # WR traditional efficiency / context
             "ypr", "rec_td_rate", "rec_yards_per_game", "rec_per_game",
             # RB traditional efficiency / context
             "ypc", "rush_td_rate", "rush_yards_per_game", "carries_per_game",
             "scrimmage_yards_per_game", "yards_per_touch", "recv_share",
             # Shared derived
             "dominator_rate", "best_season_is_final",
             "is_possession_wr", "is_scat_back",
             # WR PBP (from cfbfastR)
             "catch_rate_wr", "yards_per_target_wr", "yards_per_rec_wr",
             "explosive_rec_rate", "target_share_wr", "targets_per_game_wr",
             "epa_per_target_wr", "epa_per_play_wr_pbp", "has_wr_pbp",
             # RB PBP (from cfbfastR)
             "explosive_rate", "breakaway_rate", "target_share",
             "targets_per_game", "catch_rate", "epa_per_rush",
             "epa_per_play_pbp", "carries_per_game_pbp", "has_pbp",
             # PPA / Usage
             "usg_pass", "usg_passing_downs",
             "usg_rush", "usg_overall",
             "avg_PPA_pass", "total_PPA_pass",
             "avg_PPA_rush", "total_PPA_rush",
             "avg_PPA_all", "total_PPA_all",
             "has_ppa", "has_usage",
             # Combine
             "height_in", "weight", "forty", "vertical", "broad_jump",
             "speed_score", "has_combine",
             # Recruiting
             "recruit_stars", "recruit_rating", "recruit_rank",
             "has_recruiting", "has_recruit_year",
             # Context / biographical
             "age", "age_relative", "college_years",
             "teammate_rec_yards", "teammate_rush_yards",
             "n_drafted_skill", "elite_teammate",
             # Landing spot / depth chart opportunity
             "draft_team",
             "vacated_tgt_pct", "incumbent_tgt_share",
             "n_ret_wr_50tgt", "incumbent_wr1_age", "team_targets_prior",
             "vacated_carry_pct", "incumbent_carry_share",
             "n_ret_rb_100carry", "incumbent_rb1_age", "team_carries_prior",
             "expected_depth_rank", "has_landing_data",
             # Draft-capital delta (mock vs actual)
             "proj_pick", "proj_pick_value", "actual_pick_value",
             "draft_capital_delta", "has_mock_data"
           )),
           has_cfb_data)
}

# 08c_deploy_comps.R
# ─────────────────────────────────────────────────────────────────────────────
# Extend comp_features.rds to cover deployed (post-training) prospects so the
# production model actually sees comp signal at scoring time — comp_weighted_ppg
# is the #2 feature by Gain (9% WR / 17% RB) and was silently NA on the 2024-26
# class until this script was added.
#
# Pipeline order:
#   03 → 04 → 05 → 08b (training comps) → 11 (CV) → 07 (initial score, NA comps
#   for deploy) → 08c (this script, fills in deploy comps) → 07 (rescore with
#   complete comps) → exports
#
# Pool: strictly-past mature-outcome players from training data. For deploy
# prospects with cfb_year > pool_year_max, this is the same pool that would
# have been available at draft time — leakage-free.
#
# Outputs:
#   data/comp_features.rds  — appended with 2024+ entries
# ─────────────────────────────────────────────────────────────────────────────

setwd("~/Projects/R/college_nfl_model")
suppressMessages(library(tidyverse))

source("functions/helpers.R")
source("functions/feature_specs.R")

set.seed(42)

# Pull the comp-build helpers from 08b. Re-uses CONFIG, era_normalize,
# compute_*, build_comp_features_for_position, etc.
src08b <- readLines("08b_build_comp_features.R")
build_marker <- grep("^# ── Run", src08b)
eval(parse(text = src08b[seq_len(build_marker[1] - 1)]))

# ── Load training pool + scored deploy prospects ─────────────────────────────

if (!file.exists("output/all_class_scores.rds")) {
  stop("output/all_class_scores.rds not found — run 07_score_all_classes.R first")
}

wr_train <- readRDS("data/wr_model_data.rds")
rb_train <- readRDS("data/rb_model_data.rds")
qb_train <- if (file.exists("data/qb_model_data.rds")) readRDS("data/qb_model_data.rds") else NULL
te_train <- if (file.exists("data/te_model_data.rds")) readRDS("data/te_model_data.rds") else NULL

scored <- readRDS("output/all_class_scores.rds") |> as_tibble()

# Append QB/TE scored rows (which carry their own deploy-class rows already)
# so the comp build sees them when computing comps for deploy years.
if (file.exists("output/qb_te_class_scores.rds")) {
  qb_te_scored <- readRDS("output/qb_te_class_scores.rds")
  scored <- bind_rows(scored, qb_te_scored)
}

train_years <- unique(c(wr_train$draft_year, rb_train$draft_year,
                         if (!is.null(qb_train)) qb_train$draft_year[!is.na(qb_train$made_it)],
                         if (!is.null(te_train)) te_train$draft_year[!is.na(te_train$made_it)]))
deploy_years <- setdiff(unique(scored$draft_year), train_years)
cat(sprintf("Deploy years (not in training): %s\n", paste(deploy_years, collapse = ", ")))

# Score output uses `name`; training uses `pfr_player_name`. Align.
scored <- scored |> rename(pfr_player_name = name)

# The score output may not carry every column the comp build needs (e.g.
# `rec_yards_per_game` and the per-game rates may exist; `made_it`/`ppg` won't).
# We only need: comp-feature inputs + name, draft_year, position, pick,
# has_cfb_data. Pad missing comp inputs with NA so era_normalize doesn't crash.

needed_cols <- unique(c(
  unlist(WR_COMP_FEATURES, use.names = FALSE),
  unlist(RB_COMP_FEATURES, use.names = FALSE),
  unlist(QB_COMP_FEATURES, use.names = FALSE),
  unlist(TE_COMP_FEATURES, use.names = FALSE),
  "pfr_player_name", "draft_year", "position", "pick", "has_cfb_data"
))
for (col in setdiff(needed_cols, names(scored))) {
  scored[[col]] <- NA_real_
}
scored$ppg <- NA_real_  # deploy: no observed outcome

# ── Build deploy-side WR/RB tables that mirror the training-data shape ──────
# build_comp_features_for_position requires has_cfb_data + the comp inputs.
# It computes per-year, so multi-year deploy data is fine.

build_deploy_for_position <- function(scored_df, train_df, feature_list, pos_label) {
  cat(sprintf("\n══ Build deploy comps: %s ══\n", pos_label))

  deploy_pos <- scored_df |>
    filter(position == pos_label) |>
    select(any_of(c(needed_cols, "ppg")))

  if (nrow(deploy_pos) == 0) {
    cat("  No deploy prospects.\n")
    return(tibble())
  }

  # Combine training + deploy. Pool selection inside build_comp_features
  # uses `!is.na(ppg)` AND `draft_year < yr` — deploy prospects (ppg = NA)
  # are excluded from the pool, training prospects (ppg observed) are
  # included. Each deploy year only sees strictly-past training pool.
  combined <- bind_rows(
    train_df |> select(any_of(c(needed_cols, "ppg"))),
    deploy_pos
  )

  cat(sprintf("  Combined rows: %d (train=%d, deploy=%d)\n",
              nrow(combined), nrow(train_df), nrow(deploy_pos)))

  # Reuse the (now-vectorized) comp builder. CONFIG is the CV-tuned default
  # from 08b (n_comps=8, bandwidth=1.0, pick_window=40).
  comps_all <- build_comp_features_for_position(combined, feature_list, pos_label, CONFIG)

  # Keep only deploy-year comp rows
  comps_all |> filter(draft_year %in% deploy_years)
}

wr_deploy_comps <- build_deploy_for_position(scored, wr_train, WR_COMP_FEATURES, "WR")
rb_deploy_comps <- build_deploy_for_position(scored, rb_train, RB_COMP_FEATURES, "RB")
qb_deploy_comps <- if (!is.null(qb_train))
  build_deploy_for_position(scored, qb_train, QB_COMP_FEATURES, "QB") else tibble()
te_deploy_comps <- if (!is.null(te_train))
  build_deploy_for_position(scored, te_train, TE_COMP_FEATURES, "TE") else tibble()

# ── Append to comp_features.rds ──────────────────────────────────────────────

existing <- readRDS("data/comp_features.rds")
updated <- bind_rows(existing, wr_deploy_comps, rb_deploy_comps,
                     qb_deploy_comps, te_deploy_comps) |>
  distinct(name_clean, draft_year, position, .keep_all = TRUE)

cat("\n══ Coverage after extension ══\n")
updated |> group_by(position) |>
  summarize(
    n              = n(),
    n_with_cmp     = sum(!is.na(comp_weighted_ppg)),
    pct            = round(100 * mean(!is.na(comp_weighted_ppg)), 1),
    year_min       = min(draft_year),
    year_max       = max(draft_year),
    .groups        = "drop"
  ) |> print()

cat("\n── Deploy-year coverage ──\n")
updated |> filter(draft_year %in% deploy_years) |>
  group_by(draft_year, position) |>
  summarize(n = n(), with_cmp = sum(!is.na(comp_weighted_ppg)),
            pct = round(100 * mean(!is.na(comp_weighted_ppg)), 1),
            .groups = "drop") |> print(n = Inf)

saveRDS(updated, "data/comp_features.rds")
write_csv(updated, "data/comp_features.csv")
message(sprintf("\nSaved: data/comp_features.rds  (%d rows; %d new deploy entries)",
                nrow(updated), nrow(wr_deploy_comps) + nrow(rb_deploy_comps)))

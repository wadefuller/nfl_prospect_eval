# 04b_model_bust_qb_te.R
# ─────────────────────────────────────────────────────────────────────────────
# Trains the bust classifier for QB and TE prospects. Mirrors the WR/RB
# structure in 04_model_bust.R but uses a simpler recipe (no PBP/PPA/usage
# era-gating because we don't have those features for QB/TE in V1).
#
# Outputs: models/qb_bust_model.rds, models/te_bust_model.rds
# ─────────────────────────────────────────────────────────────────────────────

setwd("~/Projects/R/college_nfl_model")
suppressMessages({
  library(tidyverse)
  library(tidymodels)
  library(xgboost)
})

source("functions/helpers.R")
source("functions/feature_specs.R")

set.seed(42)

# Pre-2014 training cutoff for both — fewer reliable rookies before then
# and coverage shrinks quickly.
QB_YEAR_CUTOFF <- 2008
TE_YEAR_CUTOFF <- 2008

build_recipe <- function(model_df, pos = NULL) {
  rec <- recipe(made_it ~ ., data = model_df) |>
    update_role(draft_year, new_role = "ID")
  # Era zero-fill for PBP-derived features. has_qb_pbp / has_te_pbp lets
  # XGBoost learn "pre-PBP era" as a regime distinct from "had-data-but-zero".
  if (identical(pos, "QB") && "has_qb_pbp" %in% names(model_df)) {
    rec <- rec |> step_mutate(
      epa_per_dropback     = if_else(has_qb_pbp == 0L, 0, epa_per_dropback),
      epa_per_attempt      = if_else(has_qb_pbp == 0L, 0, epa_per_attempt),
      completion_pct_pbp   = if_else(has_qb_pbp == 0L, 0, completion_pct_pbp),
      sack_rate            = if_else(has_qb_pbp == 0L, 0, sack_rate),
      int_rate             = if_else(has_qb_pbp == 0L, 0, int_rate),
      negative_play_rate   = if_else(has_qb_pbp == 0L, 0, negative_play_rate),
      explosive_pass_rate  = if_else(has_qb_pbp == 0L, 0, explosive_pass_rate),
      late_down_epa        = if_else(has_qb_pbp == 0L, 0, late_down_epa),
      qb_share_team        = if_else(has_qb_pbp == 0L, 0, qb_share_team)
    )
  }
  if (identical(pos, "TE") && "has_te_pbp" %in% names(model_df)) {
    rec <- rec |> step_mutate(
      catch_rate_te        = if_else(has_te_pbp == 0L, 0, catch_rate_te),
      yards_per_target_te  = if_else(has_te_pbp == 0L, 0, yards_per_target_te),
      yards_per_rec_te     = if_else(has_te_pbp == 0L, 0, yards_per_rec_te),
      explosive_rec_rate_te = if_else(has_te_pbp == 0L, 0, explosive_rec_rate_te),
      target_share_te      = if_else(has_te_pbp == 0L, 0, target_share_te),
      targets_per_game_te  = if_else(has_te_pbp == 0L, 0, targets_per_game_te),
      epa_per_target_te    = if_else(has_te_pbp == 0L, 0, epa_per_target_te),
      epa_per_play_te_pbp  = if_else(has_te_pbp == 0L, 0, epa_per_play_te_pbp)
    )
  }
  rec |>
    step_unknown(all_nominal_predictors(), new_level = "unknown") |>
    step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
    step_impute_median(all_numeric_predictors()) |>
    step_nzv(all_predictors())
}

train_bust_model <- function(data, features, position_label, year_cutoff) {
  cat(sprintf("\n══ Bust model: %s ══\n", position_label))
  features <- intersect(features, names(data))
  model_df <- data |>
    filter(has_cfb_data, draft_year >= year_cutoff, !is.na(made_it)) |>
    select(all_of(c("made_it", features, "draft_year"))) |>
    mutate(made_it = factor(made_it, levels = c(0, 1),
                             labels = c("bust", "made_it")))
  cat("Rows with CFB data:", nrow(model_df), "\n")
  cat("Class balance     :", table(model_df$made_it), "\n")
  if (nrow(model_df) < 30 || length(unique(model_df$made_it)) < 2) {
    warning(sprintf("[%s] insufficient data — skipping", position_label))
    return(NULL)
  }

  rec  <- build_recipe(model_df, position_label)
  spec <- boost_tree(
    trees          = 500, tree_depth = 4, learn_rate = 0.025,
    mtry           = min(15, length(features)), min_n = 8,
    sample_size    = 0.8
  ) |>
    set_engine("xgboost",
               scale_pos_weight = sum(model_df$made_it == "bust") /
                                  sum(model_df$made_it == "made_it"),
               nthread = 4) |>
    set_mode("classification")

  wf <- workflow() |> add_recipe(rec) |> add_model(spec)
  fit_obj <- fit(wf, data = model_df)

  # Calibration check (in-sample by round group)
  preds <- predict(fit_obj, model_df, type = "prob") |>
    bind_cols(model_df |> select(made_it, draft_year))
  cat("\nCalibration (in-sample):\n")
  preds |>
    mutate(bucket = cut(.pred_made_it, c(0, .25, .5, .75, 1), include.lowest = TRUE)) |>
    group_by(bucket) |>
    summarise(n = n(), pred = mean(.pred_made_it),
              actual_rate = mean(made_it == "made_it"),
              .groups = "drop") |>
    print()

  list(fit = fit_obj, features = features, position = position_label)
}

qb_data <- readRDS("data/qb_model_data.rds")
te_data <- readRDS("data/te_model_data.rds")

qb_bust <- train_bust_model(qb_data, QB_BUST_FEATURES, "QB", QB_YEAR_CUTOFF)
te_bust <- train_bust_model(te_data, TE_BUST_FEATURES, "TE", TE_YEAR_CUTOFF)

if (!is.null(qb_bust)) saveRDS(qb_bust, "models/qb_bust_model.rds")
if (!is.null(te_bust)) saveRDS(te_bust, "models/te_bust_model.rds")
cat("\nSaved QB + TE bust models.\n")

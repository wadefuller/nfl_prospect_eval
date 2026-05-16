# 10c_quantile_tuned.R
# ─────────────────────────────────────────────────────────────────────────────
# Train XGBoost with pinball loss at τ=0.65 using the same time-based CV grid
# search as the production model. Compare predictions against the baseline
# (MSE) model globally and for all Rd1 RBs individually.
#
# Integration note: the baseline uses a hurdle model (p_made_it × cond_ppg).
# For producers (made_it=1), exp_ppg ≈ cond_ppg since p_made_it → 1.
# The quantile model targets cond_ppg directly for producers; we multiply by
# p_made_it from the baseline bust model to get a comparable exp_ppg.
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(tidymodels)
library(xgboost)
library(Matrix)

setwd("~/Projects/R/college_nfl_model")
source("functions/helpers.R")
set.seed(42)

TAU <- 0.65

# ── 1. Load data ──────────────────────────────────────────────────────────────

rb_data <- readRDS("data/rb_model_data.rds")
scores  <- read_csv("output/all_class_scores.csv", show_col_types = FALSE)

RB_PROD_FEATURES <- c(
  "sqrt_pick", "age", "draft_year_sc", "tier",
  "carries_final", "rush_yards_final", "rush_td_final", "ypc",
  "rb_rec", "rb_rec_yards", "rb_rec_td",
  "scrimmage_td", "yards_per_touch",
  "rush_yards_penult", "carries_penult", "rush_yards_ante",
  "rush_yds_yoy", "rush_td_rate", "recv_share",
  "teammate_rush_yards", "dominator_rate", "total_touches",
  "weight", "height_in", "forty", "vertical", "broad_jump", "speed_score",
  "usg_rush", "usg_passing_downs", "avg_PPA_rush", "total_PPA_rush",
  "usg_overall", "usg_pass", "avg_PPA_all", "total_PPA_all",
  "recruit_stars", "recruit_rating", "recruit_rank",
  "college_years", "age_relative", "n_drafted_skill", "elite_teammate",
  "has_penult", "has_ppa", "has_usage", "has_recruiting", "has_combine",
  "has_recruit_year", "is_scat_back"
)

train_all <- rb_data |>
  filter(has_cfb_data, draft_year > 2010, ppg > 0) |>
  mutate(
    log_ppg       = log(ppg),
    draft_year_sc = scale(draft_year)[, 1],
    tier          = factor(tier, levels = c("P4", "G5", "Other"))
  )

cat("Total RB producers (2011+):", nrow(train_all), "\n")

# ── 2. Preprocess ─────────────────────────────────────────────────────────────

rec <- recipe(log_ppg ~ .,
              data = train_all |> select(all_of(c("log_ppg", RB_PROD_FEATURES)))) |>
  step_unknown(all_nominal_predictors(), new_level = "unknown") |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_impute_median(all_numeric_predictors()) |>
  step_nzv(all_predictors())

prep_rec <- prep(rec, training = train_all |> select(all_of(c("log_ppg", RB_PROD_FEATURES))))
baked    <- bake(prep_rec, new_data = NULL)

X_all  <- as.matrix(baked |> select(-log_ppg))
y_all  <- baked$log_ppg
yr_all <- train_all$draft_year
pk_all <- train_all$pick

# ── 3. Time-based CV folds (mirrors production model) ─────────────────────────

cutoffs    <- c(2012, 2015, 2017, 2019, 2021)
fold_ids   <- seq_along(cutoffs)

make_cv_folds <- function(cutoff) {
  list(
    train = which(yr_all <= cutoff),
    test  = which(yr_all > cutoff & yr_all <= cutoff + 3)
  )
}

folds <- map(cutoffs, make_cv_folds) |>
  keep(~ length(.x$train) >= 10 & length(.x$test) > 0)

cat("CV folds:", length(folds), "\n")

# ── 4. Custom pinball (quantile) loss ─────────────────────────────────────────

pinball_obj <- function(preds, dtrain) {
  y    <- getinfo(dtrain, "label")
  err  <- y - preds
  grad <- ifelse(err >= 0, -TAU, 1 - TAU)
  hess <- rep(TAU * (1 - TAU), length(preds))
  list(grad = grad, hess = hess)
}

# Evaluation metric: mean pinball loss
pinball_eval <- function(preds, dtrain) {
  y   <- getinfo(dtrain, "label")
  err <- y - preds
  loss <- mean(ifelse(err >= 0, TAU * err, (TAU - 1) * err))
  list(metric = "pinball", value = loss)
}

# ── 5. Hyperparameter grid search ─────────────────────────────────────────────

set.seed(42)
n_grid <- 40

grid <- tibble(
  eta              = 10^runif(n_grid, -2.5, -1.2),
  max_depth        = sample(2:6, n_grid, replace = TRUE),
  min_child_weight = sample(c(3, 5, 8, 12, 20), n_grid, replace = TRUE),
  subsample        = runif(n_grid, 0.6, 1.0),
  colsample_bytree = runif(n_grid, 0.5, 0.9),
  lambda           = 10^runif(n_grid, -2, 2),
  alpha            = 10^runif(n_grid, -3, 0)
)

cat(sprintf("\nTuning %d hyperparameter combos across %d CV folds...\n",
            n_grid, length(folds)))

cv_scores <- map_dbl(seq_len(n_grid), function(i) {
  params <- as.list(grid[i, ])
  params$nthread <- 4

  fold_losses <- map_dbl(folds, function(fold) {
    dtrain <- xgb.DMatrix(X_all[fold$train, ], label = y_all[fold$train])
    dtest  <- xgb.DMatrix(X_all[fold$test,  ], label = y_all[fold$test])

    model <- xgb.train(
      params   = params,
      data     = dtrain,
      nrounds  = 800,
      obj      = pinball_obj,
      feval    = pinball_eval,
      watchlist = list(test = dtest),
      early_stopping_rounds = 30,
      maximize = FALSE,
      verbose  = 0
    )
    model$best_score
  })
  mean(fold_losses)
})

best_i      <- which.min(cv_scores)
best_params <- as.list(grid[best_i, ])
best_params$nthread <- 4

cat(sprintf("Best CV pinball loss: %.4f (combo %d)\n", cv_scores[best_i], best_i))
cat("Best params:\n"); print(as.data.frame(grid[best_i, ]))

# ── 6. Final model: train on all data ≤ 2020, evaluate on 2021+ ──────────────

train_idx <- which(yr_all <= 2020)
val_idx   <- which(yr_all >= 2021)

dtrain_final <- xgb.DMatrix(X_all[train_idx, ], label = y_all[train_idx])
dval_final   <- xgb.DMatrix(X_all[val_idx,  ], label = y_all[val_idx])

# Determine nrounds: run full training with early stopping on val
final_cv <- xgb.train(
  params    = best_params,
  data      = dtrain_final,
  nrounds   = 1200,
  obj       = pinball_obj,
  feval     = pinball_eval,
  watchlist = list(val = dval_final),
  early_stopping_rounds = 40,
  maximize  = FALSE,
  verbose   = 0
)
best_rounds <- final_cv$best_iteration
cat(sprintf("Best nrounds (early stopping): %d\n", best_rounds))

# Final fit on ALL training data (2011-2020)
final_model <- xgb.train(
  params  = best_params,
  data    = dtrain_final,
  nrounds = best_rounds,
  obj     = pinball_obj,
  verbose = 0
)

# ── 7. Predict on pseudo-val producers (2021+) ────────────────────────────────

log_pred_q  <- predict(final_model, dval_final)
ppg_pred_q  <- exp(log_pred_q)
ppg_actual  <- exp(y_all[val_idx])
pick_val    <- pk_all[val_idx]
name_val    <- train_all$pfr_player_name[val_idx]
year_val    <- yr_all[val_idx]
round_val   <- as.integer(as.character(train_all$round[val_idx]))

# Baseline: get exp_ppg from scores CSV (bust-adjusted); for producers
# p_made_it → high, so cond_ppg ≈ exp_ppg / p_made_it ≈ exp_ppg
baseline_lookup <- scores |>
  filter(position == "RB", !is.na(actual_ppg)) |>
  mutate(name_clean_match = clean_name(name)) |>
  select(name_clean_match, draft_year, pick, exp_ppg_base = exp_ppg,
         p_made_it, actual_ppg, actual_raw_ppg, actual_made_it)

val_df <- tibble(
  name       = name_val,
  draft_year = year_val,
  pick       = pick_val,
  round      = round_val,
  ppg_actual = ppg_actual,
  ppg_q65    = ppg_pred_q
) |>
  mutate(name_clean_match = clean_name(name)) |>
  left_join(baseline_lookup, by = c("name_clean_match", "draft_year", "pick")) |>
  mutate(
    # Baseline cond_ppg (undo bust adjustment for producers)
    ppg_base   = exp_ppg_base / pmax(p_made_it, 0.01),
    round_grp  = case_when(pick <= 32 ~ "Rd1", pick <= 64 ~ "Rd2", TRUE ~ "Rd3+") |>
                   factor(levels = c("Rd1", "Rd2", "Rd3+")),
    resid_base = ppg_actual - ppg_base,
    resid_q65  = ppg_actual - ppg_q65
  )

# ── 8. Global before/after summary ───────────────────────────────────────────

cat("\n══ Global before/after comparison (producers 2021+) ══════════════════\n")
val_df |>
  group_by(round_grp) |>
  summarise(
    n          = n(),
    bias_base  = round(mean(resid_base, na.rm = TRUE), 2),
    bias_q65   = round(mean(resid_q65), 2),
    Δbias      = round(mean(resid_q65) - mean(resid_base, na.rm = TRUE), 2),
    mae_base   = round(mean(abs(resid_base), na.rm = TRUE), 2),
    mae_q65    = round(mean(abs(resid_q65)), 2),
    Δmae       = round(mean(abs(resid_q65)) - mean(abs(resid_base), na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  print()

# ── 9. All Rd1 RBs — individual comparison ───────────────────────────────────

cat("\n══ Every Rd1 RB producer (2021+ pseudo-val) ══════════════════════════\n")
val_df |>
  filter(round_grp == "Rd1") |>
  select(name, draft_year, pick, actual_raw_ppg,
         pred_base = ppg_base, pred_q65 = ppg_q65,
         resid_base, resid_q65) |>
  mutate(across(where(is.numeric), ~ round(.x, 2))) |>
  print()

# ── 10. Visualise ─────────────────────────────────────────────────────────────

theme_nfl <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background   = element_rect(fill = "#1a1a2e", color = NA),
      panel.background  = element_rect(fill = "#16213e", color = NA),
      panel.grid.major  = element_line(color = "#2a2a4a", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      text              = element_text(color = "#c8ccd4"),
      plot.title        = element_text(color = "#ffffff", size = 15, face = "bold"),
      plot.subtitle     = element_text(color = "#8b9ab5", size = 11, margin = margin(b = 10)),
      axis.text         = element_text(color = "#8b9ab5"),
      axis.title        = element_text(color = "#c8ccd4"),
      strip.text        = element_text(color = "#ffffff", face = "bold"),
      strip.background  = element_rect(fill = "#0f3460", color = NA),
      legend.background = element_rect(fill = "#1a1a2e", color = NA),
      legend.text       = element_text(color = "#c8ccd4"),
      legend.title      = element_text(color = "#ffffff"),
      plot.margin       = margin(16, 16, 12, 16)
    )
}

WIN  <- "#22c55e"; LOSS <- "#ef4444"; GOLD <- "#f59e0b"; BLUE <- "#3b82f6"

# Plot A: Global bias before/after by round
bias_plot_df <- val_df |>
  group_by(round_grp) |>
  summarise(
    Baseline  = mean(resid_base, na.rm = TRUE),
    `τ=0.65`  = mean(resid_q65),
    .groups = "drop"
  ) |>
  pivot_longer(c(Baseline, `τ=0.65`), names_to = "model", values_to = "bias") |>
  mutate(model = factor(model, levels = c("Baseline", "τ=0.65")))

pA <- bias_plot_df |>
  ggplot(aes(x = round_grp, y = bias, fill = model)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5, alpha = 0.85) +
  geom_hline(yintercept = 0, color = "#5a6a85", linewidth = 0.7) +
  scale_fill_manual(values = c("Baseline" = BLUE, "τ=0.65" = GOLD), name = "Model") +
  labs(
    title    = "RB Model Bias: Baseline vs Quantile (τ=0.65)",
    subtitle = "Mean (actual − predicted) PPG by round · producers 2021+ pseudo-holdout",
    x = NULL, y = "Mean Bias (actual − pred PPG)"
  ) +
  theme_nfl()

# Plot B: Rd1 individual predictions
rd1_df <- val_df |>
  filter(round_grp == "Rd1") |>
  mutate(label = paste0(str_extract(name, "\\S+$"), "\n(", draft_year, ")")) |>
  select(label, pick, actual_raw_ppg, pred_base = ppg_base, pred_q65 = ppg_q65) |>
  pivot_longer(c(actual_raw_ppg, pred_base, pred_q65),
               names_to = "type", values_to = "ppg") |>
  mutate(type = factor(type,
                       levels = c("actual_raw_ppg", "pred_base", "pred_q65"),
                       labels = c("Actual PPG", "Baseline Pred", "τ=0.65 Pred")),
         label = fct_reorder(label, pick))

pB <- rd1_df |>
  ggplot(aes(x = label, y = ppg, color = type, group = type)) +
  geom_line(data = rd1_df |> filter(type != "Actual PPG"),
            aes(group = label), color = "#2a2a4a", linewidth = 0.5) +
  geom_point(aes(shape = type), size = 4.5, alpha = 0.9) +
  scale_color_manual(values = c("Actual PPG"     = WIN,
                                "Baseline Pred"  = BLUE,
                                "τ=0.65 Pred"    = GOLD),
                     name = NULL) +
  scale_shape_manual(values = c("Actual PPG" = 16, "Baseline Pred" = 17, "τ=0.65 Pred" = 15),
                     name = NULL) +
  labs(
    title    = "Rd1 RB Predictions: Baseline vs τ=0.65 vs Actual",
    subtitle = "Producers 2021+ (pseudo-holdout) · sorted by draft pick",
    x = NULL, y = "PPG"
  ) +
  theme_nfl() +
  theme(legend.position = "top")

dir.create("output/diagnostics", showWarnings = FALSE)
ggsave("output/diagnostics/07_quantile_global.png",  pA, width = 9,  height = 6, dpi = 150)
ggsave("output/diagnostics/08_quantile_rd1.png",     pB, width = 10, height = 6, dpi = 150)
cat("\nSaved: output/diagnostics/07_quantile_global.png\n")
cat("Saved: output/diagnostics/08_quantile_rd1.png\n")

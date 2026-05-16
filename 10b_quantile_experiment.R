# 10b_quantile_experiment.R
# ─────────────────────────────────────────────────────────────────────────────
# Explore quantile regression as an alternative to mean regression for RBs.
#
# Motivation: baseline model (MSE on log_ppg) underestimates Rd1–2 RBs —
# 100% of 2021–2023 Rd1/Rd2 picks outperformed, mean biases +5.9 / +4.0 PPG.
#
# Two approaches compared:
#   A) Linear quantile regression (quantreg::rq) on the preprocessed feature
#      matrix — interpretable, fast, sweeps τ cleanly.
#   B) XGBoost with custom pinball loss — same nonlinear capacity as the
#      production model, same features.
#
# Pseudo-validation: 2021+ producers held out of training (same time-based
# split as the production model's CV).
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(tidymodels)
library(xgboost)
library(quantreg)
library(Matrix)

setwd("~/Projects/R/college_nfl_model")
source("functions/helpers.R")

set.seed(42)

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

train_df <- rb_data |>
  filter(has_cfb_data, draft_year > 2010, ppg > 0) |>
  mutate(
    log_ppg       = log(ppg),
    draft_year_sc = scale(draft_year)[, 1],
    tier          = factor(tier, levels = c("P4", "G5", "Other"))
  )

cat("RB producers for training:", nrow(train_df), "\n")

# ── 2. Preprocess (recipe → matrix) ──────────────────────────────────────────

rec <- recipe(log_ppg ~ ., data = train_df |> select(all_of(c("log_ppg", RB_PROD_FEATURES)))) |>
  step_unknown(all_nominal_predictors(), new_level = "unknown") |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_impute_median(all_numeric_predictors()) |>
  step_nzv(all_predictors())

prep_rec    <- prep(rec, training = train_df |> select(all_of(c("log_ppg", RB_PROD_FEATURES))))
baked       <- bake(prep_rec, new_data = NULL)
X_all       <- as.matrix(baked |> select(-log_ppg))
y_all       <- baked$log_ppg
pick_all    <- train_df$pick
ppg_all     <- train_df$ppg
year_all    <- train_df$draft_year

# Time-based split: train ≤ 2020, pseudo-val ≥ 2021
train_idx <- which(year_all <= 2020)
val_idx   <- which(year_all >= 2021)

X_tr  <- X_all[train_idx, ]; y_tr  <- y_all[train_idx]
X_val <- X_all[val_idx,  ]; y_val <- y_all[val_idx]
pick_val <- pick_all[val_idx]; ppg_val <- ppg_all[val_idx]

cat("Train:", length(train_idx), "| Pseudo-val:", length(val_idx), "\n\n")

round_grp_val <- case_when(
  pick_val <= 32 ~ "Rd1",
  pick_val <= 64 ~ "Rd2",
  TRUE           ~ "Rd3+"
) |> factor(levels = c("Rd1", "Rd2", "Rd3+"))

taus <- c(0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80)

# ── 3A. Linear quantile regression sweep (quantreg::rq) ─────────────────────

cat("── Approach A: Linear quantile regression (rq) ─────────────────────────\n")

df_train_lm <- as.data.frame(cbind(log_ppg = y_tr, X_tr))
df_val_lm   <- as.data.frame(X_val)

rq_results <- map_dfr(taus, function(tau) {
  cat(sprintf("  τ = %.2f ...", tau))
  fit      <- rq(log_ppg ~ ., data = df_train_lm, tau = tau, method = "fn")
  log_pred <- predict(fit, newdata = df_val_lm)
  pred_ppg <- exp(log_pred)
  cat(" done\n")
  tibble(approach = "Linear QR", tau = tau, round_grp = round_grp_val,
         pred_ppg = pred_ppg, actual = ppg_val)
})

# ── 3B. XGBoost with custom pinball (quantile) loss ─────────────────────────

cat("\n── Approach B: XGBoost custom pinball loss ─────────────────────────────\n")

make_pinball <- function(tau) {
  function(preds, dtrain) {
    labels <- getinfo(dtrain, "label")
    err    <- labels - preds
    grad   <- ifelse(err >= 0, -tau, 1 - tau)
    hess   <- rep(tau * (1 - tau), length(preds))
    list(grad = grad, hess = hess)
  }
}

dtrain <- xgb.DMatrix(X_tr, label = y_tr)
dval_xgb <- xgb.DMatrix(X_val)

xgb_results <- map_dfr(taus, function(tau) {
  cat(sprintf("  τ = %.2f ...", tau))
  model <- xgb.train(
    params = list(
      eta              = 0.05,
      max_depth        = 4,
      min_child_weight = 5,
      subsample        = 0.8,
      colsample_bytree = 0.7,
      lambda           = 1,
      alpha            = 0.1,
      nthread          = 4
    ),
    data       = dtrain,
    nrounds    = 600,
    obj        = make_pinball(tau),
    verbose    = 0
  )
  log_pred <- predict(model, dval_xgb)
  pred_ppg <- exp(log_pred)
  cat(" done\n")
  tibble(approach = "XGBoost QR", tau = tau, round_grp = round_grp_val,
         pred_ppg = pred_ppg, actual = ppg_val)
})

# ── 4. Summarise bias + MAE by approach, τ, round ────────────────────────────

all_results <- bind_rows(rq_results, xgb_results)

summary_tbl <- all_results |>
  mutate(residual = actual - pred_ppg) |>
  group_by(approach, tau, round_grp) |>
  summarise(
    n    = n(),
    bias = round(mean(residual), 2),
    mae  = round(mean(abs(residual)), 2),
    .groups = "drop"
  )

cat("\n── Bias table (actual − predicted, pseudo-val 2021+) ───────────────────\n")
summary_tbl |>
  select(approach, tau, round_grp, bias) |>
  pivot_wider(names_from = round_grp, values_from = bias) |>
  arrange(approach, tau) |>
  print()

cat("\n── MAE table ────────────────────────────────────────────────────────────\n")
summary_tbl |>
  select(approach, tau, round_grp, mae) |>
  pivot_wider(names_from = round_grp, values_from = mae) |>
  arrange(approach, tau) |>
  print()

# ── 5. Visualise ─────────────────────────────────────────────────────────────

theme_nfl <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background  = element_rect(fill = "#1a1a2e", color = NA),
      panel.background = element_rect(fill = "#16213e", color = NA),
      panel.grid.major = element_line(color = "#2a2a4a", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      text             = element_text(color = "#c8ccd4"),
      plot.title       = element_text(color = "#ffffff", size = 15, face = "bold"),
      plot.subtitle    = element_text(color = "#8b9ab5", size = 11, margin = margin(b = 10)),
      axis.text        = element_text(color = "#8b9ab5"),
      axis.title       = element_text(color = "#c8ccd4"),
      strip.text       = element_text(color = "#ffffff", face = "bold"),
      strip.background = element_rect(fill = "#0f3460", color = NA),
      legend.background = element_rect(fill = "#1a1a2e", color = NA),
      legend.text      = element_text(color = "#c8ccd4"),
      legend.title     = element_text(color = "#ffffff"),
      plot.margin      = margin(16, 16, 12, 16)
    )
}

round_colors <- c("Rd1" = "#22c55e", "Rd2" = "#f59e0b", "Rd3+" = "#3b82f6")

# Baseline reference
baseline <- tibble(
  approach  = "Baseline",
  round_grp = factor(c("Rd1","Rd2","Rd3+"), levels = c("Rd1","Rd2","Rd3+")),
  bias      = c(5.87, 4.00, 1.02),
  mae       = c(5.87, 4.00, 2.82)
)

p_bias <- summary_tbl |>
  ggplot(aes(x = tau, y = bias, color = round_grp, group = round_grp)) +
  geom_hline(yintercept = 0, color = "#5a6a85", linetype = "dashed", linewidth = 0.7) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3, alpha = 0.9) +
  # Baseline as × at left edge
  geom_point(data = baseline |> mutate(tau = 0.48),
             aes(x = tau, y = bias, color = round_grp),
             shape = 4, size = 5, stroke = 1.8) +
  scale_color_manual(values = round_colors, name = "Round") +
  scale_x_continuous(breaks = c(0.48, taus),
                     labels = c("Base", paste0("τ=", taus))) +
  facet_wrap(~approach) +
  labs(
    title    = "RB Quantile Regression — Bias by τ",
    subtitle = "Residual = actual − predicted PPG on pseudo-holdout (producers 2021+)\n× = baseline mean-regression bias",
    x = NULL, y = "Mean Bias (actual − pred PPG)"
  ) +
  theme_nfl()

p_mae <- summary_tbl |>
  ggplot(aes(x = tau, y = mae, color = round_grp, group = round_grp)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3, alpha = 0.9) +
  geom_point(data = baseline |> mutate(tau = 0.48),
             aes(x = tau, y = mae, color = round_grp),
             shape = 4, size = 5, stroke = 1.8) +
  scale_color_manual(values = round_colors, name = "Round") +
  scale_x_continuous(breaks = c(0.48, taus),
                     labels = c("Base", paste0("τ=", taus))) +
  facet_wrap(~approach) +
  labs(
    title    = "RB Quantile Regression — MAE by τ",
    subtitle = "× = baseline MAE",
    x = NULL, y = "Mean Absolute Error"
  ) +
  theme_nfl()

dir.create("output/diagnostics", showWarnings = FALSE)
ggsave("output/diagnostics/05_quantile_bias.png", p_bias, width = 12, height = 6, dpi = 150)
ggsave("output/diagnostics/06_quantile_mae.png",  p_mae,  width = 12, height = 6, dpi = 150)
cat("\nSaved: output/diagnostics/05_quantile_bias.png\n")
cat("Saved: output/diagnostics/06_quantile_mae.png\n")

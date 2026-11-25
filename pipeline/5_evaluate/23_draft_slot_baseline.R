# 23_draft_slot_baseline.R
# ─────────────────────────────────────────────────────────────────────────────
# Draft-slot-only baseline under the same walk-forward folds as 11_temporal_cv.R
# and 11b_temporal_cv_qb_te.R: for each test class K in 2016–2023, fit
# early-career PPG ~ log(pick) (loess) on drafted players from classes < K,
# then predict class K. Answers "what does the model add beyond draft capital?"
#
# Output: output/draft_slot_baseline/baseline_oos.csv
# Figures + comparison: python3 docs/make_readme_figures.py
# ─────────────────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({ library(dplyr); library(readr) })

out_dir <- "output/draft_slot_baseline"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

pos_files <- c(WR = "wr", RB = "rb", QB = "qb", TE = "te")
base <- list()
for (p in names(pos_files)) {
  d <- readRDS(sprintf("data/%s_model_data.rds", pos_files[p])) |>
    filter(!is.na(ppg), !is.na(pick)) |>
    distinct(pfr_player_name, draft_year, .keep_all = TRUE)
  for (K in 2016:2023) {
    tr <- d |> filter(draft_year < K, draft_year >= 2004)
    te <- d |> filter(draft_year == K)
    if (!nrow(te)) next
    fit <- loess(ppg ~ log(pick), data = tr, span = 0.75, degree = 1)
    pr  <- predict(fit, newdata = te)
    # loess can't extrapolate past the training pick range — fall back to OLS
    miss <- is.na(pr)
    if (any(miss)) pr[miss] <- predict(lm(ppg ~ log(pick), tr), te)[miss]
    base[[length(base) + 1]] <- tibble(
      position = p, name = te$pfr_player_name, draft_year = K,
      pick = te$pick, actual = te$ppg, base_pred = pmax(pr, 0))
  }
}
base <- bind_rows(base)
stopifnot(nrow(base) > 500, all(c("WR","RB","QB","TE") %in% base$position))
write_csv(base, file.path(out_dir, "baseline_oos.csv"))
message("Saved ", nrow(base), " rows → ", file.path(out_dir, "baseline_oos.csv"))

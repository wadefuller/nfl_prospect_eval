# Guards the hurdle-probability combiners. Exits non-zero on failure.
#   Rscript tests/test_combiner.R      (from college_nfl_model/)
# History: production shipped WR p_eff = clip(0.85*sqrt(p)) without the tuned
# 0.15*base_rate term (fixed 2026-10-12) — WRs were under-predicted ~0.5 PPG.
suppressPackageStartupMessages({ library(dplyr); library(stringr) })
source("functions/helpers.R")
n_fail <- 0; n_ok <- 0
ok <- function(cond, label) { if (isTRUE(cond)) n_ok <<- n_ok + 1 else { n_fail <<- n_fail + 1; cat("FAIL:", label, "\n") } }

p <- c(0, 0.1, 0.5, 0.9, 1); br <- 0.66
ok(isTRUE(all.equal(combine_wr(p, br), pmin(1, 0.85 * sqrt(p) + 0.15 * br))), "combine_wr matches tuned power_shrink form")
ok(combine_wr(0, br) > 0, "base-rate term present (p=0 -> 0.15*base_rate, not 0)")
ok(all(combine_wr(p, br) >= 0 & combine_wr(p, br) <= 1), "output clipped to [0,1]")
ok(all(diff(combine_wr(seq(0, 1, .05), br)) >= 0), "monotone in p")
ok(inherits(try(combine_wr(p, NULL), silent = TRUE), "try-error"), "missing base_rate fails loudly")

# matches what 13_bust_tune.R selected
bc <- read.csv("output/bust_tune/best_combiners.csv")
w <- bc[bc$position == "WR", ]
ok(w$transform == "power_shrink" && abs(w$alpha - 0.85) < 1e-9 && abs(w$gamma - 0.5) < 1e-9,
   "best_combiners.csv still selects power_shrink(0.85, 0.5) — update combine_wr if retuned")

# no stray copies of the old untuned WR formula in production paths
src <- c("functions/helpers.R", "11_temporal_cv.R")
stray <- unlist(lapply(src, function(f) grep("clip01\\(0\\.85 \\* sqrt\\((p_made_it|bust_prob)\\)\\)", readLines(f), value = TRUE)))
ok(length(stray) == 0, paste("old WR formula still present:", paste(stray, collapse = " | ")))

# CV output: WR pooled bias within ±0.3 PPG (was +0.51 with the bug)
m <- read.csv("output/temporal_cv/metrics_summary.csv")
ok(abs(m$bias[m$position == "WR"]) < 0.3, sprintf("WR CV bias %.3f within ±0.3", m$bias[m$position == "WR"]))

cat(sprintf("%d ok, %d failed\n", n_ok, n_fail)); quit(status = as.integer(n_fail > 0))

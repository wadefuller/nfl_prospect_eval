# 02e_build_mock_draft.R
# ─────────────────────────────────────────────────────────────────────────────
# Builds a unified pre-draft "projected pick" lookup for WR/RB prospects.
#
# Sources:
#   2014–2021: JackLich10/nfl-draft-data on GitHub
#              https://github.com/JackLich10/nfl-draft-data
#              ESPN ovr_rk = ESPN's pre-draft overall ranking (proxy for pick).
#   2022–2026: WalterFootball
#              https://walterfootball.com/draft<YYYY>(_<round>).php
#              Walt's own 7-round mock; URLs follow a stable pattern back to ~2009.
#
# Why two sources: jacklich10's CSV stops at 2021. NFLMockDraftDatabase /
# Pro Football Network are Cloudflare/JS-blocked. Walt's mock is a single
# analyst (some bias) but covers every year through draft week with a
# scrapable URL pattern, so it's the most consistent fill for the 2022+ gap.
#
# Output: data/mock_draft_consensus.rds — one row per (name_clean, draft_year)
#   columns: name_clean, name, position, school, draft_year,
#            projected_pick, source
# ─────────────────────────────────────────────────────────────────────────────

suppressMessages({
  library(tidyverse)
})

source("functions/helpers.R")

CACHE_DIR <- "data/mock_html_cache"
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

# ── 1. JackLich10 ESPN data (2014–2021) ──────────────────────────────────────

jack_url   <- "https://raw.githubusercontent.com/JackLich10/nfl-draft-data/master/nfl_draft_prospects.csv"
jack_cache <- file.path(CACHE_DIR, "jacklich10_prospects.csv")
if (!file.exists(jack_cache)) {
  message("Downloading JackLich10 ESPN data...")
  download.file(jack_url, jack_cache, quiet = TRUE)
}

jack_raw <- readr::read_csv(jack_cache, show_col_types = FALSE, guess_max = 13000)

POS_MAP <- c(
  "Wide Receiver" = "WR",
  "Running Back"  = "RB",
  "Tight End"     = "TE",
  "Quarterback"   = "QB"
)

jack_mocks <- jack_raw |>
  filter(draft_year >= 2014, draft_year <= 2021,
         position %in% names(POS_MAP),
         !is.na(ovr_rk)) |>
  transmute(
    name_clean     = clean_name(player_name),
    name           = player_name,
    position       = POS_MAP[position],
    school         = school,
    draft_year     = as.integer(draft_year),
    projected_pick = as.integer(ovr_rk),
    source         = "ESPN (via JackLich10)"
  )

message(sprintf("  JackLich10: %d mock rows across %d years",
                nrow(jack_mocks), n_distinct(jack_mocks$draft_year)))

# ── 2. WalterFootball scrape (2022–2026) ─────────────────────────────────────

# Round-page URL: /draft<YYYY>.php (round 1 picks 1-16),
#                 /draft<YYYY>_1.php (round 1 picks 17-32),
#                 /draft<YYYY>_2.php through _7.php (rounds 2–7).
# Saved HTML lives in data/mock_html_cache so reruns don't re-scrape.

fetch_walt_page <- function(year, suffix = "") {
  url   <- sprintf("https://walterfootball.com/draft%d%s.php", year, suffix)
  fname <- sprintf("walt_%d_%s.html", year, if (suffix == "") "0" else gsub("_", "", suffix))
  cache <- file.path(CACHE_DIR, fname)
  if (!file.exists(cache)) {
    message("  fetching ", url)
    res <- tryCatch(
      download.file(url, cache, quiet = TRUE,
                    headers = c("User-Agent" = "Mozilla/5.0")),
      error = function(e) {
        message("    [warn] ", conditionMessage(e))
        return(NA)
      }
    )
    Sys.sleep(0.4)
  }
  if (file.exists(cache) && file.info(cache)$size > 1000) {
    readr::read_file(cache)
  } else NA_character_
}

# Walt's HTML format changed in 2024:
#   2024+:  <strong>TEAM: <a ...>NAME, POS, SCHOOL</a></strong>  (data-number=N)
#   ≤2023:  <b> TEAM: NAME, POS, SCHOOL </b>                     (no pick number)
# parse_walt_picks tries the new pattern first; if nothing matches it falls back
# to the older <b>-tag pattern. In both cases we discard the page-local pick
# number — score_class assigns a global pick later via cumulative row_number().

parse_walt_picks <- function(html, year) {
  if (is.na(html) || nchar(html) < 1000) return(tibble())

  # Newer format (2024+). Earlier rounds wrap the player in <a>; later rounds
  # (round 5+) drop the link, so we extract anything between ": " and "</strong>"
  # and then strip optional surrounding <a>...</a> tags before splitting.
  pat_new <- '(?s)<strong>([^<:]+):\\s*(.+?)</strong>'
  m_new   <- str_match_all(html, pat_new)[[1]]
  if (nrow(m_new) > 0) {
    raw <- tibble(team = str_squish(m_new[, 2]),
                  player_text = str_squish(m_new[, 3])) |>
      # Strip optional <a ...>...</a> wrapper around the player text
      mutate(player_text = str_replace(player_text, "^<a[^>]*>", "") |>
                            str_replace("</a>$", "") |>
                            str_squish()) |>
      filter(str_count(player_text, ",") >= 2)
  } else {
    # Older format (≤2023). Looser regex catches dates / nav noise too —
    # filter post-extraction by requiring "Team: Name, Pos, School" shape.
    pat_old <- '<b>\\s*([^<:]+?):\\s*([^<]+?)\\s*</b>'
    m_old   <- str_match_all(html, pat_old)[[1]]
    if (nrow(m_old) == 0) return(tibble())
    raw <- tibble(team = str_squish(m_old[, 2]),
                  player_text = str_squish(m_old[, 3])) |>
      filter(str_count(player_text, ",") >= 2,                 # need 2 commas
             !str_detect(team, regex("link|recent|date|press", ignore_case = TRUE)),
             !str_detect(player_text, regex("\\d{1,2}:\\d{2}", ignore_case = TRUE)))  # drop "April 28, 2022 (4:30 p.m.)"
  }

  raw |>
    tidyr::separate(player_text,
                    into  = c("name", "position", "school"),
                    sep   = "\\s*,\\s*",
                    extra = "merge",
                    fill  = "right") |>
    mutate(
      draft_year = as.integer(year),
      name       = str_squish(name),
      position   = str_squish(position),
      school     = str_squish(school)
    ) |>
    # Position should be a short code (1-5 chars, possibly with /); drop noise rows.
    filter(!is.na(position), nchar(position) <= 8, str_detect(position, "^[A-Z/]+$"))
}

scrape_walt_year <- function(year) {
  suffixes <- c("", paste0("_", 1:7))
  pages    <- map_chr(suffixes, ~ fetch_walt_page(year, .x))
  picks    <- map2(pages, suffixes, ~ parse_walt_picks(.x, year)) |> bind_rows()
  if (nrow(picks) == 0) return(tibble())
  # data-number resets per page (1-16, 1-16, 1-32, ...) → recompute global pick
  picks |>
    arrange(seq_len(nrow(picks))) |>
    mutate(pick = row_number())
}

walt_years <- 2022:2026
message("WalterFootball: scraping ", paste(walt_years, collapse = ", "), "...")

walt_raw <- map(walt_years, function(yr) {
  message("  year ", yr)
  scrape_walt_year(yr)
}) |> bind_rows()

# Position filter: keep WR/RB plus common dual-listings + secondary positions
# that recent two-way players were tagged as (Travis Hunter as CB, etc.).
# Downstream join is by (name, draft_year) so over-inclusion is safe; we just
# get extra unused rows in the cache. Hardcoded misspelling fixes also live
# here since WalterFootball occasionally typos prominent prospects.
WALT_NAME_FIXES <- c(
  "tetaiora mcmillan" = "tetairoa mcmillan"   # Walt 2025 misspelling
)
fix_walt_name <- function(nc) ifelse(nc %in% names(WALT_NAME_FIXES),
                                      WALT_NAME_FIXES[nc], nc)

walt_mocks <- walt_raw |>
  filter(
    position == "WR" | position == "RB" |
    grepl("^WR/|/WR$|^WR/|^RB/|/RB$", position) |
    position %in% c("CB", "ATH", "S", "DB")  # two-way / convert candidates
  ) |>
  transmute(
    name_clean     = fix_walt_name(clean_name(name)),
    name,
    position,
    school,
    draft_year,
    projected_pick = as.integer(pick),
    source         = "WalterFootball"
  )

message(sprintf("  Walt: %d WR/RB mock rows across %d years",
                nrow(walt_mocks), n_distinct(walt_mocks$draft_year)))

# ── 3. Combine + save ────────────────────────────────────────────────────────

mocks <- bind_rows(jack_mocks, walt_mocks) |>
  # When a player somehow appears in both (shouldn't, since year ranges are
  # disjoint) keep the better-ranked source row
  group_by(name_clean, draft_year) |>
  slice_min(projected_pick, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(draft_year, projected_pick)

saveRDS(mocks, "data/mock_draft_consensus.rds")
readr::write_csv(mocks, "data/mock_draft_consensus.csv")

cat("\n══ Mock draft consensus summary ══\n")
cat(sprintf("Total prospects: %d\n", nrow(mocks)))
print(mocks |> count(draft_year, position, source) |> tidyr::pivot_wider(
  names_from = position, values_from = n, values_fill = 0
))
cat("\nSaved: data/mock_draft_consensus.rds (and .csv)\n")

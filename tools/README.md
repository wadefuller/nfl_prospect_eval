# tools/

Helpers you run by hand from the repo root. They aren't pipeline steps.

| Script | Use |
|---|---|
| `inspect_player.R` | Print every input and output the model has for one player: `Rscript tools/inspect_player.R "Puka Nacua"` (also `--pos WR`, `--year 2023`, `--list 2026`) |
| `inspect_app.R` | The same view as a local Shiny app: `Rscript tools/inspect_app.R` |
| `build_cfb_season_from_pbp.R` | Rebuild one season's college stat tables from public play-by-play when the CollegeFootballData API quota runs out: `Rscript tools/build_cfb_season_from_pbp.R 2025` |
| `build_unified_raw_caches.R` | Stitch legacy API caches (pre-2014) and play-by-play caches (2014+) into the raw tables `02_build_features.R` expects |

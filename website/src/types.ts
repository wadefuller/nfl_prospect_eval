export interface Prospect {
  id: string;
  name: string;
  position: "WR" | "RB";
  college: string;
  tier: string | null;
  round: number;
  pick: number;
  draft_year: number; // tagged on load from YearData.draftYear
  p_made_it: number;
  exp_ppg: number;
  // Derived client-side on load: exp_ppg / p_made_it (PPG conditional on hitting)
  ppg_if_hit: number;
  // Ordinal-bucket distribution (XGBoost multiclass + Bayesian stan_polr
  // ensemble). Coexists with the continuous exp_ppg — these give a fuller
  // story about outcome risk/upside instead of a single point estimate.
  // bucket_top1 ∈ "bust" | "bench" | "flex" | "elite" | "league_winner"
  // Posterior credible intervals (80% by default) come from per-draw
  // ensembling: each Bayesian draw is geom-meaned with the XGB point
  // estimate, then 10/90 quantiles taken across draws.
  p_bust: number | null;
  p_bench: number | null;
  p_flex: number | null;
  p_elite: number | null;
  p_league_winner: number | null;
  p_bust_lo: number | null;
  p_bench_lo: number | null;
  p_flex_lo: number | null;
  p_elite_lo: number | null;
  p_league_winner_lo: number | null;
  p_bust_hi: number | null;
  p_bench_hi: number | null;
  p_flex_hi: number | null;
  p_elite_hi: number | null;
  p_league_winner_hi: number | null;
  exp_ppg_bucket: number | null;        // Σ midpoint(bucket) × P(bucket)
  exp_ppg_bucket_lo: number | null;     // 80% credible interval
  exp_ppg_bucket_hi: number | null;
  bucket_top1: string | null;
  prospect_score: number | null;
  archetype: string | null;
  blurb: string | null;
  bullish: string[] | null;
  bearish: string[] | null;
  headshot_url: string | null;
  height_in: number | null;
  weight: number | null;
  forty: number | null;
  rec_yards_final: number | null;
  rec_final: number | null;
  rec_td_final: number | null;
  rush_yards_final: number | null;
  carries_final: number | null;
  rush_td_final: number | null;
  rb_rec_yards: number | null;
  rb_rec: number | null;
  rb_rec_td: number | null;
  // ── Extended college production (WR) ─────────────────────────────────────
  rec_yards_per_game: number | null;
  ypr: number | null;
  rec_td_rate: number | null;
  dominator_rate: number | null;
  // WR play-by-play efficiency (cfbfastR)
  catch_rate_wr: number | null;
  target_share_wr: number | null;
  yards_per_target_wr: number | null;
  epa_per_target_wr: number | null;
  explosive_rec_rate: number | null;
  // ── Extended college production (RB) ─────────────────────────────────────
  rush_yards_per_game: number | null;
  ypc: number | null;
  rush_td_rate: number | null;
  // RB play-by-play efficiency (cfbfastR)
  epa_per_rush: number | null;
  explosive_rate: number | null;
  breakaway_rate: number | null;
  target_share: number | null;
  catch_rate: number | null;
  // ── Percentile ranks (0–100) within position across training distribution
  rec_yards_final_pct: number | null;
  rec_final_pct: number | null;
  rec_td_final_pct: number | null;
  rec_yards_per_game_pct: number | null;
  ypr_pct: number | null;
  rec_td_rate_pct: number | null;
  dominator_rate_pct: number | null;
  catch_rate_wr_pct: number | null;
  target_share_wr_pct: number | null;
  yards_per_target_wr_pct: number | null;
  epa_per_target_wr_pct: number | null;
  explosive_rec_rate_pct: number | null;
  rush_yards_final_pct: number | null;
  carries_final_pct: number | null;
  rush_td_final_pct: number | null;
  rush_yards_per_game_pct: number | null;
  ypc_pct: number | null;
  rush_td_rate_pct: number | null;
  rb_rec_yards_pct: number | null;
  rb_rec_pct: number | null;
  rb_rec_td_pct: number | null;
  epa_per_rush_pct: number | null;
  explosive_rate_pct: number | null;
  breakaway_rate_pct: number | null;
  target_share_pct: number | null;
  catch_rate_pct: number | null;
  // PBP availability flags
  has_wr_pbp: number | null;
  has_pbp: number | null;
  has_cfb_data: boolean | null;
  actual_ppg: number | null;
  actual_raw_ppg: number | null;
  actual_made_it: number | null;
  n_qual_seasons: number | null;
  comp_weighted_ppg: number | null;
  comp_median_ppg: number | null;
  comp_bust_rate: number | null;
  comp_names: string[] | null;
}

export interface ProspectComp {
  rank: number;
  name: string;
  college: string;
  year: number;
  round: number;
  pick: number;
  ppg: number;
  rawPpg: number | null;
  madeIt: boolean;
  similarity: number;
}

export interface YearData {
  draftYear: number;
  prospects: Prospect[];
}

export interface Meta {
  availableYears: number[];
  positions: string[];
  lastUpdated: string;
  totalProspects: number;
}

export type SortField = "pick" | "exp_ppg" | "ppg_if_hit" | "p_made_it" | "comp_weighted_ppg" | "actual_ppg" | "name" | "prospect_score" | "draft_year";
export type SortDir = "asc" | "desc";

// Model performance types
export interface ModelOverall {
  mae: number; cor: number; n: number;
  bias: number;
  wr_mae: number; rb_mae: number;
  wr_cor: number; rb_cor: number;
  wr_bias: number; rb_bias: number;
  bust_accuracy: number; wr_bust_accuracy: number; rb_bust_accuracy: number;
}
export interface ModelByYear {
  year: number; n: number; mae: number; cor: number; wr_mae: number; rb_mae: number;
}
export interface ModelByRound {
  round_grp: string; n: number; mae: number; cor: number;
  hit_rate: number; avg_exp: number; avg_actual: number;
}
export interface ScatterPoint {
  name: string; pos: string; year: number; round: number; pick: number;
  pred: number; actual: number; p_hit: number; hit: boolean;
}
export interface NotablePlayer {
  name: string; pos: string; year: number; round: number; pick: number;
  pred: number; actual: number; diff: number;
}
export interface CalibrationBucket {
  bucket: string; n: number; pred_prob: number; obs_rate: number;
}
export interface ModelPerformanceData {
  overall: ModelOverall;
  byYear: ModelByYear[];
  byRound: ModelByRound[];
  byRoundWR: ModelByRound[];
  byRoundRB: ModelByRound[];
  calibration: CalibrationBucket[];
  scatter: ScatterPoint[];
  notable: NotablePlayer[];
}

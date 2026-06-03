import type { Prospect, ProspectComp } from "../types";
import { PlayerAvatar } from "./PlayerAvatar";
import { PosBadge } from "./ProspectRow";
import { scoreColor } from "./prospectStyle";
import { BucketDistribution } from "./BucketDistribution";

interface Props {
  prospect: Prospect;
  comps: ProspectComp[] | null;
}

const CARD: React.CSSProperties = {
  background: "rgba(255,255,255,0.03)",
  border: "1px solid rgba(255,255,255,0.06)",
  borderRadius: 8,
  padding: "12px 14px",
};

const LABEL: React.CSSProperties = {
  fontSize: 10,
  textTransform: "uppercase" as const,
  letterSpacing: "0.09em",
  color: "#4A5578",
  fontWeight: 500,
  fontFamily: "var(--font-body)",
  marginBottom: 4,
};

function MiniStat({ label, value }: { label: string; value: string }) {
  return (
    <div style={{ background: "rgba(255,255,255,0.04)", borderRadius: 6, padding: "8px 12px", flex: 1 }}>
      <div style={LABEL}>{label}</div>
      <div style={{ fontFamily: "var(--font-mono)", fontSize: 18, fontWeight: 700, color: "#F0F4FF", lineHeight: 1 }}>
        {value}
      </div>
    </div>
  );
}

function CircleGauge({ score }: { score: number }) {
  const r = 42, cx = 52, cy = 52;
  const circ = 2 * Math.PI * r;
  const dash = circ * (score / 100);
  const col = scoreColor(score);
  return (
    <svg width="104" height="104" viewBox="0 0 104 104">
      <circle cx={cx} cy={cy} r={r} fill="none" stroke="rgba(255,255,255,0.06)" strokeWidth="7" />
      <circle
        cx={cx} cy={cy} r={r} fill="none"
        stroke={col} strokeWidth="7"
        strokeDasharray={`${dash} ${circ - dash}`}
        strokeLinecap="round"
        transform={`rotate(-90 ${cx} ${cy})`}
        style={{ filter: `drop-shadow(0 0 8px ${col}88)` }}
      />
      <text
        x={cx} y={cy + 2}
        textAnchor="middle" dominantBaseline="middle"
        style={{ fontFamily: "var(--font-mono)", fontSize: "24px", fontWeight: 700, fill: col }}
      >
        {score}
      </text>
    </svg>
  );
}

function SimBar({ sim }: { sim: number }) {
  const w = Math.min(100, Math.round((sim / 30) * 100));
  return (
    <div style={{ display: "flex", alignItems: "center", gap: 6 }}>
      <div style={{ width: 72, height: 3, background: "rgba(255,255,255,0.07)", borderRadius: 2, overflow: "hidden" }}>
        <div style={{ height: "100%", width: `${w}%`, background: "#3E8EF7", borderRadius: 2 }} />
      </div>
      <span style={{ fontFamily: "var(--font-mono)", fontSize: 11, color: "#4A5578" }}>{sim}%</span>
    </div>
  );
}

function MeasurablesCard({ p }: { p: Prospect }) {
  const items: { label: string; value: string }[] = [];
  if (p.height_in != null) {
    const ft = Math.floor(p.height_in / 12);
    items.push({ label: "Height", value: `${ft}'${p.height_in % 12}"` });
  }
  if (p.weight != null) items.push({ label: "Weight", value: `${p.weight} lbs` });
  if (p.forty != null) items.push({ label: "40-Yard", value: `${p.forty.toFixed(2)}s` });
  if (items.length === 0) return null;
  return (
    <div style={CARD}>
      <div style={{ ...LABEL, marginBottom: 10 }}>Measurables</div>
      <div style={{ display: "flex", gap: 6 }}>
        {items.map((i) => <MiniStat key={i.label} label={i.label} value={i.value} />)}
      </div>
    </div>
  );
}

function DriverSnapshot({ p }: { p: Prospect }) {
  const metrics = p.position === "WR"
    ? [
        { label: "Draft capital", pct: p.pick <= 32 ? 92 : p.pick <= 64 ? 78 : p.pick <= 100 ? 62 : 38 },
        { label: "Receiving volume", pct: p.rec_yards_final_pct },
        { label: "Dominator rate", pct: p.dominator_rate_pct },
        { label: "Target share", pct: p.target_share_wr_pct },
        { label: "Yards per target", pct: p.yards_per_target_wr_pct },
        { label: "Explosive receptions", pct: p.explosive_rec_rate_pct },
        { label: "Speed score", pct: p.forty == null ? null : p.forty <= 4.4 ? 85 : p.forty <= 4.5 ? 65 : 35 },
      ]
    : [
        { label: "Draft capital", pct: p.pick <= 32 ? 92 : p.pick <= 64 ? 78 : p.pick <= 100 ? 62 : 38 },
        { label: "Rushing volume", pct: p.rush_yards_final_pct },
        { label: "Yards per carry", pct: p.ypc_pct },
        { label: "Receiving usage", pct: p.rb_rec_yards_pct },
        { label: "EPA per rush", pct: p.epa_per_rush_pct },
        { label: "Explosive runs", pct: p.explosive_rate_pct },
        { label: "Breakaway rate", pct: p.breakaway_rate_pct },
      ];

  const ranked = metrics
    .filter((m): m is { label: string; pct: number } => m.pct != null && Number.isFinite(m.pct))
    .sort((a, b) => b.pct - a.pct);
  const strengths = ranked.filter(m => m.pct >= 65).slice(0, 3);
  const concerns = [...ranked].reverse().filter(m => m.pct <= 40).slice(0, 2);

  if (strengths.length === 0 && concerns.length === 0) return null;

  return (
    <div style={CARD}>
      <div style={{ ...LABEL, marginBottom: 10 }}>Why the model is here</div>
      {strengths.length > 0 && (
        <div style={{ marginBottom: concerns.length > 0 ? 10 : 0 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "#2DD4A0", marginBottom: 6 }}>Lifts</div>
          {strengths.map(m => (
            <div key={`up-${m.label}`} style={{ display: "flex", justifyContent: "space-between", gap: 10, marginBottom: 4 }}>
              <span style={{ fontSize: 12, color: "#8A9AC0" }}>{m.label}</span>
              <span style={{ fontFamily: "var(--font-mono)", fontSize: 12, color: pctColor(m.pct), fontWeight: 700 }}>
                {m.pct}p
              </span>
            </div>
          ))}
        </div>
      )}
      {concerns.length > 0 && (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "#F5A623", marginBottom: 6 }}>Drags</div>
          {concerns.map(m => (
            <div key={`down-${m.label}`} style={{ display: "flex", justifyContent: "space-between", gap: 10, marginBottom: 4 }}>
              <span style={{ fontSize: 12, color: "#8A9AC0" }}>{m.label}</span>
              <span style={{ fontFamily: "var(--font-mono)", fontSize: 12, color: pctColor(m.pct), fontWeight: 700 }}>
                {m.pct}p
              </span>
            </div>
          ))}
        </div>
      )}
    </div>
  );
}

// ── Percentile-aware stat row ───────────────────────────────────────────────
// Color the bar by percentile bucket. Uses the same palette as ProspectRow's
// score tiers so the whole UI stays visually coherent.
function pctColor(pct: number): string {
  if (pct >= 85) return "#2DD4A0"; // elite — teal
  if (pct >= 65) return "#3E8EF7"; // plus    — blue
  if (pct >= 40) return "#8A9AC0"; // average — slate
  if (pct >= 20) return "#F5A623"; // below   — amber
  return "#F75757";                // poor    — red
}

function StatRow({
  label, value, pct,
}: {
  label: string;
  value: string;
  pct: number | null | undefined;
}) {
  const hasPct = pct != null && Number.isFinite(pct);
  const w = hasPct ? Math.max(2, Math.min(100, pct!)) : 0;
  const col = hasPct ? pctColor(pct!) : "#4A5578";
  return (
    <div
      style={{
        display: "grid",
        gridTemplateColumns: "1fr auto",
        columnGap: 10,
        padding: "6px 0",
        borderBottom: "1px solid rgba(255,255,255,0.04)",
      }}
    >
      <div style={{ minWidth: 0 }}>
        <div
          style={{
            fontSize: 11,
            color: "#8A9AC0",
            fontFamily: "var(--font-body)",
            marginBottom: 3,
            whiteSpace: "nowrap",
            overflow: "hidden",
            textOverflow: "ellipsis",
          }}
        >
          {label}
        </div>
        <div
          style={{
            height: 3,
            borderRadius: 2,
            background: "rgba(255,255,255,0.06)",
            overflow: "hidden",
          }}
        >
          {hasPct && (
            <div
              style={{
                height: "100%",
                width: `${w}%`,
                background: col,
                borderRadius: 2,
              }}
            />
          )}
        </div>
      </div>
      <div style={{ display: "flex", flexDirection: "column", alignItems: "flex-end", gap: 2 }}>
        <span
          style={{
            fontFamily: "var(--font-mono)",
            fontSize: 13,
            fontWeight: 700,
            color: "#F0F4FF",
            lineHeight: 1,
          }}
        >
          {value}
        </span>
        <span
          style={{
            fontFamily: "var(--font-mono)",
            fontSize: 10,
            color: hasPct ? col : "#4A5578",
            lineHeight: 1,
          }}
        >
          {hasPct ? `${pct}p` : "—"}
        </span>
      </div>
    </div>
  );
}

// Formatters
const fmt = {
  int: (v: number) => Math.round(v).toLocaleString(),
  yds: (v: number) => Math.round(v).toLocaleString(),
  per: (v: number) => `${(v * 100).toFixed(1)}%`,
  rate: (v: number) => v.toFixed(2),
  epa: (v: number) => v.toFixed(2),
  dec1: (v: number) => v.toFixed(1),
};

function SubSection({
  title, rows,
}: {
  title: string;
  rows: { label: string; value: number | null | undefined; format: (v: number) => string; pct: number | null | undefined }[];
}) {
  const visible = rows.filter((r) => r.value != null && Number.isFinite(r.value));
  if (visible.length === 0) return null;
  return (
    <div>
      <div
        style={{
          fontSize: 10,
          textTransform: "uppercase" as const,
          letterSpacing: "0.09em",
          color: "#6B7B9F",
          fontWeight: 500,
          fontFamily: "var(--font-body)",
          marginBottom: 4,
          marginTop: 8,
        }}
      >
        {title}
      </div>
      {visible.map((r) => (
        <StatRow key={r.label} label={r.label} value={r.format(r.value!)} pct={r.pct} />
      ))}
    </div>
  );
}

function StatsCard({ p }: { p: Prospect }) {
  if (p.position === "WR") {
    const hasAny =
      p.rec_yards_final != null || p.rec_final != null || p.catch_rate_wr != null ||
      p.target_share_wr != null || p.ypr != null;
    if (!hasAny) return null;

    const volumeRows = [
      { label: "Rec Yards",           value: p.rec_yards_final,    format: fmt.yds,  pct: p.rec_yards_final_pct },
      { label: "Receptions",          value: p.rec_final,          format: fmt.int,  pct: p.rec_final_pct },
      { label: "Rec TDs",             value: p.rec_td_final,       format: fmt.int,  pct: p.rec_td_final_pct },
      { label: "Rec Yards / Game",    value: p.rec_yards_per_game, format: fmt.dec1, pct: p.rec_yards_per_game_pct },
      { label: "Yards / Reception",   value: p.ypr,                format: fmt.dec1, pct: p.ypr_pct },
      { label: "Rec TD Rate",         value: p.rec_td_rate,        format: fmt.per,  pct: p.rec_td_rate_pct },
      { label: "Dominator Rate",      value: p.dominator_rate,     format: fmt.per,  pct: p.dominator_rate_pct },
    ];
    const pbpRows = [
      { label: "Catch Rate",          value: p.catch_rate_wr,       format: fmt.per, pct: p.catch_rate_wr_pct },
      { label: "Target Share",        value: p.target_share_wr,     format: fmt.per, pct: p.target_share_wr_pct },
      { label: "Yards / Target",      value: p.yards_per_target_wr, format: fmt.dec1, pct: p.yards_per_target_wr_pct },
      { label: "EPA / Target",        value: p.epa_per_target_wr,   format: fmt.epa, pct: p.epa_per_target_wr_pct },
      { label: "Explosive Rec Rate",  value: p.explosive_rec_rate,  format: fmt.per, pct: p.explosive_rec_rate_pct },
    ];

    return (
      <div style={CARD}>
        <div style={{ ...LABEL, marginBottom: 6 }}>
          College Production · Regular Season
          <span style={{ marginLeft: 6, fontSize: 9, color: "#4A5578", fontWeight: 400, textTransform: "none" }}>
            bar = percentile vs drafted WRs
          </span>
        </div>
        <SubSection title="Volume & Efficiency" rows={volumeRows} />
        <SubSection title="Play-by-Play" rows={pbpRows} />
      </div>
    );
  }

  // RB
  const hasAny =
    p.rush_yards_final != null || p.carries_final != null ||
    p.epa_per_rush != null || p.explosive_rate != null;
  if (!hasAny) return null;

  const rushRows = [
    { label: "Rush Yards",          value: p.rush_yards_final,    format: fmt.yds,  pct: p.rush_yards_final_pct },
    { label: "Carries",             value: p.carries_final,       format: fmt.int,  pct: p.carries_final_pct },
    { label: "Rush TDs",            value: p.rush_td_final,       format: fmt.int,  pct: p.rush_td_final_pct },
    { label: "Rush Yards / Game",   value: p.rush_yards_per_game, format: fmt.dec1, pct: p.rush_yards_per_game_pct },
    { label: "Yards / Carry",       value: p.ypc,                 format: fmt.dec1, pct: p.ypc_pct },
    { label: "Rush TD Rate",        value: p.rush_td_rate,        format: fmt.per,  pct: p.rush_td_rate_pct },
  ];
  const recvRows = [
    { label: "Rec Yards",           value: p.rb_rec_yards, format: fmt.yds, pct: p.rb_rec_yards_pct },
    { label: "Receptions",          value: p.rb_rec,       format: fmt.int, pct: p.rb_rec_pct },
    { label: "Rec TDs",             value: p.rb_rec_td,    format: fmt.int, pct: p.rb_rec_td_pct },
  ];
  const pbpRows = [
    { label: "EPA / Rush",          value: p.epa_per_rush,    format: fmt.epa, pct: p.epa_per_rush_pct },
    { label: "Explosive Run Rate",  value: p.explosive_rate,  format: fmt.per, pct: p.explosive_rate_pct },
    { label: "Breakaway Rate",      value: p.breakaway_rate,  format: fmt.per, pct: p.breakaway_rate_pct },
    { label: "Target Share",        value: p.target_share,    format: fmt.per, pct: p.target_share_pct },
    { label: "Catch Rate",          value: p.catch_rate,      format: fmt.per, pct: p.catch_rate_pct },
  ];

  return (
    <div style={CARD}>
      <div style={{ ...LABEL, marginBottom: 6 }}>
        College Production · Regular Season
        <span style={{ marginLeft: 6, fontSize: 9, color: "#4A5578", fontWeight: 400, textTransform: "none" }}>
          bar = percentile vs drafted RBs
        </span>
      </div>
      <SubSection title="Rushing" rows={rushRows} />
      <SubSection title="Receiving" rows={recvRows} />
      <SubSection title="Play-by-Play" rows={pbpRows} />
    </div>
  );
}

function CompRow({ comp, index }: { comp: ProspectComp; index: number }) {
  const outcome = !comp.madeIt
    ? "BUST"
    : comp.rawPpg != null
    ? `${comp.rawPpg.toFixed(1)} ppg`
    : `${comp.ppg.toFixed(1)} ppg`;
  const outcomeColor = !comp.madeIt ? "#F75757" : comp.ppg >= 10 ? "#2DD4A0" : comp.ppg >= 6 ? "#F5A623" : "#F75757";

  return (
    <div
      style={{
        display: "flex",
        alignItems: "center",
        gap: 10,
        padding: "8px 0",
        borderBottom: index < 9 ? "1px solid rgba(255,255,255,0.05)" : "none",
      }}
    >
      <span style={{ fontFamily: "var(--font-mono)", fontSize: 11, color: "#4A5578", width: 16, textAlign: "right", flexShrink: 0 }}>
        {index + 1}
      </span>
      <div style={{ flex: 1, minWidth: 0 }}>
        <div style={{ fontFamily: "var(--font-body)", fontSize: 13, fontWeight: 500, color: "#F0F4FF", overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
          {comp.name}
        </div>
        <div style={{ fontSize: 11, color: "#4A5578", overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
          {comp.college} {comp.year} &middot; Rd {comp.round} #{comp.pick}
        </div>
      </div>
      <div style={{ display: "flex", flexDirection: "column", alignItems: "flex-end", gap: 3, flexShrink: 0 }}>
        <span style={{ fontFamily: "var(--font-mono)", fontSize: 13, fontWeight: 600, color: outcomeColor }}>
          {outcome}
        </span>
        <SimBar sim={Math.round(comp.similarity * 100)} />
      </div>
    </div>
  );
}

export function ProspectDetail({ prospect: p, comps }: Props) {
  const hasBullish = p.bullish && p.bullish.length > 0;
  const hasBearish = p.bearish && p.bearish.length > 0;
  const condPpg = p.p_made_it > 0 ? p.exp_ppg / p.p_made_it : 0;
  const actualDisplay = p.actual_raw_ppg ?? p.actual_ppg;

  return (
    <div style={{ background: "transparent" }}>
      {/* Player header */}
      <div
        className="prospect-detail-header"
        style={{
          display: "flex",
          alignItems: "center",
          gap: 12,
          padding: "12px 14px",
          background: "rgba(62,142,247,0.04)",
          borderBottom: "1px solid rgba(255,255,255,0.07)",
        }}
      >
        <PlayerAvatar url={p.headshot_url} name={p.name} size="lg" />
        <div style={{ flex: 1, minWidth: 0 }}>
          <div style={{ display: "flex", alignItems: "baseline", gap: 8, flexWrap: "wrap" }}>
            <span style={{ fontFamily: "var(--font-display)", fontSize: 17, fontWeight: 700, color: "#F0F4FF" }}>
              {p.name}
            </span>
            {p.archetype && (
              <span style={{ fontSize: 11, color: "#8A9AC0" }}>{p.archetype}</span>
            )}
          </div>
          <div style={{ fontSize: 12, color: "#4A5578", marginTop: 2 }}>
            {p.college}
            {p.tier && (
              <span
                style={{
                  marginLeft: 6,
                  color: p.tier === "P4" ? "#3E8EF7" : p.tier === "G5" ? "#A78BFA" : "#4A5578",
                }}
              >
                {p.tier}
              </span>
            )}
          </div>
        </div>
        <div style={{ display: "flex", alignItems: "center", gap: 8, flexShrink: 0 }}>
          <span style={{ fontFamily: "var(--font-mono)", fontSize: 11, color: "#4A5578" }}>
            Rd {p.round} #{p.pick}
          </span>
          <PosBadge pos={p.position} />
        </div>
      </div>

      {/* 3-column body (collapses to single column on mobile via auto-fit) */}
      <div
        className="prospect-detail-body"
        style={{
          display: "grid",
          gridTemplateColumns: "repeat(auto-fit, minmax(220px, 1fr))",
          gap: 12,
          padding: 12,
        }}
      >
        {/* Col 1: Predictions + Bullish/Bearish */}
        <div style={{ display: "flex", flexDirection: "column", gap: 12 }}>
          <div style={CARD}>
            <div style={{ ...LABEL, marginBottom: 10 }}>Model Predictions</div>
            <div
              className="prospect-predictions-row"
              style={{ display: "flex", alignItems: "center", gap: 16, flexWrap: "wrap" }}
            >
              {p.prospect_score != null && (
                <div style={{ display: "flex", flexDirection: "column", alignItems: "center", gap: 4 }}>
                  <div style={{ ...LABEL, marginBottom: 4 }}>Score</div>
                  <CircleGauge score={p.prospect_score} />
                </div>
              )}
              <div style={{ display: "flex", flexDirection: "column", gap: 10, flex: "1 1 140px", minWidth: 0 }}>
                <div>
                  <div style={LABEL}>PPG If Hit</div>
                  <div style={{ fontFamily: "var(--font-mono)", fontSize: 22, fontWeight: 700, color: "#F0F4FF", lineHeight: 1 }}>
                    {condPpg.toFixed(1)}
                  </div>
                </div>
                <div>
                  <div style={LABEL}>Hit Prob</div>
                  <div style={{ fontFamily: "var(--font-mono)", fontSize: 14, fontWeight: 600, color: "#8A9AC0" }}>
                    {(p.p_made_it * 100).toFixed(0)}%
                  </div>
                </div>
                {actualDisplay != null && (
                  <div>
                    <div style={LABEL}>Actual PPG</div>
                    <div
                      style={{
                        fontFamily: "var(--font-mono)",
                        fontSize: 18,
                        fontWeight: 700,
                        color: actualDisplay >= p.exp_ppg ? "#2DD4A0" : "#F75757",
                        lineHeight: 1,
                      }}
                    >
                      {actualDisplay.toFixed(1)}
                      <span style={{ fontSize: 11, fontWeight: 400, color: "#4A5578", marginLeft: 4 }}>
                        {actualDisplay - p.exp_ppg >= 0 ? "+" : ""}
                        {(actualDisplay - p.exp_ppg).toFixed(1)} vs exp
                      </span>
                    </div>
                  </div>
                )}
              </div>
            </div>

            {/* Outcome bucket distribution — XGB+clm ensemble. NA-safe. */}
            {p.p_bust != null && (
              <div style={{ marginTop: 14, paddingTop: 14, borderTop: "1px solid rgba(255,255,255,0.06)" }}>
                <BucketDistribution prospect={p} />
              </div>
            )}
          </div>

          {(hasBullish || hasBearish) && (
            <div style={CARD}>
              {hasBullish && (
                <div style={{ marginBottom: hasBearish ? 12 : 0 }}>
                  <div style={{ fontSize: 12, fontWeight: 600, color: "#2DD4A0", marginBottom: 6 }}>Bullish</div>
                  {p.bullish!.map((b, i) => (
                    <div key={i} style={{ display: "flex", gap: 6, marginBottom: 4 }}>
                      <span style={{ color: "#2DD4A0", fontSize: 12 }}>+</span>
                      <span style={{ fontSize: 12, color: "#8A9AC0", lineHeight: 1.5 }}>{b}</span>
                    </div>
                  ))}
                </div>
              )}
              {hasBearish && (
                <div>
                  <div style={{ fontSize: 12, fontWeight: 600, color: "#F5A623", marginBottom: 6 }}>Bearish</div>
                  {p.bearish!.map((b, i) => (
                    <div key={i} style={{ display: "flex", gap: 6, marginBottom: 4 }}>
                      <span style={{ color: "#F5A623", fontSize: 12 }}>−</span>
                      <span style={{ fontSize: 12, color: "#8A9AC0", lineHeight: 1.5 }}>{b}</span>
                    </div>
                  ))}
                </div>
              )}
            </div>
          )}
          <DriverSnapshot p={p} />
        </div>

        {/* Col 2: Measurables + Stats + Comp Summary */}
        <div style={{ display: "flex", flexDirection: "column", gap: 12 }}>
          <MeasurablesCard p={p} />
          <StatsCard p={p} />
          {p.comp_weighted_ppg != null && (
            <div style={CARD}>
              <div style={{ ...LABEL, marginBottom: 10 }}>Comp Summary</div>
              <div style={{ display: "flex", gap: 8 }}>
                <div style={{ flex: 1, background: "rgba(255,255,255,0.04)", borderRadius: 6, padding: "8px 10px" }}>
                  <div style={LABEL}>Comp PPG</div>
                  <div style={{ fontFamily: "var(--font-mono)", fontSize: 16, fontWeight: 700, color: "#F0F4FF" }}>
                    {p.comp_weighted_ppg.toFixed(1)}
                  </div>
                </div>
                <div style={{ flex: 1, background: "rgba(255,255,255,0.04)", borderRadius: 6, padding: "8px 10px" }}>
                  <div style={LABEL}>Median</div>
                  <div style={{ fontFamily: "var(--font-mono)", fontSize: 16, fontWeight: 700, color: "#F0F4FF" }}>
                    {p.comp_median_ppg?.toFixed(1) ?? "—"}
                  </div>
                </div>
                <div style={{ flex: 1, background: "rgba(255,255,255,0.04)", borderRadius: 6, padding: "8px 10px" }}>
                  <div style={LABEL}>Bust Rate</div>
                  <div
                    style={{
                      fontFamily: "var(--font-mono)",
                      fontSize: 16,
                      fontWeight: 700,
                      color: p.comp_bust_rate === 0 ? "#2DD4A0" : "#F0F4FF",
                    }}
                  >
                    {p.comp_bust_rate != null ? `${(p.comp_bust_rate * 100).toFixed(0)}%` : "—"}
                  </div>
                </div>
              </div>
            </div>
          )}
        </div>

        {/* Col 3: Player Comparisons */}
        <div style={CARD}>
          <div style={{ ...LABEL, marginBottom: 4 }}>Player Comparisons</div>
          <div style={{ fontSize: 10, color: "#4A5578", marginBottom: 12 }}>Based on college profile similarity</div>
          {comps ? (
            comps.map((c, i) => <CompRow key={c.name + c.year} comp={c} index={i} />)
          ) : (
            <div style={{ color: "#4A5578", fontSize: 13, padding: "16px 0", textAlign: "center" }}>
              Loading comparisons…
            </div>
          )}
        </div>
      </div>
    </div>
  );
}

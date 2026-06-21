import { useEffect, useState, useMemo } from "react";
import type { ModelPerformanceData, ScatterPoint } from "../types";
import { dataUrl } from "../dataUrl";

// ── Design tokens ─────────────────────────────────────────────────────────────
const C = {
  teal: "#2DD4A0",
  blue: "#3E8EF7",
  gold: "#F5A623",
  coral: "#F75757",
  muted: "#4A5578",
  text: "#F0F4FF",
  sub: "#8A9AC0",
  card: "rgba(255,255,255,0.03)",
  border: "rgba(255,255,255,0.07)",
};

const CARD: React.CSSProperties = {
  background: C.card,
  border: `1px solid ${C.border}`,
  borderRadius: 10,
  padding: "20px 24px",
};

const MONO: React.CSSProperties = { fontFamily: "var(--font-mono)" };
const BODY: React.CSSProperties = { fontFamily: "var(--font-body)" };

// One-line plain-English description of a bias number, sized for the
// stat-tile sub-label slot.
function bias_blurb(bias: number): string {
  if (Math.abs(bias) < 0.15) return "predictions are roughly centered";
  return bias > 0 ? "model runs too cold (predicts low on average)"
                  : "model runs too hot (predicts high on average)";
}

// ── Stat tile ─────────────────────────────────────────────────────────────────
function StatTile({
  label, value, sub, color = C.teal, fmt,
}: {
  label: string; value: number; sub?: string; color?: string; fmt?: (v: number) => string;
}) {
  const display = fmt ? fmt(value) : value.toString();
  return (
    <div style={{ ...CARD, display: "flex", flexDirection: "column", gap: 6 }}>
      <div style={{ fontSize: 11, fontWeight: 500, textTransform: "uppercase", letterSpacing: "0.08em", color: C.muted, ...BODY }}>
        {label}
      </div>
      <div style={{ fontSize: 32, fontWeight: 700, color, lineHeight: 1, ...MONO }}>
        {display}
      </div>
      {sub && <div style={{ fontSize: 12, color: C.muted, ...BODY }}>{sub}</div>}
    </div>
  );
}

function MethodCard({ title, body, chips }: { title: string; body: string; chips: string[] }) {
  return (
    <div style={{ ...CARD }}>
      <div style={{ fontSize: 13, fontWeight: 600, color: C.text, marginBottom: 6, ...BODY }}>{title}</div>
      <div style={{ fontSize: 13, color: C.muted, lineHeight: 1.55, ...BODY }}>{body}</div>
      <div style={{ display: "flex", flexWrap: "wrap", gap: 6, marginTop: 12 }}>
        {chips.map(chip => (
          <span key={chip} style={{
            fontSize: 10,
            color: C.sub,
            border: `1px solid ${C.border}`,
            background: "rgba(255,255,255,0.03)",
            borderRadius: 4,
            padding: "3px 6px",
            ...MONO,
          }}>{chip}</span>
        ))}
      </div>
    </div>
  );
}

function InsightCards({ data }: { data: ModelPerformanceData }) {
  const yearBest = [...data.byYear].sort((a, b) => a.mae - b.mae)[0];
  const yearWorst = [...data.byYear].sort((a, b) => b.mae - a.mae)[0];
  const roundRows = [
    ...data.byRoundWR.map(r => ({ ...r, pos: "WR" })),
    ...data.byRoundRB.map(r => ({ ...r, pos: "RB" })),
  ];
  const weakestRound = [...roundRows].sort((a, b) => b.mae - a.mae)[0];
  const weakestRank = [...roundRows].sort((a, b) => a.cor - b.cor)[0];
  const bias = data.overall.bias;
  const biasLabel = bias > 0 ? "underpredicting" : bias < 0 ? "overpredicting" : "neutral";

  const cards = [
    {
      label: "Most accurate class",
      value: String(yearBest.year),
      detail: `Off by ${yearBest.mae.toFixed(2)} PPG on average`,
      color: C.teal,
    },
    {
      label: "Toughest class",
      value: String(yearWorst.year),
      detail: `Off by ${yearWorst.mae.toFixed(2)} PPG on average`,
      color: C.gold,
    },
    {
      label: "Hardest group",
      value: `${weakestRound.pos} ${weakestRound.round_grp.replace("Round ", "Rd ")}`,
      detail: `Off by ${weakestRound.mae.toFixed(2)} PPG across ${weakestRound.n} players`,
      color: C.coral,
    },
    {
      label: "Worst at ranking",
      value: `${weakestRank.pos} ${weakestRank.round_grp.replace("Round ", "Rd ")}`,
      detail: `Order quality ${weakestRank.cor.toFixed(2)} (1 = perfect)`,
      color: weakestRank.cor < 0 ? C.coral : C.gold,
    },
    {
      label: "Overall skew",
      value: `${bias > 0 ? "+" : ""}${bias.toFixed(2)}`,
      detail: bias > 0 ? `actual outcomes are this much higher than predicted on average`
                       : bias < 0 ? `actual outcomes are this much lower than predicted on average`
                                  : "predictions are centered",
      color: Math.abs(bias) < 0.15 ? C.teal : C.gold,
    },
  ];
  void biasLabel;  // retained for backwards compatibility

  return (
    <section style={{ marginBottom: 40 }}>
      <SectionLabel>Where It Struggles</SectionLabel>
      <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(170px, 1fr))", gap: 12 }}>
        {cards.map(card => (
          <div key={card.label} style={{ ...CARD, padding: "16px 18px" }}>
            <div style={{
              fontSize: 10,
              color: C.muted,
              letterSpacing: "0.08em",
              textTransform: "uppercase",
              ...BODY,
            }}>{card.label}</div>
            <div style={{ fontSize: 20, color: card.color, fontWeight: 700, marginTop: 8, ...MONO }}>
              {card.value}
            </div>
            <div style={{ fontSize: 12, color: C.muted, marginTop: 6, lineHeight: 1.45, ...BODY }}>
              {card.detail}
            </div>
          </div>
        ))}
      </div>
    </section>
  );
}

// ── Per-year MAE bar chart ────────────────────────────────────────────────────
function YearlyMAEChart({ data }: { data: ModelPerformanceData["byYear"] }) {
  const maxMae = Math.max(...data.flatMap(d => [d.wr_mae, d.rb_mae]));
  const W = 560, H = 200, PAD = { t: 16, r: 16, b: 32, l: 44 };
  const innerW = W - PAD.l - PAD.r;
  const innerH = H - PAD.t - PAD.b;
  const barGroupW = innerW / data.length;
  const barW = barGroupW * 0.3;
  const yScale = (v: number) => innerH - (v / (maxMae * 1.1)) * innerH;
  const yTicks = [1, 2, 3, 4].filter(v => v <= maxMae * 1.15);

  return (
    <svg viewBox={`0 0 ${W} ${H}`} style={{ width: "100%", overflow: "visible" }}>
      {/* Grid lines */}
      {yTicks.map(v => {
        const y = PAD.t + yScale(v);
        return (
          <g key={v}>
            <line x1={PAD.l} x2={W - PAD.r} y1={y} y2={y} stroke={C.border} strokeDasharray="3 3" />
            <text x={PAD.l - 6} y={y + 4} textAnchor="end" fill={C.muted} fontSize={10} fontFamily="var(--font-mono)">{v}</text>
          </g>
        );
      })}
      {/* Bars */}
      {data.map((d, i) => {
        const cx = PAD.l + i * barGroupW + barGroupW / 2;
        const wrH = (d.wr_mae / (maxMae * 1.1)) * innerH;
        const rbH = (d.rb_mae / (maxMae * 1.1)) * innerH;
        return (
          <g key={d.year}>
            <rect x={cx - barW - 1} y={PAD.t + yScale(d.wr_mae)} width={barW} height={wrH} fill={C.blue} opacity={0.8} rx={2} />
            <rect x={cx + 1} y={PAD.t + yScale(d.rb_mae)} width={barW} height={rbH} fill={C.gold} opacity={0.8} rx={2} />
            <text x={cx} y={H - PAD.b + 14} textAnchor="middle" fill={C.muted} fontSize={10} fontFamily="var(--font-mono)">{d.year}</text>
          </g>
        );
      })}
      {/* Legend */}
      <rect x={PAD.l} y={4} width={10} height={10} fill={C.blue} rx={2} />
      <text x={PAD.l + 14} y={13} fill={C.sub} fontSize={10} fontFamily="var(--font-body)">WR</text>
      <rect x={PAD.l + 42} y={4} width={10} height={10} fill={C.gold} rx={2} />
      <text x={PAD.l + 56} y={13} fill={C.sub} fontSize={10} fontFamily="var(--font-body)">RB</text>
    </svg>
  );
}

function CalibrationChart({ data }: { data: ModelPerformanceData["calibration"] }) {
  const W = 560, H = 220, PAD = { t: 18, r: 18, b: 44, l: 44 };
  const innerW = W - PAD.l - PAD.r;
  const innerH = H - PAD.t - PAD.b;
  const yS = (v: number) => PAD.t + innerH - v * innerH;
  const xStep = innerW / Math.max(data.length - 1, 1);
  const ticks = [0, 0.25, 0.5, 0.75, 1];
  const point = (d: ModelPerformanceData["calibration"][number], i: number, key: "pred_prob" | "obs_rate") =>
    `${PAD.l + i * xStep},${yS(d[key])}`;
  const predLine = data.map((d, i) => point(d, i, "pred_prob")).join(" ");
  const obsLine = data.map((d, i) => point(d, i, "obs_rate")).join(" ");

  return (
    <svg viewBox={`0 0 ${W} ${H}`} style={{ width: "100%", overflow: "visible" }}>
      {ticks.map(v => (
        <g key={v}>
          <line x1={PAD.l} x2={W - PAD.r} y1={yS(v)} y2={yS(v)} stroke={C.border} strokeDasharray="3 3" />
          <text x={PAD.l - 8} y={yS(v) + 4} textAnchor="end" fill={C.muted} fontSize={9} fontFamily="var(--font-mono)">
            {Math.round(v * 100)}
          </text>
        </g>
      ))}
      <line x1={PAD.l} x2={W - PAD.r} y1={yS(0)} y2={yS(1)} stroke={C.muted} strokeDasharray="5 4" opacity={0.45} />
      <polyline points={predLine} fill="none" stroke={C.blue} strokeWidth={2} />
      <polyline points={obsLine} fill="none" stroke={C.teal} strokeWidth={2} />
      {data.map((d, i) => {
        const x = PAD.l + i * xStep;
        return (
          <g key={d.bucket}>
            <circle cx={x} cy={yS(d.pred_prob)} r={4} fill={C.blue} />
            <circle cx={x} cy={yS(d.obs_rate)} r={4} fill={C.teal} />
            <text x={x} y={H - PAD.b + 14} textAnchor="middle" fill={C.muted} fontSize={9} fontFamily="var(--font-mono)">
              {d.bucket.replace("%", "")}
            </text>
            <text x={x} y={H - 8} textAnchor="middle" fill={C.muted} fontSize={8} fontFamily="var(--font-mono)">
              n={d.n}
            </text>
          </g>
        );
      })}
      <circle cx={PAD.l + 4} cy={8} r={4} fill={C.blue} />
      <text x={PAD.l + 12} y={12} fill={C.sub} fontSize={10} fontFamily="var(--font-body)">Pred</text>
      <circle cx={PAD.l + 54} cy={8} r={4} fill={C.teal} />
      <text x={PAD.l + 62} y={12} fill={C.sub} fontSize={10} fontFamily="var(--font-body)">Actual</text>
    </svg>
  );
}

// ── Scatter: predicted vs actual ──────────────────────────────────────────────
function ScatterChart({ data }: { data: ScatterPoint[] }) {
  const [hovered, setHovered] = useState<ScatterPoint | null>(null);
  const W = 760, H = 430, PAD = { t: 24, r: 24, b: 48, l: 56 };
  const innerW = W - PAD.l - PAD.r;
  const innerH = H - PAD.t - PAD.b;
  const maxV = 18;
  const xS = (v: number) => PAD.l + (v / maxV) * innerW;
  const yS = (v: number) => PAD.t + innerH - (v / maxV) * innerH;
  const ticks = [0, 4, 8, 12, 16];

  return (
    <div style={{ position: "relative" }}>
      <svg viewBox={`0 0 ${W} ${H}`} style={{ width: "100%", overflow: "visible" }}>
        {/* Grid */}
        {ticks.map(v => (
          <g key={v}>
            <line x1={PAD.l} x2={W - PAD.r} y1={yS(v)} y2={yS(v)} stroke={C.border} strokeDasharray="3 3" />
            <line x1={xS(v)} x2={xS(v)} y1={PAD.t} y2={H - PAD.b} stroke={C.border} strokeDasharray="3 3" />
            <text x={PAD.l - 6} y={yS(v) + 4} textAnchor="end" fill={C.muted} fontSize={9} fontFamily="var(--font-mono)">{v}</text>
            <text x={xS(v)} y={H - PAD.b + 14} textAnchor="middle" fill={C.muted} fontSize={9} fontFamily="var(--font-mono)">{v}</text>
          </g>
        ))}
        {/* Perfect prediction line */}
        <line x1={xS(0)} x2={xS(maxV)} y1={yS(0)} y2={yS(maxV)} stroke={C.muted} strokeDasharray="5 3" opacity={0.5} />
        {/* Points */}
        {data.map((d, i) => {
          const col =
            d.pos === "WR" ? C.blue
            : d.pos === "RB" ? C.gold
            : d.pos === "QB" ? C.teal
            : d.pos === "TE" ? C.coral
            : C.muted;
          return (
            <circle
              key={i}
              cx={xS(Math.min(d.pred, maxV))}
              cy={yS(Math.min(d.actual, maxV))}
              r={4}
              fill={col}
              opacity={hovered ? (hovered === d ? 1 : 0.15) : 0.65}
              style={{ cursor: "pointer" }}
              onMouseEnter={() => setHovered(d)}
              onMouseLeave={() => setHovered(null)}
            />
          );
        })}
        {/* Axis labels */}
        <text x={PAD.l + innerW / 2} y={H - 4} textAnchor="middle" fill={C.muted} fontSize={11} fontFamily="var(--font-body)">Predicted PPG</text>
        <text x={12} y={PAD.t + innerH / 2} textAnchor="middle" fill={C.muted} fontSize={11} fontFamily="var(--font-body)" transform={`rotate(-90, 12, ${PAD.t + innerH / 2})`}>Actual PPG</text>
        {/* Legend */}
        <circle cx={PAD.l + 4} cy={PAD.t + 10} r={4} fill={C.blue} />
        <text x={PAD.l + 12} y={PAD.t + 14} fill={C.sub} fontSize={10} fontFamily="var(--font-body)">WR</text>
        <circle cx={PAD.l + 40} cy={PAD.t + 10} r={4} fill={C.gold} />
        <text x={PAD.l + 48} y={PAD.t + 14} fill={C.sub} fontSize={10} fontFamily="var(--font-body)">RB</text>
      </svg>
      {/* Tooltip */}
      {hovered && (
        <div style={{
          position: "absolute",
          top: 24, right: 8,
          background: "#181E2B",
          border: `1px solid ${C.border}`,
          borderRadius: 8,
          padding: "10px 14px",
          fontSize: 12,
          color: C.text,
          pointerEvents: "none",
          zIndex: 10,
          minWidth: 180,
          ...BODY,
        }}>
          <div style={{ fontWeight: 600, marginBottom: 4 }}>{hovered.name}</div>
          <div style={{ color: C.muted }}>{hovered.pos} · {hovered.year} · Rd {hovered.round} #{hovered.pick}</div>
          <div style={{ marginTop: 6, display: "flex", gap: 12 }}>
            <div><span style={{ color: C.muted }}>Pred: </span><span style={{ ...MONO, color: C.blue }}>{hovered.pred.toFixed(1)}</span></div>
            <div><span style={{ color: C.muted }}>Actual: </span><span style={{ ...MONO, color: hovered.hit ? C.teal : C.coral }}>{hovered.actual.toFixed(1)}</span></div>
          </div>
          <div style={{ marginTop: 4, color: C.muted }}>
            P(hit): <span style={{ ...MONO, color: C.sub }}>{(hovered.p_hit * 100).toFixed(0)}%</span>
          </div>
        </div>
      )}
    </div>
  );
}

// ── Round breakdown table ─────────────────────────────────────────────────────
function RoundTable({
  data, accentColor,
}: {
  data: ModelPerformanceData["byRound"];
  accentColor: string;
}) {
  return (
    <table style={{ width: "100%", borderCollapse: "collapse" }}>
      <thead>
        <tr>
          {[
            { label: "Round",     right: false },
            { label: "Players",   right: true },
            { label: "Avg miss ↓",right: true },
            { label: "Order ↑",   right: true },
            { label: "Hit % ↑",   right: true },
          ].map(h => (
            <th key={h.label} style={{
              padding: "8px 12px", textAlign: h.right ? "right" : "left",
              fontSize: 10, fontWeight: 500, letterSpacing: "0.06em",
              textTransform: "uppercase", color: C.muted,
              borderBottom: `1px solid ${C.border}`, ...BODY,
            }}>{h.label}</th>
          ))}
        </tr>
      </thead>
      <tbody>
        {data.map((r) => (
          <tr key={r.round_grp} style={{ borderBottom: `1px solid ${C.border}` }}>
            <td style={{ padding: "10px 12px", color: C.text, fontSize: 13, ...BODY }}>{r.round_grp}</td>
            <td style={{ padding: "10px 12px", textAlign: "right", color: C.muted, fontSize: 12, ...MONO }}>{r.n}</td>
            <td style={{ padding: "10px 12px", textAlign: "right", fontSize: 14, fontWeight: 700, color: C.text, ...MONO }}>{r.mae.toFixed(2)}</td>
            <td style={{ padding: "10px 12px", textAlign: "right", fontSize: 13, color: r.cor < 0 ? C.coral : accentColor, ...MONO }}>{r.cor.toFixed(3)}</td>
            <td style={{ padding: "10px 12px", textAlign: "right", fontSize: 13, color: r.hit_rate >= 0.7 ? C.teal : C.sub, ...MONO }}>
              {(r.hit_rate * 100).toFixed(0)}%
            </td>
          </tr>
        ))}
      </tbody>
    </table>
  );
}

// ── Notable misses/hits table ─────────────────────────────────────────────────
function NotableTable({ data }: { data: ModelPerformanceData["notable"] }) {
  const sorted = useMemo(() => [...data].sort((a, b) => b.diff - a.diff), [data]);
  return (
    <table style={{ width: "100%", borderCollapse: "collapse" }}>
      <thead>
        <tr>
          {["Player", "Pos", "Year", "Pick", "Predicted", "Actual", "Difference"].map(h => (
            <th key={h} style={{
              padding: "8px 12px", textAlign: h === "Player" ? "left" : "right",
              fontSize: 11, fontWeight: 500, letterSpacing: "0.06em",
              textTransform: "uppercase", color: C.muted,
              borderBottom: `1px solid ${C.border}`, ...BODY,
            }}>{h}</th>
          ))}
        </tr>
      </thead>
      <tbody>
        {sorted.map((p, i) => {
          const over = p.diff > 0;
          const didNotQualify = p.actual < 0.5;
          return (
            <tr key={i} style={{ borderBottom: `1px solid ${C.border}` }}>
              <td style={{ padding: "10px 12px", color: C.text, fontSize: 13, fontWeight: 500, ...BODY }}>{p.name}</td>
              <td style={{ padding: "10px 12px", textAlign: "right", fontSize: 11, color: C.muted, ...MONO }}>{p.pos}</td>
              <td style={{ padding: "10px 12px", textAlign: "right", fontSize: 12, color: C.muted, ...MONO }}>{p.year}</td>
              <td style={{ padding: "10px 12px", textAlign: "right", fontSize: 12, color: C.muted, ...MONO }}>#{p.pick}</td>
              <td style={{ padding: "10px 12px", textAlign: "right", fontSize: 13, color: C.sub, ...MONO }}>{p.pred.toFixed(1)}</td>
              <td style={{ padding: "10px 12px", textAlign: "right", fontSize: 13, color: didNotQualify ? C.muted : C.sub, ...MONO }}>
                {didNotQualify ? <span title="No qualifying seasons">DNQ</span> : p.actual.toFixed(1)}
              </td>
              <td style={{ padding: "10px 12px", textAlign: "right", fontSize: 13, fontWeight: 700, color: over ? C.teal : C.coral, ...MONO }}>
                {over ? "+" : ""}{p.diff.toFixed(1)}
              </td>
            </tr>
          );
        })}
      </tbody>
    </table>
  );
}

// ── Main page ─────────────────────────────────────────────────────────────────
export function ModelPage() {
  const [data, setData] = useState<ModelPerformanceData | null>(null);
  const [posFilter, setPosFilter] = useState<"ALL" | "WR" | "RB" | "QB" | "TE">("ALL");

  useEffect(() => {
    fetch(dataUrl("/data/model_performance.json"))
      .then(r => r.json())
      .then(setData);
  }, []);

  const filteredScatter = useMemo(() => {
    if (!data) return [];
    return posFilter === "ALL" ? data.scatter : data.scatter.filter(d => d.pos === posFilter);
  }, [data, posFilter]);

  if (!data) {
    return (
      <div style={{ display: "flex", alignItems: "center", justifyContent: "center", padding: "80px 0" }}>
        <div style={{ color: C.muted, fontFamily: "var(--font-mono)", fontSize: 13 }}>Loading…</div>
      </div>
    );
  }

  const o = data.overall;
  const wrN = data.byRoundWR.reduce((sum, r) => sum + r.n, 0);
  const rbN = data.byRoundRB.reduce((sum, r) => sum + r.n, 0);

  return (
    <div style={{ maxWidth: 1100, margin: "0 auto", padding: "40px 24px 64px" }}>

      {/* Page title */}
      <div style={{ marginBottom: 32 }}>
        <h2 style={{ fontFamily: "var(--font-display)", fontSize: 22, fontWeight: 700, color: C.text, marginBottom: 8 }}>
          How accurate is this model?
        </h2>
        <p style={{ fontSize: 14, color: C.muted, lineHeight: 1.6, maxWidth: 720, ...BODY }}>
          Every number on this page comes from a fair test:{" "}
          <strong style={{ color: C.sub }}>the model never saw these players when it was being built</strong>.
          For each draft class from 2016&ndash;2023 ({o.n} players total), we rebuilt the model
          using only data from earlier years, then asked it to predict that class&rsquo; pro careers
          and compared to what actually happened. So the accuracy below reflects how well the model
          would have done in real time, not how well it memorized the past.
        </p>
      </div>

      {/* ── Summary stats ── */}
      <section style={{ marginBottom: 40 }}>
        <SectionLabel>Overall Accuracy · {o.n} players, 2016–2023</SectionLabel>
        {/* Row 1: overall */}
        <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(140px, 1fr))", gap: 12, marginBottom: 12 }}>
          <StatTile label="Avg miss" value={o.mae} sub="how far off the prediction is per player, in fantasy PPG (lower is better)" color={C.teal} fmt={v => v.toFixed(2)} />
          <StatTile label="Ranking quality" value={o.cor} sub="how well the model orders players from best to worst (0 = noise, 1 = perfect)" color={C.blue} fmt={v => v.toFixed(3)} />
          <StatTile label="Hit/miss accuracy" value={o.bust_accuracy} sub="how often it correctly calls a player a hit or a bust" color={C.gold} fmt={v => `${(v * 100).toFixed(1)}%`} />
          <StatTile label="Systematic skew" value={o.bias} sub={bias_blurb(o.bias)} color={Math.abs(o.bias) < 0.15 ? C.teal : C.gold} fmt={v => `${v > 0 ? "+" : ""}${v.toFixed(2)}`} />
        </div>
        {/* Row 2: by position */}
        <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(140px, 1fr))", gap: 12 }}>
          <StatTile label="WR avg miss" value={o.wr_mae} sub={`ranking quality ${o.wr_cor.toFixed(3)} · ${wrN} receivers`} color={C.blue} fmt={v => v.toFixed(2)} />
          <StatTile label="RB avg miss" value={o.rb_mae} sub={`ranking quality ${o.rb_cor.toFixed(3)} · ${rbN} backs`} color={C.gold} fmt={v => v.toFixed(2)} />
          <StatTile label="WR skew" value={o.wr_bias} sub={bias_blurb(o.wr_bias)} color={Math.abs(o.wr_bias) < 0.15 ? C.teal : C.blue} fmt={v => `${v > 0 ? "+" : ""}${v.toFixed(2)}`} />
          <StatTile label="RB skew" value={o.rb_bias} sub={bias_blurb(o.rb_bias)} color={Math.abs(o.rb_bias) < 0.15 ? C.teal : C.gold} fmt={v => `${v > 0 ? "+" : ""}${v.toFixed(2)}`} />
        </div>
      </section>

      <InsightCards data={data} />

      {/* ── Methodology ── */}
      <section style={{ marginBottom: 40 }}>
        <SectionLabel>How It Works</SectionLabel>
        <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(260px, 1fr))", gap: 12 }}>
          {[
            {
              title: "What we're predicting",
              body: "The half-PPR fantasy points per game a player averages in the best of their first three NFL seasons. Players who never become regulars count as zero — so a high prediction is the model staking out both 'will play' and 'will produce'.",
              chips: ["Half-PPR fantasy points", "First 3 NFL years", "Busts count as zero"],
            },
            {
              title: "A distribution, not just a number",
              body: "Beyond the single expected-PPG estimate, the model produces a full probability distribution across five outcome tiers — bust, bench, flex, elite, league winner. The bars on each prospect card show where the model thinks each player is most likely to land, with credible intervals showing how confident it is.",
              chips: ["5 outcome tiers", "Probabilities, not point estimates", "Confidence intervals"],
            },
            {
              title: "What goes in",
              body: "Draft pick (where teams valued them), college production, play-by-play efficiency, combine athletic testing, high-school recruiting ratings, NFL team's opportunity at the position, and similarity to past prospects.",
              chips: ["Draft capital", "College stats", "Combine", "Recruiting", "Landing spot", "Player comps"],
            },
            {
              title: "Player comps",
              body: "For each prospect, we find the most similar past prospects and surface their NFL outcomes. Comps are built strictly from earlier draft classes — no peeking at future data — and are shown in the player profile alongside model predictions.",
              chips: ["Past classes only", "No data leakage"],
            },
            {
              title: "How we tested it",
              body: `For each draft year from 2016 through 2023, we trained the model on every earlier year only and graded its predictions against what actually happened. ${o.n} players in total — none seen during training.`,
              chips: ["Walk-forward testing", "8 draft classes", "No peeking"],
            },
          ].map(c => (
            <MethodCard key={c.title} title={c.title} body={c.body} chips={c.chips} />
          ))}
        </div>
      </section>

      {/* ── Charts row ── */}
      <section style={{ marginBottom: 40 }}>
        <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(min(100%, 320px), 1fr))", gap: 20 }}>

          {/* Per-year MAE */}
          <div style={{ ...CARD }}>
            <div style={{ display: "flex", justifyContent: "space-between", alignItems: "baseline", marginBottom: 16 }}>
              <div>
                <div style={{ fontSize: 13, fontWeight: 600, color: C.text, ...BODY }}>Accuracy year by year</div>
                <div style={{ fontSize: 11, color: C.muted, marginTop: 2, ...BODY }}>Avg fantasy PPG miss per draft class — shorter bars are better</div>
              </div>
            </div>
            <YearlyMAEChart data={data.byYear} />
          </div>

          {/* Calibration */}
          <div style={{ ...CARD }}>
            <div style={{ display: "flex", justifyContent: "space-between", alignItems: "baseline", marginBottom: 16 }}>
              <div>
                <div style={{ fontSize: 13, fontWeight: 600, color: C.text, ...BODY }}>Are the hit probabilities honest?</div>
                <div style={{ fontSize: 11, color: C.muted, marginTop: 2, ...BODY }}>When the model says "70% chance to hit", do 70% actually hit? The two lines should overlap.</div>
              </div>
            </div>
            <CalibrationChart data={data.calibration} />
          </div>
        </div>
      </section>

      <section style={{ marginBottom: 40 }}>
        <div style={{ ...CARD }}>
          <div style={{ display: "flex", justifyContent: "space-between", alignItems: "baseline", marginBottom: 16, gap: 16, flexWrap: "wrap" }}>
            <div>
              <div style={{ fontSize: 13, fontWeight: 600, color: C.text, ...BODY }}>Every prediction vs what actually happened</div>
              <div style={{ fontSize: 11, color: C.muted, marginTop: 2, ...BODY }}>Each dot is one player. Hover to see who. The dashed line is a perfect prediction — closer dots = better.</div>
            </div>
            <div style={{ display: "flex", background: "rgba(255,255,255,0.04)", borderRadius: 6, overflow: "hidden", border: `1px solid ${C.border}` }}>
              {(["ALL", "QB", "RB", "WR", "TE"] as const).map(p => (
                <button key={p} onClick={() => setPosFilter(p)} style={{
                  padding: "4px 10px", fontSize: 11, fontWeight: 600,
                  fontFamily: "var(--font-mono)", border: "none", cursor: "pointer",
                  background: posFilter === p ? C.blue : "transparent",
                  color: posFilter === p ? "#fff" : C.muted,
                }}>{p}</button>
              ))}
            </div>
          </div>
          <ScatterChart data={filteredScatter} />
        </div>
      </section>

      {/* ── By round ── */}
      <section style={{ marginBottom: 40 }}>
        <SectionLabel>Accuracy by Draft Round</SectionLabel>
        <p style={{ fontSize: 12, color: C.muted, marginBottom: 12, maxWidth: 640, ...BODY }}>
          The model is most accurate where teams agree (rounds 1&ndash;2) and noisiest in the middle rounds where careers vary widely.
          <em> Hit rate</em> is the share of players the model correctly classified as making it / not making it.
        </p>
        <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(300px, 1fr))", gap: 16 }}>
          {/* WR */}
          <div style={{ ...CARD, padding: 0, overflow: "hidden" }}>
            <div style={{
              padding: "10px 12px 8px",
              borderBottom: `1px solid ${C.border}`,
              display: "flex", alignItems: "center", gap: 8,
            }}>
              <span style={{ width: 8, height: 8, borderRadius: "50%", background: C.blue, flexShrink: 0, display: "inline-block" }} />
              <span style={{ fontSize: 12, fontWeight: 600, color: C.text, ...BODY }}>Wide Receivers</span>
              <span style={{ fontSize: 11, color: C.muted, ...MONO, marginLeft: "auto" }}>{wrN} players</span>
            </div>
            <RoundTable data={data.byRoundWR} accentColor={C.blue} />
          </div>
          {/* RB */}
          <div style={{ ...CARD, padding: 0, overflow: "hidden" }}>
            <div style={{
              padding: "10px 12px 8px",
              borderBottom: `1px solid ${C.border}`,
              display: "flex", alignItems: "center", gap: 8,
            }}>
              <span style={{ width: 8, height: 8, borderRadius: "50%", background: C.gold, flexShrink: 0, display: "inline-block" }} />
              <span style={{ fontSize: 12, fontWeight: 600, color: C.text, ...BODY }}>Running Backs</span>
              <span style={{ fontSize: 11, color: C.muted, ...MONO, marginLeft: "auto" }}>{rbN} players</span>
            </div>
            <RoundTable data={data.byRoundRB} accentColor={C.gold} />
          </div>
        </div>
      </section>

      {/* ── Notable ── */}
      <section style={{ marginBottom: 40 }}>
        <SectionLabel>Biggest Surprises</SectionLabel>
        <p style={{ fontSize: 12, color: C.muted, marginBottom: 12, ...BODY }}>
          Players whose actual NFL careers diverged most from what the model predicted (without ever seeing them).
          Green = outperformed · Red = underperformed · DNQ = never had a qualifying NFL season.
        </p>
        <div style={{ ...CARD, padding: 0, overflow: "hidden" }}>
          <NotableTable data={data.notable} />
        </div>
      </section>

      {/* ── Methodology (technical detail) ── */}
      <MethodologySection />

      {/* ── Footnote ── */}
      <div style={{
        borderTop: `1px solid ${C.border}`,
        paddingTop: 20,
        display: "flex",
        flexWrap: "wrap",
        gap: 24,
      }}>
        {[
          { term: "PPG",      def: "Half-PPR fantasy points per game, averaged over a player's best qualifying seasons in their first three NFL years." },
          { term: "Hit",      def: "A player who had at least one productive NFL season in the window we measure." },
          { term: "Avg miss", def: "How far off the model's prediction is, on average, in fantasy points per game. Lower is better." },
          { term: "DNQ",      def: "Did not qualify — the player never had enough NFL playing time to register a season." },
        ].map(({ term, def }) => (
          <div key={term} style={{ flex: "1 1 200px" }}>
            <span style={{ fontSize: 11, fontWeight: 700, color: C.sub, ...MONO }}>{term} </span>
            <span style={{ fontSize: 12, color: C.muted, lineHeight: 1.5, ...BODY }}>{def}</span>
          </div>
        ))}
      </div>
    </div>
  );
}

// ── Methodology (for ML-literate readers) ────────────────────────────────────
// Free-form prose section that goes deeper than the plain-English "How It
// Works" cards above. Targets readers comfortable with terms like MAE,
// hurdle model, isotonic recalibration, etc.

function MethodologySubsection({
  heading, children,
}: { heading: string; children: React.ReactNode }) {
  return (
    <div style={{ marginBottom: 22 }}>
      <h4 style={{
        fontSize: 13, fontWeight: 600, color: C.text, margin: "0 0 8px",
        letterSpacing: "-0.01em", ...BODY,
      }}>{heading}</h4>
      <div style={{ fontSize: 13, color: C.sub, lineHeight: 1.65, ...BODY }}>
        {children}
      </div>
    </div>
  );
}

function MethodologySection() {
  return (
    <section style={{ marginBottom: 40 }}>
      <SectionLabel>Methodology · technical detail</SectionLabel>
      <div style={{ ...CARD, padding: "26px 28px", maxWidth: 820 }}>

        <p style={{ fontSize: 13, color: C.sub, lineHeight: 1.65, marginTop: 0, marginBottom: 22, ...BODY }}>
          A two-stage hurdle model fit on every WR/RB drafted since 2002, evaluated by rolling temporal
          cross-validation. The high-level shape is described below; the exact configurations, tuned constants,
          and feature lists are intentionally left out.
        </p>

        <MethodologySubsection heading="Target & framing">
          The target is half-PPR fantasy points per game, averaged over a player's best qualifying NFL seasons
          within their first three years. Players who never log a qualifying season are folded into the same
          target as zeros, which makes the unconditional distribution heavily zero-inflated — hence a hurdle
          factoring rather than a single regression. The conditional production target gets a light shrinkage
          treatment so single-season outliers don't dominate.
        </MethodologySubsection>

        <MethodologySubsection heading="Architecture">
          A prospect's headline prediction comes from an ensemble of two complementary models, blended
          at position-specific weights tuned by cross-validation:
          <ul style={{ margin: "10px 0 0 18px", padding: 0, lineHeight: 1.75 }}>
            <li>
              <strong style={{ color: C.text }}>Continuous hurdle</strong> — two gradient-boosted stages:
              a binary classifier producing the probability a prospect ever records a qualifying NFL season,
              and a log-PPG regressor fit only on producers. The two pieces multiply through a position-tuned
              calibrator to yield a continuous expected PPG.
            </li>
            <li>
              <strong style={{ color: C.text }}>Ordinal-bucket ensemble</strong> — a gradient-boosted
              multiclass classifier and a Bayesian proportional-odds model are each fit to the five-tier
              outcome label (bust / bench / flex / elite / league winner) and their probabilities are
              geometric-mean ensembled per draw. The Bayesian model contributes posterior credible intervals
              on each bucket probability, propagated through the ensemble.
            </li>
            <li>
              <strong style={{ color: C.text }}>Comp-stack second opinion</strong> — a kNN over the
              strictly-past pool of similar historical prospects with mature NFL outcomes. Aggregated comp
              PPG and bust rate enter the displayed expected value as a third signal alongside the two
              model predictions.
            </li>
          </ul>
          The displayed bucket distribution comes from the ensemble's posterior means; the displayed
          headline PPG blends the two models then mixes with the comp-stack output.
        </MethodologySubsection>

        <MethodologySubsection heading="Features">
          Roughly six dozen features per position, grouped: draft capital, college production (across multiple
          seasons with year-over-year deltas), team context, athletic testing, high-school recruiting, college
          play-by-play efficiency, similarity to past prospects, NFL landing-spot opportunity, and a mock-vs-actual
          draft-capital delta. Era-incomplete features (PBP, advanced usage) are zero-filled when their coverage
          flag is 0 rather than median-imputed, so the tree learns "pre-era" as a regime via the flag instead of
          being pulled toward the post-era median. Comp-stack features are built from a strictly-past pool so the
          training distribution and the deployment distribution match.
        </MethodologySubsection>

        <MethodologySubsection heading="Calibration & combining">
          The hit classifier inside the continuous hurdle is reasonably calibrated for WRs out of sample but
          noticeably worse for RBs. Rather than discard the RB classifier output, it's recalibrated by isotonic
          regression refit per fold, then combined with a position-specific transform chosen by sweeping a small
          family of candidates. The bucket ensemble's two models are combined via geometric mean per posterior
          draw, then the bucket and hurdle estimates are blended with position-specific weights tuned to OOS
          MAE (currently bucket-leaning for both positions). The final mix with the comp-stack uses a separate
          set of weights tuned on validation.
        </MethodologySubsection>

        <MethodologySubsection heading="Evaluation protocol">
          For each test draft year in 2016&ndash;2023, the entire pipeline (preprocessing, both models, the
          combiner) is refit using only earlier years and evaluated on the held-out class. Reported metrics: MAE
          and RMSE on the PPG scale, Pearson correlation between predicted and actual PPG, and hit-classification
          accuracy — broken out by position and by draft round. Pipeline choices (feature set, hyperparameter
          search space, combiner family) are held fixed across folds, so the headline number reflects fit, not
          pipeline-search noise.
        </MethodologySubsection>

        <MethodologySubsection heading="Data sources">
          <ul style={{ margin: "6px 0 0 18px", padding: 0, lineHeight: 1.75 }}>
            <li>cfbfastR — college season stats (regular + postseason) and play-by-play</li>
            <li>nflreadr — NFL outcomes, draft picks, combine measurables, rosters</li>
            <li>247Sports composite — high-school recruiting ratings</li>
            <li>Public mock-draft aggregators — pre-draft consensus rankings</li>
            <li>Pro-Football-Reference career value — basis for the draft-pick value curve</li>
          </ul>
        </MethodologySubsection>

        <MethodologySubsection heading="Known limitations">
          Pre-2010 college data is sparse, so the RB training window is shorter than the WR window and pre-2010
          WR rows are downweighted. NFL combine participation has been declining for several years; a coverage
          flag plus native missing-value handling in the tree limits the damage. Mock-draft coverage is bounded
          to roughly the top of each class, so the draft-capital-delta feature is informative for early-round
          prospects but null for late-rounders. Finally, the regressor's regression-to-mean in log-PPG space caps
          predictions for elite-tail outcomes — the features can be right, but the conditional mean simply doesn't
          reach 99th-percentile careers.
        </MethodologySubsection>

      </div>
    </section>
  );
}

function SectionLabel({ children }: { children: React.ReactNode }) {
  return (
    <div style={{
      fontSize: 11, fontWeight: 500, textTransform: "uppercase",
      letterSpacing: "0.1em", color: C.muted, marginBottom: 14, ...BODY,
    }}>{children}</div>
  );
}

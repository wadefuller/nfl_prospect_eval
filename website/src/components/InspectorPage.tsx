import { useEffect, useMemo, useRef, useState } from "react";
import { PlayerAvatar } from "./PlayerAvatar";
import type { ProspectComp } from "../types";
import { dataUrl } from "../dataUrl";

// ── Types ────────────────────────────────────────────────────────────────────
interface IndexPlayer {
  id: string; name: string; position: "WR" | "RB"; college: string | null;
  draft_year: number; round: number | null; pick: number | null;
  tier: string | null; headshot_url: string | null;
  p_made_it: number | null; exp_ppg: number | null;
  actual_ppg: number | null; made_it: number | null;
}
interface IndexJson {
  lastUpdated: string; minDraftYear: number; maxDraftYear: number;
  positions: string[]; total: number; players: IndexPlayer[];
}
interface Row {
  feat: string; desc: string;
  value: number | null; valueDisplay: string; percentile: number | null;
}
interface Group { label: string; rows: Row[]; }
interface ProductionRow { metric: string; ante: number | null; penult: number | null; final: number | null; }
interface CombineRow { metric: string; value: number | null; valueDisplay: string; percentile: number | null; }
interface RawRow { field: string; value: string | null; }
interface BucketSummary {
  top1: string | null;
  means: { bust: number | null; bench: number | null; flex: number | null;
            elite: number | null; league_winner: number | null };
  lo:    { bust: number | null; bench: number | null; flex: number | null;
            elite: number | null; league_winner: number | null };
  hi:    { bust: number | null; bench: number | null; flex: number | null;
            elite: number | null; league_winner: number | null };
  exp_ppg_bucket: number | null;
  exp_ppg_bucket_lo: number | null;
  exp_ppg_bucket_hi: number | null;
}
interface PlayerJson {
  id: string; name: string; position: "WR" | "RB";
  college: string | null; draft_year: number; round: number | null;
  pick: number | null; tier: string | null; headshot_url: string | null;
  summary: {
    p_made_it: number | null; exp_ppg: number | null;
    prospect_score: number | null;
    actual_ppg: number | null; made_it: number | null;
    bucket: BucketSummary | null;
  };
  groups: Group[]; production: ProductionRow[]; combine: CombineRow[]; raw: RawRow[];
}

// ── Design tokens (matches ModelPage.tsx) ────────────────────────────────────
const C = {
  teal: "#2DD4A0", blue: "#3E8EF7", gold: "#F5A623", coral: "#F75757",
  muted: "#4A5578", text: "#F0F4FF", sub: "#8A9AC0",
  card: "rgba(255,255,255,0.03)", border: "rgba(255,255,255,0.07)",
  cardSolid: "#181E2B",
};
const MONO: React.CSSProperties = { fontFamily: "var(--font-mono)" };
const BODY: React.CSSProperties = { fontFamily: "var(--font-body)" };
const CARD: React.CSSProperties = {
  background: C.card, border: `1px solid ${C.border}`, borderRadius: 10,
  padding: "20px 24px",
};

const pctColor = (p: number | null): string => {
  if (p == null) return C.muted;
  if (p >= 80) return C.teal;
  if (p >= 60) return C.blue;
  if (p >= 40) return C.gold;
  return C.coral;
};

// ── Sub-components ───────────────────────────────────────────────────────────

function PercentileRow({ row }: { row: Row }) {
  const pct = row.percentile;
  const color = pctColor(pct);
  const fillW = pct == null ? 0 : pct;
  return (
    <div className="percentile-row" style={{
      padding: "8px 0", borderBottom: `1px solid ${C.border}`,
    }}>
      <div className="percentile-label" style={{ display: "flex", flexDirection: "column", gap: 2 }}>
        <div style={{ fontSize: 12, fontWeight: 600, color: C.text, ...MONO }}>{row.feat}</div>
        {row.desc && (
          <div style={{ fontSize: 10.5, color: C.muted, lineHeight: 1.35, ...BODY }}>
            {row.desc}
          </div>
        )}
      </div>
      <div className="percentile-value" style={{
        flex: "0 0 70px", textAlign: "right", color: C.text,
        fontSize: 12, paddingTop: 2, ...MONO,
      }}>
        {row.value == null ? "NA" : row.valueDisplay}
      </div>
      <div className="percentile-bar" style={{
        flex: 1, height: 8, background: "rgba(255,255,255,0.08)",
        borderRadius: 4, position: "relative", overflow: "hidden", marginTop: 6,
      }}>
        <div style={{
          position: "absolute", top: 0, left: 0, bottom: 0,
          width: `${fillW}%`, background: color, borderRadius: 4,
        }} />
      </div>
      <div className="percentile-pct" style={{
        flex: "0 0 48px", textAlign: "right", fontWeight: 600,
        fontSize: 12, color, paddingTop: 2, ...MONO,
      }}>
        {pct == null ? "—" : `p${String(pct).padStart(2, "0")}`}
      </div>
    </div>
  );
}

function GroupBlock({ group }: { group: Group }) {
  return (
    <div style={{ marginBottom: 24 }}>
      <h3 style={{
        fontSize: 11, fontWeight: 600, color: C.muted,
        textTransform: "uppercase", letterSpacing: "0.1em",
        margin: "8px 0 6px", paddingBottom: 4,
        borderBottom: `1px solid ${C.border}`, ...BODY,
      }}>{group.label}</h3>
      {group.rows.map(r => <PercentileRow key={r.feat} row={r} />)}
    </div>
  );
}

function ProductionChart({ data }: { data: ProductionRow[] }) {
  return (
    <div style={{
      display: "grid", gridTemplateColumns: `repeat(${data.length}, 1fr)`,
      gap: 16,
    }}>
      {data.map(row => {
        const vals = [row.ante, row.penult, row.final];
        const max = Math.max(...vals.filter((v): v is number => v != null && isFinite(v)), 1);
        const labels = ["Ante", "Penult", "Final"];
        const colors = [C.muted, C.blue, C.teal];
        return (
          <div key={row.metric} style={{ ...CARD, padding: 16 }}>
            <div style={{
              fontSize: 12, fontWeight: 600, color: C.text,
              marginBottom: 12, ...BODY,
            }}>{row.metric}</div>
            <div style={{ display: "flex", alignItems: "flex-end", gap: 8, height: 140 }}>
              {vals.map((v, i) => {
                const h = v == null ? 0 : (v / max) * 110;
                return (
                  <div key={i} style={{
                    flex: 1, display: "flex", flexDirection: "column",
                    alignItems: "center", justifyContent: "flex-end", height: "100%",
                  }}>
                    <div style={{
                      fontSize: 11, color: v == null ? C.muted : C.text,
                      marginBottom: 4, ...MONO,
                    }}>
                      {v == null ? "—" : Math.round(v).toLocaleString()}
                    </div>
                    <div style={{
                      width: "70%", height: h, background: colors[i],
                      opacity: v == null ? 0.2 : 0.85, borderRadius: 3,
                      transition: "height 0.2s",
                    }} />
                    <div style={{
                      fontSize: 10, color: C.muted, marginTop: 6, ...BODY,
                    }}>{labels[i]}</div>
                  </div>
                );
              })}
            </div>
          </div>
        );
      })}
    </div>
  );
}

function CombineGrid({ data }: { data: CombineRow[] }) {
  return (
    <div style={{
      display: "grid",
      gridTemplateColumns: "repeat(auto-fit, minmax(180px, 1fr))",
      gap: 12,
    }}>
      {data.map(row => {
        const color = pctColor(row.percentile);
        return (
          <div key={row.metric} style={{ ...CARD, padding: 14 }}>
            <div style={{
              fontSize: 10, fontWeight: 500, color: C.muted,
              textTransform: "uppercase", letterSpacing: "0.08em",
              marginBottom: 6, ...BODY,
            }}>{row.metric.replace(/_/g, " ")}</div>
            <div style={{
              display: "flex", justifyContent: "space-between",
              alignItems: "baseline", marginBottom: 8,
            }}>
              <div style={{ fontSize: 22, fontWeight: 700, color: C.text, ...MONO }}>
                {row.value == null ? "—" : row.valueDisplay}
              </div>
              <div style={{ fontSize: 12, fontWeight: 600, color, ...MONO }}>
                {row.percentile == null ? "—" : `p${String(row.percentile).padStart(2, "0")}`}
              </div>
            </div>
            <div style={{
              height: 6, background: "rgba(255,255,255,0.08)",
              borderRadius: 3, overflow: "hidden",
            }}>
              <div style={{
                height: "100%",
                width: row.percentile == null ? 0 : `${row.percentile}%`,
                background: color, borderRadius: 3,
              }} />
            </div>
          </div>
        );
      })}
    </div>
  );
}

function RawTable({ data }: { data: RawRow[] }) {
  const [filter, setFilter] = useState("");
  const filtered = useMemo(() => {
    const q = filter.trim().toLowerCase();
    if (!q) return data;
    return data.filter(r =>
      r.field.toLowerCase().includes(q) ||
      (r.value ?? "").toLowerCase().includes(q));
  }, [filter, data]);
  return (
    <div>
      <input
        type="text"
        placeholder="Filter fields…"
        value={filter}
        onChange={e => setFilter(e.target.value)}
        style={{
          background: "rgba(255,255,255,0.04)",
          border: `1px solid ${C.border}`,
          borderRadius: 6, padding: "6px 12px",
          color: C.text, fontFamily: "var(--font-mono)", fontSize: 12,
          width: 240, marginBottom: 12, outline: "none",
        }}
      />
      <div style={{ ...CARD, padding: 0, overflow: "hidden" }}>
        <table style={{ width: "100%", borderCollapse: "collapse" }}>
          <thead>
            <tr>
              {["Field", "Value"].map(h => (
                <th key={h} style={{
                  padding: "8px 12px", textAlign: "left",
                  fontSize: 10, fontWeight: 500, letterSpacing: "0.06em",
                  textTransform: "uppercase", color: C.muted,
                  borderBottom: `1px solid ${C.border}`, ...BODY,
                }}>{h}</th>
              ))}
            </tr>
          </thead>
          <tbody>
            {filtered.map(r => (
              <tr key={r.field} style={{ borderBottom: `1px solid ${C.border}` }}>
                <td style={{
                  padding: "6px 12px", fontSize: 12, color: C.text,
                  fontFamily: "var(--font-mono)",
                }}>{r.field}</td>
                <td style={{
                  padding: "6px 12px", fontSize: 12, color: r.value == null ? C.muted : C.sub,
                  fontFamily: "var(--font-mono)",
                }}>{r.value == null ? "NA" : r.value}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}

// ── Comps list ───────────────────────────────────────────────────────────────
function CompsList({ comps }: { comps: ProspectComp[] | null }) {
  if (!comps) {
    return (
      <div style={{ ...CARD, padding: 40, textAlign: "center" }}>
        <div style={{ color: C.muted, fontFamily: "var(--font-mono)", fontSize: 13 }}>
          Loading comps…
        </div>
      </div>
    );
  }
  if (comps.length === 0) {
    return (
      <div style={{ ...CARD, padding: 40, textAlign: "center" }}>
        <div style={{ color: C.muted, fontFamily: "var(--font-mono)", fontSize: 13 }}>
          No comps available for this player.
        </div>
      </div>
    );
  }
  // Aggregate stats
  const hitRate = comps.filter(c => c.madeIt).length / comps.length;
  const meanPpg = comps.reduce((s, c) => s + c.ppg, 0) / comps.length;
  const medianPpg = (() => {
    const sorted = [...comps].map(c => c.ppg).sort((a, b) => a - b);
    const m = Math.floor(sorted.length / 2);
    return sorted.length % 2 === 0 ? (sorted[m - 1] + sorted[m]) / 2 : sorted[m];
  })();

  return (
    <div>
      {/* Summary tiles */}
      <div style={{
        display: "grid", gridTemplateColumns: "repeat(3, 1fr)",
        gap: 12, marginBottom: 16,
      }}>
        <div style={{ ...CARD, padding: "14px 16px" }}>
          <div style={{
            fontSize: 10, fontWeight: 500, color: C.muted,
            textTransform: "uppercase", letterSpacing: "0.08em", ...BODY,
          }}>Comp Hit Rate</div>
          <div style={{
            fontSize: 24, fontWeight: 700, color: hitRate >= 0.6 ? C.teal : C.gold,
            ...MONO, lineHeight: 1.2, marginTop: 4,
          }}>{(hitRate * 100).toFixed(0)}%</div>
        </div>
        <div style={{ ...CARD, padding: "14px 16px" }}>
          <div style={{
            fontSize: 10, fontWeight: 500, color: C.muted,
            textTransform: "uppercase", letterSpacing: "0.08em", ...BODY,
          }}>Mean Comp PPG</div>
          <div style={{
            fontSize: 24, fontWeight: 700, color: C.text,
            ...MONO, lineHeight: 1.2, marginTop: 4,
          }}>{meanPpg.toFixed(1)}</div>
        </div>
        <div style={{ ...CARD, padding: "14px 16px" }}>
          <div style={{
            fontSize: 10, fontWeight: 500, color: C.muted,
            textTransform: "uppercase", letterSpacing: "0.08em", ...BODY,
          }}>Median Comp PPG</div>
          <div style={{
            fontSize: 24, fontWeight: 700, color: C.text,
            ...MONO, lineHeight: 1.2, marginTop: 4,
          }}>{medianPpg.toFixed(1)}</div>
        </div>
      </div>

      {/* Comp rows */}
      <div style={{ ...CARD, padding: "8px 18px" }}>
        {comps.map((c, i) => {
          const outcome = !c.madeIt
            ? "BUST"
            : c.rawPpg != null
              ? `${c.rawPpg.toFixed(1)} ppg`
              : `${c.ppg.toFixed(1)} ppg`;
          const outcomeColor = !c.madeIt
            ? C.coral
            : c.ppg >= 10 ? C.teal
            : c.ppg >= 6 ? C.gold
            : C.coral;
          const simPct = Math.round(c.similarity * 100);
          const simBarW = Math.min(100, Math.round((simPct / 30) * 100));
          return (
            <div key={c.name + c.year} style={{
              display: "flex", alignItems: "center", gap: 12,
              padding: "10px 0",
              borderBottom: i < comps.length - 1 ? `1px solid ${C.border}` : "none",
            }}>
              <span style={{
                fontFamily: "var(--font-mono)", fontSize: 11,
                color: C.muted, width: 22, textAlign: "right", flexShrink: 0,
              }}>#{c.rank}</span>
              <div style={{ flex: 1, minWidth: 0 }}>
                <div style={{
                  fontFamily: "var(--font-body)", fontSize: 14, fontWeight: 500,
                  color: C.text, whiteSpace: "nowrap", overflow: "hidden",
                  textOverflow: "ellipsis",
                }}>{c.name}</div>
                <div style={{
                  fontSize: 11, color: C.muted, marginTop: 2,
                  fontFamily: "var(--font-mono)",
                }}>
                  {c.college} · {c.year} · Rd {c.round} #{c.pick}
                </div>
              </div>
              <div style={{
                display: "flex", flexDirection: "column",
                alignItems: "flex-end", gap: 4, flexShrink: 0, minWidth: 110,
              }}>
                <span style={{
                  fontFamily: "var(--font-mono)", fontSize: 13,
                  fontWeight: 600, color: outcomeColor,
                }}>{outcome}</span>
                <div style={{ display: "flex", alignItems: "center", gap: 6 }}>
                  <div style={{
                    width: 72, height: 4, background: "rgba(255,255,255,0.07)",
                    borderRadius: 2, overflow: "hidden",
                  }}>
                    <div style={{
                      height: "100%", width: `${simBarW}%`,
                      background: C.blue, borderRadius: 2,
                    }} />
                  </div>
                  <span style={{
                    fontFamily: "var(--font-mono)", fontSize: 11, color: C.muted,
                  }}>{simPct}%</span>
                </div>
              </div>
            </div>
          );
        })}
      </div>
    </div>
  );
}

function HeaderCard({ p }: { p: PlayerJson }) {
  const fmtN = (x: number | null, d = 2) => x == null ? "—" : x.toFixed(d);
  const fmtI = (x: number | null) => x == null ? "—" : x.toLocaleString();
  const { p_made_it, exp_ppg, prospect_score, actual_ppg, made_it, bucket } = p.summary;
  const actColor = actual_ppg == null
    ? C.muted
    : (exp_ppg != null && actual_ppg >= exp_ppg ? C.teal : C.coral);
  const madeColor = made_it == null ? C.muted : (made_it === 1 ? C.teal : C.coral);

  const pill = (label: string, value: string, color: string) => (
    <div style={{
      background: `${color}22`, color, padding: "8px 14px",
      borderRadius: 8, fontFamily: "var(--font-mono)", fontWeight: 700,
      fontSize: 15, display: "flex", flexDirection: "column", gap: 2,
      minWidth: 90,
    }}>
      <div style={{
        fontSize: 10, color: C.muted, fontWeight: 500,
        letterSpacing: "0.1em", textTransform: "uppercase",
      }}>{label}</div>
      <div>{value}</div>
    </div>
  );

  return (
    <div style={{ ...CARD, padding: "20px 24px", marginBottom: 16 }}>
      <div style={{
        display: "flex", justifyContent: "space-between",
        alignItems: "center", gap: 20, flexWrap: "wrap",
      }}>
        <div style={{ display: "flex", alignItems: "center", gap: 14, minWidth: 0 }}>
          <PlayerAvatar url={p.headshot_url} name={p.name} size="lg" />
          <div style={{ minWidth: 0 }}>
            <h2 style={{
              fontFamily: "var(--font-display)", fontSize: 24, fontWeight: 700,
              color: C.text, margin: "0 0 4px", letterSpacing: "-0.02em",
            }}>{p.name}</h2>
            <div style={{ fontSize: 13, color: C.muted, ...BODY }}>
              {p.position} · {p.college ?? "?"} · {p.draft_year} ·
              {" "}Rd {fmtI(p.round)} #{fmtI(p.pick)} · {p.tier ?? "?"}
            </div>
          </div>
        </div>
        <div style={{ display: "flex", gap: 10, flexWrap: "wrap" }}>
          {prospect_score != null && pill("Score", String(prospect_score), C.teal)}
          {pill("Exp PPG", fmtN(exp_ppg, 2), C.gold)}
          {pill("P(hit)", fmtN(p_made_it, 3), C.blue)}
          {pill("Actual", fmtN(actual_ppg, 2), actColor)}
          {pill("made_it", made_it == null ? "—" : String(made_it), madeColor)}
        </div>
      </div>
      {bucket && <BucketRow bucket={bucket} />}
    </div>
  );
}

function BucketRow({ bucket }: { bucket: BucketSummary }) {
  const BUCKETS = [
    { key: "bust"          as const, label: "Bust",   color: "#4A5578" },
    { key: "bench"         as const, label: "Bench",  color: "#7A8AAB" },
    { key: "flex"          as const, label: "Flex",   color: "#3E8EF7" },
    { key: "elite"         as const, label: "Elite",  color: "#F0B441" },
    { key: "league_winner" as const, label: "LW",     color: "#2DD4A0" },
  ];
  const top = BUCKETS.reduce((a, b) => {
    const av = bucket.means[a.key] ?? 0;
    const bv = bucket.means[b.key] ?? 0;
    return bv > av ? b : a;
  });
  const topMean = bucket.means[top.key] ?? 0;
  const topLo = bucket.lo[top.key];
  const topHi = bucket.hi[top.key];

  return (
    <div style={{
      marginTop: 14, paddingTop: 14,
      borderTop: `1px solid ${C.border}`,
    }}>
      <div style={{
        display: "flex", justifyContent: "space-between", alignItems: "baseline",
        marginBottom: 8, gap: 8, flexWrap: "wrap",
      }}>
        <div style={{
          fontSize: 10, fontWeight: 500, textTransform: "uppercase",
          letterSpacing: "0.08em", color: C.muted,
        }}>Bucket Distribution (80% CI)</div>
        <div style={{ ...MONO, fontSize: 11, color: top.color, fontWeight: 600 }}>
          {top.label} {(topMean * 100).toFixed(0)}%
          {topLo != null && topHi != null && (
            <span style={{ color: C.muted, marginLeft: 4, fontSize: 10 }}>
              [{(topLo * 100).toFixed(0)}–{(topHi * 100).toFixed(0)}%]
            </span>
          )}
        </div>
      </div>
      <div style={{
        display: "flex", width: "100%", height: 10, borderRadius: 5,
        overflow: "hidden", background: "rgba(255,255,255,0.04)",
      }}>
        {BUCKETS.map(b => {
          const m = bucket.means[b.key] ?? 0;
          const lo = bucket.lo[b.key];
          const hi = bucket.hi[b.key];
          const tip = lo != null && hi != null
            ? `${b.label}: ${(m * 100).toFixed(1)}% (80% CI ${(lo * 100).toFixed(0)}–${(hi * 100).toFixed(0)}%)`
            : `${b.label}: ${(m * 100).toFixed(1)}%`;
          return (
            <div key={b.key} title={tip}
              style={{ width: `${m * 100}%`, background: b.color, transition: "width 0.2s" }} />
          );
        })}
      </div>
      <div style={{
        display: "grid", gridTemplateColumns: "repeat(5, 1fr)",
        gap: 4, marginTop: 8, ...MONO, fontSize: 10,
      }}>
        {BUCKETS.map(b => {
          const m = bucket.means[b.key] ?? 0;
          const lo = bucket.lo[b.key];
          const hi = bucket.hi[b.key];
          return (
            <div key={b.key} style={{ display: "flex", flexDirection: "column", alignItems: "center", gap: 2 }}>
              <div style={{ display: "flex", alignItems: "center", gap: 4, color: C.muted }}>
                <span style={{ width: 6, height: 6, borderRadius: 1, background: b.color, display: "inline-block" }} />
                <span style={{ fontSize: 9, textTransform: "uppercase", letterSpacing: "0.05em" }}>{b.label}</span>
              </div>
              <span style={{ color: C.sub, fontWeight: 600 }}>{(m * 100).toFixed(0)}%</span>
              {lo != null && hi != null && (
                <span style={{ color: C.muted, fontSize: 9 }}>
                  {(lo * 100).toFixed(0)}–{(hi * 100).toFixed(0)}
                </span>
              )}
            </div>
          );
        })}
      </div>
    </div>
  );
}

// ── Player picker (left sidebar) ─────────────────────────────────────────────
type PickerSort = "exp_ppg" | "actual_ppg";

function PlayerPicker({
  players, posFilter, yearFilter, query, onQuery, onPosFilter, onYearFilter,
  selectedId, onSelect, availableYears,
}: {
  players: IndexPlayer[];
  posFilter: string; yearFilter: number | "ALL";
  query: string;
  onQuery: (q: string) => void;
  onPosFilter: (p: string) => void;
  onYearFilter: (y: number | "ALL") => void;
  selectedId: string | null;
  onSelect: (id: string) => void;
  availableYears: number[];
}) {
  const [sortBy, setSortBy] = useState<PickerSort>("exp_ppg");
  const [sortDir, setSortDir] = useState<"asc" | "desc">("desc");

  function handleSort(field: PickerSort) {
    if (sortBy === field) setSortDir(d => d === "desc" ? "asc" : "desc");
    else { setSortBy(field); setSortDir("desc"); }
  }

  const filtered = useMemo(() => {
    const q = query.trim().toLowerCase();
    const base = players.filter(p => {
      if (posFilter !== "ALL" && p.position !== posFilter) return false;
      if (yearFilter !== "ALL" && p.draft_year !== yearFilter) return false;
      if (q && !p.name.toLowerCase().includes(q) &&
          !(p.college ?? "").toLowerCase().includes(q)) return false;
      return true;
    });
    return [...base].sort((a, b) => {
      const av = a[sortBy] ?? -Infinity;
      const bv = b[sortBy] ?? -Infinity;
      return sortDir === "desc" ? bv - av : av - bv;
    });
  }, [players, posFilter, yearFilter, query, sortBy, sortDir]);

  return (
    <div className="inspector-picker" style={{
      ...CARD, padding: 0, display: "flex", flexDirection: "column",
    }}>
      <div style={{ padding: 14, borderBottom: `1px solid ${C.border}` }}>
        <input
          type="text"
          placeholder="Search name or college…"
          value={query}
          onChange={e => onQuery(e.target.value)}
          style={{
            width: "100%", background: "rgba(255,255,255,0.04)",
            border: `1px solid ${C.border}`, borderRadius: 6,
            padding: "6px 10px", fontSize: 12, color: C.text,
            fontFamily: "var(--font-body)", outline: "none",
            marginBottom: 8,
          }}
        />
        <div style={{ display: "flex", gap: 6, marginBottom: 6 }}>
          {["ALL", "WR", "RB"].map(p => (
            <button
              key={p}
              onClick={() => onPosFilter(p)}
              style={{
                flex: 1, padding: "5px 0", fontSize: 11, fontWeight: 600,
                fontFamily: "var(--font-mono)", border: "none", cursor: "pointer",
                borderRadius: 4,
                background: posFilter === p ? C.blue : "rgba(255,255,255,0.04)",
                color: posFilter === p ? "#fff" : C.muted,
              }}
            >{p}</button>
          ))}
        </div>
        <select
          value={yearFilter}
          onChange={e => onYearFilter(e.target.value === "ALL" ? "ALL" : Number(e.target.value))}
          style={{
            width: "100%", background: "rgba(255,255,255,0.04)",
            border: `1px solid ${C.border}`, borderRadius: 6,
            padding: "5px 10px", fontSize: 12, color: C.sub,
            fontFamily: "var(--font-body)", outline: "none",
          }}
        >
          <option value="ALL" style={{ background: C.cardSolid }}>All draft classes</option>
          {availableYears.map(y => (
            <option key={y} value={y} style={{ background: C.cardSolid }}>
              {y} Draft
            </option>
          ))}
        </select>
      </div>
      {/* Sort header */}
      <div style={{
        display: "flex", justifyContent: "flex-end", gap: 2,
        padding: "4px 10px 4px 14px",
        borderBottom: `1px solid ${C.border}`,
        background: "rgba(255,255,255,0.01)",
      }}>
        {(["exp_ppg", "actual_ppg"] as PickerSort[]).map(field => {
          const label = field === "exp_ppg" ? "Exp" : "Act";
          const active = sortBy === field;
          return (
            <button
              key={field}
              onClick={() => handleSort(field)}
              style={{
                background: "none", border: "none", cursor: "pointer",
                fontSize: 10, fontWeight: 600, letterSpacing: "0.06em",
                textTransform: "uppercase", fontFamily: "var(--font-mono)",
                color: active ? C.sub : C.muted,
                padding: "2px 8px", borderRadius: 3,
                display: "flex", alignItems: "center", gap: 3,
              }}
            >
              {label}
              <span style={{ color: active ? C.blue : "#2E3650", fontSize: 10 }}>
                {active ? (sortDir === "desc" ? "↓" : "↑") : "↕"}
              </span>
            </button>
          );
        })}
      </div>
      <div style={{ overflowY: "auto", flex: 1 }}>
        {filtered.length === 0 ? (
          <div style={{ padding: 14, fontSize: 12, color: C.muted, ...BODY }}>
            No players match.
          </div>
        ) : filtered.map(p => {
          const sel = p.id === selectedId;
          return (
            <button
              key={p.id}
              onClick={() => onSelect(p.id)}
              style={{
                display: "flex", width: "100%", textAlign: "left",
                alignItems: "center", gap: 10,
                padding: "8px 14px", border: "none", cursor: "pointer",
                background: sel ? "rgba(62,142,247,0.12)" : "transparent",
                borderLeft: `3px solid ${sel ? C.blue : "transparent"}`,
                borderBottom: `1px solid ${C.border}`,
                transition: "background 0.1s",
              }}
              onMouseEnter={e => { if (!sel) (e.currentTarget as HTMLButtonElement).style.background = "rgba(255,255,255,0.03)"; }}
              onMouseLeave={e => { if (!sel) (e.currentTarget as HTMLButtonElement).style.background = "transparent"; }}
            >
              <PlayerAvatar url={p.headshot_url} name={p.name} size="sm" />
              <div style={{ minWidth: 0, flex: 1 }}>
                <div style={{
                  fontSize: 13, fontWeight: 600, color: sel ? C.text : C.sub,
                  ...BODY, display: "flex", alignItems: "center", gap: 6,
                  whiteSpace: "nowrap", overflow: "hidden", textOverflow: "ellipsis",
                }}>
                  <span style={{
                    display: "inline-block", width: 6, height: 6, borderRadius: "50%",
                    background: p.position === "WR" ? C.blue : C.gold, flexShrink: 0,
                  }} />
                  <span style={{ overflow: "hidden", textOverflow: "ellipsis" }}>{p.name}</span>
                </div>
                <div style={{
                  fontSize: 11, color: C.muted, marginTop: 2,
                  fontFamily: "var(--font-mono)",
                  display: "flex", justifyContent: "space-between", gap: 6,
                }}>
                  <span>{p.position} · {p.draft_year} · Rd {p.round ?? "?"} #{p.pick ?? "?"}</span>
                  <span style={{ display: "flex", gap: 8, flexShrink: 0 }}>
                    {p.exp_ppg != null && (
                      <span style={{
                        color: sortBy === "exp_ppg" ? C.gold : C.muted,
                        fontWeight: sortBy === "exp_ppg" ? 700 : 400,
                      }}>{p.exp_ppg.toFixed(1)}</span>
                    )}
                    {p.actual_ppg != null ? (
                      <span style={{
                        color: sortBy === "actual_ppg" ? C.teal : C.sub,
                        fontWeight: sortBy === "actual_ppg" ? 700 : 400,
                      }}>{p.actual_ppg.toFixed(1)}</span>
                    ) : (
                      <span style={{ color: "#2E3650" }}>—</span>
                    )}
                  </span>
                </div>
              </div>
            </button>
          );
        })}
      </div>
    </div>
  );
}

// ── Main page ────────────────────────────────────────────────────────────────
export function InspectorPage() {
  const [index, setIndex] = useState<IndexJson | null>(null);
  const [player, setPlayer] = useState<PlayerJson | null>(null);
  const [selectedId, setSelectedId] = useState<string | null>(null);
  const [tab, setTab] = useState<"percentile" | "production" | "combine" | "comps" | "raw">("percentile");
  const [posFilter, setPosFilter] = useState("ALL");
  const [yearFilter, setYearFilter] = useState<number | "ALL">("ALL");
  const [query, setQuery] = useState("");
  const [comps, setComps] = useState<Record<string, ProspectComp[] | null>>({});
  const loadingComps = useRef(new Set<string>());

  // Load index once
  useEffect(() => {
    fetch(dataUrl("/data/inspector/index.json"))
      .then(r => r.json())
      .then((d: IndexJson) => {
        setIndex(d);
        // Default to top of list (highest exp_ppg, since index is sorted desc)
        if (d.players.length > 0) setSelectedId(d.players[0].id);
      });
  }, []);

  // Load player on selection change
  useEffect(() => {
    if (!selectedId) return;
    let cancelled = false;
    fetch(dataUrl(`/data/inspector/players/${selectedId}.json`))
      .then(r => r.json())
      .then((p: PlayerJson) => {
        if (!cancelled) setPlayer(p);
      });
    return () => { cancelled = true; };
  }, [selectedId]);

  // Lazy-load comps when the Comps tab is opened (cache per id)
  useEffect(() => {
    if (tab !== "comps" || !selectedId) return;
    if (comps[selectedId] !== undefined) return; // already loading or loaded
    if (loadingComps.current.has(selectedId)) return;
    loadingComps.current.add(selectedId);
    fetch(dataUrl(`/data/comps/${selectedId}.json`))
      .then(r => r.ok ? r.json() : null)
      .then((d: { prospectId: string; comps: ProspectComp[] } | null) => {
        loadingComps.current.delete(selectedId);
        setComps(prev => ({ ...prev, [selectedId]: d?.comps ?? [] }));
      })
      .catch(() => {
        loadingComps.current.delete(selectedId);
        setComps(prev => ({ ...prev, [selectedId]: [] }));
      });
  }, [tab, selectedId, comps]);

  const availableYears = useMemo(() => {
    if (!index) return [];
    return Array.from(new Set(index.players.map(p => p.draft_year))).sort((a, b) => b - a);
  }, [index]);

  if (!index) {
    return (
      <div style={{
        display: "flex", alignItems: "center", justifyContent: "center", padding: "80px 0",
      }}>
        <div style={{ color: C.muted, fontFamily: "var(--font-mono)", fontSize: 13 }}>Loading…</div>
      </div>
    );
  }

  const tabs: Array<{ k: typeof tab; label: string }> = [
    { k: "percentile", label: "Percentile strip" },
    { k: "production", label: "Production" },
    { k: "combine", label: "Combine" },
    { k: "comps", label: "Comps" },
    { k: "raw", label: "Raw data" },
  ];

  return (
    <div className="inspector-page" style={{ maxWidth: 1400, margin: "0 auto", padding: "32px 24px 64px" }}>
      <div style={{ marginBottom: 24 }}>
        <h2 style={{
          fontFamily: "var(--font-display)", fontSize: 22, fontWeight: 700,
          color: C.text, marginBottom: 6,
        }}>Prospect Inspector</h2>
        <p style={{ fontSize: 13, color: C.muted, ...BODY, maxWidth: 720, lineHeight: 1.6 }}>
          Per-player feature drill-down for every WR and RB drafted{" "}
          {index.minDraftYear}–{index.maxDraftYear}. Percentiles are computed against
          the same-position training cohort (drafted players with cfbfastR data).
        </p>
      </div>

      <div className="inspector-layout">
        <PlayerPicker
          players={index.players}
          posFilter={posFilter}
          yearFilter={yearFilter}
          query={query}
          onQuery={setQuery}
          onPosFilter={setPosFilter}
          onYearFilter={setYearFilter}
          selectedId={selectedId}
          onSelect={setSelectedId}
          availableYears={availableYears}
        />

        <div className="inspector-detail">
          {!player || player.id !== selectedId ? (
            <div style={{ ...CARD, padding: 40, textAlign: "center" }}>
              <div style={{ color: C.muted, fontFamily: "var(--font-mono)", fontSize: 13 }}>
                {selectedId ? "Loading player…" : "Select a player from the list."}
              </div>
            </div>
          ) : (
            <>
              <HeaderCard p={player} />

              {/* Tab strip */}
              <div className="inspector-tabs" style={{
                display: "flex", gap: 4, marginBottom: 16,
                borderBottom: `1px solid ${C.border}`,
              }}>
                {tabs.map(t => (
                  <button
                    key={t.k}
                    onClick={() => setTab(t.k)}
                    style={{
                      padding: "8px 14px", border: "none", cursor: "pointer",
                      background: "transparent",
                      color: tab === t.k ? C.text : C.muted,
                      borderBottom: `2px solid ${tab === t.k ? C.blue : "transparent"}`,
                      fontSize: 13, fontWeight: 500, fontFamily: "var(--font-body)",
                      marginBottom: -1, transition: "color 0.15s",
                    }}
                  >{t.label}</button>
                ))}
              </div>

              {tab === "percentile" && (
                <div style={{ ...CARD, padding: "16px 24px" }}>
                  {player.groups.map(g => <GroupBlock key={g.label} group={g} />)}
                  <div style={{
                    display: "flex", gap: 14, marginTop: 12, flexWrap: "wrap",
                    fontSize: 11, color: C.muted, ...BODY,
                  }}>
                    <span><span style={{ color: C.teal }}>■</span> ≥80</span>
                    <span><span style={{ color: C.blue }}>■</span> 60–80</span>
                    <span><span style={{ color: C.gold }}>■</span> 40–60</span>
                    <span><span style={{ color: C.coral }}>■</span> &lt;40</span>
                  </div>
                </div>
              )}

              {tab === "production" && (
                <div>
                  <p style={{ fontSize: 12, color: C.muted, marginBottom: 12, ...BODY }}>
                    Season-by-season production (chronological). Ante = 2 years before draft ·
                    Penult = year before draft · Final = best season.
                  </p>
                  <ProductionChart data={player.production} />
                </div>
              )}

              {tab === "combine" && (
                <div>
                  <p style={{ fontSize: 12, color: C.muted, marginBottom: 12, ...BODY }}>
                    Combine measurables vs drafted {player.position} cohort. Percentile
                    bar shows where this player ranks in the training distribution.
                  </p>
                  <CombineGrid data={player.combine} />
                </div>
              )}

              {tab === "comps" && (
                <div>
                  <p style={{ fontSize: 12, color: C.muted, marginBottom: 12, ...BODY }}>
                    The 10 most similar historical prospects by Euclidean distance across
                    standardized features. Outcome shows raw NFL PPG (or BUST) — bar shows
                    similarity strength relative to the typical match.
                  </p>
                  <CompsList comps={comps[selectedId!] ?? null} />
                </div>
              )}

              {tab === "raw" && (
                <div>
                  <p style={{ fontSize: 12, color: C.muted, marginBottom: 12, ...BODY }}>
                    All {player.raw.length} feature columns the model sees. Position-irrelevant
                    fields are filtered out.
                  </p>
                  <RawTable data={player.raw} />
                </div>
              )}
            </>
          )}
        </div>
      </div>
    </div>
  );
}

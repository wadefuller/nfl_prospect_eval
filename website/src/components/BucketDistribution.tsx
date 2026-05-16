import type { Prospect } from "../types";

// Stacked horizontal bar showing the ensemble's posterior-mean distribution
// over outcome buckets, with 80% credible intervals from the Bayesian
// stan_polr posterior propagated through the geom-mean ensemble.
//
// Buckets ordered worst → best so the bar reads left-to-right as a journey:
//   bust → bench → flex → elite → league_winner

const BUCKET_ORDER = [
  { key: "p_bust",          label: "Bust",   longLabel: "Bust",          color: "#4A5578" },
  { key: "p_bench",         label: "Bench",  longLabel: "Bench",         color: "#7A8AAB" },
  { key: "p_flex",          label: "Flex",   longLabel: "Flex",          color: "#3E8EF7" },
  { key: "p_elite",         label: "Elite",  longLabel: "Elite",         color: "#F0B441" },
  { key: "p_league_winner", label: "LW",     longLabel: "League Winner", color: "#2DD4A0" },
] as const;

interface Props { prospect: Prospect }

export function BucketDistribution({ prospect: p }: Props) {
  if (p.p_bust == null || p.p_bench == null || p.p_flex == null ||
      p.p_elite == null || p.p_league_winner == null) {
    return null;
  }
  const probs = BUCKET_ORDER.map((b) => {
    const mean = (p[b.key] ?? 0) as number;
    const lo   = (p[`${b.key}_lo` as keyof Prospect] ?? null) as number | null;
    const hi   = (p[`${b.key}_hi` as keyof Prospect] ?? null) as number | null;
    return { ...b, mean, lo, hi };
  });
  const top = probs.reduce((a, b) => (b.mean > a.mean ? b : a));
  const hasCI = top.lo != null && top.hi != null;

  return (
    <div>
      <div
        style={{
          fontSize: 10,
          fontWeight: 500,
          textTransform: "uppercase",
          letterSpacing: "0.08em",
          color: "#4A5578",
          marginBottom: 8,
          display: "flex",
          alignItems: "baseline",
          justifyContent: "space-between",
          gap: 8,
        }}
      >
        <span>Outcome Distribution</span>
        <span style={{ fontFamily: "var(--font-mono)", color: top.color, textTransform: "none", letterSpacing: "normal" }}>
          {top.longLabel} {(top.mean * 100).toFixed(0)}%
          {hasCI && (
            <span style={{ color: "#4A5578", marginLeft: 4, fontSize: 9 }}>
              [{(top.lo! * 100).toFixed(0)}–{(top.hi! * 100).toFixed(0)}%]
            </span>
          )}
        </span>
      </div>

      {/* Stacked bar */}
      <div
        style={{
          display: "flex",
          width: "100%",
          height: 8,
          borderRadius: 4,
          overflow: "hidden",
          background: "rgba(255,255,255,0.04)",
        }}
      >
        {probs.map((b) => {
          const tooltip = b.lo != null && b.hi != null
            ? `${b.longLabel}: ${(b.mean * 100).toFixed(1)}% (80% CI ${(b.lo * 100).toFixed(0)}–${(b.hi * 100).toFixed(0)}%)`
            : `${b.longLabel}: ${(b.mean * 100).toFixed(1)}%`;
          return (
            <div
              key={b.key}
              title={tooltip}
              style={{
                width: `${b.mean * 100}%`,
                background: b.color,
                transition: "width 0.2s",
              }}
            />
          );
        })}
      </div>

      {/* Legend with mean ± CI */}
      <div
        style={{
          display: "grid",
          gridTemplateColumns: "repeat(5, 1fr)",
          gap: 4,
          marginTop: 8,
          fontFamily: "var(--font-mono)",
          fontSize: 10,
        }}
      >
        {probs.map((b) => (
          <div key={b.key} style={{ display: "flex", flexDirection: "column", alignItems: "center", gap: 2 }}>
            <div style={{ display: "flex", alignItems: "center", gap: 4, color: "#4A5578" }}>
              <span
                style={{
                  width: 6,
                  height: 6,
                  borderRadius: 1,
                  background: b.color,
                  display: "inline-block",
                }}
              />
              <span style={{ fontSize: 9, textTransform: "uppercase", letterSpacing: "0.05em" }}>{b.label}</span>
            </div>
            <span style={{ color: "#8A9AC0", fontWeight: 600 }}>
              {(b.mean * 100).toFixed(0)}%
            </span>
            {b.lo != null && b.hi != null && (
              <span style={{ color: "#4A5578", fontSize: 9 }}>
                {(b.lo * 100).toFixed(0)}–{(b.hi * 100).toFixed(0)}
              </span>
            )}
          </div>
        ))}
      </div>
    </div>
  );
}

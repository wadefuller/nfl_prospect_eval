import type { Prospect } from "../types";
import { ink, scale, surface, font, NUM } from "../theme";

// Stacked horizontal bar showing the ensemble's posterior-mean distribution
// over outcome buckets, with 80% credible intervals from the Bayesian
// stan_polr posterior propagated through the geom-mean ensemble.
//
// Buckets ordered worst → best so the bar reads left-to-right as a journey:
//   bust → bench → flex → elite → league_winner
//
// Colour: these tiers carry polarity (a bust is a loss, a league winner is a
// big win, flex is neutral), so the ramp is diverging — two warm steps, a
// neutral midpoint, two cool. The previous order was gray/gray/blue/amber/green,
// a non-monotone rainbow whose bench↔flex pair measured ΔE 12.3 against normal
// vision, i.e. hard to separate even for full-colour readers. The scale in
// theme.ts is validated; see the note there.

const BUCKET_ORDER = [
  { key: "p_bust",          label: "Bust",   longLabel: "Bust",          color: scale.worst },
  { key: "p_bench",         label: "Bench",  longLabel: "Bench",         color: scale.poor },
  { key: "p_flex",          label: "Flex",   longLabel: "Flex",          color: scale.mid },
  { key: "p_elite",         label: "Elite",  longLabel: "Elite",         color: scale.good },
  { key: "p_league_winner", label: "LW",     longLabel: "League Winner", color: scale.best },
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
          color: ink.muted,
          marginBottom: 9,
          display: "flex",
          alignItems: "baseline",
          justifyContent: "space-between",
          gap: 8,
        }}
      >
        <span>Outcome Distribution</span>
        <span style={{ ...NUM, color: top.color, textTransform: "none", letterSpacing: "normal" }}>
          {top.longLabel} {(top.mean * 100).toFixed(0)}%
          {hasCI && (
            <span style={{ color: ink.faint, marginLeft: 4, fontSize: 9 }}>
              [{(top.lo! * 100).toFixed(0)}–{(top.hi! * 100).toFixed(0)}%]
            </span>
          )}
        </span>
      </div>

      {/* Stacked bar. Segments are separated by a 2px surface-coloured rule
          (drawn as a right border so percentage widths still sum to 100%),
          which is the mark spec's spacer and also the secondary encoding that
          keeps adjacent segments readable without relying on hue alone. */}
      <div
        style={{
          display: "flex",
          width: "100%",
          height: 10,
          borderRadius: 5,
          overflow: "hidden",
          background: "rgba(255,255,255,0.05)",
        }}
      >
        {probs.map((b, i) => {
          const tooltip = b.lo != null && b.hi != null
            ? `${b.longLabel}: ${(b.mean * 100).toFixed(1)}% (80% CI ${(b.lo * 100).toFixed(0)}–${(b.hi * 100).toFixed(0)}%)`
            : `${b.longLabel}: ${(b.mean * 100).toFixed(1)}%`;
          const isLast = i === probs.length - 1;
          return (
            <div
              key={b.key}
              title={tooltip}
              style={{
                width: `${b.mean * 100}%`,
                background: b.color,
                borderRight: !isLast && b.mean > 0 ? `2px solid ${surface.card}` : undefined,
                boxSizing: "border-box",
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
          fontFamily: font.mono,
          fontVariantNumeric: "tabular-nums",
          fontSize: 10,
        }}
      >
        {probs.map((b) => (
          <div key={b.key} style={{ display: "flex", flexDirection: "column", alignItems: "center", gap: 2 }}>
            <div style={{ display: "flex", alignItems: "center", gap: 4, color: ink.muted }}>
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
            <span style={{ color: ink.secondary, fontWeight: 700 }}>
              {(b.mean * 100).toFixed(0)}%
            </span>
            {b.lo != null && b.hi != null && (
              <span style={{ color: ink.faint, fontSize: 9 }}>
                {(b.lo * 100).toFixed(0)}–{(b.hi * 100).toFixed(0)}
              </span>
            )}
          </div>
        ))}
      </div>
    </div>
  );
}

import type { Prospect } from "../types";
import { ink, line, surface, posColor, NUM, qualityColor } from "../theme";
import { StatTile } from "./ui";

/**
 * "Class at a glance" strip above the board.
 *
 * The Prospects route previously opened straight onto a bare table with no
 * framing — no headline, no sense of the class's shape. This gives the page a
 * top-level read before the row-by-row detail.
 */
export function ClassSummary({
  prospects, allClasses, year,
}: { prospects: Prospect[]; allClasses: boolean; year: number }) {
  if (prospects.length === 0) return null;

  const ppgs = prospects.map((p) => p.exp_ppg).filter(Number.isFinite).sort((a, b) => a - b);
  const medPpg = ppgs.length ? ppgs[Math.floor(ppgs.length / 2)] : null;

  // Count of prospects the model projects as genuine starters.
  const starters = prospects.filter((p) => (p.value_score ?? 0) >= 75).length;

  const order = ["QB", "RB", "WR", "TE"];
  const counts = order
    .map((pos) => ({ pos, n: prospects.filter((p) => p.position === pos).length }))
    .filter((d) => d.n > 0);
  const total = counts.reduce((s, d) => s + d.n, 0);

  return (
    <div
      className="class-summary"
      style={{
        display: "grid",
        // Fixed 4-up: three stat tiles + the position mix. auto-fit is wrong
        // here because the position-mix span kept empty tracks alive, which
        // stopped the tiles stretching to fill the row.
        gridTemplateColumns: "repeat(4, minmax(0, 1fr))",
        gap: 10,
        marginBottom: 16,
      }}
    >
      <StatTile
        label={allClasses ? "Prospects · all classes" : `${year} class`}
        value={prospects.length}
        sub={allClasses ? "across every draft class" : "graded prospects"}
        tone={ink.faint}
      />

      <StatTile
        label="Projected starters"
        value={starters}
        sub="value score 75+"
        tone={starters > 0 ? qualityColor(80) : ink.faint}
      />

      <StatTile
        label="Median projection"
        value={medPpg != null ? medPpg.toFixed(1) : "—"}
        sub="half-PPR points per game"
        tone={ink.faint}
      />

      {/* Position mix — a labelled stacked bar, not colour alone. */}
      <div
        style={{
          background: surface.card,
          border: `1px solid ${line.hairline}`,
          borderRadius: 10,
          padding: "12px 14px",
          display: "flex",
          flexDirection: "column",
          gap: 8,
          minWidth: 0,
        }}
      >
        <div
          style={{
            fontSize: 9.5,
            textTransform: "uppercase",
            letterSpacing: "0.09em",
            color: ink.muted,
            fontWeight: 600,
          }}
        >
          Position mix
        </div>
        <div style={{ display: "flex", height: 8, borderRadius: 4, overflow: "hidden", background: "rgba(255,255,255,0.05)" }}>
          {counts.map((d, i) => (
            <div
              key={d.pos}
              title={`${d.pos}: ${d.n}`}
              style={{
                width: `${(d.n / total) * 100}%`,
                background: posColor[d.pos] ?? ink.muted,
                borderRight: i < counts.length - 1 ? `2px solid ${surface.card}` : undefined,
                boxSizing: "border-box",
              }}
            />
          ))}
        </div>
        <div style={{ display: "flex", flexWrap: "wrap", gap: "3px 10px" }}>
          {counts.map((d) => (
            <span key={d.pos} style={{ display: "inline-flex", alignItems: "center", gap: 4, fontSize: 10.5 }}>
              <span
                style={{
                  width: 6, height: 6, borderRadius: 1.5,
                  background: posColor[d.pos] ?? ink.muted, display: "inline-block",
                }}
              />
              <span style={{ color: ink.muted }}>{d.pos}</span>
              <span style={{ ...NUM, color: ink.secondary, fontWeight: 700 }}>{d.n}</span>
            </span>
          ))}
        </div>
      </div>
    </div>
  );
}

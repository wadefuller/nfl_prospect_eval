import type { Prospect } from "../types";
import { PlayerAvatar } from "./PlayerAvatar";
import { scoreColor } from "./prospectStyle";

interface Props {
  prospect: Prospect;
  expanded: boolean;
  onClick: () => void;
  hasActuals: boolean;
  allClasses: boolean;
}

// Mobile-only compact stat pill. Visible at < sm breakpoint so the row
// communicates the model's call before the user has to expand the row.
function MobileStatPill({
  label,
  value,
  color = "#F0F4FF",
  bg = "rgba(255,255,255,0.05)",
}: {
  label: string;
  value: string;
  color?: string;
  bg?: string;
}) {
  return (
    <div
      style={{
        background: bg,
        borderRadius: 5,
        padding: "3px 7px",
        display: "flex",
        flexDirection: "column",
        alignItems: "center",
        lineHeight: 1.05,
        minWidth: 38,
      }}
    >
      <span
        style={{
          fontSize: 8,
          color: "#4A5578",
          textTransform: "uppercase",
          letterSpacing: "0.06em",
          fontWeight: 500,
        }}
      >
        {label}
      </span>
      <span
        style={{
          fontFamily: "var(--font-mono)",
          fontSize: 13,
          fontWeight: 700,
          color,
        }}
      >
        {value}
      </span>
    </div>
  );
}

const POS_STYLE: Record<string, { bg: string; color: string }> = {
  WR: { bg: "rgba(245,166,35,0.15)", color: "#FFBF4D" },
  RB: { bg: "rgba(45,212,160,0.15)", color: "#5EEBC0" },
};

export function PosBadge({ pos }: { pos: string }) {
  const s = POS_STYLE[pos] ?? { bg: "rgba(138,154,192,0.15)", color: "#8A9AC0" };
  return (
    <span
      style={{
        background: s.bg,
        color: s.color,
        borderRadius: 4,
        padding: "2px 7px",
        fontSize: 11,
        fontWeight: 600,
        fontFamily: "var(--font-mono)",
        whiteSpace: "nowrap",
      }}
    >
      {pos}
    </span>
  );
}

function ResultCell({ prospect: p }: { prospect: Prospect }) {
  if (p.actual_ppg != null) {
    const displayPpg = p.actual_raw_ppg ?? p.actual_ppg;
    // Compare against exp_ppg (the model's unconditional prediction) so the
    // green/red outcome reflects true model performance, independent of how
    // the prediction is displayed in the headline column.
    return (
      <span
        style={{
          fontFamily: "var(--font-mono)",
          fontSize: 13,
          fontWeight: 600,
          color: displayPpg >= p.exp_ppg ? "#2DD4A0" : "#F75757",
        }}
      >
        {displayPpg.toFixed(1)}
      </span>
    );
  }
  return <span style={{ color: "#4A5578" }}>—</span>;
}

export function ProspectRow({ prospect: p, expanded, onClick, hasActuals, allClasses }: Props) {
  const sc = p.prospect_score != null ? scoreColor(p.prospect_score) : "#4A5578";
  const expandedBg = "rgba(62,142,247,0.05)";
  const normalBg = "transparent";

  const tdBase: React.CSSProperties = {
    padding: "0 12px",
    height: 44,
    borderBottom: expanded ? "none" : "1px solid rgba(255,255,255,0.05)",
    transition: "background 0.15s",
  };

  // Map prospect_score → color for the mobile score pill.
  const mobileScoreColor = p.prospect_score != null ? sc : "#4A5578";

  function onEnter(e: React.MouseEvent<HTMLTableRowElement>) {
    if (!expanded) {
      Array.from(e.currentTarget.cells).forEach(
        (c) => ((c as HTMLTableCellElement).style.background = "rgba(255,255,255,0.025)")
      );
    }
  }
  function onLeave(e: React.MouseEvent<HTMLTableRowElement>) {
    if (!expanded) {
      Array.from(e.currentTarget.cells).forEach(
        (c) => ((c as HTMLTableCellElement).style.background = normalBg)
      );
    }
  }

  return (
    <tr
      onClick={onClick}
      style={{ cursor: "pointer", background: expanded ? expandedBg : normalBg }}
      onMouseEnter={onEnter}
      onMouseLeave={onLeave}
    >
      {/* Player */}
      <td
        className="sm:!h-[44px] sm:!py-0"
        style={{
          ...tdBase,
          height: "auto",
          padding: "8px 10px 8px 12px",
          borderLeft: expanded ? "2px solid #3E8EF7" : "2px solid transparent",
        }}
      >
        <div style={{ display: "flex", alignItems: "center", gap: 10 }}>
          <PlayerAvatar url={p.headshot_url} name={p.name} size="sm" />

          {/* Identity block — name, college, tier */}
          <div style={{ minWidth: 0, flex: 1 }}>
            <div style={{ display: "flex", alignItems: "center", gap: 7, flexWrap: "wrap" }}>
              <span
                style={{
                  fontFamily: "var(--font-display)",
                  fontSize: 14,
                  fontWeight: 600,
                  color: "#F0F4FF",
                  whiteSpace: "nowrap",
                }}
              >
                {p.name}
              </span>
              {/* Archetype hidden on mobile to free up vertical space */}
              {p.archetype && (
                <span
                  className="hidden sm:inline"
                  style={{
                    background: "rgba(255,255,255,0.06)",
                    color: "#8A9AC0",
                    borderRadius: 3,
                    padding: "1px 6px",
                    fontSize: 10,
                    fontWeight: 500,
                    whiteSpace: "nowrap",
                  }}
                >
                  {p.archetype}
                </span>
              )}
            </div>
            <div style={{ fontSize: 11, color: "#4A5578", marginTop: 1 }}>
              <span style={{ whiteSpace: "nowrap" }}>{p.college}</span>
              {p.tier && (
                <span
                  style={{
                    marginLeft: 6,
                    color:
                      p.tier === "P4"
                        ? "#3E8EF7"
                        : p.tier === "G5"
                        ? "#A78BFA"
                        : "#4A5578",
                  }}
                >
                  {p.tier}
                </span>
              )}
              {/* Mobile: show position + pick + year inline since those columns are hidden */}
              <span className="sm:hidden" style={{ marginLeft: 6, color: "#4A5578" }}>
                · {p.position} · Rd {p.round} #{p.pick}
                {allClasses && ` · ${p.draft_year}`}
              </span>
            </div>
          </div>

          {/* Mobile-only stat strip — horizontally swipeable to reveal more
              metrics. The chevron sits outside the scroll area so it's
              always visible. */}
          <div
            className="sm:hidden"
            style={{ display: "flex", alignItems: "center", gap: 4, flexShrink: 0, minWidth: 0 }}
          >
            <div
              className="mobile-pill-strip"
              style={{
                display: "flex",
                alignItems: "center",
                gap: 6,
                maxWidth: "min(58vw, 220px)",
                overflowX: "auto",
                overflowY: "hidden",
                scrollbarWidth: "none",
                WebkitOverflowScrolling: "touch",
                touchAction: "pan-x",
                // Right-edge fade hints at more content offscreen
                WebkitMaskImage:
                  "linear-gradient(to right, #000 calc(100% - 18px), transparent 100%)",
                maskImage:
                  "linear-gradient(to right, #000 calc(100% - 18px), transparent 100%)",
                paddingRight: 4,
              }}
            >
              {p.prospect_score != null && (
                <MobileStatPill
                  label="SCORE"
                  value={String(p.prospect_score)}
                  color={mobileScoreColor}
                  bg={`${mobileScoreColor}22`}
                />
              )}
              <MobileStatPill
                label="PPG"
                value={p.exp_ppg.toFixed(1)}
                color="#2DD4A0"
              />
              {p.actual_ppg != null &&
                (() => {
                  const display = p.actual_raw_ppg ?? p.actual_ppg;
                  const beat = display >= p.exp_ppg;
                  return (
                    <MobileStatPill
                      label="REAL"
                      value={display.toFixed(1)}
                      color={beat ? "#2DD4A0" : "#F75757"}
                    />
                  );
                })()}
              {p.p_made_it != null && (
                <MobileStatPill
                  label="HIT"
                  value={`${Math.round(p.p_made_it * 100)}%`}
                  color="#8A9AC0"
                />
              )}
              {p.comp_weighted_ppg != null && (
                <MobileStatPill
                  label="COMP"
                  value={p.comp_weighted_ppg.toFixed(1)}
                  color="#A78BFA"
                />
              )}
              {p.comp_bust_rate != null && (
                <MobileStatPill
                  label="BUST"
                  value={`${Math.round(p.comp_bust_rate * 100)}%`}
                  color={p.comp_bust_rate === 0 ? "#2DD4A0" : "#F5A623"}
                />
              )}
              {p.round != null && p.pick != null && (
                <MobileStatPill
                  label="PICK"
                  value={`${p.round}.${p.pick}`}
                  color="#8A9AC0"
                />
              )}
            </div>
            {/* Chevron — communicates that the row is interactive */}
            <span
              style={{
                color: expanded ? "#3E8EF7" : "#4A5578",
                fontSize: 14,
                marginLeft: 2,
                transition: "transform 0.2s",
                transform: expanded ? "rotate(90deg)" : "rotate(0deg)",
                display: "inline-block",
                lineHeight: 1,
                flexShrink: 0,
              }}
              aria-hidden="true"
            >
              ›
            </span>
          </div>
        </div>
      </td>

      {/* Pick */}
      <td className="hidden sm:table-cell" style={tdBase}>
        <span style={{ fontFamily: "var(--font-mono)", fontSize: 12, color: "#8A9AC0" }}>
          Rd {p.round} #{p.pick}
        </span>
      </td>

      {/* Year — all-classes only */}
      {allClasses && (
        <td className="hidden sm:table-cell" style={tdBase}>
          <span style={{ fontFamily: "var(--font-mono)", fontSize: 12, color: "#8A9AC0" }}>
            {p.draft_year}
          </span>
        </td>
      )}

      {/* Pos */}
      <td className="hidden sm:table-cell" style={tdBase}>
        <PosBadge pos={p.position} />
      </td>

      {/* Score */}
      <td className="hidden sm:table-cell" style={tdBase}>
        {p.prospect_score != null ? (
          <span
            style={{
              background: `${sc}22`,
              color: sc,
              borderRadius: 4,
              padding: "2px 10px",
              fontSize: 13,
              fontWeight: 700,
              fontFamily: "var(--font-mono)",
            }}
          >
            {p.prospect_score}
          </span>
        ) : (
          <span style={{ color: "#4A5578" }}>—</span>
        )}
      </td>

      {/* Adj PPG */}
      <td className="hidden sm:table-cell" style={tdBase}>
        <span
          style={{
            fontFamily: "var(--font-mono)",
            fontSize: 14,
            fontWeight: 600,
            color: "#2DD4A0",
          }}
        >
          {p.exp_ppg.toFixed(1)}
        </span>
      </td>

      {/* Comp PPG */}
      <td className="hidden sm:table-cell" style={tdBase}>
        <span style={{ fontFamily: "var(--font-mono)", fontSize: 13, color: "#8A9AC0" }}>
          {p.comp_weighted_ppg != null ? p.comp_weighted_ppg.toFixed(1) : "—"}
        </span>
      </td>

      {/* Last col */}
      <td className="hidden sm:table-cell" style={tdBase}>
        {allClasses ? (
          <ResultCell prospect={p} />
        ) : hasActuals ? (
          p.actual_ppg != null ? (
            (() => {
              const display = p.actual_raw_ppg ?? p.actual_ppg;
              return (
                <span
                  style={{
                    fontFamily: "var(--font-mono)",
                    fontSize: 13,
                    fontWeight: 600,
                    color: display >= p.exp_ppg ? "#2DD4A0" : "#F75757",
                  }}
                >
                  {display.toFixed(1)}
                </span>
              );
            })()
          ) : (
            <span style={{ color: "#4A5578" }}>—</span>
          )
        ) : (
          <span
            style={{
              fontFamily: "var(--font-mono)",
              fontSize: 13,
              color: p.comp_bust_rate === 0 ? "#2DD4A0" : "#8A9AC0",
            }}
          >
            {p.comp_bust_rate != null ? `${(p.comp_bust_rate * 100).toFixed(0)}%` : "—"}
          </span>
        )}
      </td>
    </tr>
  );
}

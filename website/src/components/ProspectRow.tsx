import type { Prospect } from "../types";
import { PlayerAvatar } from "./PlayerAvatar";
import { qualityColor, ink, line, surface, posColor, tierColor, font } from "../theme";
import { ScoreBadge, PosBadge as UIPosBadge } from "./ui";

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
  color = ink.primary,
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
          color: ink.faint,
          textTransform: "uppercase",
          letterSpacing: "0.06em",
          fontWeight: 500,
        }}
      >
        {label}
      </span>
      <span
        style={{
          fontFamily: font.mono,
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

export function PosBadge({ pos }: { pos: string }) {
  return <UIPosBadge pos={pos} color={posColor[pos] ?? ink.secondary} />;
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
          fontFamily: font.mono,
          fontSize: 13,
          fontWeight: 600,
          color: displayPpg >= p.exp_ppg ? "#2DC98F" : "#E5484D",
        }}
      >
        {displayPpg.toFixed(1)}
      </span>
    );
  }
  return <span style={{ color: ink.faint }}>—</span>;
}

export function ProspectRow({ prospect: p, expanded, onClick, hasActuals, allClasses }: Props) {
  const sc = p.prospect_score != null ? qualityColor(p.prospect_score) : ink.faint;
  const expandedBg = surface.selected;
  const normalBg = "transparent";

  const tdBase: React.CSSProperties = {
    padding: "0 12px",
    height: 44,
    borderBottom: expanded ? "none" : `1px solid ${line.hairline}`,
    transition: "background 0.15s",
  };

  // Map prospect_score → color for the mobile score pill.
  const mobileScoreColor = p.prospect_score != null ? sc : ink.faint;

  function onEnter(e: React.MouseEvent<HTMLTableRowElement>) {
    if (!expanded) {
      Array.from(e.currentTarget.cells).forEach(
        (c) => ((c as HTMLTableCellElement).style.background = surface.hover)
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
          borderLeft: expanded ? "2px solid #4C93F0" : "2px solid transparent",
        }}
      >
        <div style={{ display: "flex", alignItems: "center", gap: 10 }}>
          <PlayerAvatar url={p.headshot_url} name={p.name} size="sm" />

          {/* Identity block — name, college, tier */}
          <div style={{ minWidth: 0, flex: 1 }}>
            <div style={{ display: "flex", alignItems: "center", gap: 7, flexWrap: "wrap" }}>
              <span
                style={{
                  fontFamily: font.display,
                  fontSize: 14,
                  fontWeight: 600,
                  color: ink.primary,
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
                    color: ink.secondary,
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
            <div style={{ fontSize: 11, color: ink.faint, marginTop: 1 }}>
              <span style={{ whiteSpace: "nowrap" }}>{p.college}</span>
              {p.tier && (
                <span
                  style={{
                    marginLeft: 6,
                    color:
                      p.tier === "P4"
                        ? tierColor.P4
                        : p.tier === "G5"
                        ? tierColor.G5
                        : ink.faint,
                  }}
                >
                  {p.tier}
                </span>
              )}
              {/* Mobile: show position + pick + year inline since those columns are hidden */}
              <span className="sm:hidden" style={{ marginLeft: 6, color: ink.faint }}>
                · {p.position} · Rd {p.round} #{p.pick}
                {allClasses && ` · ${p.draft_year}`}
              </span>
            </div>
          </div>

          {/* Mobile-only stat strip — horizontally swipeable to reveal more
              metrics. The chevron sits outside the scroll area so it's
              always visible. Uses `sm:!hidden` so the !important wins
              over the inline `display: flex` at the sm+ breakpoint. */}
          <div
            className="sm:!hidden"
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
              {p.value_score != null && (
                <MobileStatPill
                  label="VALUE"
                  value={String(p.value_score)}
                  color={qualityColor(p.value_score)}
                  bg={`${qualityColor(p.value_score)}22`}
                />
              )}
              {p.prospect_score != null && (
                <MobileStatPill
                  label="POS"
                  value={String(p.prospect_score)}
                  color={mobileScoreColor}
                  bg={`${mobileScoreColor}22`}
                />
              )}
              <MobileStatPill
                label="PPG"
                value={p.exp_ppg.toFixed(1)}
                color="#2DC98F"
              />
              {p.actual_ppg != null &&
                (() => {
                  const display = p.actual_raw_ppg ?? p.actual_ppg;
                  const beat = display >= p.exp_ppg;
                  return (
                    <MobileStatPill
                      label="REAL"
                      value={display.toFixed(1)}
                      color={beat ? "#2DC98F" : "#E5484D"}
                    />
                  );
                })()}
              {p.p_made_it != null && (
                <MobileStatPill
                  label="HIT"
                  value={`${Math.round(p.p_made_it * 100)}%`}
                  color={ink.secondary}
                />
              )}
              {p.comp_weighted_ppg != null && (
                <MobileStatPill
                  label="COMP"
                  value={p.comp_weighted_ppg.toFixed(1)}
                  color={tierColor.G5}
                />
              )}
              {p.comp_bust_rate != null && (
                <MobileStatPill
                  label="BUST"
                  value={`${Math.round(p.comp_bust_rate * 100)}%`}
                  color={p.comp_bust_rate === 0 ? "#2DC98F" : "#EFA030"}
                />
              )}
              {p.round != null && p.pick != null && (
                <MobileStatPill
                  label="PICK"
                  value={`${p.round}.${p.pick}`}
                  color={ink.secondary}
                />
              )}
            </div>
            {/* Chevron — communicates that the row is interactive */}
            <span
              style={{
                color: expanded ? "#4C93F0" : ink.faint,
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
        <span style={{ fontFamily: font.mono, fontSize: 12, color: ink.secondary }}>
          Rd {p.round} #{p.pick}
        </span>
      </td>

      {/* Year — all-classes only */}
      {allClasses && (
        <td className="hidden sm:table-cell" style={tdBase}>
          <span style={{ fontFamily: font.mono, fontSize: 12, color: ink.secondary }}>
            {p.draft_year}
          </span>
        </td>
      )}

      {/* Pos */}
      <td className="hidden sm:table-cell" style={tdBase}>
        <PosBadge pos={p.position} />
      </td>

      {/* Pos Score */}
      <td className="hidden sm:table-cell" style={tdBase}>
        <ScoreBadge value={p.prospect_score} />
      </td>

      {/* Value (cross-position) */}
      <td className="hidden sm:table-cell" style={tdBase}>
        <ScoreBadge value={p.value_score} />
      </td>

      {/* Adj PPG */}
      <td className="hidden sm:table-cell" style={tdBase}>
        <span
          style={{
            fontFamily: font.mono,
            fontSize: 14,
            fontWeight: 600,
            color: "#2DC98F",
          }}
        >
          {p.exp_ppg.toFixed(1)}
        </span>
      </td>

      {/* Comp PPG */}
      <td className="hidden sm:table-cell" style={tdBase}>
        <span style={{ fontFamily: font.mono, fontSize: 13, color: ink.secondary }}>
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
                    fontFamily: font.mono,
                    fontSize: 13,
                    fontWeight: 600,
                    color: display >= p.exp_ppg ? "#2DC98F" : "#E5484D",
                  }}
                >
                  {display.toFixed(1)}
                </span>
              );
            })()
          ) : (
            <span style={{ color: ink.faint }}>—</span>
          )
        ) : (
          <span
            style={{
              fontFamily: font.mono,
              fontSize: 13,
              color: p.comp_bust_rate === 0 ? "#2DC98F" : ink.secondary,
            }}
          >
            {p.comp_bust_rate != null ? `${(p.comp_bust_rate * 100).toFixed(0)}%` : "—"}
          </span>
        )}
      </td>
    </tr>
  );
}

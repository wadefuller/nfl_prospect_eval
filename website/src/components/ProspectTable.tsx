import React, { useEffect, useRef } from "react";
import type { Prospect, ProspectComp, SortField, SortDir } from "../types";
import { ProspectRow } from "./ProspectRow";
import { ProspectDetail } from "./ProspectDetail";
import { ClassSummary } from "./ClassSummary";
import { ink, line, surface, font } from "../theme";
import { Chip } from "./ui";

interface Props {
  prospects: Prospect[];
  sortField: SortField;
  sortDir: SortDir;
  onSort: (field: SortField) => void;
  expandedId: string | null;
  onExpand: (id: string) => void;
  comps: Record<string, ProspectComp[]>;
  draftYear?: number;
  allClasses: boolean;
}

function SortTh({
  label,
  field,
  currentField,
  currentDir,
  onSort,
  className = "",
  width,
}: {
  label: string;
  field: SortField;
  currentField: SortField;
  currentDir: SortDir;
  onSort: (f: SortField) => void;
  className?: string;
  width?: string;
}) {
  const active = currentField === field;
  return (
    <th
      onClick={() => onSort(field)}
      className={className}
      style={{
        width,
        padding: "11px 12px",
        textAlign: "left",
        fontSize: 10,
        fontWeight: 600,
        letterSpacing: "0.09em",
        textTransform: "uppercase",
        color: active ? "#8FBEFB" : ink.muted,
        borderBottom: `1px solid ${line.border}`,
        background: surface.raised,
        cursor: "pointer",
        userSelect: "none",
        whiteSpace: "nowrap",
        fontFamily: font.body,
      }}
    >
      {label}
      {active && (
        <span style={{ marginLeft: 4, color: "#4C93F0" }}>
          {currentDir === "asc" ? "↑" : "↓"}
        </span>
      )}
      {!active && (
        <span style={{ marginLeft: 4, color: ink.faint, opacity: .6 }}>↕</span>
      )}
    </th>
  );
}

function StaticTh({ label, className = "" }: { label: string; className?: string }) {
  return (
    <th
      className={className}
      style={{
        padding: "11px 12px",
        textAlign: "left",
        fontSize: 10,
        fontWeight: 600,
        letterSpacing: "0.09em",
        textTransform: "uppercase",
        color: ink.muted,
        borderBottom: `1px solid ${line.border}`,
        background: surface.raised,
        whiteSpace: "nowrap",
        fontFamily: font.body,
      }}
    >
      {label}
    </th>
  );
}

export function ProspectTable({
  prospects,
  sortField,
  sortDir,
  onSort,
  expandedId,
  onExpand,
  comps,
  draftYear,
  allClasses,
}: Props) {
  const hasActuals = !allClasses && prospects.some((p) => p.actual_ppg != null);
  const colCount = allClasses ? 9 : 8;

  // When a row is expanded on mobile, scroll the detail panel into view so the
  // model's predictions appear in the viewport instead of below the fold.
  const expandedRef = useRef<HTMLTableRowElement | null>(null);
  useEffect(() => {
    if (!expandedId || !expandedRef.current) return;
    // Only auto-scroll on narrow screens (sm breakpoint is 640px).
    if (typeof window === "undefined" || window.innerWidth >= 640) return;
    const el = expandedRef.current;
    // Defer so the row has rendered before we measure.
    const t = window.setTimeout(() => {
      // Account for the sticky table header so we don't scroll the detail
      // *under* it.
      const headerH = parseInt(
        getComputedStyle(document.documentElement).getPropertyValue("--header-h") || "88",
        10
      );
      const rect = el.getBoundingClientRect();
      const offset = window.scrollY + rect.top - (Number.isFinite(headerH) ? headerH : 88) - 8;
      window.scrollTo({ top: offset, behavior: "smooth" });
    }, 60);
    return () => window.clearTimeout(t);
  }, [expandedId]);

  // Mobile sort chips — mirrors the metric pills on each row so the user
  // can tap a chip to sort by that metric. Tapping the active chip toggles
  // direction; tapping a different chip switches field (and direction
  // resets to a sensible default via App's handleSort).
  const mobileSortChips: { label: string; field: SortField; show: boolean }[] = (
    [
      { label: "Value", field: "value_score", show: true },
      { label: "Pos Score", field: "prospect_score", show: true },
      { label: "PPG", field: "exp_ppg", show: true },
      { label: "Hit %", field: "p_made_it", show: true },
      { label: "Comp", field: "comp_weighted_ppg", show: true },
      { label: "Pick", field: "pick", show: true },
      { label: "Name", field: "name", show: true },
      { label: "Actual", field: "actual_ppg", show: hasActuals },
      { label: "Year", field: "draft_year", show: allClasses },
    ] satisfies { label: string; field: SortField; show: boolean }[]
  ).filter((c) => c.show);

  return (
    <div style={{ padding: "0 14px" }}>
      <ClassSummary prospects={prospects} allClasses={allClasses} year={draftYear ?? 0} />

      {/* Mobile-only sort chip strip — `sm:!hidden` (!important) is needed
          because the inline `display: flex` below would otherwise beat
          Tailwind's `sm:hidden` (which is `display: none` without !important). */}
      <div
        className="sm:!hidden mobile-pill-strip"
        style={{
          display: "flex",
          alignItems: "center",
          gap: 6,
          overflowX: "auto",
          overflowY: "hidden",
          scrollbarWidth: "none",
          WebkitOverflowScrolling: "touch",
          touchAction: "pan-x",
          padding: "8px 12px",
          borderBottom: "1px solid rgba(255,255,255,0.05)",
          position: "sticky",
          top: "var(--header-h, 88px)",
          zIndex: 11,
          background: surface.raised,
          backdropFilter: "blur(8px)",
          WebkitMaskImage:
            "linear-gradient(to right, #000 calc(100% - 18px), transparent 100%)",
          maskImage:
            "linear-gradient(to right, #000 calc(100% - 18px), transparent 100%)",
        }}
      >
        <span
          style={{
            fontSize: 10,
            color: ink.muted,
            textTransform: "uppercase",
            letterSpacing: "0.09em",
            fontWeight: 600,
            flexShrink: 0,
            marginRight: 2,
          }}
        >
          Sort
        </span>
        {mobileSortChips.map((c) => (
          <Chip key={c.field} active={sortField === c.field} onClick={() => onSort(c.field)}>
            {c.label}
            {sortField === c.field && (
              <span style={{ fontFamily: font.mono, fontSize: 11 }}>
                {sortDir === "asc" ? "↑" : "↓"}
              </span>
            )}
          </Chip>
        ))}
      </div>

      <div
        style={{
          background: surface.card,
          border: `1px solid ${line.hairline}`,
          borderRadius: 12,
          // `clip`, not `hidden`: overflow:hidden makes this a scroll
          // container, which would re-anchor the sticky <thead> to this card
          // instead of the viewport (the header then floated two rows down the
          // table). overflow:clip clips the corners identically without
          // becoming a containing block for sticky descendants.
          overflow: "clip",
        }}
      >
      <table style={{ width: "100%", borderCollapse: "collapse" }}>
        <thead
          className="hidden sm:table-header-group"
          style={{
            position: "sticky",
            top: "var(--header-h, 88px)",
            zIndex: 10,
            backdropFilter: "blur(8px)",
            background: surface.raised,
          }}
        >
          <tr>
            <SortTh
              label="Player"
              field="name"
              currentField={sortField}
              currentDir={sortDir}
              onSort={onSort}
              className="pl-[14px]"
              width="30%"
            />
            <SortTh
              label="Pick"
              field="pick"
              currentField={sortField}
              currentDir={sortDir}
              onSort={onSort}
              className="hidden sm:table-cell"
            />
            {allClasses && (
              <SortTh
                label="Year"
                field="draft_year"
                currentField={sortField}
                currentDir={sortDir}
                onSort={onSort}
                className="hidden sm:table-cell"
              />
            )}
            <StaticTh label="Pos" className="hidden sm:table-cell" />
            <SortTh
              label="Pos Score"
              field="prospect_score"
              currentField={sortField}
              currentDir={sortDir}
              onSort={onSort}
              className="hidden sm:table-cell"
            />
            <SortTh
              label="Value"
              field="value_score"
              currentField={sortField}
              currentDir={sortDir}
              onSort={onSort}
              className="hidden sm:table-cell"
            />
            <SortTh
              label="Adj PPG"
              field="exp_ppg"
              currentField={sortField}
              currentDir={sortDir}
              onSort={onSort}
              className="hidden sm:table-cell"
            />
            <SortTh
              label="Comp PPG"
              field="comp_weighted_ppg"
              currentField={sortField}
              currentDir={sortDir}
              onSort={onSort}
              className="hidden sm:table-cell"
            />
            {allClasses ? (
              <StaticTh label="Result" className="hidden sm:table-cell" />
            ) : hasActuals ? (
              <SortTh
                label="Actual"
                field="actual_ppg"
                currentField={sortField}
                currentDir={sortDir}
                onSort={onSort}
                className="hidden sm:table-cell"
              />
            ) : (
              <StaticTh label="Bust %" className="hidden sm:table-cell" />
            )}
          </tr>
        </thead>
        <tbody>
          {prospects.map((p) => (
            <React.Fragment key={p.id}>
              <ProspectRow
                prospect={p}
                expanded={expandedId === p.id}
                onClick={() => onExpand(p.id)}
                hasActuals={hasActuals}
                allClasses={allClasses}
              />
              {expandedId === p.id && (
                <tr ref={expandedRef}>
                  <td
                    colSpan={colCount}
                    style={{
                      padding: 0,
                      borderBottom: "1px solid rgba(255,255,255,0.07)",
                      borderLeft: `2px solid ${"#4C93F0"}`,
                    }}
                  >
                    <ProspectDetail prospect={p} comps={comps[p.id] ?? null} />
                  </td>
                </tr>
              )}
            </React.Fragment>
          ))}
          {prospects.length === 0 && (
            <tr>
              <td
                colSpan={colCount}
                style={{
                  padding: "48px 24px",
                  textAlign: "center",
                  color: ink.muted,
                  fontFamily: font.body,
                }}
              >
                No prospects found for this filter.
              </td>
            </tr>
          )}
        </tbody>
      </table>
      </div>
    </div>
  );
}

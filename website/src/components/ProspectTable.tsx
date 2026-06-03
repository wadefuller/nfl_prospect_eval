import React, { useEffect, useRef } from "react";
import type { Prospect, ProspectComp, SortField, SortDir } from "../types";
import { ProspectRow } from "./ProspectRow";
import { ProspectDetail } from "./ProspectDetail";

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
}: {
  label: string;
  field: SortField;
  currentField: SortField;
  currentDir: SortDir;
  onSort: (f: SortField) => void;
  className?: string;
}) {
  const active = currentField === field;
  return (
    <th
      onClick={() => onSort(field)}
      className={className}
      style={{
        padding: "10px 12px",
        textAlign: "left",
        fontSize: 11,
        fontWeight: 500,
        letterSpacing: "0.08em",
        textTransform: "uppercase",
        color: active ? "#8A9AC0" : "#4A5578",
        borderBottom: "1px solid rgba(255,255,255,0.07)",
        cursor: "pointer",
        userSelect: "none",
        whiteSpace: "nowrap",
        fontFamily: "var(--font-body)",
      }}
    >
      {label}
      {active && (
        <span style={{ marginLeft: 4, color: "#3E8EF7" }}>
          {currentDir === "asc" ? "↑" : "↓"}
        </span>
      )}
      {!active && (
        <span style={{ marginLeft: 4, color: "#2E3650" }}>↕</span>
      )}
    </th>
  );
}

function StaticTh({ label, className = "" }: { label: string; className?: string }) {
  return (
    <th
      className={className}
      style={{
        padding: "10px 12px",
        textAlign: "left",
        fontSize: 11,
        fontWeight: 500,
        letterSpacing: "0.08em",
        textTransform: "uppercase",
        color: "#4A5578",
        borderBottom: "1px solid rgba(255,255,255,0.07)",
        whiteSpace: "nowrap",
        fontFamily: "var(--font-body)",
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
  allClasses,
}: Props) {
  const hasActuals = !allClasses && prospects.some((p) => p.actual_ppg != null);
  const colCount = allClasses ? 8 : 7;

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
      { label: "Score", field: "prospect_score", show: true },
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
    <div>
      {/* Mobile-only sort chip strip */}
      <div
        className="sm:hidden mobile-pill-strip"
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
          background: "rgba(11,14,19,0.92)",
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
            color: "#4A5578",
            textTransform: "uppercase",
            letterSpacing: "0.08em",
            fontWeight: 500,
            flexShrink: 0,
            marginRight: 2,
          }}
        >
          Sort
        </span>
        {mobileSortChips.map((c) => {
          const active = sortField === c.field;
          return (
            <button
              key={c.field}
              onClick={() => onSort(c.field)}
              style={{
                appearance: "none",
                border: active ? "1px solid #3E8EF7" : "1px solid rgba(255,255,255,0.08)",
                background: active ? "rgba(62,142,247,0.15)" : "rgba(255,255,255,0.04)",
                color: active ? "#3E8EF7" : "#8A9AC0",
                fontFamily: "var(--font-body)",
                fontSize: 12,
                fontWeight: active ? 600 : 500,
                padding: "5px 10px",
                borderRadius: 999,
                whiteSpace: "nowrap",
                cursor: "pointer",
                flexShrink: 0,
                display: "inline-flex",
                alignItems: "center",
                gap: 4,
              }}
            >
              {c.label}
              {active && (
                <span style={{ fontFamily: "var(--font-mono)", fontSize: 11 }}>
                  {sortDir === "asc" ? "↑" : "↓"}
                </span>
              )}
            </button>
          );
        })}
      </div>

      <table style={{ width: "100%", borderCollapse: "collapse" }}>
        <thead
          className="hidden sm:table-header-group"
          style={{
            position: "sticky",
            top: "var(--header-h, 88px)",
            zIndex: 10,
            backdropFilter: "blur(8px)",
            background: "rgba(11,14,19,0.92)",
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
              label="Score"
              field="prospect_score"
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
                      borderLeft: "2px solid #3E8EF7",
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
                  color: "#4A5578",
                  fontFamily: "var(--font-body)",
                }}
              >
                No prospects found for this filter.
              </td>
            </tr>
          )}
        </tbody>
      </table>
    </div>
  );
}

import React from "react";
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

  return (
    <div>
      <table style={{ width: "100%", borderCollapse: "collapse" }}>
        <thead
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
                <tr>
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

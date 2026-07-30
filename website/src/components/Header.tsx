import { useEffect, useRef } from "react";
import { ink, line, surface, font, accent, posColor, MAXW } from "../theme";

interface HeaderProps {
  years: number[];
  year: number;
  onYearChange: (y: number) => void;
  posFilter: string;
  onPosFilterChange: (p: string) => void;
  allClasses: boolean;
  onAllClassesToggle: () => void;
  totalProspects: number;
  lastUpdated: string;
  route: string;
}

export function Header({
  years,
  year,
  onYearChange,
  posFilter,
  onPosFilterChange,
  allClasses,
  onAllClassesToggle,
  totalProspects,
  lastUpdated,
  route,
}: HeaderProps) {
  const onModelPage = route === "#/model";
  const onInspectorPage = route === "#/inspector";
  const onProspectsPage = !onModelPage && !onInspectorPage;
  const ref = useRef<HTMLElement>(null);

  useEffect(() => {
    if (!ref.current) return;
    const el = ref.current;
    const update = () => {
      document.documentElement.style.setProperty("--header-h", `${el.offsetHeight}px`);
    };
    update();
    const ro = new ResizeObserver(update);
    ro.observe(el);
    return () => ro.disconnect();
  }, []);

  const btnBase: React.CSSProperties = {
    padding: "6px 11px",
    fontSize: 12,
    fontWeight: 600,
    fontFamily: font.mono,
    border: "none",
    cursor: "pointer",
    transition: "all 0.15s",
  };

  const navLink = (href: string, label: string, active: boolean): React.ReactElement => (
    <a
      href={href}
      style={{
        fontSize: 13.5,
        fontWeight: 600,
        fontFamily: font.body,
        color: active ? ink.primary : ink.secondary,
        textDecoration: "none",
        padding: "2px 0 10px",
        borderBottom: `2px solid ${active ? accent : "transparent"}`,
        transition: "color 0.15s, border-color 0.15s",
        whiteSpace: "nowrap",
      }}
    >
      {label}
    </a>
  );

  return (
    <header
      ref={ref}
      style={{
        padding: "16px 24px 0",
        borderBottom: `1px solid ${line.border}`,
        position: "sticky",
        top: 0,
        zIndex: 20,
        backdropFilter: "blur(12px)",
        background: "rgba(11,14,19,0.88)",
      }}
    >
      <div
        style={{
          display: "flex",
          alignItems: "flex-start",
          justifyContent: "space-between",
          maxWidth: MAXW,
          margin: "0 auto",
          width: "100%",
          flexWrap: "wrap",
          gap: 12,
        }}
      >
        {/* Left: brand + nav tabs */}
        <div>
          <h1
            style={{
              fontFamily: font.display,
              fontSize: 22,
              fontWeight: 700,
              letterSpacing: "-0.025em",
              color: ink.primary,
              lineHeight: 1.2,
            }}
          >
            DraftScout
          </h1>
          <p style={{ fontSize: 12, color: ink.muted, marginTop: 3, fontFamily: font.body }}>
            {onModelPage
              ? `Model performance · Updated ${lastUpdated}`
              : onInspectorPage
                ? `Prospect inspector · Updated ${lastUpdated}`
                : `${totalProspects} prospects · Updated ${lastUpdated}`}
          </p>
          {/* Nav tabs sit on the bottom border */}
          <div style={{ display: "flex", gap: 22, marginTop: 12 }}>
            {navLink("#/", "Prospects", onProspectsPage)}
            {navLink("#/model", "Model", onModelPage)}
            {navLink("#/inspector", "Inspector", onInspectorPage)}
          </div>
        </div>

        {/* Right: prospect controls — hidden on model + inspector pages */}
        {onProspectsPage && (
          <div style={{ display: "flex", alignItems: "center", gap: 8, flexWrap: "wrap", paddingBottom: 12 }}>
            {/* Position filter */}
            <div
              style={{
                display: "flex",
                background: surface.raised,
                borderRadius: 8,
                overflow: "hidden",
                border: `1px solid ${line.border}`,
              }}
            >
              {["ALL", "QB", "RB", "WR", "TE"].map((pos) => (
                <button
                  key={pos}
                  onClick={() => onPosFilterChange(pos)}
                  style={{
                    ...btnBase,
                    background:
                      posFilter === pos
                        ? pos === "ALL" ? accent : `${posColor[pos]}26`
                        : "transparent",
                    color:
                      posFilter === pos
                        ? pos === "ALL" ? "#08111F" : posColor[pos]
                        : ink.secondary,
                  }}
                >
                  {pos}
                </button>
              ))}
            </div>

            {/* Year selector */}
            <select
              value={year}
              onChange={(e) => onYearChange(Number(e.target.value))}
              disabled={allClasses}
              style={{
                background: surface.raised,
                border: `1px solid ${line.border}`,
                borderRadius: 8,
                padding: "6px 12px",
                fontSize: 13,
                fontFamily: font.body,
                color: allClasses ? ink.faint : ink.secondary,
                cursor: allClasses ? "not-allowed" : "pointer",
                opacity: allClasses ? 0.5 : 1,
                outline: "none",
              }}
            >
              {[...years].reverse().map((y) => (
                <option key={y} value={y} style={{ background: surface.raised }}>
                  {y} Draft
                </option>
              ))}
            </select>

            {/* All Classes toggle */}
            <button
              onClick={onAllClassesToggle}
              style={{
                ...btnBase,
                padding: "6px 14px",
                borderRadius: 8,
                border: "1px solid",
                borderColor: allClasses ? "rgba(76,147,240,0.55)" : line.border,
                background: allClasses ? "rgba(76,147,240,0.16)" : surface.raised,
                color: allClasses ? "#8FBEFB" : ink.secondary,
                fontSize: 12,
                fontFamily: font.body,
                fontWeight: 600,
                whiteSpace: "nowrap",
              }}
            >
              All Classes
            </button>
          </div>
        )}
      </div>
    </header>
  );
}

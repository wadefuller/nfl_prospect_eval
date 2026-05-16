import { useEffect, useRef } from "react";

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
    fontFamily: "var(--font-mono)",
    border: "none",
    cursor: "pointer",
    transition: "all 0.15s",
  };

  const navLink = (href: string, label: string, active: boolean): React.ReactElement => (
    <a
      href={href}
      style={{
        fontSize: 13,
        fontWeight: 500,
        fontFamily: "var(--font-body)",
        color: active ? "#F0F4FF" : "#4A5578",
        textDecoration: "none",
        padding: "2px 0",
        borderBottom: active ? "2px solid #3E8EF7" : "2px solid transparent",
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
        borderBottom: "1px solid rgba(255,255,255,0.07)",
        position: "sticky",
        top: 0,
        zIndex: 20,
        backdropFilter: "blur(12px)",
        background: "rgba(11,14,19,0.92)",
      }}
    >
      <div
        style={{
          display: "flex",
          alignItems: "flex-start",
          justifyContent: "space-between",
          maxWidth: 1400,
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
              fontFamily: "var(--font-display)",
              fontSize: 22,
              fontWeight: 700,
              letterSpacing: "-0.025em",
              color: "#F0F4FF",
              lineHeight: 1.2,
            }}
          >
            DraftScout
          </h1>
          <p style={{ fontSize: 12, color: "#4A5578", marginTop: 2, fontFamily: "var(--font-body)" }}>
            {onModelPage
              ? `Model performance · Updated ${lastUpdated}`
              : onInspectorPage
                ? `Prospect inspector · Updated ${lastUpdated}`
                : `${totalProspects} prospects · Updated ${lastUpdated}`}
          </p>
          {/* Nav tabs sit on the bottom border */}
          <div style={{ display: "flex", gap: 20, marginTop: 12 }}>
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
                background: "rgba(255,255,255,0.04)",
                borderRadius: 6,
                overflow: "hidden",
                border: "1px solid rgba(255,255,255,0.07)",
              }}
            >
              {["ALL", "WR", "RB"].map((pos) => (
                <button
                  key={pos}
                  onClick={() => onPosFilterChange(pos)}
                  style={{
                    ...btnBase,
                    background: posFilter === pos ? "#3E8EF7" : "transparent",
                    color: posFilter === pos ? "#fff" : "#4A5578",
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
                background: "rgba(255,255,255,0.04)",
                border: "1px solid rgba(255,255,255,0.07)",
                borderRadius: 6,
                padding: "6px 12px",
                fontSize: 13,
                fontFamily: "var(--font-body)",
                color: allClasses ? "#4A5578" : "#8A9AC0",
                cursor: allClasses ? "not-allowed" : "pointer",
                opacity: allClasses ? 0.5 : 1,
                outline: "none",
              }}
            >
              {[...years].reverse().map((y) => (
                <option key={y} value={y} style={{ background: "#181E2B" }}>
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
                borderRadius: 6,
                border: "1px solid",
                borderColor: allClasses ? "#3E8EF7" : "rgba(255,255,255,0.07)",
                background: allClasses ? "rgba(62,142,247,0.15)" : "rgba(255,255,255,0.04)",
                color: allClasses ? "#3E8EF7" : "#4A5578",
                fontSize: 12,
                fontFamily: "var(--font-body)",
                fontWeight: 500,
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

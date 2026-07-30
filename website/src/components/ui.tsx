/**
 * Shared presentational primitives.
 *
 * The three routes previously each grew their own card / label / stat-tile
 * treatment, so they read as three different products. Everything visual is
 * built from these pieces now.
 */
import React from "react";
import { CARD, LABEL, NUM, ink, line, surface, font, qualityColor } from "../theme";

export function Card({
  children, style, pad,
}: { children: React.ReactNode; style?: React.CSSProperties; pad?: number | string }) {
  return <div style={{ ...CARD, ...(pad != null ? { padding: pad } : null), ...style }}>{children}</div>;
}

export function SectionLabel({
  children, style,
}: { children: React.ReactNode; style?: React.CSSProperties }) {
  return <div style={{ ...LABEL, ...style }}>{children}</div>;
}

/** Page heading block — identical rhythm on every route. */
export function PageHead({
  title, blurb,
}: { title: string; blurb?: React.ReactNode }) {
  return (
    <div style={{ marginBottom: 22 }}>
      <h1
        style={{
          fontFamily: font.display,
          fontSize: 26,
          fontWeight: 700,
          color: ink.primary,
          letterSpacing: "-0.015em",
          lineHeight: 1.15,
        }}
      >
        {title}
      </h1>
      {blurb && (
        <p
          style={{
            marginTop: 7,
            fontSize: 13.5,
            lineHeight: 1.6,
            color: ink.secondary,
            maxWidth: 680,
          }}
        >
          {blurb}
        </p>
      )}
    </div>
  );
}

/**
 * Hero/stat tile. `tone` drives an accent hairline on the left edge, so the
 * value's meaning is carried by position + label + number, never colour alone.
 */
export function StatTile({
  label, value, sub, tone, size = "md",
}: {
  label: string;
  value: React.ReactNode;
  sub?: React.ReactNode;
  tone?: string;
  size?: "sm" | "md" | "lg";
}) {
  const fs = size === "lg" ? 30 : size === "sm" ? 18 : 24;
  return (
    <div
      style={{
        ...CARD,
        padding: "12px 14px",
        borderLeft: tone ? `2px solid ${tone}` : CARD.border as string,
        display: "flex",
        flexDirection: "column",
        gap: 5,
        minWidth: 0,
      }}
    >
      <div style={{ ...LABEL, fontSize: 9.5 }}>{label}</div>
      <div
        style={{
          ...NUM,
          fontSize: fs,
          fontWeight: 700,
          color: ink.primary,
          lineHeight: 1,
          letterSpacing: "-0.02em",
        }}
      >
        {value}
      </div>
      {sub && (
        <div style={{ fontSize: 11, color: ink.muted, lineHeight: 1.45 }}>{sub}</div>
      )}
    </div>
  );
}

/** Numeric badge on a tinted chip. Colour is decorative; the number carries it. */
export function ScoreBadge({
  value, size = "md",
}: { value: number | null | undefined; size?: "sm" | "md" }) {
  if (value == null) return <span style={{ color: ink.faint }}>—</span>;
  const c = qualityColor(value);
  return (
    <span
      style={{
        ...NUM,
        background: `${c}1F`,
        color: c,
        border: `1px solid ${c}33`,
        borderRadius: 6,
        padding: size === "sm" ? "1px 7px" : "3px 9px",
        fontSize: size === "sm" ? 12 : 13,
        fontWeight: 700,
      }}
    >
      {value}
    </span>
  );
}

/**
 * Horizontal magnitude bar. 4px rounded data-end anchored to the track start,
 * per the mark spec.
 */
export function Bar({
  pct, color, height = 4, track = true,
}: { pct: number | null | undefined; color?: string; height?: number; track?: boolean }) {
  const ok = pct != null && Number.isFinite(pct);
  const w = ok ? Math.max(1.5, Math.min(100, pct)) : 0;
  const c = color ?? qualityColor(pct);
  return (
    <div
      style={{
        height,
        borderRadius: height / 2,
        background: track ? "rgba(255,255,255,0.065)" : "transparent",
        overflow: "hidden",
        width: "100%",
      }}
    >
      {ok && <div style={{ height: "100%", width: `${w}%`, background: c, borderRadius: height / 2 }} />}
    </div>
  );
}

export function PosBadge({ pos, color }: { pos: string; color: string }) {
  return (
    <span
      style={{
        ...NUM,
        background: `${color}1A`,
        color,
        border: `1px solid ${color}2E`,
        borderRadius: 5,
        padding: "1.5px 6px",
        fontSize: 10.5,
        fontWeight: 700,
        letterSpacing: "0.03em",
        whiteSpace: "nowrap",
      }}
    >
      {pos}
    </span>
  );
}

/** Small pill used for filters / segmented controls. */
export function Chip({
  active, children, onClick, title,
}: { active?: boolean; children: React.ReactNode; onClick?: () => void; title?: string }) {
  return (
    <button
      onClick={onClick}
      title={title}
      style={{
        appearance: "none",
        cursor: "pointer",
        whiteSpace: "nowrap",
        flexShrink: 0,
        fontFamily: font.body,
        fontSize: 12.5,
        fontWeight: active ? 650 : 500,
        padding: "5px 11px",
        borderRadius: 999,
        border: `1px solid ${active ? "rgba(76,147,240,0.55)" : line.border}`,
        background: active ? "rgba(76,147,240,0.16)" : surface.raised,
        color: active ? "#8FBEFB" : ink.secondary,
        display: "inline-flex",
        alignItems: "center",
        gap: 5,
        transition: "background .13s, border-color .13s, color .13s",
      }}
    >
      {children}
    </button>
  );
}

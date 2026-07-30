/**
 * Single source of truth for colour, type and surface tokens.
 *
 * Before this file the components carried ~150 hard-coded hex literals,
 * including four near-identical muted blues (#8A9AC0 / #7A8AAB / #6B7B9F /
 * #5E6B8C) and three ambers, plus a `CARD` style object defined separately in
 * three files and two *different* implementations of `pctColor`. Everything
 * now resolves through here.
 */

// ── Surfaces ────────────────────────────────────────────────────────────────
export const surface = {
  page:      "#0B0E13",
  /** Chart/card surface. All data colours below are validated against this. */
  card:      "#141A26",
  raised:    "#1C2331",
  hover:     "rgba(255,255,255,0.025)",
  selected:  "rgba(62,142,247,0.07)",
} as const;

export const line = {
  hairline: "rgba(255,255,255,0.055)",
  border:   "rgba(255,255,255,0.09)",
  strong:   "rgba(255,255,255,0.16)",
} as const;

// ── Ink ─────────────────────────────────────────────────────────────────────
export const ink = {
  primary:   "#F0F4FF",
  secondary: "#93A1C4",
  muted:     "#5E6B8C",
  faint:     "#3E4864",
} as const;

// ── Brand / status ──────────────────────────────────────────────────────────
export const accent = "#4C93F0";
export const status = {
  good:     "#2DC98F",
  warning:  "#EFA030",
  critical: "#E5484D",
} as const;

/**
 * Ordered outcome scale — diverging, 5 steps: two warm, a neutral midpoint,
 * two cool. Used for outcome buckets, score badges and percentile bars so a
 * given quality reads identically everywhere.
 *
 * Validated with the dataviz palette validator against surface.card:
 *   CVD separation      PASS  worst adjacent ΔE 14.8 (deutan)
 *   Normal-vision floor PASS  worst adjacent ΔE 17.2  (was 12.3 — a hard fail)
 *   Contrast vs surface PASS  all five ≥ 3:1
 * The validator's remaining "lightness band" and "chroma floor" flags are
 * scoped to categorical palettes and are correct-by-design here: a diverging
 * midpoint is meant to read gray, and the poles are meant to vary in lightness.
 */
export const scale = {
  worst: "#E5484D",
  poor:  "#EFA030",
  mid:   "#5A688C",
  good:  "#4C93F0",
  best:  "#2DC98F",
} as const;

/** Quality 0–100 → diverging scale step. The one ramp, used by every surface. */
export function qualityColor(v: number | null | undefined): string {
  if (v == null || !Number.isFinite(v)) return ink.faint;
  if (v >= 85) return scale.best;
  if (v >= 65) return scale.good;
  if (v >= 40) return scale.mid;
  if (v >= 20) return scale.poor;
  return scale.worst;
}

/**
 * Position identity — a categorical set, a different job from the ordered
 * scale above. All four appear together (position-mix bar, position scatter),
 * so this is validated on the *all-pairs* list, not just adjacent pairs:
 *   CVD separation      PASS  worst pair ΔE 9.7 (protan) / 9.4 (tritan)
 *   Normal-vision floor PASS  worst pair ΔE 20.5
 *   Contrast vs surface PASS  all four ≥ 3:1
 * The previous violet QB sat ΔE 2.4 from the blue under protanopia —
 * indistinguishable for red-blind readers.
 *
 * Three of these coincide with steps of the outcome scale. That is safe here
 * because a position is always rendered as its literal two-letter label on a
 * low-chroma tint, never as a bare numeric badge, so hue is reinforcement
 * rather than the carrier of identity.
 */
export const posColor: Record<string, string> = {
  QB: "#D06BB4",
  RB: "#2DC98F",
  WR: "#EFA030",
  TE: "#4C93F0",
};

export const tierColor: Record<string, string> = {
  P4: accent,
  G5: "#A78BFA",
};

// ── Type ────────────────────────────────────────────────────────────────────
export const font = {
  display: "var(--font-display)",
  body:    "var(--font-body)",
  mono:    "var(--font-mono)",
} as const;

// ── Shared style objects (previously duplicated per-file) ───────────────────
export const CARD: React.CSSProperties = {
  background: surface.card,
  border: `1px solid ${line.hairline}`,
  borderRadius: 10,
  padding: "14px 16px",
};

export const LABEL: React.CSSProperties = {
  fontSize: 10,
  textTransform: "uppercase",
  letterSpacing: "0.09em",
  color: ink.muted,
  fontWeight: 600,
  fontFamily: font.body,
};

/** Tabular numerals — stops digits jittering as values change width. */
export const NUM: React.CSSProperties = {
  fontFamily: font.mono,
  fontVariantNumeric: "tabular-nums",
};

/** Max content width, shared by every route so the three pages line up. */
export const MAXW = 1440;

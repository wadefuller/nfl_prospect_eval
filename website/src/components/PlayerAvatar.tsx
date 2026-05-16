import { useState } from "react";

interface Props {
  url: string | null;
  name: string;
  size?: "sm" | "lg";
}

export function PlayerAvatar({ url, name, size = "sm" }: Props) {
  const [imgError, setImgError] = useState(false);
  const initials = name
    .split(" ")
    .filter(Boolean)
    .map((w) => w[0])
    .slice(0, 2)
    .join("")
    .toUpperCase();

  const dim = size === "sm" ? 32 : 56;
  const fontSize = size === "sm" ? 11 : 18;

  const baseStyle: React.CSSProperties = {
    width: dim,
    height: dim,
    borderRadius: "50%",
    flexShrink: 0,
    border: "2px solid rgba(255,255,255,0.1)",
    display: "flex",
    alignItems: "center",
    justifyContent: "center",
    background: "rgba(255,255,255,0.06)",
    color: "rgba(255,255,255,0.25)",
    fontFamily: "var(--font-display)",
    fontSize,
    fontWeight: 700,
    overflow: "hidden",
  };

  if (url && !imgError) {
    return (
      <img
        src={url}
        alt={name}
        style={{ ...baseStyle, objectFit: "cover" }}
        onError={() => setImgError(true)}
      />
    );
  }

  return <div style={baseStyle}>{initials}</div>;
}

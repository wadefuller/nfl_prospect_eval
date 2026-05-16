export function scoreColor(score: number): string {
  if (score >= 90) return "#2DD4A0";
  if (score >= 80) return "#3E8EF7";
  if (score >= 70) return "#F5A623";
  return "#F75757";
}


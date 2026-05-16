const pptxgen = require("pptxgenjs");
const fs = require("fs");

// ── Load data ───────────────────────────────────────────────────────────────
const csv = fs.readFileSync("output/all_class_scores.csv", "utf8");
const lines = csv.trim().split("\n");
const headers = lines[0].split(",");
const rows = lines.slice(1).map(line => {
  // Handle potential commas in quoted fields
  const vals = [];
  let current = "";
  let inQuotes = false;
  for (const ch of line) {
    if (ch === '"') { inQuotes = !inQuotes; continue; }
    if (ch === ',' && !inQuotes) { vals.push(current); current = ""; continue; }
    current += ch;
  }
  vals.push(current);
  const obj = {};
  headers.forEach((h, i) => obj[h] = vals[i] || "");
  return obj;
});

// ── Color Palette: Midnight Navy + Gold ─────────────────────────────────────
const C = {
  bg:        "0F1628",  // deep midnight
  bgCard:    "1A2340",  // card navy
  bgLight:   "243056",  // lighter card
  gold:      "D4A843",  // warm gold accent
  goldDim:   "9A7B30",  // muted gold
  white:     "FFFFFF",
  light:     "BCC5D8",  // soft gray-blue
  dim:       "6B7A99",  // muted text
  green:     "2ECC71",  // positive
  red:       "E74C3C",  // negative
  teal:      "1ABC9C",  // accent 2
  blue:      "3498DB",  // accent 3
};

const yearColors = {
  "2021": "E74C3C", "2022": "E67E22", "2023": "F1C40F",
  "2024": "2ECC71", "2025": "3498DB", "2026": "9B59B6"
};

// ── Helpers ─────────────────────────────────────────────────────────────────
function getTop(position, year, n = 10) {
  return rows
    .filter(r => r.position === position && r.draft_year === String(year))
    .sort((a, b) => parseFloat(b.exp_ppg) - parseFloat(a.exp_ppg))
    .slice(0, n);
}

function fmt(val, decimals = 2) {
  const n = parseFloat(val);
  return isNaN(n) ? "—" : n.toFixed(decimals);
}

function pct(val) {
  const n = parseFloat(val);
  return isNaN(n) ? "—" : Math.round(n * 100) + "%";
}

// ── Build Presentation ──────────────────────────────────────────────────────
const pres = new pptxgen();
pres.layout = "LAYOUT_WIDE";  // 13.3" x 7.5"
pres.author = "Wade Fuller";
pres.title = "NFL Draft Prospects 2021-2026";

// ═══════════════════════════════════════════════════════════════════════════
// SLIDE 1: Title
// ═══════════════════════════════════════════════════════════════════════════
let slide = pres.addSlide();
slide.background = { color: C.bg };

// Gold accent bar at top
slide.addShape(pres.shapes.RECTANGLE, {
  x: 0, y: 0, w: 13.3, h: 0.06, fill: { color: C.gold }
});

// Title
slide.addText("NFL Draft Prospects", {
  x: 0.8, y: 1.6, w: 11.7, h: 1.2,
  fontSize: 48, fontFace: "Georgia", bold: true,
  color: C.white, margin: 0
});

slide.addText("2021 - 2026", {
  x: 0.8, y: 2.7, w: 11.7, h: 0.8,
  fontSize: 36, fontFace: "Georgia",
  color: C.gold, margin: 0
});

// Subtitle
slide.addText("WR & RB Class Analysis  |  College to NFL Production Model", {
  x: 0.8, y: 3.8, w: 11.7, h: 0.5,
  fontSize: 16, fontFace: "Calibri", italic: true,
  color: C.light, margin: 0
});

// Thin separator line
slide.addShape(pres.shapes.LINE, {
  x: 0.8, y: 4.6, w: 4.5, h: 0,
  line: { color: C.goldDim, width: 1 }
});

// Method description
slide.addText([
  { text: "Single-stage XGBoost regression with empirical Bayes shrinkage targets", options: { breakLine: true, fontSize: 13, color: C.light } },
  { text: "Time-based cross-validation  |  Games-weighted PPG  |  6-game minimum", options: { breakLine: true, fontSize: 13, color: C.dim } },
  { text: "College stats, combine, recruiting, PPA, conference tier, and draft capital", options: { fontSize: 13, color: C.dim } },
], {
  x: 0.8, y: 5.0, w: 11.7, h: 1.2,
  fontFace: "Calibri", valign: "top", margin: 0,
  paraSpaceAfter: 6
});

// Bottom bar
slide.addShape(pres.shapes.RECTANGLE, {
  x: 0, y: 7.2, w: 13.3, h: 0.3, fill: { color: C.bgCard }
});
slide.addText("Half-PPR Fantasy PPG  |  Top-2 NFL Seasons  |  April 2025", {
  x: 0.8, y: 7.2, w: 11.7, h: 0.3,
  fontSize: 9, fontFace: "Calibri", color: C.dim, valign: "middle", margin: 0
});

// ═══════════════════════════════════════════════════════════════════════════
// SLIDE 2: Model Performance
// ═══════════════════════════════════════════════════════════════════════════
slide = pres.addSlide();
slide.background = { color: C.bg };

slide.addShape(pres.shapes.RECTANGLE, {
  x: 0, y: 0, w: 13.3, h: 0.06, fill: { color: C.gold }
});

slide.addText("Model Performance", {
  x: 0.8, y: 0.4, w: 11.7, h: 0.7,
  fontSize: 32, fontFace: "Georgia", bold: true, color: C.white, margin: 0
});

slide.addText("Single-stage XGBoost evaluated with time-based cross-validation (train on past, test on future)", {
  x: 0.8, y: 1.05, w: 11.7, h: 0.4,
  fontSize: 12, fontFace: "Calibri", italic: true, color: C.dim, margin: 0
});

// WR card
slide.addShape(pres.shapes.RECTANGLE, {
  x: 0.8, y: 1.7, w: 5.6, h: 3.5, fill: { color: C.bgCard }
});
slide.addShape(pres.shapes.RECTANGLE, {
  x: 0.8, y: 1.7, w: 5.6, h: 0.06, fill: { color: C.blue }
});

slide.addText("Wide Receivers", {
  x: 1.1, y: 1.9, w: 5.0, h: 0.45,
  fontSize: 20, fontFace: "Georgia", bold: true, color: C.blue, margin: 0
});

// WR metrics
const wrMetrics = [
  { label: "Production R²", value: "0.372", desc: "Variance explained" },
  { label: "Production RMSE", value: "2.76", desc: "PPG error" },
  { label: "Bust Classification AUC", value: "0.759", desc: "Separation quality" },
];
wrMetrics.forEach((m, i) => {
  const yOff = 2.6 + i * 0.85;
  slide.addText(m.value, {
    x: 1.3, y: yOff, w: 1.5, h: 0.5,
    fontSize: 28, fontFace: "Georgia", bold: true, color: C.gold, margin: 0
  });
  slide.addText(m.label, {
    x: 3.0, y: yOff, w: 3.0, h: 0.3,
    fontSize: 14, fontFace: "Calibri", bold: true, color: C.white, margin: 0
  });
  slide.addText(m.desc, {
    x: 3.0, y: yOff + 0.28, w: 3.0, h: 0.2,
    fontSize: 10, fontFace: "Calibri", color: C.dim, margin: 0
  });
});

// RB card
slide.addShape(pres.shapes.RECTANGLE, {
  x: 6.9, y: 1.7, w: 5.6, h: 3.5, fill: { color: C.bgCard }
});
slide.addShape(pres.shapes.RECTANGLE, {
  x: 6.9, y: 1.7, w: 5.6, h: 0.06, fill: { color: C.green }
});

slide.addText("Running Backs", {
  x: 7.2, y: 1.9, w: 5.0, h: 0.45,
  fontSize: 20, fontFace: "Georgia", bold: true, color: C.green, margin: 0
});

const rbMetrics = [
  { label: "Production R²", value: "0.289", desc: "Variance explained" },
  { label: "Production RMSE", value: "3.65", desc: "PPG error" },
  { label: "Bust Classification AUC", value: "0.696", desc: "Separation quality" },
];
rbMetrics.forEach((m, i) => {
  const yOff = 2.6 + i * 0.85;
  slide.addText(m.value, {
    x: 7.4, y: yOff, w: 1.5, h: 0.5,
    fontSize: 28, fontFace: "Georgia", bold: true, color: C.gold, margin: 0
  });
  slide.addText(m.label, {
    x: 9.1, y: yOff, w: 3.0, h: 0.3,
    fontSize: 14, fontFace: "Calibri", bold: true, color: C.white, margin: 0
  });
  slide.addText(m.desc, {
    x: 9.1, y: yOff + 0.28, w: 3.0, h: 0.2,
    fontSize: 10, fontFace: "Calibri", color: C.dim, margin: 0
  });
});

// Key features section
slide.addText("Key Model Features", {
  x: 0.8, y: 5.6, w: 11.7, h: 0.4,
  fontSize: 16, fontFace: "Georgia", bold: true, color: C.gold, margin: 0
});

const features = [
  ["Draft Capital", "Log pick number drives 35% of RB and 25% of WR predictions"],
  ["College Production", "Receiving yards, scrimmage yards, rushing stats, touchdowns"],
  ["Receiving Ability (RB)", "RB receiving yards, catch share, pass-usage rate"],
  ["Advanced Metrics", "PPA (predicted points added), usage rates, conference tier"],
  ["Physical Profile", "Combine data, BMI, speed scores with missingness indicators"],
];

features.forEach((f, i) => {
  const xOff = (i % 3) * 4.1 + 0.8;
  const yOff = i < 3 ? 6.15 : 6.75;
  slide.addText(f[0], {
    x: xOff, y: yOff, w: 3.8, h: 0.25,
    fontSize: 11, fontFace: "Calibri", bold: true, color: C.white, margin: 0
  });
  slide.addText(f[1], {
    x: xOff, y: yOff + 0.22, w: 3.8, h: 0.25,
    fontSize: 9, fontFace: "Calibri", color: C.dim, margin: 0
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// PER-YEAR CLASS SLIDES (2021-2026, WR + RB)
// ═══════════════════════════════════════════════════════════════════════════
function addClassSlide(position, year) {
  const s = pres.addSlide();
  s.background = { color: C.bg };

  const posColor = position === "WR" ? C.blue : C.green;
  const yearLabel = year === 2026 ? `${year} (Mock Draft)` : String(year);
  const top = getTop(position, year, 10);
  const hasActual = top.some(r => r.actual_raw_ppg && r.actual_raw_ppg !== "NA" && r.actual_raw_ppg !== "");

  // Top accent
  s.addShape(pres.shapes.RECTANGLE, {
    x: 0, y: 0, w: 13.3, h: 0.06, fill: { color: C.gold }
  });

  // Year badge
  s.addShape(pres.shapes.RECTANGLE, {
    x: 0.8, y: 0.35, w: 1.1, h: 0.5, fill: { color: yearColors[String(year)] }
  });
  s.addText(String(year), {
    x: 0.8, y: 0.35, w: 1.1, h: 0.5,
    fontSize: 18, fontFace: "Georgia", bold: true, color: C.white,
    align: "center", valign: "middle", margin: 0
  });

  // Title
  s.addText(`${position} Class - Top Prospects`, {
    x: 2.1, y: 0.35, w: 10, h: 0.5,
    fontSize: 24, fontFace: "Georgia", bold: true, color: C.white, margin: 0
  });

  s.addText("Ranked by expected fantasy PPG (half-PPR, best 2 of first 3 NFL seasons)", {
    x: 2.1, y: 0.85, w: 10, h: 0.3,
    fontSize: 11, fontFace: "Calibri", italic: true, color: C.dim, margin: 0
  });

  // Table
  const colW = hasActual
    ? [0.5, 3.0, 2.5, 0.6, 0.7, 1.2, 1.3, 1.5]  // with actual
    : [0.5, 3.0, 2.8, 0.6, 0.8, 1.3, 1.5];        // without actual

  const headerTexts = hasActual
    ? ["#", "Name", "College", "Rd", "Pick", "P(Made It)", "Exp PPG", "Actual PPG"]
    : ["#", "Name", "College", "Rd", "Pick", "P(Made It)", "Exp PPG"];

  const headerRow = headerTexts.map(h => ({
    text: h,
    options: {
      fontSize: 10, fontFace: "Calibri", bold: true,
      color: C.gold, fill: { color: C.bg },
      border: [
        { type: "none" }, { type: "none" },
        { pt: 1, color: C.goldDim }, { type: "none" }
      ],
      align: "center", valign: "middle",
      margin: [3, 4, 3, 4]
    }
  }));

  const dataRows = top.map((r, i) => {
    const isOdd = i % 2 === 0;
    const rowFill = isOdd ? C.bgCard : C.bgLight;
    const actual = r.actual_raw_ppg && r.actual_raw_ppg !== "NA" && r.actual_raw_ppg !== ""
      ? parseFloat(r.actual_raw_ppg).toFixed(1) : "—";
    const expPpg = parseFloat(r.exp_ppg);

    // Color the exp PPG
    let ppgColor = C.white;
    if (expPpg >= 8) ppgColor = C.gold;
    else if (expPpg >= 6) ppgColor = C.teal;

    const baseOpts = (align = "center") => ({
      fontSize: 11, fontFace: "Calibri", color: C.white,
      fill: { color: rowFill }, align, valign: "middle",
      border: [{ type: "none" }, { type: "none" }, { pt: 0.5, color: "2A3560" }, { type: "none" }],
      margin: [3, 5, 3, 5]
    });

    const row = [
      { text: String(i + 1), options: { ...baseOpts(), fontSize: 10, color: C.dim } },
      { text: r.name, options: { ...baseOpts("left"), bold: true } },
      { text: r.college, options: { ...baseOpts("left"), color: C.light } },
      { text: r.round, options: baseOpts() },
      { text: r.pick, options: baseOpts() },
      { text: pct(r.p_made_it), options: baseOpts() },
      { text: fmt(r.exp_ppg), options: { ...baseOpts(), bold: true, color: ppgColor } },
    ];

    if (hasActual) {
      let actualColor = C.dim;
      if (actual !== "—") {
        const av = parseFloat(actual);
        if (av >= expPpg) actualColor = C.green;
        else if (av < expPpg * 0.6) actualColor = C.red;
        else actualColor = C.light;
      }
      row.push({ text: actual, options: { ...baseOpts(), color: actualColor, bold: actual !== "—" } });
    }

    return row;
  });

  const tableX = hasActual ? 0.6 : 1.2;
  const tableW = hasActual ? 12.1 : 10.9;
  s.addTable([headerRow, ...dataRows], {
    x: tableX, y: 1.35, w: tableW,
    colW,
    rowH: [0.4, ...Array(dataRows.length).fill(0.45)],
    margin: 0
  });

  // Footer
  s.addShape(pres.shapes.RECTANGLE, {
    x: 0, y: 7.2, w: 13.3, h: 0.3, fill: { color: C.bgCard }
  });

  const posLabel = position === "WR" ? "Wide Receivers" : "Running Backs";
  s.addText(`${posLabel}  |  ${yearLabel}  |  Half-PPR Fantasy Points Per Game`, {
    x: 0.8, y: 7.2, w: 11.7, h: 0.3,
    fontSize: 9, fontFace: "Calibri", color: C.dim, valign: "middle", margin: 0
  });
}

for (const year of [2021, 2022, 2023, 2024, 2025, 2026]) {
  addClassSlide("WR", year);
  addClassSlide("RB", year);
}

// ═══════════════════════════════════════════════════════════════════════════
// SLIDE: WR Class Comparison (Bar Chart)
// ═══════════════════════════════════════════════════════════════════════════
function addComparisonChart(position) {
  const s = pres.addSlide();
  s.background = { color: C.bg };

  s.addShape(pres.shapes.RECTANGLE, {
    x: 0, y: 0, w: 13.3, h: 0.06, fill: { color: C.gold }
  });

  const posColor = position === "WR" ? C.blue : C.green;
  const posLabel = position === "WR" ? "Wide Receiver" : "Running Back";

  s.addText(`${posLabel} Class Comparison: 2021-2026`, {
    x: 0.8, y: 0.35, w: 11.7, h: 0.6,
    fontSize: 28, fontFace: "Georgia", bold: true, color: C.white, margin: 0
  });

  s.addText("Top 5 prospects per class ranked by expected PPG", {
    x: 0.8, y: 0.9, w: 11.7, h: 0.3,
    fontSize: 12, fontFace: "Calibri", italic: true, color: C.dim, margin: 0
  });

  // Build chart data - one series per year
  const chartData = [];
  for (const year of [2021, 2022, 2023, 2024, 2025, 2026]) {
    const top5 = getTop(position, year, 5);
    chartData.push({
      name: String(year),
      labels: top5.map(r => r.name),
      values: top5.map(r => parseFloat(r.exp_ppg))
    });
  }

  s.addChart(pres.charts.BAR, chartData, {
    x: 0.5, y: 1.4, w: 12.3, h: 5.5,
    barDir: "bar",
    barGrouping: "clustered",
    chartColors: Object.values(yearColors),
    catAxisLabelColor: C.light,
    catAxisLabelFontSize: 8,
    valAxisLabelColor: C.dim,
    valAxisLabelFontSize: 8,
    chartArea: { fill: { color: C.bgCard }, roundedCorners: false },
    plotArea: { fill: { color: C.bgCard } },
    valGridLine: { color: "2A3560", size: 0.5 },
    catGridLine: { style: "none" },
    showLegend: true,
    legendPos: "b",
    legendColor: C.light,
    legendFontSize: 9,
    showValue: false,
  });

  s.addShape(pres.shapes.RECTANGLE, {
    x: 0, y: 7.2, w: 13.3, h: 0.3, fill: { color: C.bgCard }
  });
  s.addText(`${posLabel} Class Comparison  |  Expected Fantasy PPG`, {
    x: 0.8, y: 7.2, w: 11.7, h: 0.3,
    fontSize: 9, fontFace: "Calibri", color: C.dim, valign: "middle", margin: 0
  });
}

addComparisonChart("WR");
addComparisonChart("RB");

// ═══════════════════════════════════════════════════════════════════════════
// SLIDE: Class Depth
// ═══════════════════════════════════════════════════════════════════════════
slide = pres.addSlide();
slide.background = { color: C.bg };

slide.addShape(pres.shapes.RECTANGLE, {
  x: 0, y: 0, w: 13.3, h: 0.06, fill: { color: C.gold }
});

slide.addText("Class Depth by Position", {
  x: 0.8, y: 0.35, w: 11.7, h: 0.6,
  fontSize: 28, fontFace: "Georgia", bold: true, color: C.white, margin: 0
});

slide.addText("Players projected above PPG thresholds - measures class-wide talent density", {
  x: 0.8, y: 0.9, w: 11.7, h: 0.3,
  fontSize: 12, fontFace: "Calibri", italic: true, color: C.dim, margin: 0
});

// Build depth table
const depthHeader = ["Year", "Pos", ">= 3 PPG", ">= 4 PPG", ">= 5 PPG", ">= 6 PPG", "Top Prospect", "Best PPG"].map(h => ({
  text: h,
  options: {
    fontSize: 11, fontFace: "Calibri", bold: true,
    color: C.gold, fill: { color: C.bg },
    border: [{ type: "none" }, { type: "none" }, { pt: 1, color: C.goldDim }, { type: "none" }],
    align: "center", valign: "middle", margin: [3, 4, 3, 4]
  }
}));

const depthRows = [];
for (const year of [2021, 2022, 2023, 2024, 2025, 2026]) {
  for (const pos of ["WR", "RB"]) {
    const subset = rows.filter(r => r.position === pos && r.draft_year === String(year));
    const gte3 = subset.filter(r => parseFloat(r.exp_ppg) >= 3).length;
    const gte4 = subset.filter(r => parseFloat(r.exp_ppg) >= 4).length;
    const gte5 = subset.filter(r => parseFloat(r.exp_ppg) >= 5).length;
    const gte6 = subset.filter(r => parseFloat(r.exp_ppg) >= 6).length;
    const best = subset.sort((a, b) => parseFloat(b.exp_ppg) - parseFloat(a.exp_ppg))[0];
    const isOdd = depthRows.length % 2 === 0;
    const rowFill = isOdd ? C.bgCard : C.bgLight;
    const posColor = pos === "WR" ? C.blue : C.green;

    const baseOpts = (align = "center") => ({
      fontSize: 10, fontFace: "Calibri", color: C.white,
      fill: { color: rowFill }, align, valign: "middle",
      border: [{ type: "none" }, { type: "none" }, { pt: 0.5, color: "2A3560" }, { type: "none" }],
      margin: [2, 4, 2, 4]
    });

    depthRows.push([
      { text: year === 2026 ? "2026*" : String(year), options: { ...baseOpts(), bold: true, color: yearColors[String(year)] } },
      { text: pos, options: { ...baseOpts(), bold: true, color: posColor } },
      { text: String(gte3), options: baseOpts() },
      { text: String(gte4), options: baseOpts() },
      { text: String(gte5), options: { ...baseOpts(), bold: true, color: gte5 >= 6 ? C.gold : C.white } },
      { text: String(gte6), options: { ...baseOpts(), bold: true, color: gte6 >= 4 ? C.gold : C.white } },
      { text: best ? best.name : "—", options: { ...baseOpts("left") } },
      { text: best ? fmt(best.exp_ppg) : "—", options: { ...baseOpts(), bold: true, color: C.gold } },
    ]);
  }
}

slide.addTable([depthHeader, ...depthRows], {
  x: 0.6, y: 1.4, w: 12.1,
  colW: [0.8, 0.6, 1.0, 1.0, 1.0, 1.0, 3.5, 1.0],
  rowH: [0.4, ...Array(depthRows.length).fill(0.38)],
  margin: 0
});

slide.addShape(pres.shapes.RECTANGLE, {
  x: 0, y: 7.2, w: 13.3, h: 0.3, fill: { color: C.bgCard }
});
slide.addText("* 2026 based on mock draft data  |  PPG = Half-PPR Fantasy Points Per Game", {
  x: 0.8, y: 7.2, w: 11.7, h: 0.3,
  fontSize: 9, fontFace: "Calibri", color: C.dim, valign: "middle", margin: 0
});

// ═══════════════════════════════════════════════════════════════════════════
// SLIDE: Key Takeaways
// ═══════════════════════════════════════════════════════════════════════════
slide = pres.addSlide();
slide.background = { color: C.bg };

slide.addShape(pres.shapes.RECTANGLE, {
  x: 0, y: 0, w: 13.3, h: 0.06, fill: { color: C.gold }
});

slide.addText("Key Takeaways", {
  x: 0.8, y: 0.4, w: 11.7, h: 0.7,
  fontSize: 32, fontFace: "Georgia", bold: true, color: C.white, margin: 0
});

// Compute takeaway stats
const bestWR = rows.filter(r => r.position === "WR").sort((a, b) => parseFloat(b.exp_ppg) - parseFloat(a.exp_ppg))[0];
const bestRB = rows.filter(r => r.position === "RB").sort((a, b) => parseFloat(b.exp_ppg) - parseFloat(a.exp_ppg))[0];

// Deepest WR class (>= 5 ppg)
const wrDepth = {};
for (const yr of [2021,2022,2023,2024,2025,2026]) {
  wrDepth[yr] = rows.filter(r => r.position === "WR" && r.draft_year === String(yr) && parseFloat(r.exp_ppg) >= 5).length;
}
const deepestWRYear = Object.entries(wrDepth).sort((a, b) => b[1] - a[1])[0];

const rbDepth = {};
for (const yr of [2021,2022,2023,2024,2025,2026]) {
  rbDepth[yr] = rows.filter(r => r.position === "RB" && r.draft_year === String(yr) && parseFloat(r.exp_ppg) >= 5).length;
}
const deepestRBYear = Object.entries(rbDepth).sort((a, b) => b[1] - a[1])[0];

// Takeaway cards
const takeaways = [
  {
    title: `Top-Projected WR: ${bestWR.name} (${bestWR.draft_year})`,
    detail: `${fmt(bestWR.exp_ppg)} expected PPG — ${bestWR.college}, Round ${bestWR.round} Pick ${bestWR.pick}`,
    color: C.blue
  },
  {
    title: `Top-Projected RB: ${bestRB.name} (${bestRB.draft_year})`,
    detail: `${fmt(bestRB.exp_ppg)} expected PPG — ${bestRB.college}, Round ${bestRB.round} Pick ${bestRB.pick}`,
    color: C.green
  },
  {
    title: `Deepest WR Class (>= 5 PPG): ${deepestWRYear[0]}`,
    detail: `${deepestWRYear[1]} prospects projected above 5.0 PPG threshold`,
    color: C.blue
  },
  {
    title: `Deepest RB Class (>= 5 PPG): ${deepestRBYear[0]}`,
    detail: `${deepestRBYear[1]} prospects projected above 5.0 PPG threshold`,
    color: C.green
  },
  {
    title: "2025 RB Class is Historically Strong",
    detail: "Henderson, Hampton, Jeanty, and Judkins all project above 7.5 PPG",
    color: C.gold
  },
  {
    title: "2026 Mock: Jeremiyah Love Leads RBs",
    detail: `${fmt(rows.filter(r => r.name === "Jeremiyah Love")[0]?.exp_ppg || 0)} projected PPG from Notre Dame — highest RB prospect in the 2026 mock class`,
    color: "9B59B6"
  },
];

takeaways.forEach((t, i) => {
  const col = i % 2;
  const row = Math.floor(i / 2);
  const x = col * 5.9 + 0.8;
  const y = row * 1.65 + 1.5;

  // Card background
  slide.addShape(pres.shapes.RECTANGLE, {
    x, y, w: 5.5, h: 1.35, fill: { color: C.bgCard }
  });
  // Left accent
  slide.addShape(pres.shapes.RECTANGLE, {
    x, y, w: 0.06, h: 1.35, fill: { color: t.color }
  });

  slide.addText(t.title, {
    x: x + 0.25, y: y + 0.15, w: 5.0, h: 0.4,
    fontSize: 14, fontFace: "Calibri", bold: true, color: C.white, margin: 0
  });
  slide.addText(t.detail, {
    x: x + 0.25, y: y + 0.6, w: 5.0, h: 0.5,
    fontSize: 11, fontFace: "Calibri", color: C.light, margin: 0
  });
});

// Model notes at bottom
slide.addText([
  { text: "Model: ", options: { bold: true, color: C.gold } },
  { text: "Single-stage XGBoost regression  |  Empirical Bayes shrinkage (k=16)  |  Games-weighted PPG  |  6-game min  |  Time-based CV", options: { color: C.dim } },
], {
  x: 0.8, y: 6.6, w: 11.7, h: 0.3,
  fontSize: 10, fontFace: "Calibri", margin: 0
});

slide.addShape(pres.shapes.RECTANGLE, {
  x: 0, y: 7.2, w: 13.3, h: 0.3, fill: { color: C.bgCard }
});
slide.addText("NFL Draft Prospects 2021-2026  |  College to NFL Production Model", {
  x: 0.8, y: 7.2, w: 11.7, h: 0.3,
  fontSize: 9, fontFace: "Calibri", color: C.dim, valign: "middle", margin: 0
});

// ═══════════════════════════════════════════════════════════════════════════
// SAVE
// ═══════════════════════════════════════════════════════════════════════════
pres.writeFile({ fileName: "output/NFL_Draft_Prospects_2021_2026.pptx" })
  .then(() => console.log("Saved: output/NFL_Draft_Prospects_2021_2026.pptx"))
  .catch(err => console.error("Error:", err));

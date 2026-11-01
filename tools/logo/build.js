// Builds the DataEditR hex logo (text outlined with Inter) and writes the SVG.
// npm install opentype.js @fontsource/inter
// node build.js ../../man/figures/logo.svg
// then: rsvg-convert -w 860 -h 990 ../../man/figures/logo.svg -o ../../man/figures/logo.png
// DataEditR logo: a zoomed-in table filling the hexagon (cut by its edge),
// with "DataEditR" typed into the cell being edited.
const fs = require("fs");
const path = require("path");
const opentype = require("opentype.js");
const fontFile = (w) => require.resolve(`@fontsource/inter/files/inter-latin-${w}-normal.woff`);
const F = { 500: opentype.loadSync(fontFile(500)), 600: opentype.loadSync(fontFile(600)),
            700: opentype.loadSync(fontFile(700)) };
const out = process.argv[2] || "logo.svg";
const r2 = (n) => +n.toFixed(2);

// text as outlined path; align "left" | "right" | "center"
function textPath(str, x, baseline, size, weight, fill, align = "left", spacing = 0) {
  const font = F[weight];
  const glyphs = font.stringToGlyphs(str);
  let w = 0; const pos = [];
  glyphs.forEach((g, i) => {
    pos.push(w);
    w += g.advanceWidth * size / font.unitsPerEm;
    if (i < glyphs.length - 1) w += font.getKerningValue(g, glyphs[i + 1]) * size / font.unitsPerEm + spacing;
  });
  const x0 = align === "right" ? x - w : align === "center" ? x - w / 2 : x;
  const d = glyphs.map((g, i) => g.getPath(x0 + pos[i], baseline, size).toPathData(1)).join("");
  return { svg: `<path d="${d}" fill="${fill}"/>`, width: w, x0 };
}

// hexagon
const outer = "200,0 400,115.5 400,346.5 200,462 0,346.5 0,115.5";
const inner = "200,16 386,123.4 386,338.6 200,446 14,338.6 14,123.4";
const C = { navy: "#1E3A8A", blue: "#1D4ED8", ink: "#0F172A", line: "#DCE3EC",
            head: "#EEF3FB", rowhead: "#F6F8FB", value: "#8A97A8", badge: "#DBEAFE" };

// grid: row-number column, then data columns (the last one cut by the edge)
const rnX = 6, rnW = 40;
const colW = 66;
const cols = ["mpg", "cyl", "disp", "hp", "drat", "wt"];
const colX = cols.map((_, i) => rnX + rnW + i * colW);
const right = colX[colX.length - 1] + colW;
const rowH = 36, editH = 74;
const editY = 231 - editH / 2;                 // the edited row is centred
const headY = editY - 2 * rowH;                // header, one row, then the edited row
const data = [
  [21, 6, 160, 110, 3.9, 2.62], null,
  [22.8, 4, 108, 93, 3.85, 2.32], [21.4, 6, 258, 110, 3.08, 3.215],
  [18.7, 8, 360, 175, 3.15, 3.44], [18.1, 6, 225, 105, 2.76, 3.46],
  [14.3, 8, 360, 245, 3.21, 3.57], [24.4, 4, 146.7, 62, 3.69, 3.19]
];
const rows = [];
let y = headY + rowH;
data.forEach((v, i) => {
  const h = v === null ? editH : rowH;
  rows.push({ y, h, n: i + 1, v });
  y += h;
});
const bottom = y;
const edit = rows.find((r) => r.v === null);
const fs14 = 14, mid14 = 0.36 * fs14;          // baseline offset to centre digits

const parts = [];
parts.push(`<defs><clipPath id="hex"><polygon points="${inner}"/></clipPath></defs>`);
parts.push(`<polygon points="${outer}" fill="${C.navy}"/>`);
parts.push(`<g clip-path="url(#hex)">`);
parts.push(`<rect x="0" y="0" width="400" height="462" fill="#FFFFFF"/>`);
// app header above the table: data name and size
parts.push(`<rect x="0" y="0" width="400" height="${headY}" fill="#F1F4F9"/>`);
{
  const ty = headY - 26;
  const name = textPath("mtcars", 0, 0, 18, 600, C.navy);
  const dims = textPath("32 × 11", 0, 0, 12, 500, "#475569");
  const pillW = dims.width + 16, gapW = 8;
  const total = name.width + gapW + pillW;
  const x0 = 200 - total / 2;
  parts.push(textPath("mtcars", x0, ty + 0.36 * 18, 18, 600, C.navy).svg);
  const px = x0 + name.width + gapW;
  parts.push(`<rect x="${r2(px)}" y="${ty - 10}" width="${r2(pillW)}" height="20" rx="10" fill="#E2E8F0"/>`);
  parts.push(textPath("32 × 11", px + pillW / 2, ty + 0.36 * 12, 12, 500, "#475569", "center").svg);
}
// header row and row-number column
parts.push(`<rect x="0" y="${headY}" width="400" height="${rowH}" fill="${C.head}"/>`);
parts.push(`<rect x="0" y="${headY + rowH}" width="${rnX + rnW}" height="${bottom - headY - rowH}" fill="${C.rowhead}"/>`);
parts.push(`<rect x="0" y="${edit.y}" width="${rnX + rnW}" height="${edit.h}" fill="${C.badge}"/>`);
// grid lines
const gl = [`M0 ${headY}H400`];
rows.forEach((r) => gl.push(`M0 ${r.y}H400`));
gl.push(`M0 ${bottom}H400`);
[rnX + rnW, ...colX.slice(1), right].forEach((x) => gl.push(`M${x} ${headY}V462`));
parts.push(`<path d="${gl.join("")}" stroke="${C.line}" stroke-width="1.8"/>`);
// header: type badge + column name
cols.forEach((name, i) => {
  const cx = colX[i] + 7;
  const by = headY + rowH / 2;
  parts.push(`<rect x="${cx}" y="${by - 7}" width="23" height="14" rx="4" fill="${C.badge}"/>`);
  parts.push(textPath("123", cx + 11.5, by + 3.2, 9, 600, C.blue, "center").svg);
  parts.push(textPath(name, cx + 28, by + mid14, fs14, 600, C.navy).svg);
});
// rows: numbers and values
rows.forEach((r) => {
  const mid = r.y + r.h / 2;
  const nx = rnX + rnW - 9;
  if (r.v === null) {
    parts.push(textPath(String(r.n), nx, mid + mid14, fs14, 700, C.blue, "right").svg);
    return;
  }
  parts.push(textPath(String(r.n), nx, mid + mid14, fs14, 500, C.value, "right").svg);
  r.v.forEach((val, i) => {
    parts.push(textPath(String(val), colX[i] + colW - 9, mid + mid14, fs14, 500, C.value, "right").svg);
  });
});
// the cell being edited, inside the hexagon, with the name typed into it
const box = { x: rnX + rnW + 6, y: edit.y + 5, h: edit.h - 10, r: 10 };
box.w = 358 - box.x;
parts.push(`<rect x="${box.x}" y="${box.y + 5}" width="${box.w}" height="${box.h}" rx="${box.r}" fill="#0B1F5C" opacity="0.14"/>`);
parts.push(`<rect x="${box.x}" y="${box.y}" width="${box.w}" height="${box.h}" rx="${box.r}" fill="#FFFFFF" stroke="${C.blue}" stroke-width="4"/>`);
const size = 56, gap = 6, caretW = 4.5;
const probe = textPath("DataEditR", 0, 0, size, 700, C.ink, "left", -1);
// the word is centred on the hexagon, horizontally and vertically
const tx = 200 - probe.width / 2;
const base = 231 + 0.727 * size / 2;
parts.push(textPath("DataEdit", tx, base, size, 700, C.ink, "left", -1).svg);
const rW = textPath("R", 0, 0, size, 700, C.blue).width;
parts.push(textPath("R", tx + probe.width - rW, base, size, 700, C.blue).svg);
const caretX = tx + probe.width + gap;
parts.push(`<rect x="${r2(caretX)}" y="${r2(231 - 0.42 * size)}" width="${caretW}" height="${r2(0.84 * size)}" rx="2.25" fill="${C.blue}"/>`);
parts.push(`</g>`);
// inner edge of the border
parts.push(`<polygon points="${inner}" fill="none" stroke="#14306F" stroke-width="2"/>`);
console.log("box", box, "text", r2(tx), r2(tx + probe.width), "caret end", r2(caretX + caretW), "header", headY);

fs.writeFileSync(out, `<svg xmlns="http://www.w3.org/2000/svg" width="400" height="462" viewBox="0 0 400 462" role="img" aria-label="DataEditR">\n<title>DataEditR</title>\n${parts.join("\n")}\n</svg>\n`);

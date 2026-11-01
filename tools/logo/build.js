// Builds man/figures/logo.svg (text outlined with Inter Bold).
// npm install opentype.js @fontsource/inter && node build.js
// PNGs: rsvg-convert -w 860 -h 990 ../../man/figures/logo.svg -o ../../man/figures/logo.png
// DataEditR logo: the name typed into a cell of a table centred in the hexagon
const fs = require("fs");
const opentype = require("opentype.js");
const font = opentype.loadSync(require.resolve("@fontsource/inter/files/inter-latin-700-normal.woff"));

// table geometry (viewBox 400 x 462, hexagon centre 200,231)
const T = { x: 55, y: 126, w: 290, h: 210, r: 14 };
const head = 30, foot = 30, rowH = 50, rhW = 34;
const colW = (T.w - rhW) / 4;
const xs = [0, 1, 2, 3, 4].map((i) => T.x + rhW + i * colW);         // data column edges
const ys = [T.y + head, T.y + head + rowH, T.y + head + 2 * rowH, T.y + head + 3 * rowH];
const f = (n) => +n.toFixed(2);
const out = [];
out.push('<polygon points="200,0 400,115.5 400,346.5 200,462 0,346.5 0,115.5" fill="#1E3A8A"/>');
out.push('<polygon points="200,16.2 386,123.6 386,338.4 200,445.8 14,338.4 14,123.6" fill="#1D4ED8"/>');
// soft shadow under the table
out.push(`<rect x="${T.x}" y="${T.y + 6}" width="${T.w}" height="${T.h}" rx="${T.r}" fill="#0B1F5C" opacity="0.28"/>`);
out.push(`<rect x="${T.x}" y="${T.y}" width="${T.w}" height="${T.h}" rx="${T.r}" fill="#FFFFFF"/>`);
// header and footer bands
const r = T.r, x0 = T.x, x1 = T.x + T.w, y0 = T.y, y1 = T.y + T.h;
out.push(`<path d="M${x0} ${y0 + head}V${y0 + r}a${r} ${r} 0 0 1 ${r}-${r}h${T.w - 2 * r}a${r} ${r} 0 0 1 ${r} ${r}v${head - r}z" fill="#EFF5FF"/>`);
out.push(`<path d="M${x0} ${y1 - foot}h${T.w}v${foot - r}a${r} ${r} 0 0 1-${r} ${r}H${x0 + r}a${r} ${r} 0 0 1-${r}-${r}z" fill="#F3F6FB"/>`);
// row header column (body rows), active row highlighted
out.push(`<rect x="${x0}" y="${ys[0]}" width="${rhW}" height="${3 * rowH}" fill="#F3F6FB"/>`);
out.push(`<rect x="${x0}" y="${ys[1]}" width="${rhW}" height="${rowH}" fill="#DBEAFE"/>`);
// grid lines
const lines = [];
ys.forEach((y) => lines.push(`M${x0} ${y}h${T.w}`));
xs.slice(0, 4).forEach((x) => lines.push(`M${f(x)} ${y0}V${ys[3]}`));
out.push(`<path d="${lines.join("")}" stroke="#DBEAFE" stroke-width="2.5"/>`);
// header pills (column names)
xs.slice(0, 4).forEach((x) => out.push(`<rect x="${f(x + colW / 2 - 16)}" y="${y0 + 10}" width="32" height="10" rx="5" fill="#BFDBFE"/>`));
// row numbers
[0, 2].forEach((i) => out.push(`<rect x="${x0 + 10}" y="${ys[i] + rowH / 2 - 4.5}" width="14" height="9" rx="4.5" fill="#CBD5E1"/>`));
out.push(`<rect x="${x0 + 10}" y="${ys[1] + rowH / 2 - 4.5}" width="14" height="9" rx="4.5" fill="#1D4ED8"/>`);
// values in rows 1 and 3 (right aligned like numbers)
const widths = [[22, 30, 18, 26], [28, 20, 30, 22]];
[0, 2].forEach((ri, k) => {
  xs.slice(0, 4).forEach((x, c) => {
    const w = widths[k][c];
    out.push(`<rect x="${f(x + colW - 10 - w)}" y="${ys[ri] + rowH / 2 - 4.5}" width="${w}" height="9" rx="4.5" fill="#E6ECF5"/>`);
  });
});
// footer: + Add row
const fy = y1 - foot / 2;
out.push(`<path d="M${x0 + 18} ${fy - 6}v12M${x0 + 12} ${fy}h12" stroke="#1D4ED8" stroke-width="3" stroke-linecap="round"/>`);
out.push(`<rect x="${x0 + 34}" y="${fy - 4}" width="42" height="8" rx="4" fill="#BFDBFE"/>`);
// the cell being edited: spans the data columns of the middle row
const E = { x: xs[0] + 4, y: 231 - 31, w: xs[4] - xs[0] - 8, h: 62, r: 8 };
out.push(`<rect x="${f(E.x)}" y="${E.y + 5}" width="${f(E.w)}" height="${E.h}" rx="${E.r}" fill="#0B1F5C" opacity="0.16"/>`);
out.push(`<rect x="${f(E.x)}" y="${E.y}" width="${f(E.w)}" height="${E.h}" rx="${E.r}" fill="#FFFFFF" stroke="#1D4ED8" stroke-width="4"/>`);
// typed name with caret, centred in the cell
const text = "DataEditR", size = 44, spacing = -0.8;
const glyphs = font.stringToGlyphs(text);
let x = 0; const pos = [];
glyphs.forEach((g, i) => {
  pos.push(x);
  x += g.advanceWidth * size / font.unitsPerEm;
  if (i < glyphs.length - 1) x += font.getKerningValue(g, glyphs[i + 1]) * size / font.unitsPerEm + spacing;
});
const tw = x, gap = 5, caretW = 4;
const total = tw + gap + caretW;
const tx = E.x + E.w / 2 - total / 2;
const cap = 0.727 * size;
const baseline = 231 + cap / 2;
const paths = glyphs.map((g, i) => {
  const d = g.getPath(tx + pos[i], baseline, size).toPathData(2);
  return `<path d="${d}" fill="${i === glyphs.length - 1 ? "#1D4ED8" : "#0F172A"}"/>`;
});
out.push(...paths);
out.push(`<rect x="${f(tx + tw + gap)}" y="${f(231 - 22)}" width="${caretW}" height="44" rx="2" fill="#1D4ED8"/>`);
const inner = out.join("\n");

fs.writeFileSync("../../man/figures/logo.svg", `<svg xmlns="http://www.w3.org/2000/svg" width="400" height="462" viewBox="0 0 400 462" role="img" aria-label="DataEditR">\n<title>DataEditR</title>\n${inner}\n</svg>\n`);
console.log("text width", tw.toFixed(1), "start", tx.toFixed(1), "cell", E.x, E.w);

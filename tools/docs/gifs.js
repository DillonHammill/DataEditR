// Animated demos for the vignettes and README.
// node gifs.js <scenario> <port> <outdir>
// Each recording writes <outdir>/<name>-frames/ (PNG frames + ffmpeg concat list).
const fs = require("fs");
const puppeteer = require("puppeteer-core");
const [scenario, port, out] = process.argv.slice(2);
const G = "edit1-grid";
const W = 1000, H = 600;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

async function launch() {
  const browser = await puppeteer.launch({
    executablePath: process.env.CHROME_PATH || "/usr/bin/google-chrome",
    headless: true,
    args: ["--no-sandbox", "--disable-gpu", `--window-size=${W},${H}`, "--font-render-hinting=none"],
    defaultViewport: { width: W, height: H, deviceScaleFactor: 1 },
  });
  const page = await browser.newPage();
  page.on("pageerror", (e) => console.log("PAGE ERROR " + e.message));
  return { browser, page };
}

async function open(page) {
  for (let i = 0; i < 60; i++) {
    try {
      await page.goto(`http://127.0.0.1:${port}`, { waitUntil: "networkidle2", timeout: 5000 });
      break;
    } catch (e) { await sleep(1000); }
  }
  await page.waitForFunction((g) => document.getElementById(g) && document.getElementById(g).__editr &&
    document.getElementById(g).__editr.ready, { timeout: 30000 }, G);
  await sleep(600);
  await decorate(page);
  await page.mouse.move(W / 2, H / 2);
}

// fake pointer and a caption pill (headless screenshots show neither)
async function decorate(page) {
  await page.evaluate(() => {
    const c = document.createElement("div");
    c.id = "fake-pointer";
    c.style.cssText = "position:fixed;z-index:2147483647;width:20px;height:20px;margin:-2px 0 0 -2px;" +
      "pointer-events:none;left:-50px;top:-50px;transition:transform 80ms;" +
      "background:no-repeat url(\"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='20' height='20'>" +
      "<path d='M2 1l14 9-6.5 1.6L6 18.5z' fill='black' stroke='white' stroke-width='1.6'/></svg>\")";
    document.body.appendChild(c);
    document.addEventListener("mousemove", (e) => { c.style.left = e.clientX + "px"; c.style.top = e.clientY + "px"; }, true);
    document.addEventListener("mousedown", () => { c.style.transform = "scale(0.8)"; }, true);
    document.addEventListener("mouseup", () => { c.style.transform = ""; }, true);
    const cap = document.createElement("div");
    cap.id = "demo-caption";
    cap.style.cssText = "position:fixed;z-index:2147483646;left:50%;bottom:58px;transform:translateX(-50%);" +
      "background:rgba(15,23,42,0.88);color:#fff;font:600 15px Inter,system-ui,sans-serif;" +
      "padding:8px 16px;border-radius:999px;pointer-events:none;white-space:nowrap;opacity:0;transition:opacity 150ms;";
    document.body.appendChild(cap);
  });
}

async function caption(page, text) {
  await page.evaluate((t) => {
    const c = document.getElementById("demo-caption");
    if (!c) return;
    c.textContent = t || "";
    c.style.opacity = t ? "1" : "0";
  }, text);
  await sleep(text ? 500 : 150);
}

async function settle(page, ms = 600) {
  await sleep(ms);
  await page.waitForFunction(() => !document.documentElement.classList.contains("shiny-busy"), { timeout: 10000 });
  await sleep(150);
}

const T = (page, fn, ...a) => page.evaluate(fn, G, ...a);

async function cell(page, id, field) {
  return page.evaluate(async (g, id, field) => {
    const t = document.getElementById(g).__editr.table;
    // scrollToRow() never settles when the row is already on screen
    await Promise.race([t.scrollToRow(id, "nearest", false).catch(() => null),
                        new Promise((r) => setTimeout(r, 400))]);
    const b = t.getRow(id).getCell(field).getElement().getBoundingClientRect();
    return { x: b.x + b.width / 2, y: b.y + b.height / 2, r: b.right, w: b.width };
  }, G, id, field);
}

async function header(page, field) {
  return page.evaluate((g, field) => {
    const t = document.getElementById(g).__editr.table;
    const el = t.getColumn(field).getElement();
    const title = el.querySelector(".editr-title") || el.querySelector(".tabulator-col-title");
    const b = title.getBoundingClientRect();
    const c = el.getBoundingClientRect();
    return { x: b.x + b.width / 2, y: b.y + b.height / 2, right: c.right, top: c.top, h: c.height };
  }, G, field);
}

async function filterBox(page, field, which = 0) {
  return page.evaluate((g, field, which) => {
    const t = document.getElementById(g).__editr.table;
    const els = t.getColumn(field).getElement().querySelectorAll(".tabulator-header-filter input");
    const b = els[which].getBoundingClientRect();
    return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
  }, G, field, which);
}

async function el(page, selector) {
  return page.evaluate((s) => {
    const e = document.querySelector(s);
    const b = e.getBoundingClientRect();
    return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
  }, selector);
}

async function byText(page, selector, text) {
  return page.evaluate((s, t) => {
    const e = [...document.querySelectorAll(s)].find((x) => x.textContent.trim().startsWith(t));
    const b = e.getBoundingClientRect();
    return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
  }, selector, text);
}

async function moveTo(page, p, steps = 14) {
  await page.mouse.move(p.x, p.y, { steps });
  await sleep(120);
}

async function clickAt(page, p, opts = {}) {
  await moveTo(page, p);
  await page.mouse.click(p.x, p.y, opts);
}

// the "+" in the corner above the row numbers, then "Add row" or "Add column"
async function addFromCorner(page, what) {
  await clickAt(page, await el(page, ".editr-add-btn"));
  await sleep(500);
  await clickAt(page, await el(page, `.editr-add-menu [data-add="${what}"]`));
}

async function type(page, text, delay = 85) {
  for (const ch of text) { await page.keyboard.type(ch); await sleep(delay); }
}

async function chord(page, mod, key) {
  await page.keyboard.down(mod);
  await sleep(120);
  await page.keyboard.press(key);
  await sleep(120);
  await page.keyboard.up(mod);
}

async function editCell(page, id, field, text) {
  const p = await cell(page, id, field);
  await clickAt(page, p, { clickCount: 2 });
  await page.waitForSelector(".tabulator-cell.tabulator-editing input, .tabulator-cell.tabulator-editing select", { timeout: 3000 });
  await chord(page, "Control", "KeyA");
  await type(page, text);
  await sleep(150);
  await page.keyboard.press("Enter");
  await settle(page, 500);
}

async function menu(page, label) {
  await page.waitForSelector(".tabulator-menu");
  await sleep(400);
  const p = await byText(page, ".tabulator-menu .tabulator-menu-item", label);
  await clickAt(page, p);
  await settle(page, 500);
}

async function rename(page, field, name) {
  const p = await header(page, field);
  await clickAt(page, p, { clickCount: 2 });
  await page.waitForSelector("input.editr-rename");
  await chord(page, "Control", "KeyA");
  await type(page, name);
  await page.keyboard.press("Enter");
  await settle(page, 500);
}

async function nextId(page) {
  return T(page, (g) => document.getElementById(g).__editr.nextId - 1);
}

// record frames while actions run
async function record(page, name, actions) {
  const dir = `${out}/${name}-frames`;
  fs.rmSync(dir, { recursive: true, force: true });
  fs.mkdirSync(dir, { recursive: true });
  let on = true, i = 0;
  const times = [];
  const loop = (async () => {
    while (on) {
      const f = `${dir}/${String(i).padStart(4, "0")}.png`;
      await page.screenshot({ path: f });
      times.push([f, Date.now()]);
      i += 1;
      await sleep(30);
    }
  })();
  await sleep(800);
  await actions();
  await sleep(1400);
  on = false;
  await loop;
  const lines = [];
  for (let k = 0; k < times.length; k++) {
    const d = k + 1 < times.length ? (times[k + 1][1] - times[k][1]) / 1000 : 2;
    lines.push(`file '${times[k][0]}'`, `duration ${d.toFixed(3)}`);
  }
  lines.push(`file '${times[times.length - 1][0]}'`);
  fs.writeFileSync(`${dir}/list.txt`, lines.join("\n"));
  console.log(`recorded ${name}: ${times.length} frames`);
}

async function reload(page) {
  await page.reload({ waitUntil: "networkidle2" });
  await page.waitForFunction((g) => document.getElementById(g) && document.getElementById(g).__editr &&
    document.getElementById(g).__editr.ready, { timeout: 30000 }, G);
  await sleep(600);
  // the changes of the last segment were kept as a draft: start afresh
  const discard = await page.$("#edit1-draft_discard");
  if (discard) {
    await discard.click();
    await page.waitForFunction(() => !document.querySelector(".modal.show"), { timeout: 10000 });
    await sleep(600);
  }
  await decorate(page);
  await page.mouse.move(W / 2, H / 2);
}

const scenarios = {
  // mtcars, no tracking: the editing features
  async editing(page) {
    await record(page, "edit-cells", async () => {
      await caption(page, "Double-click a cell to edit it");
      await editCell(page, 1, "c2", "22.5");
      await editCell(page, 2, "c4", "165");
      await caption(page, "Text in a number column asks what to do");
      const p = await cell(page, 3, "c2");
      await clickAt(page, p, { clickCount: 2 });
      await page.waitForSelector(".tabulator-cell.tabulator-editing input");
      await chord(page, "Control", "KeyA");
      await type(page, "unknown");
      await page.keyboard.press("Enter");
      await page.waitForSelector(".editr-guard");
      await sleep(1200);
      await clickAt(page, await el(page, '.editr-guard [data-act="na"]'));
      await settle(page, 600);
      await caption(page, "");
    });
    await reload(page);
    await record(page, "ranges", async () => {
      await caption(page, "Drag to select a range: the status bar sums it");
      let p = await cell(page, 2, "c3");
      await moveTo(page, p);
      await page.mouse.down();
      const q = await cell(page, 7, "c5");
      await page.mouse.move(q.x, q.y, { steps: 18 });
      await page.mouse.up();
      await settle(page, 1200);
      await caption(page, "Ctrl+D fills down from the first row");
      await chord(page, "Control", "KeyD");
      await settle(page, 1200);
      await caption(page, "Delete clears the selection");
      p = await cell(page, 5, "c3");
      await clickAt(page, p);
      await page.keyboard.down("Shift");
      await clickAt(page, await cell(page, 6, "c5"));
      await page.keyboard.up("Shift");
      await sleep(400);
      await page.keyboard.press("Delete");
      await settle(page, 1000);
      await caption(page, "Ctrl+Z and Ctrl+Y undo and redo");
      await chord(page, "Control", "KeyZ");
      await settle(page, 900);
      await chord(page, "Control", "KeyZ");
      await settle(page, 900);
      await chord(page, "Control", "KeyY");
      await settle(page, 900);
      await caption(page, "");
    });
    await reload(page);
    await record(page, "rows-columns", async () => {
      await caption(page, "Right-click a row to insert or delete rows");
      await clickAt(page, await cell(page, 3, "c2"), { button: "right" });
      await menu(page, "Insert row below");
      let id = await nextId(page);
      await editCell(page, id, "c1", "My car");
      await editCell(page, id, "c2", "26");
      await clickAt(page, await cell(page, 5, "c2"), { button: "right" });
      await menu(page, "Delete row");
      await caption(page, "Right-click a header for column options");
      await clickAt(page, await header(page, "c4"), { button: "right" });
      await menu(page, "Insert column right");
      await sleep(400);
      await caption(page, "Or add a row at the end with the + above the row numbers");
      await addFromCorner(page, "row");
      await settle(page, 700);
      id = await nextId(page);
      await editCell(page, id, "c1", "Another car");
      await caption(page, "");
    });
    await reload(page);
    await record(page, "rename-resize", async () => {
      await caption(page, "Double-click a column name to rename it");
      await rename(page, "c2", "miles_per_gallon");
      await caption(page, "Edit row names like any other cell");
      await editCell(page, 1, "c1", "Mazda RX4 (2024)");
      await caption(page, "Drag a header edge to resize a column");
      const h = await header(page, "c2");
      await moveTo(page, { x: h.right - 2, y: h.top + 14 });
      await page.mouse.down();
      await page.mouse.move(h.right + 70, h.top + 14, { steps: 20 });
      await page.mouse.up();
      await settle(page, 800);
      await caption(page, "");
    });
    await reload(page);
    await record(page, "find", async () => {
      await caption(page, "Ctrl+F finds values; Enter jumps to the next match");
      await clickAt(page, await cell(page, 1, "c3"));
      await chord(page, "Control", "KeyF");
      await sleep(300);
      await type(page, "Merc", 120);
      await settle(page, 900);
      for (let k = 0; k < 3; k++) {
        await page.keyboard.press("Enter");
        await sleep(700);
      }
      await page.keyboard.press("Escape");
      await sleep(400);
      await caption(page, "");
    });
  },

  // mtcars: filters, columns, done/export, open
  async filtering(page) {
    await record(page, "filter-rows", async () => {
      await caption(page, "Type under a header to filter rows");
      await clickAt(page, await filterBox(page, "c1"));
      await type(page, "Merc", 120);
      await settle(page, 1200);
      await caption(page, "Numbers filter by range");
      await clickAt(page, await filterBox(page, "c2", 0));
      await type(page, "17");
      await page.keyboard.press("Tab");
      await settle(page, 1200);
      await caption(page, "Columns with few values offer a list");
      await clickAt(page, await filterBox(page, "c3"));
      await page.waitForSelector(".tabulator-edit-list .tabulator-edit-list-item");
      await sleep(600);
      for (const v of ["6", "8"]) {
        await clickAt(page, await byText(page, ".tabulator-edit-list .tabulator-edit-list-item", v));
        await sleep(500);
      }
      await sleep(400);
      await moveTo(page, { x: W - 40, y: H - 90 });
      await page.mouse.click(W - 40, H - 90);
      await settle(page, 1200);
      await caption(page, "Filters show as chips; remove them there");
      await clickAt(page, await el(page, ".editr-chip-clear"));
      await settle(page, 900);
      await caption(page, "");
    });
    await reload(page);
    await record(page, "select-columns", async () => {
      await caption(page, "Show or hide columns from the Select menu");
      await clickAt(page, await el(page, "#edit1-columns"));
      await page.waitForSelector(".editr-columns-menu");
      await sleep(500);
      for (const name of ["disp", "drat", "qsec", "vs", "am"]) {
        await clickAt(page, await byText(page, ".editr-columns-list label", name));
        await sleep(350);
      }
      await sleep(800);
      await page.mouse.click(W - 30, H - 30);
      await settle(page, 600);
      await caption(page, "Or hide one from its header menu");
      await clickAt(page, await header(page, "c12"), { button: "right" });
      await menu(page, "Hide column");
      await sleep(700);
      await caption(page, "Show all brings them back");
      await clickAt(page, await el(page, "#edit1-columns"));
      await page.waitForSelector(".editr-columns-menu");
      await sleep(500);
      await clickAt(page, await byText(page, ".editr-columns-menu button, .editr-columns-menu a", "Show all"));
      await sleep(700);
      await page.mouse.click(W - 30, H - 30);
      await settle(page, 600);
      await caption(page, "");
    });
    await reload(page);
    await record(page, "done-export", async () => {
      await caption(page, "Filter the data you want to keep");
      await clickAt(page, await filterBox(page, "c2", 0));
      await type(page, "25");
      await page.keyboard.press("Tab");
      await settle(page, 1000);
      await caption(page, "Done can return only what's on screen");
      await clickAt(page, await el(page, "#done_toggle"));
      await sleep(700);
      await clickAt(page, await el(page, 'input[name="return_what"][value="view"]'));
      await sleep(1200);
      await clickAt(page, await el(page, "#done_toggle"));
      await sleep(400);
      await caption(page, "Export downloads all data or only what's on screen");
      await clickAt(page, await byText(page, "button.editr-tool", "Export"));
      await sleep(1500);
      await page.keyboard.press("Escape");
      await page.mouse.click(W - 30, H - 30);
      await sleep(400);
      await caption(page, "");
    });
    await reload(page);
    await record(page, "open-data", async () => {
      await caption(page, "Open other data without leaving the editor");
      await clickAt(page, await el(page, "#open_toggle"));
      await sleep(700);
      await clickAt(page, await el(page, "#open_object"));
      await chord(page, "Control", "KeyA");
      await type(page, "iris", 120);
      await sleep(300);
      await clickAt(page, await el(page, "#open_load"));
      await settle(page, 1500);
      await caption(page, "or start a new empty table");
      await clickAt(page, await el(page, "#open_toggle"));
      await sleep(600);
      await clickAt(page, await el(page, "#open_rows"));
      await chord(page, "Control", "KeyA");
      await type(page, "5");
      await clickAt(page, await el(page, "#open_cols"));
      await chord(page, "Control", "KeyA");
      await type(page, "4");
      await clickAt(page, await el(page, "#open_new"));
      await settle(page, 1500);
      await caption(page, "");
    });
  },

  // mtcars with track = TRUE: review, revert, code
  async review(page) {
    await record(page, "review", async () => {
      await caption(page, "Changed cells are highlighted (track = TRUE)");
      await editCell(page, 1, "c2", "22.5");
      await editCell(page, 3, "c5", "95");
      await clickAt(page, await cell(page, 5, "c2"), { button: "right" });
      await menu(page, "Delete row");
      await rename(page, "c5", "horsepower");
      await caption(page, "Review lists every change");
      await clickAt(page, await el(page, "#edit1-open_changes"));
      await settle(page, 1500);
      await caption(page, "Revert one change, or restore a removed row");
      await clickAt(page, await byText(page, ".editr-change button.btn", "Restore"));
      await settle(page, 1200);
      await caption(page, "The Code tab reproduces the edits");
      await clickAt(page, await el(page, "#edit1-open_code"));
      await settle(page, 2200);
      await caption(page, "");
    });
  },

  // template: type data into a labelled template
  async template(page) {
    await record(page, "template", async () => {
      await caption(page, "Start from column names and type in your data");
      await editCell(page, 1, "c1", "2024-03-01");
      await editCell(page, 1, "c2", "Australia");
      await editCell(page, 1, "c3", "24.5");
      await addFromCorner(page, "row");
      await settle(page, 600);
      const id = await nextId(page);
      await editCell(page, id, "c1", "2024-03-02");
      await editCell(page, id, "c2", "New Zealand");
      await editCell(page, id, "c3", "18.2");
      await caption(page, "Add columns from the same + menu");
      await addFromCorner(page, "col");
      await settle(page, 900);
      await caption(page, "");
    });
  },

  // column options
  async options(page) {
    await record(page, "col-checkbox", async () => {
      await caption(page, "Checkbox columns: col_options = list(fast = c(TRUE, FALSE))");
      for (const id of [1, 4]) {
        await clickAt(page, await cell(page, id, "c3"), { clickCount: 2 });
        await sleep(600);
        await page.keyboard.press("Space");
        await settle(page, 700);
      }
      await caption(page, "");
    });
    await record(page, "col-dropdown", async () => {
      await caption(page, "Dropdown columns pick from a list of options");
      for (const [id, val] of [[2, "orange"], [5, "green"]]) {
        await clickAt(page, await cell(page, id, "c4"), { clickCount: 2 });
        await page.waitForSelector(".tabulator-edit-list .tabulator-edit-list-item");
        await sleep(700);
        await clickAt(page, await byText(page, ".tabulator-edit-list .tabulator-edit-list-item", val));
        await settle(page, 700);
      }
      await caption(page, "");
    });
    await record(page, "col-date", async () => {
      await caption(page, "Date columns use a date picker");
      await clickAt(page, await cell(page, 3, "c5"), { clickCount: 2 });
      await page.waitForSelector(".tabulator-cell.tabulator-editing input");
      await sleep(600);
      await type(page, "03152025", 140);
      await sleep(400);
      await page.keyboard.press("Enter");
      await settle(page, 900);
      await caption(page, "");
    });
    await record(page, "col-password", async () => {
      await caption(page, "Password columns hide what you type");
      await clickAt(page, await cell(page, 2, "c6"), { clickCount: 2 });
      await page.waitForSelector(".tabulator-cell.tabulator-editing input");
      await chord(page, "Control", "KeyA");
      await type(page, "s3cret", 150);
      await page.keyboard.press("Enter");
      await settle(page, 900);
      await caption(page, "");
    });
  },

  // database: edit, done, confirm, commit
  async db(page) {
    await record(page, "db-commit", async () => {
      await caption(page, "Edit rows of a database table");
      await editCell(page, 1, "c3", "21.5");
      await editCell(page, 2, "c2", "Mazda RX4 Wagon");
      await clickAt(page, await cell(page, 4, "c2"), { button: "right" });
      await menu(page, "Delete row");
      await clickAt(page, await el(page, "#edit1-open_code"));
      await caption(page, "The Code tab shows the matching rows_*() calls");
      await settle(page, 2500);
      await clickAt(page, await el(page, "#edit1-open_code"));
      await caption(page, "Done asks before writing to the database");
      await clickAt(page, await el(page, "#done"));
      await page.waitForSelector("#shiny-modal .modal-dialog");
      await sleep(2200);
      await caption(page, "Commit writes every change in one transaction");
      await moveTo(page, await el(page, "#db_commit"));
      await sleep(1800);
    });
    await page.click("#db_commit");
    await sleep(1500);
  },

  // paste a block copied from a spreadsheet into a labelled template
  async paste(page) {
    const block = [
      ["S-01", "15/03/2024", "45.2", "yes"],
      ["S-02", "16/03/2024", "51.8", "no"],
      ["S-03", "18/03/2024", "47.0", "yes"],
      ["S-04", "19/03/2024", "n/a", "yes"],
      ["S-05", "20/03/2024", "49.3", "no"],
      ["S-06", "21/03/2024", "52.6", "yes"],
    ];
    const text = block.map((r) => r.join("\t")).join("\r\n") + "\r\n";
    await record(page, "paste", async () => {
      await caption(page, "Copy cells in Excel, Google Sheets or LibreOffice…");
      await page.evaluate((rows) => {
        const box = document.createElement("div");
        box.id = "demo-sheet";
        box.style.cssText = "position:fixed;right:24px;top:150px;z-index:2147483645;background:#fff;" +
          "border:1px solid #CBD5E1;border-radius:8px;box-shadow:0 8px 24px rgba(15,23,42,.18);" +
          "font:12px Inter,system-ui,sans-serif;color:#0F172A;padding:10px 12px;";
        box.innerHTML = '<div style="font-weight:600;margin-bottom:6px;color:#166534">Copied from a spreadsheet</div>' +
          '<table style="border-collapse:collapse">' + rows.map((r) => "<tr>" + r.map((v) =>
            '<td style="border:1px dashed #16A34A;padding:3px 8px">' + v + "</td>").join("") + "</tr>").join("") +
          "</table>";
        document.body.appendChild(box);
      }, block);
      await sleep(2200);
      await caption(page, "…click a cell and press Ctrl+V");
      await clickAt(page, await cell(page, 1, "c1"));
      await sleep(700);
      await page.evaluate((t) => {
        document.getElementById("demo-sheet").remove();
        const table = document.getElementById("edit1-grid").__editr.table;
        const dt = new DataTransfer();
        dt.setData("text/plain", t);
        table.element.dispatchEvent(new ClipboardEvent("paste", { clipboardData: dt, bubbles: true, cancelable: true }));
      }, text);
      await page.waitForSelector(".editr-guard");
      await sleep(600);
      await caption(page, "Values that do not fit their column are flagged once");
      await sleep(2600);
      await clickAt(page, await el(page, '.editr-guard [data-act="na"]'));
      await settle(page, 900);
      await caption(page, "Rows are added, dates and yes/no are converted");
      await sleep(2600);
      await caption(page, "");
    });
  },

  // the column types from col_options
  // visits: drag to fill, the colour picker and comments
  async polish(page) {
    await record(page, "fill", async () => {
      await caption(page, "Drag the corner of a selection to fill");
      let p = await cell(page, 1, "c1");
      await moveTo(page, p);
      await page.mouse.down();
      const q = await cell(page, 2, "c3");
      await page.mouse.move(q.x, q.y, { steps: 12 });
      await page.mouse.up();
      await settle(page, 700);
      const h = await el(page, ".tabulator-range-fill-handle");
      await moveTo(page, h);
      await page.mouse.down();
      const t = await cell(page, 7, "c3");
      await page.mouse.move(t.x, t.y, { steps: 30 });
      await sleep(600);
      await page.mouse.up();
      await settle(page, 600);
      await caption(page, "Two or more values carry on as a series");
      await sleep(1800);
      await caption(page, "Colour columns get a colour picker");
      p = await cell(page, 7, "c5");
      await clickAt(page, p, { clickCount: 2 });
      await page.waitForSelector(".editr-colour-box");
      await sleep(900);
      await clickAt(page, await page.evaluate(() => {
        const b = document.querySelectorAll(".editr-colour-pick")[12].getBoundingClientRect();
        return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
      }));
      await settle(page, 700);
      await caption(page, "Right-click a cell to add a comment");
      p = await cell(page, 2, "c4");
      await clickAt(page, p, { button: "right" });
      await menu(page, "Add comment");
      await page.waitForSelector(".editr-comment-box textarea");
      await type(page, "Re-weighed: first reading 71.8", 55);
      await sleep(500);
      await chord(page, "Control", "Enter");
      await settle(page, 700);
      await moveTo(page, p);
      await sleep(1600);
      await caption(page, "");
    });
  },

  async types(page) {
    await record(page, "col-types", async () => {
      await caption(page, "textarea: Shift+Enter starts a new line");
      let p = await cell(page, 3, "c2");
      await clickAt(page, p, { clickCount: 2 });
      await page.waitForSelector(".tabulator-cell.tabulator-editing textarea");
      await type(page, "Haemolysed", 70);
      await page.keyboard.down("Shift");
      await page.keyboard.press("Enter");
      await page.keyboard.up("Shift");
      await type(page, "Discard", 70);
      await page.keyboard.press("Enter");
      await settle(page, 900);
      await caption(page, "c(min = 0, max = 14): values outside the limits are flagged");
      p = await cell(page, 4, "c5");
      await clickAt(page, p, { clickCount: 2 });
      await page.waitForSelector(".tabulator-cell.tabulator-editing input");
      await type(page, "17", 120);
      await page.keyboard.press("Enter");
      await page.waitForSelector(".editr-guard");
      await sleep(1800);
      await clickAt(page, await el(page, '.editr-guard [data-act="cancel"]'));
      await settle(page, 500);
      await editCell(page, 4, "c5", "7.2");
      await caption(page, "\"rating\": click a star");
      p = await cell(page, 4, "c7");
      await clickAt(page, p, { clickCount: 2 });
      await page.waitForSelector(".tabulator-cell.tabulator-editing svg");
      await sleep(500);
      const star = await page.evaluate(() => {
        const s = document.querySelectorAll(".tabulator-cell.tabulator-editing svg")[3].getBoundingClientRect();
        return { x: s.x + s.width / 2, y: s.y + s.height / 2 };
      });
      await clickAt(page, star);
      await settle(page, 900);
      await caption(page, "\"slider\": drag to set a value from 0 to 100");
      p = await cell(page, 4, "c6");
      await clickAt(page, p, { clickCount: 2 });
      await page.waitForSelector(".tabulator-cell.tabulator-editing input[type=range]");
      const r = await page.evaluate(() => {
        const b = document.querySelector(".tabulator-cell.tabulator-editing input[type=range]").getBoundingClientRect();
        return { x: b.x, y: b.y + b.height / 2, w: b.width };
      });
      await moveTo(page, { x: r.x + 4, y: r.y });
      await page.mouse.down();
      await page.mouse.move(r.x + r.w * 0.45, r.y, { steps: 16 });
      await page.mouse.up();
      await sleep(300);
      await page.keyboard.press("Enter");
      await settle(page, 900);
      await caption(page, "\"url\": open the address in a new tab");
      await moveTo(page, await el(page, ".editr-url-open"));
      await sleep(1600);
      await caption(page, "");
    });
  },

  // README: a tour of the main features
  async readme(page) {
    await record(page, "readme", async () => {
      await caption(page, "Edit cells");
      await editCell(page, 1, "c2", "22.5");
      await editCell(page, 3, "c5", "95");
      await caption(page, "Select a range and fill down (Ctrl+D)");
      let p = await cell(page, 4, "c3");
      await moveTo(page, p);
      await page.mouse.down();
      await page.mouse.move((await cell(page, 7, "c3")).x, (await cell(page, 7, "c3")).y, { steps: 12 });
      await page.mouse.up();
      await sleep(500);
      await chord(page, "Control", "KeyD");
      await settle(page, 700);
      await caption(page, "Add and remove rows");
      await clickAt(page, await cell(page, 2, "c2"), { button: "right" });
      await menu(page, "Insert row below");
      const id = await nextId(page);
      await editCell(page, id, "c1", "My car");
      await editCell(page, id, "c2", "26");
      await caption(page, "Rename columns");
      await rename(page, "c5", "horsepower");
      await caption(page, "Filter rows under the headers");
      await clickAt(page, await filterBox(page, "c2", 0));
      await type(page, "20");
      await page.keyboard.press("Tab");
      await settle(page, 900);
      await caption(page, "Show or hide columns");
      await clickAt(page, await el(page, "#edit1-columns"));
      await page.waitForSelector(".editr-columns-menu");
      await sleep(300);
      for (const name of ["drat", "qsec", "vs", "am"]) {
        await clickAt(page, await byText(page, ".editr-columns-list label", name));
        await sleep(200);
      }
      await page.mouse.click(W - 30, H - 30);
      await settle(page, 500);
      await caption(page, "Find values (Ctrl+F)");
      await chord(page, "Control", "KeyF");
      await type(page, "Merc", 110);
      await settle(page, 800);
      await page.keyboard.press("Escape");
      await caption(page, "Undo and redo (Ctrl+Z / Ctrl+Y)");
      await clickAt(page, await cell(page, 1, "c3"));
      await chord(page, "Control", "KeyZ");
      await settle(page, 700);
      await chord(page, "Control", "KeyY");
      await settle(page, 700);
      await clickAt(page, await el(page, ".editr-chip button"));
      await settle(page, 600);
      await caption(page, "Review every change and revert any of them");
      await clickAt(page, await el(page, "#edit1-open_changes"));
      await settle(page, 1500);
      await clickAt(page, await page.evaluate(() => {
        const row = [...document.querySelectorAll(".editr-change")].find((x) => x.textContent.includes("Datsun 710"));
        const b = row.querySelector("button.btn").getBoundingClientRect();
        return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
      }));
      await settle(page, 1200);
      await caption(page, "Get the code that reproduces your edits");
      await clickAt(page, await el(page, "#edit1-open_code"));
      await settle(page, 2200);
      await clickAt(page, await el(page, "#edit1-open_code"));
      await settle(page, 500);
      await caption(page, "Return everything or only what's on screen, and save to file");
      await clickAt(page, await el(page, "#done_toggle"));
      await sleep(2000);
      await clickAt(page, await el(page, "#done_toggle"));
      await sleep(300);
      await caption(page, "Press Done to get the edited data back in R");
      await moveTo(page, await el(page, "#done"));
      await sleep(1500);
    });
  },
};

(async () => {
  const { browser, page } = await launch();
  try {
    await open(page);
    await scenarios[scenario](page);
  } catch (e) {
    console.log("ERROR " + e.stack);
    await page.screenshot({ path: `${out}/error-${scenario}.png` });
  }
  await browser.close();
})();

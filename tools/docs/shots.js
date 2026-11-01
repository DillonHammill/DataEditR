// Documentation screenshots: node shots.js <scenario> <port> <outdir>
const puppeteer = require("puppeteer-core");
const G = "edit1-grid";
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const fs = require("fs");
const [scenario, port, out] = process.argv.slice(2);
const W = 1200, H = 720;
const DPR = scenario === "gif" ? 1 : 1.25;

async function launch() {
  const browser = await puppeteer.launch({
    executablePath: process.env.CHROME_PATH || "/usr/bin/google-chrome",
    headless: true,
    args: ["--no-sandbox", "--disable-gpu", `--window-size=${W},${H}`,
           "--font-render-hinting=none"],
    defaultViewport: { width: W, height: H, deviceScaleFactor: DPR },
  });
  const page = await browser.newPage();
  page.on("pageerror", (e) => console.log("PAGE ERROR " + e.message));
  for (let i = 0; i < 60; i++) {
    try {
      await page.goto(`http://127.0.0.1:${port}`, { waitUntil: "networkidle2", timeout: 5000 });
      break;
    } catch (e) { await sleep(1000); }
  }
  await page.waitForFunction((g) => document.getElementById(g) &&
    document.getElementById(g).__editr && document.getElementById(g).__editr.ready,
    { timeout: 30000 }, G);
  await sleep(800);
  return { browser, page };
}

async function settle(page, ms = 600) {
  await sleep(ms);
  await page.waitForFunction(() => !document.documentElement.classList.contains("shiny-busy"),
                             { timeout: 10000 });
  await sleep(200);
}

const T = (page, fn, ...a) => page.evaluate(fn, G, ...a);

async function cell(page, id, field) {
  return page.evaluate(async (g, id, field) => {
    const t = document.getElementById(g).__editr.table;
    // scrollToRow() never settles when the row is already on screen
    await Promise.race([t.scrollToRow(id, "nearest", false).catch(() => null),
                        new Promise((r) => setTimeout(r, 400))]);
    const el = t.getRow(id).getCell(field).getElement();
    const b = el.getBoundingClientRect();
    return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
  }, G, id, field);
}

async function header(page, field) {
  return page.evaluate((g, field) => {
    const t = document.getElementById(g).__editr.table;
    const el = t.getColumn(field).getElement().querySelector(".editr-title") || t.getColumn(field).getElement().querySelector(".tabulator-col-title");
    const b = el.getBoundingClientRect();
    return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
  }, G, field);
}

async function edit(page, id, field, text) {
  const p = await cell(page, id, field);
  await page.mouse.click(p.x, p.y, { clickCount: 2 });
  await page.waitForSelector(".tabulator-cell.tabulator-editing input, .tabulator-cell.tabulator-editing select", { timeout: 3000 });
  await page.keyboard.down("Control"); await page.keyboard.press("KeyA"); await page.keyboard.up("Control");
  await page.keyboard.type(text);
  await page.keyboard.press("Enter");
  await settle(page);
}

async function click(page, id, field, opts = {}) {
  const p = await cell(page, id, field);
  if (opts.shift) await page.keyboard.down("Shift");
  await page.mouse.click(p.x, p.y, opts.right ? { button: "right" } : {});
  if (opts.shift) await page.keyboard.up("Shift");
  await sleep(200);
}

async function menuItem(page, label) {
  await page.waitForSelector(".tabulator-menu");
  const items = await page.$$(".tabulator-menu .tabulator-menu-item");
  for (const it of items) {
    const txt = await page.evaluate((e) => e.textContent.trim(), it);
    if (txt.startsWith(label)) { await it.click(); await settle(page); return; }
  }
  throw new Error("menu item not found: " + label);
}

async function clickText(page, selector, text) {
  await page.evaluate((selector, text) => {
    const el = [...document.querySelectorAll(selector)].find((e) => e.textContent.trim().startsWith(text));
    el.click();
  }, selector, text);
  await sleep(400);
}

async function shot(page, name, clip) {
  await sleep(250);
  await page.screenshot({ path: `${out}/${name}.png`, clip });
  console.log("shot " + name);
}

async function escape(page) {
  await page.keyboard.press("Escape");
  await page.mouse.click(W - 5, H - 5);
  await settle(page, 300);
}

async function done(page) {
  await page.click("#done");
  await sleep(1500);
}


// fake mouse pointer so recordings show where the user clicks
async function pointer(page) {
  await page.evaluate(() => {
    const c = document.createElement("div");
    c.id = "fake-pointer";
    c.style.cssText = "position:fixed;z-index:99999;width:18px;height:18px;margin:-3px 0 0 -3px;" +
      "pointer-events:none;left:-50px;top:-50px;transition:transform 80ms;" +
      "background:no-repeat url(\"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='18' height='18'>" +
      "<path d='M2 1l13 8-6 1.5L6 17z' fill='black' stroke='white' stroke-width='1.5'/></svg>\")";
    document.body.appendChild(c);
    document.addEventListener("mousemove", (e) => { c.style.left = e.clientX + "px"; c.style.top = e.clientY + "px"; }, true);
    document.addEventListener("mousedown", () => { c.style.transform = "scale(0.8)"; }, true);
    document.addEventListener("mouseup", () => { c.style.transform = ""; }, true);
  });
}

async function moveTo(page, p) {
  await page.mouse.move(p.x, p.y, { steps: 12 });
  await sleep(150);
}

async function slowType(page, text) {
  for (const ch of text) { await page.keyboard.type(ch); await sleep(90); }
}

// record frames while actions run, then build a GIF with ffmpeg
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
      await sleep(40);
    }
  })();
  await sleep(700);
  await actions();
  await sleep(1200);
  on = false;
  await loop;
  const lines = [];
  for (let k = 0; k < times.length; k++) {
    const d = k + 1 < times.length ? (times[k + 1][1] - times[k][1]) / 1000 : 1.5;
    lines.push(`file '${times[k][0]}'`, `duration ${d.toFixed(3)}`);
  }
  lines.push(`file '${times[times.length - 1][0]}'`);
  fs.writeFileSync(`${dir}/list.txt`, lines.join("\n"));
  console.log("recorded " + name + " frames " + times.length);
}

const scenarios = {
  async main(page) {
    await shot(page, "editor");
    // open menu
    await page.click("#open_toggle");
    await shot(page, "open-menu");
    await page.click("#open_toggle");
    await settle(page, 300);
    // row menu
    await click(page, 4, "c3", { right: true });
    await page.waitForSelector(".tabulator-menu");
    await shot(page, "row-menu");
    await escape(page);
    // column menu
    let p = await header(page, "c4");
    await page.mouse.click(p.x, p.y, { button: "right" });
    await page.waitForSelector(".tabulator-menu");
    await shot(page, "column-menu");
    await escape(page);
    // rename a column
    p = await header(page, "c2");
    await page.mouse.click(p.x, p.y, { clickCount: 2 });
    await page.waitForSelector("input.editr-rename");
    await page.keyboard.down("Control"); await page.keyboard.press("KeyA"); await page.keyboard.up("Control");
    await page.keyboard.type("mpg_us");
    await shot(page, "rename");
    await page.keyboard.press("Escape");
    await settle(page, 300);
    // range selection and status bar
    await click(page, 2, "c2");
    await click(page, 7, "c4", { shift: true });
    await settle(page);
    await shot(page, "range");
    // type guard
    p = await cell(page, 3, "c2");
    await page.mouse.click(p.x, p.y, { clickCount: 2 });
    await page.waitForSelector(".tabulator-cell.tabulator-editing input");
    await page.keyboard.down("Control"); await page.keyboard.press("KeyA"); await page.keyboard.up("Control");
    await page.keyboard.type("unknown");
    await page.keyboard.press("Enter");
    await page.waitForSelector(".editr-guard");
    await shot(page, "type-guard");
    await page.click('.editr-guard [data-act="cancel"]');
    await settle(page);
    // find
    await click(page, 1, "c2");
    await page.keyboard.down("Control"); await page.keyboard.press("KeyF"); await page.keyboard.up("Control");
    await sleep(200);
    await page.keyboard.type("Merc");
    await settle(page, 600);
    await shot(page, "find");
    await page.evaluate(() => {
      const f = document.getElementById("edit1-find");
      f.value = ""; f.dispatchEvent(new Event("input", { bubbles: true }));
    });
    await settle(page, 400);
    // header filters
    await T(page, (g) => {
      const t = document.getElementById(g).__editr.table;
      t.setHeaderFilterValue("c3", [4, 6]);
    });
    await settle(page, 600);
    p = await page.evaluate((g) => {
      const t = document.getElementById(g).__editr.table;
      const el = t.getColumn("c2").getElement().querySelector(".tabulator-header-filter input");
      const b = el.getBoundingClientRect();
      return { x: b.x + b.width / 4, y: b.y + b.height / 2 };
    }, G);
    await page.mouse.click(p.x, p.y);
    await page.keyboard.type("20");
    await page.keyboard.press("Tab");
    await settle(page, 800);
    await shot(page, "filters");
    // columns menu
    await page.click("#edit1-columns");
    await page.waitForSelector(".editr-columns-menu");
    for (const name of ["drat", "qsec", "vs", "am"]) {
      await page.evaluate((n) => {
        const l = [...document.querySelectorAll(".editr-columns-list label")].find((x) => x.textContent.trim() === n);
        l.querySelector("input").click();
      }, name);
    }
    await settle(page, 400);
    await shot(page, "columns-menu");
    await escape(page);
    // done menu
    await page.click("#done_toggle");
    await sleep(400);
    await shot(page, "done-menu");
    await page.click("#done_toggle");
    await settle(page, 300);
    // export menu
    await clickText(page, ".editr-toolbar button", "Export");
    await shot(page, "export-menu");
    await escape(page);
    // clear filters, then edit for the review panel
    await T(page, (g) => document.getElementById(g).__editr.table.clearHeaderFilter());
    await settle(page, 600);
    await T(page, (g) => {
      const t = document.getElementById(g).__editr.table;
      t.getColumns().forEach((c) => { if (c.getField() && !c.isVisible()) c.show(); });
    });
    await settle(page);
    await edit(page, 1, "c2", "21.5");
    await edit(page, 3, "c5", "95");
    await click(page, 5, "c2", { right: true });
    await menuItem(page, "Delete row");
    p = await header(page, "c5");
    await page.mouse.click(p.x, p.y, { clickCount: 2 });
    await page.waitForSelector("input.editr-rename");
    await page.keyboard.down("Control"); await page.keyboard.press("KeyA"); await page.keyboard.up("Control");
    await page.keyboard.type("horsepower");
    await page.keyboard.press("Enter");
    await settle(page);
    await page.click(".editr-addrow");
    await settle(page);
    const id = await T(page, (g) => document.getElementById(g).__editr.nextId - 1);
    await edit(page, id, "c1", "My car");
    await edit(page, id, "c2", "26");
    await page.click("#edit1-open_changes");
    await settle(page, 1000);
    await shot(page, "review");
    await page.click("#edit1-tab_code");
    await settle(page, 1000);
    await shot(page, "code");
    await page.click("#edit1-open_code");
    await settle(page);
    // shortcuts
    await page.click("#edit1-help");
    await page.waitForSelector("#edit1-shortcuts.show");
    await sleep(500);
    await shot(page, "shortcuts");
    await page.keyboard.press("Escape");
    await sleep(500);
    await done(page);
  },
  async gif(page) {
    await pointer(page);
    await page.mouse.move(600, 400);
    await record(page, "editing", async () => {
      // edit a value
      let p = await cell(page, 2, "c2");
      await moveTo(page, p);
      await page.mouse.click(p.x, p.y, { clickCount: 2 });
      await page.waitForSelector(".tabulator-cell.tabulator-editing input");
      await page.keyboard.down("Control"); await page.keyboard.press("KeyA"); await page.keyboard.up("Control");
      await slowType(page, "25.5");
      await page.keyboard.press("Enter");
      await settle(page, 500);
      // select a range and fill down
      p = await cell(page, 3, "c4");
      await moveTo(page, p);
      await page.mouse.down();
      const q = await cell(page, 8, "c5");
      await page.mouse.move(q.x, q.y, { steps: 15 });
      await page.mouse.up();
      await settle(page, 700);
      await page.keyboard.down("Control"); await page.keyboard.press("KeyD"); await page.keyboard.up("Control");
      await settle(page, 900);
      // undo the fill
      await moveTo(page, await page.evaluate(() => {
        const b = document.getElementById("edit1-undo").getBoundingClientRect();
        return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
      }));
      await page.click("#edit1-undo");
      await settle(page, 900);
    });
    await page.click("#edit1-undo");
    await settle(page);
    await record(page, "filtering", async () => {
      // text filter on row names
      let p = await page.evaluate((g) => {
        const t = document.getElementById(g).__editr.table;
        const el = t.getColumn("c1").getElement().querySelector(".tabulator-header-filter input");
        const b = el.getBoundingClientRect();
        return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
      }, G);
      await moveTo(page, p);
      await page.mouse.click(p.x, p.y);
      await slowType(page, "Merc");
      await settle(page, 900);
      // range filter on mpg
      p = await page.evaluate((g) => {
        const t = document.getElementById(g).__editr.table;
        const el = t.getColumn("c2").getElement().querySelector(".tabulator-header-filter input");
        const b = el.getBoundingClientRect();
        return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
      }, G);
      await moveTo(page, p);
      await page.mouse.click(p.x, p.y);
      await slowType(page, "18");
      await page.keyboard.press("Tab");
      await settle(page, 1200);
      // clear the filters from the toolbar
      p = await page.evaluate(() => {
        const el = [...document.querySelectorAll(".editr-toolbar button, .editr-toolbar a")].find((e) => e.textContent.trim() === "Clear filters");
        const b = el.getBoundingClientRect();
        return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
      });
      await moveTo(page, p);
      await page.mouse.click(p.x, p.y);
      await settle(page, 900);
    });
    await record(page, "reviewing", async () => {
      await edit(page, 1, "c3", "4");
      let p = await cell(page, 4, "c2");
      await moveTo(page, p);
      await page.mouse.click(p.x, p.y, { button: "right" });
      await sleep(500);
      await menuItem(page, "Delete row");
      p = await page.evaluate(() => {
        const b = document.getElementById("edit1-open_changes").getBoundingClientRect();
        return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
      });
      await moveTo(page, p);
      await page.mouse.click(p.x, p.y);
      await settle(page, 1200);
      p = await page.evaluate(() => {
        const btn = [...document.querySelectorAll(".editr-change button.btn")].find((b) => b.textContent.trim() === "Restore");
        const b = btn.getBoundingClientRect();
        return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
      });
      await moveTo(page, p);
      await page.mouse.click(p.x, p.y);
      await settle(page, 1200);
    });
    await done(page);
  },
  async empty(page) {
    await shot(page, "template-empty");
    await done(page);
  },
  async size(page) {
    await shot(page, "template-size");
    await done(page);
  },
  async types(page) {
    await shot(page, "col-types");
    await done(page);
  },
  async template(page) {
    await shot(page, "template");
    await done(page);
  },
  async options(page) {
    let p = await cell(page, 3, "c4");
    await page.mouse.click(p.x, p.y, { clickCount: 2 });
    await sleep(800);
    await shot(page, "col-options");
    await page.keyboard.press("Escape");
    await settle(page);
    p = await cell(page, 2, "c5");
    await page.mouse.click(p.x, p.y, { clickCount: 2 });
    await sleep(600);
    await shot(page, "col-options-date");
    await page.keyboard.press("Escape");
    await done(page);
  },
  async custom(page) {
    await shot(page, "custom");
    await done(page);
  },
  async stretch(page) {
    await shot(page, "stretch");
    await done(page);
  },
  async track(page) {
    await edit(page, 1, "c2", "22");
    await edit(page, 2, "c4", "8");
    await edit(page, 4, "c5", "250");
    await click(page, 3, "c2", { right: true });
    await menuItem(page, "Insert row below");
    await settle(page);
    const id = await T(page, (g) => document.getElementById(g).__editr.nextId - 1);
    await edit(page, id, "c1", "New car");
    await edit(page, id, "c2", "30");
    await click(page, 6, "c3");
    await settle(page);
    await shot(page, "track");
    await done(page);
  },
  async actions(page) {
    const menuButton = (field) => page.evaluate((g, f) => {
      const t = document.getElementById(g).__editr.table;
      const b = t.getColumn(f).getElement().querySelector(".tabulator-header-popup-button").getBoundingClientRect();
      return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
    }, G, field);
    const pick = (label) => page.evaluate((l) => [...document.querySelectorAll(".tabulator-menu .tabulator-menu-item")]
      .find((x) => x.textContent.trim().startsWith(l)).click(), label);
    // the column menu
    let p = await menuButton("c3");
    await page.mouse.click(p.x, p.y);
    await page.waitForSelector(".tabulator-menu");
    await sleep(400);
    await shot(page, "column-actions");
    // a formula
    await pick("Formula");
    await page.waitForSelector("#shiny-modal .modal-dialog");
    await sleep(800);
    await page.click("#edit1-fx_name", { clickCount: 3 });
    await page.keyboard.type("ph_class");
    await page.click("#edit1-fx_expr");
    await page.keyboard.type('case_when(ph < 7 ~ "acidic", ph > 7 ~ "basic", .default = "neutral")');
    await sleep(1500);
    await shot(page, "formula");
    await page.click("#edit1-action_apply");
    await settle(page, 1500);
    // recode the sites (one dialog, the most common values first)
    p = await menuButton("c2");
    await page.mouse.click(p.x, p.y);
    await page.waitForSelector(".tabulator-menu");
    await sleep(300);
    await pick("Recode values");
    await page.waitForSelector("#shiny-modal .modal-dialog");
    await sleep(800);
    await page.click("#edit1-rc_3");
    await page.keyboard.type("north");
    await sleep(1200);
    await shot(page, "recode");
    await page.click("#edit1-action_apply");
    await settle(page, 1500);
    // a rule from the column menu
    p = await menuButton("c1");
    await page.mouse.click(p.x, p.y);
    await page.waitForSelector(".tabulator-menu");
    await sleep(300);
    await pick("Rules");
    await page.waitForSelector("#shiny-modal .modal-dialog");
    await sleep(800);
    await page.click("#edit1-ru_required");
    await page.click("#edit1-ru_unique");
    await page.click("#edit1-ru_pattern");
    await page.keyboard.type("S-[0-9]{2}");
    await sleep(1200);
    await shot(page, "rules-dialog");
    await page.click("#edit1-rules_save");
    await settle(page, 1200);
    await page.click("#edit1-problems_status");
    await settle(page, 1000);
    await shot(page, "problems");
    // the code
    await page.click("#edit1-tab_code");
    await settle(page, 1200);
    await shot(page, "actions-code");
    await page.click("#done");
    await sleep(1000);
  },
  async summary(page) {
    const sum = (f, act) => `.editr-summary[data-field="${f}"] [data-act="${act}"]`;
    await sleep(1200);
    // the details of a column
    await page.click(sum("c4", "details"));
    await sleep(500);
    await shot(page, "summaries");
    await page.keyboard.press("Escape");
    await sleep(300);
    // click the tallest bar to show those rows
    await page.evaluate(() => {
      const box = document.querySelector('.editr-summary[data-field="c4"]');
      const bars = [...box.querySelectorAll(".editr-sum-bar-v")];
      const tall = bars.reduce((a, b) => (+b.getAttribute("height") > +a.getAttribute("height") ? b : a));
      tall.parentNode.querySelector(".editr-sum-hit").dispatchEvent(new MouseEvent("click", { bubbles: true }));
    });
    await settle(page, 900);
    await shot(page, "summary-filter", { x: 0, y: 0, width: W, height: 420 });
    await page.click("#edit1-chips .editr-chip button");
    await settle(page, 700);
    // the record form
    await click(page, 7, "c2");
    await page.click("#edit1-open_record");
    await settle(page, 1000);
    await shot(page, "record");
    await page.click("#done");
    await sleep(1000);
  },
  async labelled(page) {
    await sleep(1000);
    // the cell editor offers the labels
    const p = await cell(page, 2, "c3");
    await page.mouse.click(p.x, p.y, { clickCount: 2 });
    await sleep(700);
    await shot(page, "labelled");
    await page.keyboard.press("Escape");
    await sleep(300);
    // the Labels dialog
    await page.evaluate((g) => document.getElementById(g).__editr.table.getColumn("c2").getElement()
      .querySelector(".tabulator-header-popup-button").click(), G);
    await page.waitForSelector(".tabulator-menu");
    await sleep(300);
    await page.evaluate(() => [...document.querySelectorAll(".tabulator-menu .tabulator-menu-item")]
      .find((x) => x.textContent.trim().startsWith("Labels")).click());
    await page.waitForSelector("#edit1-lb_values");
    await sleep(900);
    await page.click("#edit1-lb_values");
    await page.keyboard.down("Control"); await page.keyboard.press("End"); await page.keyboard.up("Control");
    await page.keyboard.type("\n3 = Other");
    await sleep(1500);
    await shot(page, "labels-dialog");
    await page.click("#edit1-action_apply");
    await settle(page, 1200);
    await page.click("#done");
    await sleep(1000);
  },
  async db_rows(page) {
    await sleep(1500);
    await page.click("#edit1-load_rows");
    await page.waitForSelector("#edit1-ld_filter");
    await sleep(800);
    await page.type("#edit1-ld_filter", 'carrier == "UA" & dep_delay > 60');
    await page.select("#edit1-ld_sort", "dep_delay");
    await page.click("#edit1-ld_desc");
    await page.click("#edit1-ld_limit", { clickCount: 3 });
    await page.keyboard.type("1000");
    await sleep(1800);
    await shot(page, "db-rows");
    await page.click("#edit1-load_apply");
    await settle(page, 2500);
    await shot(page, "db-rows-loaded");
    await page.click("#done");
    await sleep(1000);
  },
  async restore(page) {
    await page.waitForSelector("#shiny-modal .modal-dialog");
    await sleep(900);
    await shot(page, "restore");
    await page.click("#edit1-draft_restore");
    await settle(page, 1200);
    await edit(page, 2, "c3", "8");
    await page.click("#edit1-open_changes");
    await settle(page, 800);
    await page.click("#edit1-tab_history");
    await settle(page, 1000);
    await page.click("#edit1-history .editr-note-btn");
    await sleep(300);
    await page.keyboard.type("Typo in the lab sheet");
    await page.keyboard.press("Enter");
    await settle(page, 900);
    await shot(page, "history");
    await page.click("#done");
    await sleep(1000);
  },
  async db(page) {
    await shot(page, "db-editor");
    await edit(page, 1, "c3", "21.5");
    await edit(page, 2, "c2", "Mazda RX4 Wagon");
    await click(page, 4, "c2", { right: true });
    await menuItem(page, "Delete row");
    await page.click("#done");
    await page.waitForSelector("#shiny-modal .modal-dialog");
    await sleep(700);
    await shot(page, "db-commit");
    await page.click("#db_commit");
    await sleep(1500);
  },
};

(async () => {
  const { browser, page } = await launch();
  try {
    await scenarios[scenario](page);
  } catch (e) {
    console.log("ERROR " + e.stack);
    await page.screenshot({ path: `${out}/error-${scenario}.png` });
  }
  await browser.close();
})();

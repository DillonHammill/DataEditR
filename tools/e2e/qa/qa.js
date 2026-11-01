// QA scenarios: drive the editor for one scenario of qa_app.R.
// Usage: node qa.js <scenario> <port> <outdir>
const path = require("path");
const fs = require("fs");
const { execFileSync } = require("child_process");
const L = require("../lib");
const { check, sleep } = L;
const [scenario, port, out] = process.argv.slice(2);

const fieldOf = async (page, title) => {
  const c = (await L.columns(page)).find((x) => x.title === title);
  if (!c) throw new Error("no column " + title);
  return c.field;
};
const nextId = (page) => page.evaluate(() => document.querySelector(".editr_grid").__editr.nextId - 1);

async function addRow(page) {
  await page.click(".editr-addrow");
  await L.settle(page);
  return nextId(page);
}

async function done(page, what = "all") {
  if (what === "view") {
    await page.click("#done_toggle");
    await sleep(400);
    await page.click('input[name="return_what"][value="view"]');
    await sleep(300);
    await page.click("#done_toggle");
    await sleep(300);
  }
  await page.click("#done");
  await sleep(1500);
}

async function canEdit(page, id, field) {
  const p = await L.cellCentre(page, id, field);
  await page.mouse.click(p.x, p.y, { clickCount: 2 });
  await sleep(500);
  const editing = await page.$(".tabulator-cell.tabulator-editing");
  if (editing) await page.keyboard.press("Escape");
  await sleep(200);
  return !!editing;
}

async function canRename(page, field) {
  const p = await page.evaluate((f) => {
    const t = document.querySelector(".editr_grid").__editr.table;
    const b = t.getColumn(f).getElement().querySelector(".tabulator-col-title").getBoundingClientRect();
    return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
  }, field);
  await page.mouse.click(p.x, p.y, { clickCount: 2 });
  await sleep(500);
  const input = await page.$("input.editr-rename");
  if (input) await page.keyboard.press("Escape");
  await sleep(200);
  return !!input;
}

async function revert(page, text, action) {
  const ok = await page.evaluate((text, action) => {
    const item = [...document.querySelectorAll(".editr-change")]
      .find((x) => x.textContent.includes(text) &&
              [...x.querySelectorAll("button.btn")].some((b) => b.textContent.trim() === action));
    if (!item) return false;
    [...item.querySelectorAll("button.btn")].find((b) => b.textContent.trim() === action).click();
    return true;
  }, text, action);
  await L.settle(page, 900);
  return ok;
}

async function waitFile(file, ms = 8000) {
  for (let i = 0; i < ms / 200; i++) {
    if (fs.existsSync(file) && !fs.existsSync(file + ".crdownload")) return true;
    await sleep(200);
  }
  return false;
}

async function openHistory(page) {
  await page.click("#edit1-open_changes");
  await sleep(400);
  await page.click("#edit1-tab_history");
  await L.settle(page, 900);
}
const historyOf = (page) => page.$$eval("#edit1-history .editr-step", (els) => els.map((e) => ({
  tag: e.querySelector(".editr-tag").textContent.trim(),
  title: e.querySelector(".editr-step-title").textContent.trim(),
  note: (e.querySelector(".editr-note-text") || {}).textContent || "",
})));
// add a note to the n-th step shown (0 = newest); escape = cancel the edit
async function addNote(page, n, text, escape = false) {
  const btns = await page.$$("#edit1-history .editr-note-btn");
  await btns[n].click();
  await sleep(300);
  await page.keyboard.type(text);
  await page.keyboard.press(escape ? "Escape" : "Enter");
  await L.settle(page, 900);
}
async function confirmCancel(page) {
  await page.click("#cancel");
  await page.waitForSelector("#shiny-modal .modal-dialog", { timeout: 5000 });
  await sleep(700);
  const msg = await page.$eval("#shiny-modal", (e) => e.textContent.replace(/\s+/g, " "));
  check(/Discard your changes/.test(msg), "Cancel asks first: " + msg.trim().slice(0, 90));
  await page.click("#cancel_discard");
  await sleep(1500);
}

const scenarios = {
  async df(page) {
    const id7 = 7;
    await L.editCell(page, 1, "c2", "30");
    await L.editCell(page, 2, "c1", "Mazda RX4 Wagon");
    await L.renameColumn(page, "c5", "horsepower");
    await L.rowMenu(page, 3, "c2", "Insert row below");
    check(await nextId(page) === id7, "new row id 7");
    await L.editCell(page, id7, "c1", "My car");
    await L.editCell(page, id7, "c2", "25");
    await L.rowMenu(page, 5, "c2", "Delete row");
    await L.headerMenu(page, "c4", "Insert column right");
    const v1 = await fieldOf(page, "V1");
    await L.editCell(page, 1, v1, "note");
    await L.headerMenu(page, "c6", "Delete column");
    await L.selectCell(page, 1, "c3");
    await L.selectCell(page, 3, "c3", true);
    await L.key(page, "d", true);
    await L.selectCell(page, 1, "c4");
    await L.paste(page, "100\r\n200\r\n");
    await L.selectCell(page, 6, "c4");
    await L.key(page, "Delete");
    await L.key(page, "z", true);
    let d = await L.gridData(page);
    check(Number(d.find((r) => r.__editr_id__ === 6).c4) === 225, "undo restored the cleared cell");
    await L.key(page, "y", true);
    d = await L.gridData(page);
    check(d.find((r) => r.__editr_id__ === 6).c4 == null, "redo cleared it again");
    const status = await page.$eval("#edit1-status", (e) => e.textContent);
    check(/row added/.test(status) && /row removed/.test(status), "status bar: " + status.trim());
    await done(page);
  },

  async matrix(page) {
    await L.editCell(page, 1, "c2", "10");
    const id = await addRow(page);
    await L.editCell(page, id, "c1", "d");
    await L.editCell(page, id, "c2", "7");
    await L.editCell(page, id, "c3", "8");
    await done(page);
  },

  async tibble(page) {
    check((await L.columns(page))[0].title !== " ", "no row names column for a tibble");
    await L.editCell(page, 2, "c2", "B");
    await L.editCell(page, 1, "c3", "15/03/2024");
    await L.editCell(page, 3, "c4", "yes");
    await done(page);
  },

  async datatable(page) {
    await L.editCell(page, 1, "c2", "X");
    await L.rowMenu(page, 2, "c1", "Delete row");
    await done(page);
  },

  async template(page) {
    await L.selectCell(page, 1, "c1");
    await L.paste(page, "A\t1\t2024-01-01\r\nB\t2\t2024-01-02\r\nC\t3\t2024-01-03\r\n");
    check((await L.gridData(page)).length === 3, "rows added for the pasted block");
    await done(page);
  },

  async text(page) {
    const p = await L.cellCentre(page, 1, "c2");
    await page.mouse.click(p.x, p.y, { clickCount: 2 });
    await page.waitForSelector(".tabulator-cell.tabulator-editing input");
    await page.keyboard.down("Control");
    await page.keyboard.press("KeyA");
    await page.keyboard.up("Control");
    await page.keyboard.type("n/a");
    await page.keyboard.press("Enter");
    await page.waitForSelector(".editr-guard", { timeout: 3000 });
    await page.click('.editr-guard [data-act="text"]');
    await L.settle(page);
    const badge = await page.evaluate(() => [...document.querySelectorAll(".editr-badge")].map((b) => b.textContent)[1]);
    check(badge === "abc", "mpg is now a text column (" + badge + ")");
    await done(page);
  },

  async empty(page) {
    check((await L.gridData(page)).length === 3 && (await L.columns(page)).length === 2,
          "3 x 2 template");
    await L.editCell(page, 1, "c1", "5");
    await L.editCell(page, 2, "c2", "hello");
    await done(page);
  },

  async file(page) {
    await L.editCell(page, 1, "c2", "99");
    await done(page);
  },

  async binds(page) {
    const cols = await L.columns(page);
    check(!cols.some((c) => c.title === "secret"), "hidden column not shown");
    check(cols.some((c) => c.title === "notes"), "bound column shown");
    check((await L.gridData(page)).length === 4, "bound row shown");
    const score = await fieldOf(page, "score"), id = await fieldOf(page, "id");
    const group = await fieldOf(page, "group"), notes = await fieldOf(page, "notes");
    check(!(await canEdit(page, 1, score)), "read-only column cannot be edited");
    check(!(await canRename(page, id)), "protected name cannot be renamed");
    await L.renameColumn(page, group, "team");
    await L.editCell(page, 1, notes, "first note");
    await L.editCell(page, 4, group, "c");
    await L.selectCell(page, 1, score);
    await L.key(page, "Delete");
    const d = await L.gridData(page);
    check(Number(d[0][score]) === 1, "clearing a read-only cell is ignored");
    await done(page);
  },

  async factor(page) {
    await L.editCell(page, 2, "c2", "C");
    await done(page);
  },

  async factor_keep(page) {
    await L.editCell(page, 2, "c2", "medium");
    await L.rowMenu(page, 3, "c2", "Insert row below");
    const id = await nextId(page);
    await L.editCell(page, id, "c1", "4");
    await L.editCell(page, id, "c2", "high");
    await done(page);
  },

  async subset(page) {
    const client = await page.target().createCDPSession();
    await client.send("Browser.setDownloadBehavior", { behavior: "allow", downloadPath: path.resolve(out) });
    const cyl = await fieldOf(page, "cyl");
    await page.evaluate((f) => {
      document.querySelector(".editr_grid").__editr.table.setHeaderFilterValue(f, [4]);
    }, cyl);
    await L.settle(page, 800);
    await page.click("#edit1-columns");
    await page.waitForSelector(".editr-columns-menu");
    for (const name of ["disp", "hp", "drat", "wt", "qsec", "vs", "am"]) {
      await page.evaluate((n) => {
        [...document.querySelectorAll(".editr-columns-list label")]
          .find((l) => l.textContent.trim() === n).querySelector("input").click();
      }, name);
    }
    await page.mouse.click(1300, 850);
    await L.settle(page, 800);
    const rows = await page.$eval("#edit1-rows", (e) => e.textContent);
    check(/^11 of 32 rows/.test(rows) && /4 of 11 columns/.test(rows), "status: " + rows);
    // export what's on screen
    await page.evaluate(() => {
      [...document.querySelectorAll("button.editr-tool")].find((b) => b.textContent.includes("Export")).click();
    });
    await sleep(400);
    await page.click("#export_view");
    check(await waitFile(path.join(out, "cars-subset.csv")), "export downloaded cars-subset.csv");
    await page.evaluate(() => {
      [...document.querySelectorAll("button.editr-tool")].find((b) => b.textContent.includes("Export")).click();
    });
    await sleep(400);
    await page.click("#export_all");
    check(await waitFile(path.join(out, "cars.csv")), "export downloaded cars.csv");
    await done(page, "view");
  },

  async cancel(page) {
    await L.editCell(page, 1, "c2", "99");
    await L.rowMenu(page, 2, "c2", "Delete row");
    // "Keep editing" closes the question and keeps the changes
    await page.click("#cancel");
    await page.waitForSelector("#shiny-modal .modal-dialog", { timeout: 5000 });
    await sleep(700);
    await page.click('#shiny-modal button[data-dismiss="modal"], #shiny-modal button[data-bs-dismiss="modal"]');
    await sleep(900);
    check(Number((await L.gridData(page))[0].c2) === 99, "Keep editing keeps the changes");
    await sleep(1500);
    check(fs.readdirSync(path.join(out, "drafts")).some((f) => f.endsWith(".rds")), "draft saved while editing");
    await confirmCancel(page);
  },

  async cancel_value(page) {
    await L.editCell(page, 1, "c2", "99");
    await confirmCancel(page);
  },

  async log(page) {
    await L.editCell(page, 1, "c2", "30");
    await L.selectCell(page, 1, "c3");
    await L.selectCell(page, 3, "c3", true);
    await L.key(page, "d", true);
    await L.selectCell(page, 4, "c4");
    await L.paste(page, "100\r\n200\r\n");
    await L.selectCell(page, 1, "c4");
    await L.selectCell(page, 2, "c4", true);
    await L.key(page, "Delete");
    await L.key(page, "z", true);
    await openHistory(page);
    let h = await historyOf(page);
    check(JSON.stringify(h.map((x) => x.tag)) === JSON.stringify(["Undo", "Clear", "Paste", "Fill", "Edit"]),
          "history tags: " + h.map((x) => x.tag).join(", "));
    check(JSON.stringify(h.map((x) => x.title)) === JSON.stringify([
      "Edited 2 cells in disp", "Cleared 2 cells in disp", "Pasted 2 cells in disp",
      "Filled down 1 cell in cyl", "Edited mpg in row Mazda RX4"]), "history titles: " + h.map((x) => x.title).join(" | "));
    await addNote(page, 4, "source sheet said 30");
    await addNote(page, 2, "not saved", true);
    h = await historyOf(page);
    check(h[4].note === "source sheet said 30" && h[2].note === "", "note saved, escaped note discarded");
    await page.screenshot({ path: path.join(out, "history.png") });
    // a new edit shows up at the top
    await L.editCell(page, 5, "c2", "1");
    await L.settle(page, 600);
    h = await historyOf(page);
    check(h.length === 6 && h[0].title === "Edited mpg in row Hornet Sportabout", "new step on top: " + h[0].title);
    await done(page);
  },

  async recover_crash(page) {
    await L.editCell(page, 1, "c2", "30");
    await L.editCell(page, 2, "c3", "8");
    await openHistory(page);
    await addNote(page, 1, "first");
    await sleep(2500);
    // R stops without warning while the page is open
    process.kill(Number(fs.readFileSync(path.join(out, "pid"), "utf8")), "SIGKILL");
    await sleep(1000);
  },

  async recover(page) {
    await page.waitForSelector("#shiny-modal .modal-dialog", { timeout: 8000 });
    await sleep(700);
    const msg = await page.$eval("#shiny-modal", (e) => e.textContent.replace(/\s+/g, " "));
    check(/Restore unsaved changes/.test(msg) && /2 unsaved changes/.test(msg),
          "offers the draft: " + msg.trim().slice(0, 140));
    await page.screenshot({ path: path.join(out, "restore.png") });
    await page.click("#edit1-draft_restore");
    await L.settle(page, 1500);
    let d = await L.gridData(page);
    check(Number(d[0].c2) === 30 && Number(d[1].c3) === 8, "edits restored");
    const status = await page.$eval("#edit1-status", (e) => e.textContent);
    check(/2 cells edited/.test(status), "status bar: " + status.trim());
    await openHistory(page);
    let h = await historyOf(page);
    check(h.length === 2 && h[1].note === "first", "history and note restored");
    // the restored steps can be undone and redone (toolbar buttons)
    await page.click("#edit1-undo");
    await L.settle(page, 900);
    d = await L.gridData(page);
    check(Number(d[1].c3) === 6, "undo after restoring");
    await page.click("#edit1-redo");
    await L.settle(page, 900);
    await L.editCell(page, 3, "c2", "40");
    await done(page);
  },

  async types(page) {
    const f = {};
    for (const t of ["pct", "stars", "ph", "when", "at", "notes", "grp", "ok"]) f[t] = await fieldOf(page, t);
    const marked = await page.evaluate((f) => {
      const t = document.querySelector(".editr_grid").__editr.table;
      const is = (id, fld) => t.getRow(id).getCell(fld).getElement().classList.contains("editr-invalid");
      return { pct2: is(2, f.pct), grp2: is(2, f.grp), ph2: is(2, f.ph), pct1: is(1, f.pct) };
    }, f);
    check(marked.pct2 && marked.grp2 && !marked.ph2 && !marked.pct1,
          "1.5 above the slider maximum and “w” marked: " + JSON.stringify(marked));
    const stars = await page.evaluate((fld) => document.querySelector(".editr_grid").__editr.table
      .getRow(1).getCell(fld).getElement().querySelectorAll("svg").length, f.stars);
    check(stars === 10, "10 stars (" + stars + ")");
    // slider limits from col_options
    let p = await L.cellCentre(page, 3, f.pct);
    await page.mouse.click(p.x, p.y, { clickCount: 2 });
    await page.waitForSelector(".tabulator-cell.tabulator-editing input[type=range]");
    const attrs = await page.$eval(".tabulator-cell.tabulator-editing input[type=range]",
      (i) => [i.min, i.max, i.step].join(" "));
    check(attrs === "0 1 0.05", "slider min/max/step 0 1 0.05 (" + attrs + ")");
    await page.$eval(".tabulator-cell.tabulator-editing input[type=range]", (i) => {
      i.value = "0.45";
      i.dispatchEvent(new Event("input", { bubbles: true }));
    });
    await page.keyboard.press("Enter");
    await L.settle(page);
    // paste over the slider limits
    await L.selectCell(page, 1, f.pct);
    await L.paste(page, "0.5\r\n2\r\n");
    await page.waitForSelector(".editr-guard", { timeout: 3000 });
    const g = await page.$eval(".editr-guard", (e) => e.textContent);
    check(/more than 1/.test(g), "2 is more than the slider maximum");
    await page.click('.editr-guard [data-act="na"]');
    await L.settle(page);
    // typed value over the number limit
    p = await L.cellCentre(page, 3, f.ph);
    await page.mouse.click(p.x, p.y, { clickCount: 2 });
    await page.waitForSelector(".tabulator-cell.tabulator-editing input");
    await page.keyboard.type("15");
    await page.keyboard.press("Enter");
    await page.waitForSelector(".editr-guard", { timeout: 3000 });
    await page.click('.editr-guard [data-act="cancel"]');
    await L.settle(page);
    await L.editCell(page, 3, f.ph, "7.5");
    const pick = async (id, field, value) => {
      const q = await L.cellCentre(page, id, field);
      await page.mouse.click(q.x, q.y, { clickCount: 2 });
      await page.waitForSelector(".tabulator-cell.tabulator-editing input");
      await page.$eval(".tabulator-cell.tabulator-editing input", (i, v) => {
        i.value = v;
        i.dispatchEvent(new Event("input", { bubbles: true }));
      }, value);
      await page.keyboard.press("Enter");
      await L.settle(page);
    };
    await pick(2, f.when, "2024-03-16T14:05");
    await pick(2, f.at, "08:15");
    // a partly filled time (hour only) keeps the old (missing) value
    let q = await L.cellCentre(page, 3, f.at);
    await page.mouse.click(q.x, q.y, { clickCount: 2 });
    await page.waitForSelector(".tabulator-cell.tabulator-editing input");
    await page.keyboard.type("7");
    await page.keyboard.press("Enter");
    await L.settle(page);
    const at3 = (await L.gridData(page)).find((r) => r.__editr_id__ === 3)[f.at];
    check(at3 === null || at3 === undefined, "partly typed time is ignored (" + at3 + ")");
    // emptying a text cell stores a missing value
    q = await L.cellCentre(page, 1, f.notes);
    await page.mouse.click(q.x, q.y, { clickCount: 2 });
    await page.waitForSelector(".tabulator-cell.tabulator-editing textarea");
    await page.keyboard.down("Control");
    await page.keyboard.press("KeyA");
    await page.keyboard.up("Control");
    await page.keyboard.press("Backspace");
    await page.keyboard.press("Enter");
    await L.settle(page);
    await L.selectCell(page, 2, f.grp);
    await L.paste(page, "z\r\n");
    await L.selectCell(page, 3, f.ok);
    await L.paste(page, "yes\r\n");
    await done(page);
  },

  async open(page) {
    await page.click("#open_toggle");
    await sleep(500);
    await page.click("#open_object", { clickCount: 3 });
    await page.keyboard.type("iris");
    await page.click("#open_load");
    await L.settle(page, 1500);
    check((await L.gridData(page)).length === 150, "iris loaded");
    const sl = await fieldOf(page, "Sepal.Length");
    await L.editCell(page, 1, sl, "9.9");
    await done(page);
  },

  async open_new(page) {
    await page.click("#open_toggle");
    await sleep(500);
    await page.click("#open_rows", { clickCount: 3 });
    await page.keyboard.type("2");
    await page.click("#open_cols", { clickCount: 3 });
    await page.keyboard.type("2");
    await page.click("#open_new");
    await L.settle(page, 1500);
    const cols = await L.columns(page);
    check((await L.gridData(page)).length === 2 && cols.length === 2, "2 x 2 table");
    await L.editCell(page, 1, cols[0].field, "1.5");
    await L.editCell(page, 2, cols[1].field, "b");
    await done(page);
  },

  async open_upload(page) {
    await page.click("#open_toggle");
    await sleep(500);
    const input = await page.$("#open_file");
    await input.uploadFile(path.resolve(out, "upload.csv"));
    await L.settle(page, 2500);
    const cols = await L.columns(page);
    check(cols.map((c) => c.title).join() === "a,b", "uploaded file opened: " + cols.map((c) => c.title).join());
    await L.editCell(page, 2, cols[1].field, "w");
    await done(page);
  },

  async review(page) {
    await L.editCell(page, 1, "c2", "30");
    await L.editCell(page, 2, "c3", "8");
    await L.renameColumn(page, "c5", "horsepower");
    await L.rowMenu(page, 3, "c2", "Delete row");
    await L.rowMenu(page, 4, "c2", "Insert row below");
    const id = await nextId(page);
    await L.editCell(page, id, "c1", "New");
    await page.click(".editr-addcol-btn");
    await L.settle(page);
    const v1 = await fieldOf(page, "V1");
    await L.editCell(page, 1, v1, "x");
    await page.click("#edit1-open_changes");
    await L.settle(page, 900);
    const n = await page.$$eval(".editr-change", (e) => e.length);
    check(n >= 6, n + " changes listed");
    check(await revert(page, "Mazda RX4 Wag", "Revert"), "reverted the cyl edit");
    check(await revert(page, "Datsun 710", "Restore"), "restored the deleted row");
    check(await revert(page, "New", "Remove"), "removed the added row");
    check(await revert(page, "horsepower", "Revert"), "reverted the rename");
    const left = await page.$$eval(".editr-change", (e) => e.map((x) => x.textContent.replace(/\s+/g, " ").trim()));
    check(left.length >= 2, "left: " + left.join(" | "));
    await page.click("#edit1-open_code");
    await L.settle(page, 900);
    const code = await page.$eval("#edit1-pane_code", (e) => e.textContent);
    check(/mpg = replace\(mpg, 1, 30\)/.test(code), "code panel shows the kept edit");
    await done(page);
  },

  async console(page) {
    await L.editCell(page, 2, "c3", "5");
    await done(page);
  },

  async hide(page) {
    check(!(await page.$("#open_toggle")) && !(await page.$("#done_toggle")), "Open menu and Done options hidden");
    const exportBtn = await page.evaluate(() => [...document.querySelectorAll("button.editr-tool")]
      .some((b) => b.textContent.includes("Export")));
    check(!exportBtn, "Export hidden");
    const p = await L.cellCentre(page, 1, "c2");
    await page.mouse.click(p.x, p.y, { clickCount: 2 });
    await sleep(500);
    await page.keyboard.press("Space");
    await L.settle(page);
    await done(page);
  },

  async db(page) {
    const d0 = await L.gridData(page);
    check(d0.map((r) => r.c1).join() === "1,2,4,5", "filtered rows cyl >= 6: ids " + d0.map((r) => r.c1).join());
    // editor rows are numbered in load order: 1-4 are database ids 1, 2, 4, 5
    await L.editCell(page, 1, "c3", "22");
    await L.rowMenu(page, 4, "c3", "Delete row");
    const id = await addRow(page);
    await L.editCell(page, id, "c2", "New");
    await L.editCell(page, id, "c3", "30");
    await L.editCell(page, id, "c4", "6");
    await page.click("#done");
    await page.waitForSelector("#shiny-modal .modal-dialog", { timeout: 5000 });
    const msg = await page.$eval("#shiny-modal", (e) => e.textContent);
    check(/1 row\(s\) updated, 1 inserted and 1 deleted/.test(msg), "commit summary");
    await sleep(900);
    await page.click("#db_commit");
    await sleep(2000);
  },

  async db_conflict(page) {
    // someone else changes row 1 after it was loaded
    execFileSync("Rscript", ["-e",
      `con <- DBI::dbConnect(RSQLite::SQLite(), "${path.resolve(out, "cars.sqlite")}");` +
      `DBI::dbExecute(con, "UPDATE cars SET mpg = 99 WHERE id = 1"); DBI::dbDisconnect(con)`]);
    await L.editCell(page, 1, "c3", "22");
    await L.editCell(page, 2, "c3", "23");
    await page.click("#done");
    await page.waitForSelector("#shiny-modal .modal-dialog", { timeout: 5000 });
    await sleep(900);
    await page.click("#db_commit");
    await sleep(1500);
    const msg = await page.$eval("#shiny-modal", (e) => e.textContent);
    check(/Commit failed/.test(msg) && /changed/.test(msg), "conflict reported: " + msg.replace(/\s+/g, " ").slice(0, 160));
    await page.click("#shiny-modal .modal-footer button");
    await sleep(600);
    await confirmCancel(page);
  },

  async shiny(page) {
    const unsaved = () => page.$eval("#unsaved", (e) => e.textContent);
    check(await unsaved() === "0 unsaved changes", "0 unsaved changes at start");
    await L.editCell(page, 1, "c2", "30");
    await L.editCell(page, 2, "c3", "8");
    check(await unsaved() === "2 unsaved changes", await unsaved());
    check((await page.$$(".tabulator-cell.editr-changed")).length === 2, "two cells highlighted");
    await page.click("#save");
    await L.settle(page, 1000);
    check(await unsaved() === "0 unsaved changes", "saved: " + await unsaved());
    check((await page.$$(".tabulator-cell.editr-changed")).length === 0, "highlights cleared");
    await L.editCell(page, 3, "c2", "1");
    check(await unsaved() === "1 unsaved changes", await unsaved());
    await page.click("#save");
    await L.settle(page, 1000);
    check(fs.existsSync(path.join(out, "commits.rds")), "commits saved");
  },

  async shiny_db(page) {
    const unsaved = () => page.$eval("#unsaved", (e) => e.textContent);
    await L.editCell(page, 1, "c3", "30");
    const id = await addRow(page);
    await L.editCell(page, id, "c2", "Z");
    await L.editCell(page, id, "c3", "10");
    check(await unsaved() === "2 unsaved changes", await unsaved());
    await page.click("#save");
    await L.settle(page, 1500);
    check(await unsaved() === "0 unsaved changes", "saved: " + await unsaved());
    const d = await L.gridData(page);
    check(d.length === 5 && Number(d[4].c1) === 5, "rows read back with the generated key: " +
          JSON.stringify(d.map((r) => r.c1)));
    await L.editCell(page, 5, "c3", "11");
    await page.click("#save");
    await L.settle(page, 1500);
  },
};

(async () => {
  if (scenario.startsWith("shiny")) L.useGrid("edit-grid");
  const { browser, page } = await L.launch(Number(port));
  try {
    await scenarios[scenario](page);
  } catch (e) {
    console.log("  FAIL exception: " + e.stack);
    process.exitCode = 1;
  } finally {
    await browser.close();
    console.log(L.failures() === 0 ? "BROWSER CHECKS PASSED" : L.failures() + " BROWSER CHECK(S) FAILED");
    if (L.failures()) process.exitCode = 1;
  }
})();

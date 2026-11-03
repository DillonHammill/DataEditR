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
    const b = t.getColumn(f).getElement().querySelector(".editr-title").getBoundingClientRect();
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

// open a column's menu with its button and pick an item (and a submenu item)
async function columnMenu(page, field, label, sub) {
  const p = await page.evaluate((f) => {
    const t = document.querySelector(".editr_grid").__editr.table;
    const b = t.getColumn(f).getElement().querySelector(".tabulator-header-popup-button").getBoundingClientRect();
    return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
  }, field);
  await page.mouse.click(p.x, p.y);
  await sleep(300);
  await page.evaluate((label) => [...document.querySelectorAll(".tabulator-menu .tabulator-menu-item")]
    .find((x) => x.textContent.trim().startsWith(label)).click(), label);
  await sleep(400);
  if (sub) {
    await page.evaluate((sub) => [...document.querySelectorAll(".tabulator-menu .tabulator-menu-item")]
      .find((x) => x.textContent.trim() === sub).click(), sub);
  }
  await L.settle(page, 900);
}
async function modalText(page) {
  await page.waitForSelector("#shiny-modal .modal-dialog", { timeout: 5000 });
  await sleep(700);
  return page.$eval("#shiny-modal", (e) => e.textContent.replace(/\s+/g, " ").trim());
}
async function typeInto(page, sel, text) {
  await page.click(sel, { clickCount: 3 });
  await page.keyboard.type(text);
  await sleep(700);
}
const previewText = (page) => page.$eval("#edit1-action_preview", (e) => e.textContent.replace(/\s+/g, " ").trim());
async function apply(page) {
  await page.click("#edit1-action_apply");
  await L.settle(page, 1500);
  await page.waitForFunction(() => !document.querySelector("#shiny-modal .modal-dialog"), { timeout: 5000 });
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

  async actions(page) {
    // a formula column after value
    await columnMenu(page, "c4", "Formula");
    await modalText(page);
    await typeInto(page, "#edit1-fx_name", "value2");
    await typeInto(page, "#edit1-fx_expr", "value * 2");
    let pv = await previewText(page);
    check(/New column value2/.test(pv) && /mutate\(value2 = value \* 2, \.after = value\)/.test(pv), "formula preview: " + pv.slice(0, 120));
    await page.screenshot({ path: path.join(out, "formula.png") });
    await apply(page);
    const titles = (await L.columns(page)).map((c) => c.title);
    check(titles.join() === "id,name,grp,value,value2,when", "new column placed after value: " + titles.join());
    // a formula with an error keeps the dialog open
    await columnMenu(page, "c4", "Formula");
    await modalText(page);
    await page.click('input[name="edit1-fx_target"][value="replace"]');
    await typeInto(page, "#edit1-fx_expr", "valu * 2");
    pv = await previewText(page);
    check(/no column called 'valu'/.test(pv), "formula error shown: " + pv);
    await page.click("#edit1-action_apply");
    await sleep(800);
    check(!!(await page.$("#shiny-modal .modal-dialog")), "dialog stays open on error");
    await page.click("#shiny-modal .modal-footer button.btn-default, #shiny-modal .modal-footer [data-bs-dismiss]");
    await sleep(700);
    // find and replace, ignoring case
    await columnMenu(page, "c2", "Find and replace");
    await modalText(page);
    await typeInto(page, "#edit1-rp_find", "alpha");
    await typeInto(page, "#edit1-rp_replace", "ALPHA");
    await page.click("#edit1-rp_case");
    await sleep(800);
    pv = await previewText(page);
    check(/2 cells change/.test(pv), "replace preview: " + pv.slice(0, 80));
    await apply(page);
    // recode groups
    await columnMenu(page, "c3", "Recode values");
    await modalText(page);
    const values = await page.$$eval("#shiny-modal .editr-recode-from", (els) => els.map((e) => e.textContent.trim()));
    check(values.join() === "a,b,c", "values to recode, most common first: " + values.join());
    await typeInto(page, "#edit1-rc_1", "A group");
    await typeInto(page, "#edit1-rc_2", "B group");
    await page.screenshot({ path: path.join(out, "recode.png") });
    await apply(page);
    // text to dates: one value cannot be read
    await columnMenu(page, "c5", "Change type", "Date");
    const msg = await modalText(page);
    check(/1 value cannot be turned into date/.test(msg) && /\bx\b/.test(msg), "type change asks: " + msg.slice(0, 100));
    await apply(page);
    const kinds = await page.evaluate(() => document.querySelector(".editr_grid").__editr.colMap.c5.kind);
    check(kinds === "date", "column is now dates: " + kinds);
    // sort, undo, redo
    await columnMenu(page, "c4", "Sort descending");
    let d = await L.gridData(page);
    check(d.map((r) => r.c1).join() === "6,5,3,1,2,4", "sorted by value: " + d.map((r) => r.c1).join());
    await page.click("#edit1-undo");
    await L.settle(page, 900);
    d = await L.gridData(page);
    check(d.map((r) => r.c1).join() === "1,2,3,4,5,6", "undo puts the rows back");
    await page.click("#edit1-redo");
    await L.settle(page, 900);
    // a typed edit in between
    await L.editCell(page, 6, "c4", "10");
    // history and code show the verbs
    await page.click("#edit1-open_code");
    await L.settle(page, 1200);
    const code = await page.$eval("#edit1-code", (e) => e.textContent);
    for (const v of ["mutate(value2 = value * 2, .after = value)", "gsub(\"alpha\", \"ALPHA\", name, ignore.case = TRUE)",
                     "case_match(grp", "as.Date(when, format = \"%Y-%m-%d\")", "arrange(desc(value))"]) {
      check(code.includes(v), "code has " + v);
    }
    await page.screenshot({ path: path.join(out, "code.png") });
    await page.click("#edit1-tab_history");
    await L.settle(page, 900);
    const tags = await page.$$eval("#edit1-history .editr-tag", (els) => els.map((e) => e.textContent.trim()));
    check(tags.join() === "Edit,Redo,Undo,Sort,Type,Recode,Replace,Formula", "history: " + tags.join());
    await done(page);
  },

  async rules(page) {
    const status = () => page.$eval("#edit1-problems_status", (e) => e.textContent.trim());
    const marked = () => page.$$eval(".tabulator-cell.editr-problem", (els) => els.length);
    check(await status() === "\u26a0 2 problems", "status: " + await status());
    check(await marked() === 3, "cells marked: " + await marked());
    const tip = await page.evaluate(() => {
      const t = document.querySelector(".editr_grid").__editr.table;
      return t.getRow(2).getCell("c2").getElement().title;
    });
    check(tip === "pH is between 0 and 14", "tooltip: " + tip);
    await page.click("#edit1-problems_status");
    await L.settle(page, 900);
    let rules = await page.$$eval("#edit1-problems .editr-rule-text", (els) => els.map((e) => e.textContent.trim()));
    check(rules.join("|") === "pH is between 0 and 14|End is after the start", "problems tab: " + rules.join("|"));
    // a rule from the column menu
    await columnMenu(page, "c1", "Rules");
    await modalText(page);
    await page.click("#edit1-ru_unique");
    await typeInto(page, "#edit1-ru_pattern", "S-[0-9]{2}");
    const pv = await page.$eval("#edit1-rules_preview", (e) => e.textContent.trim());
    check(/2 rows break/.test(pv), "rules preview: " + pv);
    await page.screenshot({ path: path.join(out, "rules-dialog.png") });
    await page.click("#edit1-rules_save");
    await L.settle(page, 1200);
    check(await status() === "\u26a0 4 problems", "after the new rule: " + await status());
    await page.screenshot({ path: path.join(out, "problems.png") });
    // clicking a problem selects its cell
    await page.evaluate(() => [...document.querySelectorAll("#edit1-problems .editr-rule-row")][0].click());
    await L.settle(page, 700);
    const sel = await page.evaluate(() => {
      const r = document.querySelector(".editr_grid").__editr.table.getRanges()[0];
      return r ? r.getCells()[0][0].getField() + ":" + r.getCells()[0][0].getRow().getIndex() : "";
    });
    check(sel === "c2:2", "problem selected: " + sel);
    // fix values
    await L.editCell(page, 2, "c2", "7.5");
    await L.editCell(page, 3, "c1", "S-03");
    check(await status() === "\u26a0 1 problem", "after fixing: " + await status());
    // Done asks, Keep editing, then return anyway
    await page.click("#done");
    let msg = await modalText(page);
    check(/1 row breaks/.test(msg) && /End is after the start/.test(msg), "Done asks: " + msg.slice(0, 90));
    await page.click("#shiny-modal .modal-footer [data-bs-dismiss], #shiny-modal .modal-footer [data-dismiss]");
    await sleep(800);
    await page.click("#done");
    await modalText(page);
    await page.click("#done_anyway");
    await sleep(1500);
  },

  async summary_form(page) {
    const rows = () => page.$eval("#edit1-rows", (e) => e.textContent.trim());
    const chips = () => page.$eval("#edit1-chips", (e) => e.textContent.trim());
    const sumText = (f) => page.$eval('.editr-summary[data-field="' + f + '"] .editr-sum-foot',
      (e) => [...e.children].map((x) => x.textContent.trim()).filter((x) => x).join(" "));
    await sleep(800);
    check(await page.$$eval(".editr-summary", (els) => els.length) === 4, "a summary for each column");
    check(await sumText("c2") === "13% NA 3 distinct", "site summary: " + await sumText("c2"));
    check(await sumText("c3") === "13% NA 7 distinct", "ph summary: " + await sumText("c3"));
    // a bar of the histogram filters the rows
    const bins = await page.$$eval('.editr-summary[data-field="c3"] g[data-act="bin"] title', (els) => els.map((e) => e.textContent));
    // 7.05 and 7.1 are in the bar from 7 to 7.2; 7.2 is in the next bar
    const i = bins.findIndex((t) => /^7 to 7\.2: 2 rows/.test(t));
    check(i >= 0, "ph bins: " + bins.join(" | "));
    await page.evaluate((i) => document.querySelectorAll('.editr-summary[data-field="c3"] g[data-act="bin"] .editr-sum-hit')[i]
      .dispatchEvent(new MouseEvent("click", { bubbles: true })), i);
    await L.settle(page, 900);
    check(await rows() === "2 of 8 rows · 4 columns" && /7 ≤ ph < 7\.2/.test(await chips()),
      "bin filter: " + await rows() + " / " + await chips());
    // the last bar includes its upper end
    await page.evaluate((n) => document.querySelectorAll('.editr-summary[data-field="c3"] g[data-act="bin"] .editr-sum-hit')[n - 1]
      .dispatchEvent(new MouseEvent("click", { bubbles: true })), bins.length);
    await L.settle(page, 900);
    check(await rows() === "1 of 8 rows · 4 columns" && /ph 8\.4.8\.6/.test(await chips()),
      "last bin filter: " + await rows() + " / " + await chips());
    // the missing share shows the rows where it is missing
    await page.click("#edit1-chips .editr-chip button");
    await L.settle(page, 700);
    await page.click('.editr-summary[data-field="c2"] [data-act="missing"]');
    await L.settle(page, 900);
    check(/site is missing/.test(await chips()) && await rows() === "1 of 8 rows · 4 columns", "missing filter: " + await chips() + " / " + await rows());
    await page.click("#edit1-chips .editr-chip button");
    await L.settle(page, 700);
    // a value of the most common values
    await page.click('.editr-summary[data-field="c2"] [data-act="value"]');
    await L.settle(page, 900);
    check(/site is north/.test(await chips()) && await rows() === "3 of 8 rows · 4 columns", "value filter: " + await chips());
    await page.click("#edit1-chips .editr-chip button");
    await L.settle(page, 700);
    // a value of a text column (its header filter is "contains") filters
    // on that exact value
    await page.click('.editr-summary[data-field="c1"] [data-act="value"]');
    await L.settle(page, 900);
    check(/sample is S-01/.test(await chips()) && await rows() === "1 of 8 rows · 4 columns", "exact value filter: " + await chips() + " / " + await rows());
    await page.click("#edit1-chips .editr-chip button");
    await L.settle(page, 700);
    // details
    await page.click('.editr-summary[data-field="c3"] [data-act="details"]');
    await sleep(400);
    const det = await page.$eval(".editr-sum-details", (e) => [...e.querySelectorAll("tr")]
      .map((r) => [...r.children].map((x) => x.textContent).join(" ")).join(" | "));
    check(/Smallest 6\.4/.test(det) && /Largest 8\.6/.test(det) && /Median 7\.2/.test(det), "details: " + det.slice(0, 160));
    await page.keyboard.press("Escape");
    // hide and show the summaries
    await page.click("#edit1-summary");
    await L.settle(page, 600);
    check(await page.$eval(".editr_grid", (e) => e.classList.contains("editr-summaries-off")), "summaries hidden");
    await page.click("#edit1-summary");
    await L.settle(page, 600);
    // the record form follows the selected row
    await L.selectCell(page, 3, "c1");
    await page.click("#edit1-open_record");
    await L.settle(page, 900);
    const title = () => page.$eval("#edit1-record .editr-form-title", (e) => e.textContent);
    check(await title() === "Row 3", "form shows row 3: " + await title());
    // a value that breaks a rule is shown under its field
    await page.click("#edit1-grid-form-c3", { clickCount: 3 });
    await page.keyboard.type("15");
    await page.keyboard.press("Enter");
    await L.settle(page, 1200);
    const msg = () => page.$eval('#edit1-record .editr-form-field[data-field="c3"]',
      (e) => (e.classList.contains("editr-form-bad") ? "bad: " : "") + e.querySelector(".editr-form-msg").textContent);
    check(await msg() === "bad: pH is between 0 and 14", "rule message in the form: " + await msg());
    await page.click("#edit1-grid-form-c3", { clickCount: 3 });
    await page.keyboard.type("7.25");
    await page.keyboard.press("Enter");
    await L.settle(page, 900);
    check(Number((await L.gridData(page)).find((r) => r.__editr_id__ === 3).c3) === 7.25, "form edit reaches the grid");
    check(await msg() === "", "fixed value clears the message: " + await msg());
    await page.select("#edit1-grid-form-c4", "FALSE");
    await L.settle(page, 900);
    await L.selectCell(page, 5, "c2");
    await L.settle(page, 500);
    check(await title() === "Row 5", "form follows the grid: " + await title());
    // a new record
    await page.evaluate(() => [...document.querySelectorAll("#edit1-record .editr-form-head button")].find((b) => b.textContent === "New").click());
    await L.settle(page, 900);
    check(await title() === "Row 9", "new record: " + await title());
    await page.keyboard.type("S-09");
    await page.keyboard.press("Enter");
    await L.settle(page, 700);
    await page.select("#edit1-grid-form-c2", "west");
    await L.settle(page, 700);
    await page.type("#edit1-grid-form-c3", "6.8");
    await page.keyboard.press("Enter");
    await L.settle(page, 900);
    await page.screenshot({ path: path.join(out, "form.png") });
    await done(page);
  },

  async labelled(page) {
    const grid = () => L.gridData(page);
    const val = async (id, f) => (await grid()).find((r) => r.__editr_id__ === id)[f];
    const cellText = (id, f) => page.evaluate((id, f) => document.querySelector(".editr_grid").__editr.table
      .getRow(id).getCell(f).getElement().textContent.replace(/\s+/g, " ").trim(), id, f);
    const chips = () => page.$eval("#edit1-chips", (e) => e.textContent.trim());
    await sleep(800);
    // labels in the header and the cells
    const head = await page.evaluate(() => {
      const t = document.querySelector(".editr_grid").__editr.table;
      const el = t.getColumn("c2").getElement();
      return [el.querySelector(".editr-badge").textContent, el.querySelector(".editr-name").title];
    });
    check(head[0] === "lbl" && head[1] === "Sex of respondent", "badge and variable label: " + head);
    check(await cellText(1, "c2") === "1 Male", "value with its label: " + await cellText(1, "c2"));
    check(await cellText(3, "c3") === "-99 No answer" && await page.evaluate(() => document.querySelector(".editr_grid").__editr.table
      .getRow(3).getCell("c3").getElement().classList.contains("editr-user-na")), "user-defined missing value is muted");
    // a label typed into a cell is stored as its value
    await L.editCell(page, 1, "c2", "Female");
    check(await val(1, "c2") === 2, "typed label stored as its value");
    // text that is neither asks, without turning the codes into text
    await L.editCell(page, 2, "c2", "nobody");
    const guard = await page.$$eval(".editr-guard button", (b) => b.map((x) => x.textContent.trim()));
    check(guard.length === 2 && !guard.some((x) => /text/.test(x)), "guard without convert: " + guard);
    await page.click('.editr-guard [data-act="cancel"]');
    await L.settle(page, 600);
    check(await val(2, "c2") === 2, "cancelled edit kept the value");
    // pasted labels
    await L.selectCell(page, 3, "c2");
    await L.paste(page, "Refused\r\nmale\r\n");
    check(await val(3, "c2") === 9 && await val(4, "c2") === 1, "pasted labels stored as values");
    // the Labels dialog
    const menuButton = (field) => page.evaluate((f) => document.querySelector(".editr_grid").__editr.table
      .getColumn(f).getElement().querySelector(".tabulator-header-popup-button").click(), field);
    const pick = (label) => page.evaluate((l) => [...document.querySelectorAll(".tabulator-menu .tabulator-menu-item")]
      .find((x) => x.textContent.trim().startsWith(l)).click(), label);
    await menuButton("c2");
    await sleep(400);
    await pick("Labels");
    await page.waitForSelector("#edit1-lb_values");
    await sleep(900);
    check(await page.$eval("#edit1-lb_values", (e) => e.value) === "1 = Male\n2 = Female\n9 = Refused", "dialog lists the value labels");
    await page.click("#edit1-lb_values");
    await page.keyboard.down("Control"); await page.keyboard.press("End"); await page.keyboard.up("Control");
    await page.keyboard.type("\n3 = Other");
    await sleep(1500);
    const prev = await page.$eval("#edit1-action_preview", (e) => e.textContent.replace(/\s+/g, " "));
    check(/4 value labels/.test(prev) && /"Other" = 3/.test(prev), "preview: " + prev.slice(0, 160));
    await page.click("#edit1-action_apply");
    await L.settle(page, 1500);
    await L.editCell(page, 6, "c2", "Other");
    check(await val(6, "c2") === 3, "new label can be typed");
    // the variable label of a plain column
    await menuButton("c5");
    await sleep(400);
    await pick("Labels");
    await page.waitForSelector("#edit1-lb_label");
    await sleep(900);
    await page.click("#edit1-lb_label", { clickCount: 3 });
    await page.keyboard.type("Age (years)");
    await sleep(1200);
    await page.click("#edit1-action_apply");
    await L.settle(page, 1500);
    // the summary lists labelled values and filters by them
    await page.click('.editr-summary[data-field="c4"] [data-act="value"]');
    await L.settle(page, 900);
    check(/country is Australia/.test(await chips()), "chip uses the label: " + await chips());
    await page.click("#edit1-chips .editr-chip button");
    await L.settle(page, 700);
    // the record form suggests labels and stores values
    await L.selectCell(page, 5, "c1");
    await page.click("#edit1-open_record");
    await L.settle(page, 900);
    check(await page.$eval("#edit1-grid-form-c2", (e) => e.value) === "2 \u00b7 Female", "form shows the label");
    const opts = await page.$$eval("#edit1-record datalist[id$='-c4'] option", (o) => o.map((x) => x.value));
    check(opts.join("|") === "AU \u00b7 Australia|NZ \u00b7 New Zealand", "form suggestions: " + opts);
    await page.click("#edit1-grid-form-c4", { clickCount: 3 });
    await page.keyboard.type("New Zealand");
    await page.keyboard.press("Enter");
    await L.settle(page, 900);
    check(await val(5, "c4") === "NZ", "form label stored as its value");
    // a new record
    await page.evaluate(() => [...document.querySelectorAll("#edit1-record .editr-form-head button")].find((b) => b.textContent === "New").click());
    await L.settle(page, 900);
    await page.keyboard.type("R09");
    await page.keyboard.press("Enter");
    await L.settle(page, 600);
    await page.keyboard.type("2 \u00b7 Female");
    await page.keyboard.press("Enter");
    await L.settle(page, 600);
    await page.keyboard.type("-99");
    await page.keyboard.press("Enter");
    await L.settle(page, 600);
    await page.keyboard.type("AU");
    await page.keyboard.press("Enter");
    await L.settle(page, 600);
    await page.keyboard.type("57");
    await page.keyboard.press("Enter");
    await L.settle(page, 900);
    check(await val(9, "c2") === 2 && await val(9, "c3") === -99, "new record from the form");
    await page.screenshot({ path: path.join(out, "labelled.png") });
    await done(page);
  },

  async fill(page) {
    const col = async (f) => (await L.gridData(page)).map((r) => r[f]);
    const handle = () => page.evaluate(() => {
      const b = document.querySelector(".tabulator-range-fill-handle").getBoundingClientRect();
      return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
    });
    // drag the handle of a selection to a cell (Ctrl: copy)
    const drag = async (id, field, ctrl = false) => {
      const h = await handle();
      const t = await L.cellCentre(page, id, field);
      await page.mouse.move(h.x, h.y);
      await page.mouse.down();
      await page.mouse.move(t.x, t.y, { steps: 8 });
      await sleep(200);
      if (ctrl) await page.keyboard.down("Control");
      await page.mouse.up();
      if (ctrl) await page.keyboard.up("Control");
      await L.settle(page, 900);
    };
    const select = async (a, fa, b, fb) => {
      await L.selectCell(page, a, fa);
      await L.selectCell(page, b, fb, true);
    };
    // series of text, numbers and month ends; colours repeat
    await select(1, "c1", 2, "c4");
    await drag(5, "c4");
    check(JSON.stringify(await col("c1")) === JSON.stringify(["Week 1", "Week 2", "Week 3", "Week 4", "Week 5", null]),
          "text series: " + JSON.stringify(await col("c1")));
    check(JSON.stringify(await col("c2")) === JSON.stringify([10, 20, 30, 40, 50, null]), "number series");
    check(JSON.stringify(await col("c3")) === JSON.stringify(["2026-01-31", "2026-02-28", "2026-03-31", "2026-04-30", "2026-05-31", null]),
          "month ends: " + JSON.stringify(await col("c3")));
    check(JSON.stringify(await col("c4")) === JSON.stringify(["grey50", "steelblue", "grey50", "steelblue", "grey50", null]),
          "colours repeated");
    await page.screenshot({ path: path.join(out, "fill.png") });
    // Ctrl copies instead
    await select(1, "c2", 2, "c2");
    await drag(6, "c2", true);
    check(JSON.stringify(await col("c2")) === JSON.stringify([10, 20, 10, 20, 10, 20]), "Ctrl copies: " + JSON.stringify(await col("c2")));
    // the colour picker: a swatch, and an R colour typed
    let p = await L.cellCentre(page, 6, "c4");
    await page.mouse.click(p.x, p.y, { clickCount: 2 });
    await page.waitForSelector(".editr-colour-box");
    await sleep(300);
    const picks = await page.$$eval(".editr-colour-pick", (bs) => bs.map((b) => b.title));
    check(picks.slice(0, 3).join() === "grey50,steelblue,black", "column colours first: " + picks.slice(0, 4).join());
    await page.screenshot({ path: path.join(out, "colour.png") });
    const b = await page.evaluate(() => { const r = document.querySelectorAll(".editr-colour-pick")[2].getBoundingClientRect(); return { x: r.x + 6, y: r.y + 6 }; });
    await page.mouse.click(b.x, b.y);
    await L.settle(page, 800);
    p = await L.cellCentre(page, 3, "c4");
    await page.mouse.click(p.x, p.y, { clickCount: 2 });
    await page.waitForSelector(".editr-colour-edit input");
    await page.keyboard.down("Control"); await page.keyboard.press("KeyA"); await page.keyboard.up("Control");
    await page.keyboard.type("darkolivegreen3");
    await page.keyboard.press("Enter");
    await L.settle(page, 800);
    const sw = await page.evaluate(() => document.querySelector(".editr_grid").__editr.table.getRow(3).getCell("c4")
      .getElement().querySelector(".editr-swatch").style.background);
    check(sw === "rgb(162, 205, 90)", "R colour swatch: " + sw);
    // comments can all be deleted at once (after a question)
    await L.selectCell(page, 1, "c2");
    await page.keyboard.down("Shift"); await page.keyboard.press("F2"); await page.keyboard.up("Shift");
    await page.waitForSelector(".editr-comment-box textarea");
    await page.keyboard.type("first dose");
    await page.keyboard.down("Control"); await page.keyboard.press("Enter"); await page.keyboard.up("Control");
    await L.settle(page, 600);
    await L.rowMenu(page, 2, "c1", "Delete all comments (1)");
    await page.waitForSelector(".editr-comments-clear");
    await page.click('.editr-comments-clear [data-act="delete"]');
    await L.settle(page, 800);
    check(await page.$$eval(".tabulator-cell.editr-commented", (c) => c.length) === 0, "all comments deleted");
    // a paste wider than the table adds a column; undo takes it away again
    await L.selectCell(page, 1, "c4");
    await L.paste(page, "red\tnote\n");
    await L.settle(page, 900);
    const titles = (await L.columns(page)).map((c) => c.title);
    check(titles.join() === "week,n,day,colour,V1", "paste added V1: " + titles.join());
    await page.click("#edit1-undo");
    await L.settle(page, 1200);
    check((await L.columns(page)).length === 4 && (await col("c4"))[0] === "grey50", "undo removed the column");
    await done(page);
  },

  async comments(page) {
    const commented = () => page.evaluate(() => [...document.querySelectorAll(".tabulator-cell.editr-commented")]
      .map((c) => c.closest(".tabulator-row").querySelector(".editr-rowheader").textContent + "|" + c.getAttribute("tabulator-field")).sort());
    check(JSON.stringify(await commented()) === JSON.stringify(["3|c2"]), "comment of the data shown: " + JSON.stringify(await commented()));
    // add one from the menu
    await L.rowMenu(page, 5, "c3", "Add comment");
    await page.waitForSelector(".editr-comment-box textarea");
    await page.keyboard.type("Re-measured");
    await page.keyboard.down("Control"); await page.keyboard.press("Enter"); await page.keyboard.up("Control");
    await L.settle(page, 800);
    // and one with Shift+F2
    await L.selectCell(page, 2, "c4");
    await page.keyboard.down("Shift"); await page.keyboard.press("F2"); await page.keyboard.up("Shift");
    await page.waitForSelector(".editr-comment-box textarea");
    await page.keyboard.type("Check the units");
    await page.screenshot({ path: path.join(out, "comment.png") });
    await page.evaluate(() => [...document.querySelectorAll(".editr-comment-box button")].find((b) => b.textContent.trim() === "Save").click());
    await L.settle(page, 800);
    // "Delete all comments" asks first; Cancel keeps them
    await L.rowMenu(page, 1, "c3", "Delete all comments (3)");
    await page.waitForSelector(".editr-comments-clear");
    check(/Delete all 3 comments\?/.test(await page.$eval(".editr-comments-clear", (e) => e.textContent)),
          "delete all asks first");
    await page.screenshot({ path: path.join(out, "comments-clear.png") });
    await page.click('.editr-comments-clear [data-act="cancel"]');
    await L.settle(page, 600);
    check((await commented()).length === 3, "Cancel keeps the comments");
    // delete the one the data came with
    await L.rowMenu(page, 3, "c2", "Delete comment");
    await L.settle(page, 800);
    check(JSON.stringify(await commented()) === JSON.stringify(["2|c4", "5|c3"]), "comments in the grid: " + JSON.stringify(await commented()));
    // earlier sessions in the History tab
    await L.editCell(page, 1, "c2", "22");
    await openHistory(page);
    const hist = await page.$eval("#edit1-history", (e) => e.textContent.replace(/\s+/g, " "));
    check(/This session 1/.test(hist) && /Earlier sessions 1/.test(hist) && /ana/.test(hist) &&
          /Edited hp in row Valiant/.test(hist) && /From the manual/.test(hist),
          "earlier session listed: " + hist.slice(0, 200));
    await page.evaluate(() => { document.querySelector(".editr-session").open = true; });
    await sleep(300);
    await page.screenshot({ path: path.join(out, "history.png") });
    await done(page);
  },

  // issues reported for DataEditR 1.x
  async issues(page) {
    const t = (fn, ...a) => page.evaluate(fn, ...a);
    const text = (id, f) => t((id, f) => document.querySelector(".editr_grid").__editr.table.getRow(id)
      .getCell(f).getElement().innerText.trim(), id, f);
    const html = (id, f) => t((id, f) => document.querySelector(".editr_grid").__editr.table.getRow(id)
      .getCell(f).getElement().innerHTML, id, f);
    const col = async (f) => (await L.gridData(page)).map((r) => r[f]);
    const cols = await L.columns(page);
    const f = Object.fromEntries(cols.map((c) => [c.title, c.field]));
    // #40 row names out of sight
    check(cols.every((c) => c.title.trim() !== ""), "row names column hidden");
    // #76 dropdown shows the names
    check(await text(1, f.state) === "New South Wales", "dropdown shows the name: " + await text(1, f.state));
    // #69 html cells, without scripts or handlers
    const h1 = await html(1, f.photo), h2 = await html(2, f.photo);
    check(/<br>/.test(h1) && /target="_blank"/.test(h1) && /<b>bold<\/b>/.test(h2) &&
          !/script|onerror/.test(h2), "html shown and cleaned");
    // #74 widths
    const widths = await t(() => Object.fromEntries(document.querySelector(".editr_grid").__editr.table
      .getColumns().map((c) => [c.getField(), c.getWidth()])));
    check(widths[f.n] === 80 && widths[f.note] <= 320, "col_widths and long text at most 320 px: " +
          widths[f.n] + ", " + widths[f.note]);
    await page.screenshot({ path: path.join(out, "issues.png") });
    // #76 pick a name; paste a name
    let p = await L.cellCentre(page, 4, f.state);
    await page.mouse.click(p.x, p.y, { clickCount: 2 });
    await page.waitForSelector(".tabulator-edit-list-item");
    await sleep(300);
    const it = await t(() => { const e = [...document.querySelectorAll(".tabulator-edit-list-item")]
      .find((x) => x.textContent.trim() === "Victoria"); const b = e.getBoundingClientRect();
      return { x: b.x + 10, y: b.y + b.height / 2 }; });
    await page.mouse.click(it.x, it.y);
    await L.settle(page, 800);
    await L.selectCell(page, 5, f.state);
    await L.paste(page, "queensland\n");
    check(await text(4, f.state) === "Victoria" && await text(5, f.state) === "Queensland",
          "names picked and pasted");
    // #67 "!" filter; #68 three filters together
    await page.click('.tabulator-col[tabulator-field="' + f.code + '"] .tabulator-header-filter input');
    await page.keyboard.type("!ab");
    await L.settle(page, 900);
    const active = () => t(() => document.querySelector(".editr_grid").__editr.table.getRows("active")
      .map((r) => r.getIndex()));
    check(JSON.stringify(await active()) === JSON.stringify([3, 4]), "!ab keeps rows 3 and 4: " + JSON.stringify(await active()));
    check(/code does not contain/.test(await page.$eval("#edit1-chips", (e) => e.textContent)), "chip says does not contain");
    await t((fs, fst) => { const tb = document.querySelector(".editr_grid").__editr.table;
      tb.setHeaderFilterValue(fs, ["virginica", "setosa"]); tb.setHeaderFilterValue(fst, ["VIC"]); },
            f.species, f.state);
    await L.settle(page, 900);
    check(JSON.stringify(await active()) === JSON.stringify([4]), "three filters together: " + JSON.stringify(await active()));
    await page.click("#edit1-chips .editr-chip-clear");
    await L.settle(page, 900);
    check((await active()).length === 5, "filters cleared");
    // #53 insert and delete cells
    await L.selectCell(page, 2, f.n);
    await L.rowMenu(page, 2, f.n, "Insert cells, shift down");
    check(JSON.stringify(await col(f.n)) === JSON.stringify([1, null, 2, 3, 4]), "cells moved down: " + JSON.stringify(await col(f.n)));
    await L.selectCell(page, 1, f.n);
    await L.rowMenu(page, 1, f.n, "Delete cells, shift up");
    check(JSON.stringify(await col(f.n)) === JSON.stringify([null, 2, 3, 4, null]), "cells moved up: " + JSON.stringify(await col(f.n)));
    // #36 paste with the first row as column names
    await L.selectCell(page, 1, f.species);
    await page.keyboard.down("Control"); await page.keyboard.down("Shift");
    await page.keyboard.press("KeyV");
    await page.keyboard.up("Shift"); await page.keyboard.up("Control");
    await L.paste(page, "kind\textra\nA\tB\n");
    await L.settle(page, 900);
    const titles = (await L.columns(page)).map((c) => c.title);
    check(titles.includes("kind") && titles.includes("extra") && !titles.includes("species"),
          "columns named by the pasted header: " + titles.join(","));
    await done(page);
  },

  async shiny_modal(page) {
    const t = (fn, ...a) => page.evaluate(fn, ...a);
    const info = () => t(() => { const g = document.getElementById("edit-grid"); const tb = g.__editr.table;
      return { rows: tb.getRows().length, shown: g.querySelectorAll(".tabulator-row").length,
               height: g.getBoundingClientRect().height,
               cols: tb.getColumns().map((c) => c.getDefinition().title).filter((x) => x && x.trim()) }; });
    const open = async () => {
      await t(() => document.getElementById("modify").click());
      await page.waitForFunction(() => { const g = document.getElementById("edit-grid");
        return g && g.__editr && g.__editr.ready; }, { timeout: 10000 });
      await sleep(1200);
    };
    const close = async () => {
      await page.keyboard.press("Escape");
      await sleep(300);
      await t(() => { const b = document.querySelector("#shiny-modal button[data-dismiss], #shiny-modal button[data-bs-dismiss]"); if (b) b.click(); });
      await page.waitForFunction(() => !document.getElementById("edit-grid"), { timeout: 5000 }).catch(() => null);
      await sleep(800);
    };
    await sleep(1500); // the modal fades in
    let g = await info();
    check(g.rows === 5 && g.shown === 5 && g.height > 100, "the grid is drawn in the modal (#25): " + g.shown + " rows");
    // the modal may still be settling: try the double-click again if needed
    for (let i = 0; i < 3; i++) {
      try {
        await L.renameColumn(page, "c2", "MPG");
        break;
      } catch (e) {
        await sleep(1000);
      }
    }
    await L.editCell(page, 1, "c3", "8");
    const tab = await page.$eval("#tab", (e) => e.textContent.replace(/\s+/g, " "));
    check(/MPG/.test(tab) && /8\.00/.test(tab), "rename and edit in the modal reach R (#24)");
    await page.screenshot({ path: path.join(out, "modal.png") });
    await close();
    await t(() => Shiny.setInputValue("ds", "iris"));
    await sleep(1500);
    await open();
    g = await info();
    check(g.rows === 20 && g.cols.includes("Species"), "other data shown after switching (#61): " + g.rows + " rows");
    await close();
    await t(() => Shiny.setInputValue("ds", "mtcars"));
    await sleep(1500);
    await open();
    g = await info();
    check(g.rows === 5 && g.cols.includes("mpg"), "the first data again, not repeated (#58): " + g.rows + " rows");
    await close();
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
      "Filled 1 cell in cyl", "Edited mpg in row Mazda RX4"]), "history titles: " + h.map((x) => x.title).join(" | "));
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

  async db_rows(page) {
    const info = () => page.$eval("#edit1-load_rows", (e) => e.textContent.replace(/\s+/g, " ").trim());
    check(await info() === "Rows 500 of 5,000", "rows button: " + await info());
    check((await L.gridData(page)).length === 500, "500 rows loaded");
    await L.editCell(page, 1, "c3", "1000");
    // load other rows: the database filters and sorts them
    await page.click("#edit1-load_rows");
    await page.waitForSelector("#edit1-ld_filter");
    await sleep(800);
    await page.type("#edit1-ld_filter", 'carrier == "DL" & delay >= 200');
    await page.select("#edit1-ld_sort", "delay");
    await page.click("#edit1-ld_desc");
    await page.click("#edit1-ld_limit", { clickCount: 3 });
    await page.keyboard.type("20");
    await sleep(1500);
    const prev = await page.$eval("#edit1-load_preview", (e) => e.textContent.replace(/\s+/g, " ").trim());
    check(/^\d+ rows match; the first 20 will be loaded/.test(prev), "preview: " + prev);
    const n = Number(prev.split(" ")[0]);
    check(await page.$("#edit1-load_save") !== null, "unsaved changes are saved or discarded");
    await page.click("#edit1-load_save");
    await L.settle(page, 2500);
    const d = await L.gridData(page);
    const delays = d.map((r) => Number(r.c3));
    check(d.length === 20 && d.every((r) => r.c2 === "DL") &&
          delays.every((x, i) => i === 0 || delays[i - 1] >= x), "loaded rows filtered and sorted");
    check(await info() === "Rows 20 of " + n, "rows button after loading: " + await info());
    // edit a loaded row and commit
    await L.editCell(page, d[0].__editr_id__, "c3", "-1");
    await page.click("#done");
    await page.waitForSelector("#db_commit", { timeout: 5000 });
    await sleep(900);
    await page.click("#db_commit");
    await sleep(2000);
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

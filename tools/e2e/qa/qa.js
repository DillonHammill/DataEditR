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

// is the whole of an open menu on top of the page (and so clickable)?
const onTop = (page, sel) => page.evaluate((sel) => {
  const menu = document.querySelector(sel);
  if (!menu) return false;
  const r = menu.getBoundingClientRect();
  for (let i = 0; i <= 8; i++) {
    for (let j = 0; j <= 8; j++) {
      const hit = document.elementFromPoint(r.left + 10 + (r.width - 20) * i / 8,
                                            r.top + 10 + (r.height - 20) * j / 8);
      if (!menu.contains(hit)) return false;
    }
  }
  return true;
}, sel);

// the Filters panel: ids of the rows on screen, opening it and adding filters
const shownIds = (page) => page.evaluate(() => document.querySelector(".editr_grid").__editr.table
  .getRows("active").map((r) => r.getIndex()).sort((a, b) => a - b));
const editr = (page, fn, ...args) => page.evaluate(
  `(${fn})(document.querySelector(".editr_grid").__editr, ...${JSON.stringify(args)})`);
const panelOpen = (page) => page.$eval("#edit1-panel", (p) => p.getAttribute("data-open") === "filters");
async function openFilters(page) {
  if (!(await panelOpen(page))) await page.click("#edit1-filter");
  await page.waitForSelector("#edit1-filters .editr-fp-add", { visible: true });
  await sleep(200);
}
async function closeFilters(page) {
  if (await panelOpen(page)) await page.click("#edit1-filter");
  await L.settle(page, 400);
}
const filterCount = (page) => page.$eval("#edit1-filters .editr-fp-count", (e) => e.textContent);
const cardCount = (page) => page.$$eval("#edit1-filters .editr-fp-card", (r) => r.length);
// the filters of the panel as "field logic values"
const cardsNow = (page) => editr(page, (g) => g.fp.cards.map((c) => [c.field, c.logic].concat(c.values).join(" ")));
const openCard = (page) => page.$("#edit1-filters .editr-fp-card.editr-fp-open");
const cardText = (page, sel) => page.$eval("#edit1-filters .editr-fp-card.editr-fp-open " + sel, (e) => e.textContent);
async function addCard(page, field) {
  await page.click("#edit1-filters .editr-fp-add");
  const opt = `#edit1-filters .editr-fp-colopt[data-field="${field}"]`;
  await page.waitForSelector(opt);
  await page.click(opt);
  await sleep(150);
}
async function setLogic(page, logic) {
  const sel = await (await openCard(page)).$(".editr-fp-logic");
  if (await sel.evaluate((s) => s.value) !== logic) {
    await sel.select(logic);
    await sleep(150);
  }
}
// fill the open card as a person would: tick values, press TRUE or FALSE,
// type words, a pattern or the ends of a range
async function fillCard(page, logic, values) {
  await setLogic(page, logic);
  const card = await openCard(page);
  const shown = (v) => (v === "NA" ? "missing" : String(v));
  if (await card.$(".editr-fp-opts")) {
    for (const v of values) {
      const ok = await card.evaluate((el, want) => {
        const o = [...el.querySelectorAll(".editr-fp-opt")]
          .find((x) => x.querySelector(".editr-fp-val").textContent === want);
        if (o) o.querySelector("input").click();
        return !!o;
      }, shown(v));
      if (!ok) throw new Error("no value " + v);
    }
  } else if (await card.$(".editr-fp-bool")) {
    for (const v of values) {
      const ok = await card.evaluate((el, want) => {
        const b = [...el.querySelectorAll(".editr-fp-choice")].find((x) => x.firstElementChild.textContent === want);
        if (b) b.click();
        return !!b;
      }, shown(v));
      if (!ok) throw new Error("no choice " + v);
    }
  } else if (await card.$(".editr-fp-words")) {
    const input = await card.$(".editr-fp-words input");
    for (const v of values) {
      await input.focus();
      await page.keyboard.type(String(v));
      await page.keyboard.press("Enter");
    }
  } else if (await card.$(".editr-fp-pattern")) {
    await (await card.$(".editr-fp-pattern")).focus();
    await page.keyboard.type(String(values[0]));
  } else {
    const inputs = await card.$$(".editr-fp-ends input");
    for (let i = 0; i < values.length; i++) {
      const type = await inputs[i].evaluate((e) => e.type);
      if (type === "date" || type === "datetime-local") {
        await inputs[i].evaluate((e, v) => {
          e.value = v;
          e.dispatchEvent(new Event("input", { bubbles: true }));
        }, String(values[i]).replace(" ", "T"));
      } else {
        await inputs[i].click({ clickCount: 3 });
        await page.keyboard.type(String(values[i]));
      }
    }
  }
}
async function addFilter(page, field, logic, values = []) {
  await addCard(page, field);
  await fillCard(page, logic, values);
}
// replace every filter with these, wait for the grid to follow, close the panel
async function setFilters(page, rules) {
  await openFilters(page);
  const clear = await page.$("#edit1-filters .editr-fp-clear");
  if (clear) await clear.click();
  await sleep(100);
  for (const r of rules) await addFilter(page, ...r);
  await sleep(350);
  await closeFilters(page);
}

// the Code tab (the panel is left on it)
async function codeNow(page) {
  await page.evaluate(() => DataEditR.panel("edit1-", "code", false));
  await L.settle(page, 600);
  return page.$eval("#edit1-code", (e) => e.textContent);
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

  // conditional formatting (R/formats.R)
  async formats(page) {
    const t = (fn, ...a) => page.evaluate(fn, ...a);
    const st = (id, f) => t((id, f) => { const e = document.querySelector(".editr_grid").__editr.table.getRow(id).getCell(f).getElement();
      const s = getComputedStyle(e); return { bg: s.backgroundColor, color: s.color, weight: s.fontWeight,
        changed: e.classList.contains("editr-changed") }; }, id, f);
    const plain = "rgba(0, 0, 0, 0)";
    // formats from code: cells, whole rows and a colour scale
    check((await st(3, "c2")).bg === "rgb(220, 252, 231)" && (await st(3, "c2")).color === "rgb(22, 101, 52)",
          "mpg 22.8 > 22 is green: " + JSON.stringify(await st(3, "c2")));
    check((await st(1, "c2")).bg === plain, "mpg 21 is not formatted: " + (await st(1, "c2")).bg);
    check((await st(5, "c4")).bg === "rgb(255, 255, 224)" && (await st(5, "c1")).bg === "rgb(255, 255, 224)",
          "V8 row 5 is light yellow in every column: " + (await st(5, "c4")).bg);
    const hpMax = await st(7, "c5"), hpMin = await st(8, "c5");
    check(hpMax.bg === "rgb(248, 113, 113)" && hpMin.bg === "rgb(255, 255, 255)",
          "hp scale from white (62) to red (245): " + hpMin.bg + " " + hpMax.bg);
    await page.screenshot({ path: path.join(out, "formats.png") });
    // an edit changes the formatting, and edited cells are highlighted by default
    await L.editCell(page, 1, "c2", "30");
    await L.settle(page, 1000);
    const e1 = await st(1, "c2");
    check(e1.bg === "rgb(220, 252, 231)" && e1.changed, "edited mpg 30 is green and highlighted: " + JSON.stringify(e1));
    await L.editCell(page, 2, "c3", "8");
    await L.settle(page, 1000);
    check((await st(2, "c6")).bg === "rgb(255, 255, 224)", "row 2 is V8 after the edit: " + (await st(2, "c6")).bg);
    // a format from the column menu: whole rows where disp is above 200, in red
    await columnMenu(page, "c4", "Conditional formatting");
    const m = await modalText(page);
    check(/Conditional formatting for disp/.test(m) && /set by the app/.test(m), "dialog lists the app's formats: " + m.slice(0, 160));
    await page.select("#edit1-fm_op", "gt");
    await sleep(300);
    await typeInto(page, "#edit1-fm_value", "200");
    await page.click('input[name="edit1-fm_apply"][value="row"]');
    await page.click('input[name="edit1-fm_style"][value="red"]');
    await sleep(900);
    const pv = await page.$eval("#edit1-formats_preview", (e) => e.textContent.trim());
    check(/^4 rows match/.test(pv), "preview: " + pv);
    await page.screenshot({ path: path.join(out, "formats-dialog.png") });
    await page.click("#edit1-format_add");
    await L.settle(page, 1200);
    const r4 = await st(4, "c2");
    check(r4.bg === "rgb(254, 226, 226)" && r4.color === "rgb(153, 27, 27)", "row 4 (disp 258) is red: " + JSON.stringify(r4));
    // a colour scale from the menu on drat, then remove the red rows again
    await columnMenu(page, "c6", "Conditional formatting");
    await modalText(page);
    await page.click('input[name="edit1-fm_type"][value="scale"]');
    await sleep(300);
    await page.click('input[name="edit1-fm_scale"][value="white_blue"]');
    await sleep(900);
    const pv2 = await page.$eval("#edit1-formats_preview", (e) => e.textContent.trim());
    check(/^Shades 8 values from 2.76 to 3.9\b/.test(pv2), "scale preview: " + pv2);
    await page.click("#edit1-format_add");
    await L.settle(page, 1200);
    await L.selectCell(page, 1, "c1"); // the menu selected the column
    check((await st(6, "c6")).bg === "rgb(255, 255, 255)" && (await st(1, "c6")).bg === "rgb(96, 165, 250)",
          "drat scale: " + (await st(6, "c6")).bg + " " + (await st(1, "c6")).bg);
    await columnMenu(page, "c4", "Conditional formatting");
    const m2 = await modalText(page);
    check(/list\(~ disp > 200, fill = "#FEE2E2", color = "#991B1B", columns = "all"\)/.test(m2),
          "the format's code is shown: " + m2.slice(0, 300));
    await page.screenshot({ path: path.join(out, "formats-list.png") });
    await page.evaluate(() => [...document.querySelectorAll("#shiny-modal .editr-fmt-remove")][0].click());
    await L.settle(page, 1000);
    check(!/disp > 200/.test(await modalText(page)), "removed from the list");
    await page.keyboard.press("Escape");
    await L.settle(page, 800);
    check((await st(4, "c2")).bg === plain, "row 4 is plain again: " + (await st(4, "c2")).bg);
    // columns inserted from the cell menu, left and right of the clicked cell
    await L.rowMenu(page, 1, "c2", "Insert column left");
    await L.rowMenu(page, 1, "c3", "Insert column right");
    const titles = (await L.columns(page)).map((c) => c.title).filter((x) => x.trim());
    check(titles.join(",") === "V1,mpg,cyl,V2,disp,hp,drat,wt", "columns: " + titles.join(","));
    // a row is not redrawn while one of its cells is being edited (that
    // removed the editor); it is redrawn when the edit ends
    const c6 = await L.cellCentre(page, 6, "c2");
    await page.mouse.click(c6.x, c6.y, { clickCount: 2 });
    await page.waitForSelector(".tabulator-cell.tabulator-editing", { timeout: 3000 });
    await t(() => { const g = document.querySelector(".editr_grid").__editr;
      g.handle("set_formats", { formats: [{ type: "rule", ids: [6], all: true, fill: "#E5E7EB" }] }); });
    await sleep(300);
    check(!!(await page.$(".tabulator-cell.tabulator-editing")), "the editor stays open while its row's format changes");
    await page.keyboard.press("Escape");
    await sleep(400);
    check((await st(6, "c4")).bg === "rgb(229, 231, 235)", "row 6 redrawn after the edit: " + (await st(6, "c4")).bg);
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
    // #37 whole numbers: 2.5 is marked, 1.5 is refused, 4 is stored
    const marked = await t((fd) => document.querySelector(".editr_grid").__editr.table.getRow(3)
      .getCell(fd).getElement().classList.contains("editr-invalid"), f.size);
    check(marked, "2.5 marked in an integer column");
    await L.editCell(page, 1, f.size, "1.5");
    await page.waitForSelector(".editr-guard", { timeout: 3000 });
    const guard = await page.$eval(".editr-guard", (e) => e.textContent);
    check(/not a whole number/.test(guard) && /whole numbers/.test(guard), "1.5 refused: " + guard.slice(0, 160));
    await page.click('.editr-guard [data-act="cancel"]');
    await L.settle(page, 600);
    await L.editCell(page, 1, f.size, "4");
    check((await col(f.size))[0] === 4, "4 stored: " + (await col(f.size))[0]);
    // #14 "dropdown" offers the values of the column
    const p0 = await L.cellCentre(page, 4, f.grade);
    await page.mouse.click(p0.x, p0.y, { clickCount: 2 });
    await page.waitForSelector(".tabulator-edit-list-item");
    await sleep(300);
    const items = await t(() => [...document.querySelectorAll(".tabulator-edit-list-item")]
      .map((x) => x.textContent.trim()).filter((x) => x));
    check(JSON.stringify(items) === JSON.stringify(["a", "b", "c"]), "dropdown of the column's values: " + JSON.stringify(items));
    const ic = await t(() => { const e = [...document.querySelectorAll(".tabulator-edit-list-item")]
      .find((x) => x.textContent.trim() === "c"); const b = e.getBoundingClientRect();
      return { x: b.x + 10, y: b.y + b.height / 2 }; });
    await page.mouse.click(ic.x, ic.y);
    await L.settle(page, 800);
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
    // the Filters panel lists a "!ab" header filter as "does not contain ab"
    await t((fc) => document.querySelector(".editr_grid").__editr.table.setHeaderFilterValue(fc, "!ab"), f.code);
    await L.settle(page, 800);
    await openFilters(page);
    check((await cardsNow(page)).join("|") === f.code + " not_contain ab", "panel lists !ab as not contain: " + (await cardsNow(page)).join("|"));
    check(/^2 of 5 rows shown/.test(await filterCount(page)), "same rows in the panel: " + await filterCount(page));
    // dropdown names are listed and shown, and stored as values
    await addCard(page, f.state);
    await setLogic(page, "equal");
    const offered = await page.$$eval("#edit1-filters .editr-fp-open .editr-fp-val", (o) => o.map((x) => x.textContent));
    check(offered.includes("Victoria") && !offered.includes("VIC"), "dropdown names listed: " + offered.join(","));
    await fillCard(page, "equal", ["Victoria"]);
    await L.settle(page, 500);
    const stored = await editr(page, (g, fs) => g.conditions.filter((c) => c.field === fs).map((c) => c.values.join()).join(), f.state);
    check(stored === "VIC", "the value is stored: " + stored);
    check(JSON.stringify(await active()) === "[4]", "!ab and Victoria: " + JSON.stringify(await active()));
    await closeFilters(page);
    const chipsNow = await page.$eval("#edit1-chips", (e) => e.textContent);
    check(/state is Victoria/.test(chipsNow), "toolbar chip names the choice: " + chipsNow.slice(0, 120));
    // HTML is searched as the cells show it, without the tags
    await setFilters(page, [[f.photo, "contain", ["details"]]]);
    check(JSON.stringify(await active()) === "[1]", "html contains its text: " + JSON.stringify(await active()));
    await setFilters(page, [[f.photo, "contain", ["href"]]]);
    check(JSON.stringify(await active()) === "[]", "html tags are not searched: " + JSON.stringify(await active()));
    // the Code tab searches what the cells show too
    await setFilters(page, [[f.photo, "contain", ["details"]]]);
    let shownCode = await codeNow(page);
    check(shownCode.includes('grepl("details", tolower(gsub("<[^>]*>", " ", photo)), fixed = TRUE)') &&
          !/by position/.test(shownCode),
          "html searched without its tags in the code: " + JSON.stringify(shownCode.slice(-160)));
    await setFilters(page, [[f.state, "contain", ["vict"]]]);
    shownCode = await codeNow(page);
    check(shownCode.includes('filter(state %in% "VIC")'),
          "dropdown names searched in the code: " + JSON.stringify(shownCode.slice(-120)));
    await setFilters(page, []);
    check((await active()).length === 5, "panel filters cleared");
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
    // a row name edited in the modal (#24)
    await L.editCell(page, 1, "c1", "First car");
    const tab = await page.$eval("#tab", (e) => e.textContent.replace(/\s+/g, " "));
    check(/MPG/.test(tab) && /8\.00/.test(tab) && /First car/.test(tab),
          "rename, row name and edit in the modal reach R (#24): " + tab.slice(0, 120));
    // clicking a column name in the modal selects the column (#24)
    const hc = await L.headerCentre(page, "c4");
    await page.mouse.click(hc.x, hc.y);
    await sleep(400);
    const sel = await t(() => { const r = document.getElementById("edit-grid").__editr.table.getRanges();
      return r.length ? r[0].getColumns().map((c) => c.getField()) : []; });
    check(JSON.stringify(sel) === JSON.stringify(["c4"]), "header click selects the column: " + JSON.stringify(sel));
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

  async filter_dialog(page) {
    const f = {};
    for (const n of ["name", "n", "grp", "d", "ok", "pw", "when", "extra"]) f[n] = await fieldOf(page, n);
    const label = await page.evaluate(() => document.querySelector("#edit1-filter").textContent.trim());
    check(label === "Filters", "Filters button in the toolbar: " + label);
    const chips = () => page.$$eval(".editr-chips .editr-chip > span", (s) => s.map((e) => e.textContent));
    const keeps = () => cardText(page, ".editr-fp-keeps");

    // the button opens the Filters tab of the side panel, and closes it
    await openFilters(page);
    check(await page.$eval("#edit1-filter", (b) => b.getAttribute("aria-pressed")) === "true", "button is pressed");
    check(await page.$eval("#edit1-tab_filters", (b) => b.getAttribute("aria-selected")) === "true", "Filters tab selected");
    check(/No filters/.test(await page.$eval("#edit1-filters", (e) => e.textContent)), "says there are no filters");
    check(/^8 of 8 rows shown/.test(await filterCount(page)), "count: " + await filterCount(page));
    await closeFilters(page);
    check(!(await panelOpen(page)), "the button closes the panel again");
    await openFilters(page);

    // the column picker: every column, with a hint of its values (none for
    // passwords); typing narrows it and Enter adds a filter
    await page.click("#edit1-filters .editr-fp-add");
    await page.waitForSelector("#edit1-filters .editr-fp-picker");
    const listed = await page.$$eval("#edit1-filters .editr-fp-colopt", (o) => o.map((x) => x.getAttribute("data-field")));
    check(listed.length === 9 && listed.includes(f.pw), "picker lists every column and the row names: " + listed.length);
    const pwOpt = await page.$eval(`#edit1-filters .editr-fp-colopt[data-field="${f.pw}"]`, (o) => o.textContent);
    check(pwOpt === "abcpw", "nothing about the values of a password: " + pwOpt);
    check(!!(await page.$(`#edit1-filters .editr-fp-colopt[data-field="${f.n}"] .editr-fp-spark`)), "numbers have a sparkline");
    check(/3 values/.test(await page.$eval(`#edit1-filters .editr-fp-colopt[data-field="${f.grp}"]`, (o) => o.textContent)),
          "text columns say how many values");
    await page.keyboard.type("gr");
    await sleep(100);
    const narrowed = await page.$$eval("#edit1-filters .editr-fp-colopt", (o) => o.map((x) => x.getAttribute("data-field")));
    check(narrowed.join() === f.grp, "typing narrows the columns: " + narrowed.join());
    await page.keyboard.press("Enter");
    await sleep(200);
    check((await cardsNow(page)).join("|") === `${f.grp} equal`, "Enter adds a filter: " + (await cardsNow(page)).join("|"));
    check(!(await page.$("#edit1-filters .editr-fp-picker")), "picker closes");

    // values with how many rows have each; the grid follows at once
    const values = await page.$$eval("#edit1-filters .editr-fp-opt", (o) => o.map((x) =>
      x.querySelector(".editr-fp-val").textContent + "=" + x.querySelector(".editr-fp-n").textContent));
    check(values.join() === "a=3,b=2,c=2,missing=1", "values with counts: " + values.join());
    check(await keeps() === "not set yet", "not set until a value is ticked: " + await keeps());
    await fillCard(page, "equal", ["a"]);
    await L.settle(page, 400);
    check((await shownIds(page)).join() === "1,3,7", "the grid follows the panel: " + (await shownIds(page)).join());
    check(await keeps() === "keeps 3 of 8", "keeps: " + await keeps());
    check(/^3 of 8 rows shown/.test(await filterCount(page)), "count: " + await filterCount(page));
    check((await chips()).join("|") === "grp is a", "chip: " + (await chips()).join("|"));
    check(await page.$eval("#edit1-filter_count", (e) => e.textContent) === "1", "count on the button");

    // the conditions offered, in plain words, for each type of column
    const logics = () => page.$$eval("#edit1-filters .editr-fp-open .editr-fp-logic option", (o) => o.map((x) => x.textContent));
    check((await logics()).join() === "contains,does not contain,is any of,is none of,matches the pattern," +
          "does not match the pattern,is missing,is not missing", "text: " + (await logics()).join());
    await addCard(page, f.n);
    check((await logics()).join() === "is between,is not between,is at least,is at most,is above,is below," +
          "is any of,is none of,contains,does not contain,matches the pattern,does not match the pattern," +
          "is missing,is not missing", "numbers: " + (await logics()).join());
    check(await page.$$eval("#edit1-filters .editr-fp-card.editr-fp-open", (c) => c.length) === 1,
          "adding a filter folds the others");
    const folded = await page.$eval("#edit1-filters .editr-fp-card:not(.editr-fp-open) .editr-fp-sum", (h) => h.textContent);
    check(folded === "is a", "a folded filter says what it does: " + folded);
    check(!!(await page.$("#edit1-filters .editr-fp-match")), "all or any, once there are two filters");
    await addCard(page, f.ok);
    check((await logics()).join() === "is,is not,is missing,is not missing", "logical: " + (await logics()).join());
    const choices = await page.$$eval("#edit1-filters .editr-fp-open .editr-fp-choice", (b) => b.map((x) => x.textContent));
    check(choices.join("|") === "TRUE4 rows|FALSE3 rows|missing1 row", "TRUE and FALSE with counts: " + choices.join("|"));
    await addCard(page, f.d);
    check((await logics())[0] === "is between", "dates start with a range");
    await addCard(page, f.name);
    check((await cardsNow(page)).slice(-1)[0] === `${f.name} equal`, "names start with a list of them");
    check(await page.$$eval("#edit1-filters .editr-fp-open .editr-fp-opt", (o) => o.length) === 8, "every name and missing");

    // passwords are typed, never listed
    await addCard(page, f.pw);
    await setLogic(page, "equal");
    check(!(await page.$("#edit1-filters .editr-fp-open .editr-fp-opts")), "no list for a password");
    check(!/secret/.test(await page.$eval("#edit1-filters", (e) => e.textContent)), "no password in the panel");
    await page.click("#edit1-filters .editr-fp-clear");
    await sleep(200);
    check(!(await cardCount(page)) && (await shownIds(page)).length === 8, "Clear all");

    // every logic, with the rows expected by hand
    const cases = [
      ["n equal", f.n, "equal", [4], [4]],
      ["n equal (two values)", f.n, "equal", [4, 5], [4, 5]],
      ["n not equal keeps missing", f.n, "not_equal", [4], [1, 2, 3, 5, 6, 7, 8]],
      ["n greater than", f.n, "gt", [5], [6, 7, 8]],
      ["n less than", f.n, "lt", [2.5], [1]],
      ["n greater than or equal", f.n, "gte", [5], [5, 6, 7, 8]],
      ["n less than or equal", f.n, "lte", [2.5], [1, 2]],
      ["n between includes the ends", f.n, "between", [2.5, 5], [2, 4, 5]],
      ["n between in either order", f.n, "between", [5, 2.5], [2, 4, 5]],
      ["n not between in either order", f.n, "not_between", [5, 2.5], [1, 3, 6, 7, 8]],
      ["n not between keeps missing", f.n, "not_between", [2.5, 5], [1, 3, 6, 7, 8]],
      ["n contain", f.n, "contain", [".5"], [2]],
      ["n missing", f.n, "missing", [], [3]],
      ["n not missing", f.n, "present", [], [1, 2, 4, 5, 6, 7, 8]],
      ["name equal", f.name, "equal", ["alpha"], [1]],
      ["name not equal keeps missing", f.name, "not_equal", ["alpha"], [2, 3, 4, 5, 6, 7, 8]],
      ["name contain ignores case", f.name, "contain", ["ETA"], [2, 7, 8]],
      ["name contain (any of)", f.name, "contain", ["al", "ze"], [1, 7]],
      ["name not contain keeps missing", f.name, "not_contain", ["eta"], [1, 3, 4, 5, 6]],
      ["name regex", f.name, "regex", ["^[a-e]"], [1, 2, 4, 5, 8]],
      ["name not regex", f.name, "not_regex", ["^[a-e]"], [3, 6, 7]],
      ["name missing", f.name, "missing", [], [6]],
      ["d greater than", f.d, "gt", ["2024-04-01"], [5, 6, 8]],
      ["d less than or equal", f.d, "lte", ["2024-02-01"], [1, 2]],
      ["d between", f.d, "between", ["2024-02-01", "2024-04-01"], [2, 3, 4]],
      ["d not between keeps missing", f.d, "not_between", ["2024-02-01", "2024-04-01"], [1, 5, 6, 7, 8]],
      ["d equal", f.d, "equal", ["2024-03-01"], [3]],
      ["ok equal", f.ok, "equal", ["TRUE"], [1, 4, 5, 7]],
      ["ok equal TRUE or missing", f.ok, "equal", ["TRUE", "NA"], [1, 3, 4, 5, 7]],
      ["ok not equal keeps missing", f.ok, "not_equal", ["TRUE"], [2, 3, 6, 8]],
      ["ok missing", f.ok, "missing", [], [3]],
      ["grp equal NA", f.grp, "equal", ["NA"], [8]],
      ["pw equal (typed)", f.pw, "equal", ["secret3"], [3]],
      ["when greater than", f.when, "gt", ["2024-03-16 10:30"], [7, 8]],
      ["when contain keeps the text as typed", f.when, "contain", ["2024-03-15"], [5]],
    ];
    const codes = {};
    for (const [what, field, logic, values, want] of cases) {
      await setFilters(page, [[field, logic, values]]);
      const got = await shownIds(page);
      check(got.join() === want.join(), what + ": " + got.join());
      // the Code tab writes the filter as code, checked in R by running it
      const code = await codeNow(page);
      codes[what] = (code.match(/x_view <- x %>%\n {2}(filter\([^\n]*)/) || [])[1];
      check(!!codes[what] && !/by position/.test(code), what + ": code " + (codes[what] || code.slice(-200)));
    }
    const want = {
      "n between in either order": "filter(between(n, 2.5, 5))",
      "n not between keeps missing": "filter(is.na(n) | !between(n, 2.5, 5))",
      "name contain ignores case": 'filter(grepl("eta", tolower(name), fixed = TRUE))',
      "d between": 'filter(between(d, as.Date("2024-02-01"), as.Date("2024-04-01")))',
      "ok equal TRUE or missing": "filter(ok %in% c(TRUE, NA))",
      "grp equal NA": "filter(is.na(grp))",
      // a date-time column (col_options) is a date-time in the data
      "when greater than": 'filter(when > as.POSIXct("2024-03-16 10:30:00"))',
    };
    for (const [what, code] of Object.entries(want)) {
      check(codes[what] === code, what + ": " + codes[what]);
    }

    // several filters, also on one column, must all match ...
    await setFilters(page, [[f.name, "contain", ["a"]], [f.name, "not_contain", ["e"]]]);
    check((await shownIds(page)).join() === "1,3", "two filters on one column");
    await setFilters(page, [[f.n, "gte", [4]], [f.grp, "equal", ["c"]], [f.ok, "equal", ["TRUE"]]]);
    check((await shownIds(page)).join() === "5", "three columns");
    check((await chips()).join("|") === "n ≥ 4|grp is c|ok is TRUE", "chips: " + (await chips()).join("|"));
    check(await page.$eval("#edit1-filter_count", (e) => e.textContent) === "3", "count on the button");
    await page.click(".editr-chips .editr-chip:nth-child(2) button");
    await L.settle(page, 500);
    check((await shownIds(page)).join() === "4,5,7", "chip removes its filter: " + (await shownIds(page)).join());
    await openFilters(page);
    check(await cardCount(page) === 2, "the panel lists the filters that are left");

    // ... or any of them
    await setFilters(page, [[f.n, "gte", [7]], [f.grp, "equal", ["c"]]]);
    check((await shownIds(page)).join() === "", "n ≥ 7 and grp c: none");
    await openFilters(page);
    await page.click('#edit1-filters .editr-fp-seg[aria-checked="false"]');
    await L.settle(page, 400);
    check((await shownIds(page)).join() === "5,6,7,8", "n ≥ 7 or grp c: " + (await shownIds(page)).join());
    check(await page.$eval("#edit1-filters .editr-fp-join", (e) => e.textContent) === "or", "cards are joined by or");
    check(/^Any of:/.test(await page.$eval("#edit1-chips", (e) => e.textContent)), "the chips say any of");
    const anyCode = await codeNow(page);
    check(anyCode.endsWith('x_view <- x %>%\n  filter(n >= 7 | grp == "c")'), "any in the code: " + JSON.stringify(anyCode));
    await openFilters(page);
    await page.click('#edit1-filters .editr-fp-seg[aria-checked="false"]');
    await L.settle(page, 400);
    check((await shownIds(page)).join() === "", "back to all");

    // folding a filter; counts follow edits
    await setFilters(page, [[f.n, "gte", [7]]]);
    await openFilters(page);
    await page.click("#edit1-filters .editr-fp-name");
    await sleep(150);
    check(await page.$eval("#edit1-filters .editr-fp-sum", (e) => e.textContent) === "is at least 7", "folded");
    await page.click("#edit1-filters .editr-fp-name");
    await sleep(150);
    check(!!(await openCard(page)), "opened again");
    await L.editCell(page, 7, f.n, "3");
    await sleep(300);
    check(await keeps() === "keeps 1 of 8", "counts follow an edit: " + await keeps());
    await page.click("#edit1-undo");
    await L.settle(page, 600);

    // the range: a histogram to drag the ends on, or the keyboard
    await setFilters(page, []);
    await openFilters(page);
    await addCard(page, f.n);
    const handles = await page.$$("#edit1-filters .editr-fp-handle");
    const track = await (await page.$("#edit1-filters .editr-fp-track")).boundingBox();
    const h0 = await handles[0].boundingBox();
    await page.mouse.move(h0.x + h0.width / 2, h0.y + h0.height / 2);
    await page.mouse.down();
    for (let i = 1; i <= 5; i++) {
      await page.mouse.move(h0.x + h0.width / 2 + (track.x + track.width * 0.5 - h0.x - h0.width / 2) * i / 5,
                            h0.y + h0.height / 2);
      await sleep(30);
    }
    await page.mouse.up();
    await L.settle(page, 500);
    check((await cardsNow(page)).join() === `${f.n} between 4.5 8`, "dragged: " + (await cardsNow(page)).join());
    check((await shownIds(page)).join() === "5,6,7,8", "rows of the dragged range: " + (await shownIds(page)).join());
    const ends = await page.$$eval("#edit1-filters .editr-fp-ends input", (i) => i.map((x) => x.value));
    check(ends.join() === "4.5,8", "the boxes show the ends: " + ends.join());
    const bins = await page.$$eval("#edit1-filters .editr-fp-bin", (b) => b.map((x) =>
      x.classList.contains("editr-fp-on") ? 1 : x.classList.contains("editr-fp-off") ? 0 : "?"));
    // bars of width 1 from 1 to 8: those reaching 4.5 or more are lit
    check(bins.join("") === "0001111", "histogram shows the range: " + bins.join(""));
    // the ends do not pass each other
    await handles[1].focus();
    await page.keyboard.press("Home");
    await L.settle(page, 400);
    check((await cardsNow(page)).join() === `${f.n} between 4.5 4.5`, "To stops at From: " + (await cardsNow(page)).join());
    await page.keyboard.press("End");
    await L.settle(page, 400);
    await handles[0].focus();
    await page.keyboard.press("Home");
    await L.settle(page, 500);
    check((await cardsNow(page)).join() === `${f.n} between 1 8`, "Home: " + (await cardsNow(page)).join());
    check(await handles[0].evaluate((h) => h.getAttribute("aria-valuenow")) === "1", "slider value for screen readers");

    // unfinished or wrong filters are left out, and say why
    await setFilters(page, [[f.n, "gt", []]]);
    await openFilters(page);
    check(await keeps() === "not set yet" && (await shownIds(page)).length === 8, "no value yet");
    await fillCard(page, "gt", ["abc"]);
    await sleep(350);
    check(/is not a number/.test(await cardText(page, ".editr-fp-msg")), "not a number: " + await cardText(page, ".editr-fp-msg"));
    check((await shownIds(page)).length === 8, "a bad number filters nothing");
    await setFilters(page, [[f.name, "regex", ["("]]]);
    await openFilters(page);
    check(/regular expression/.test(await cardText(page, ".editr-fp-msg")), "bad regex: " + await cardText(page, ".editr-fp-msg"));
    await setFilters(page, [[f.n, "between", [4]]]);
    await openFilters(page);
    check(await keeps() === "not set yet" && (await shownIds(page)).length === 8, "between needs both ends");

    // header filters and summary picks are listed; unchanged ones stay put
    await setFilters(page, []);
    await page.evaluate((a, b) => {
      const g = document.querySelector(".editr_grid").__editr;
      g.table.setHeaderFilterValue(a, { min: "2.5", max: "" });
      g.table.setHeaderFilterValue(b, ["a", "b"]);
    }, f.n, f.grp);
    await L.settle(page, 800);
    check((await shownIds(page)).join() === "2,4,7", "header filters: " + (await shownIds(page)).join());
    await openFilters(page);
    check((await cardsNow(page)).join("|") === `${f.n} gte 2.5|${f.grp} equal a b`, "listed: " + (await cardsNow(page)).join("|"));
    check(/From the box under the column name/.test(await page.$eval("#edit1-filters", (e) => e.textContent)), "says where they come from");
    check(/^3 of 8 rows shown/.test(await filterCount(page)), "same count: " + await filterCount(page));
    await addFilter(page, f.name, "not_contain", ["zeta"]);
    await L.settle(page, 500);
    check((await shownIds(page)).join() === "2,4", "added to them: " + (await shownIds(page)).join());
    const state = () => editr(page, (g) => ({
      header: g.table.getHeaderFilters().length, conditions: g.conditions.length }));
    let st = await state();
    check(st.header === 2 && st.conditions === 1, "unchanged header filters stay: " + JSON.stringify(st));
    const minBox = () => page.evaluate((a) => document.querySelector(".editr_grid").__editr.table
      .getColumn(a).getElement().querySelector(".editr-range input").value, f.n);
    check(await minBox() === "2.5", "box still shows it");

    // changing a listed filter moves it to the panel; removing one clears it
    await page.click(`#edit1-filters .editr-fp-card[data-field="${f.n}"] .editr-fp-name`);
    await sleep(150);
    await setLogic(page, "gt");
    await page.click(`#edit1-filters .editr-fp-card[data-field="${f.grp}"] .editr-fp-remove`);
    await L.settle(page, 500);
    st = await state();
    check(st.header === 0 && st.conditions === 2, "changed filters move to the panel: " + JSON.stringify(st));
    check(await minBox() === "", "box is cleared");
    check((await shownIds(page)).join() === "4,5,6,8", "n > 2.5 and not zeta: " + (await shownIds(page)).join());
    // with any, the header boxes join the "or"
    await setFilters(page, [[f.n, "gte", [7]]]);
    await page.evaluate((b) => document.querySelector(".editr_grid").__editr.table.setHeaderFilterValue(b, ["c"]), f.grp);
    await L.settle(page, 600);
    await openFilters(page);
    await page.click('#edit1-filters .editr-fp-seg[aria-checked="false"]');
    await L.settle(page, 500);
    st = await state();
    check(st.header === 0 && st.conditions === 2, "any takes in the header box: " + JSON.stringify(st));
    check((await shownIds(page)).join() === "5,6,7,8", "n ≥ 7 or grp c: " + (await shownIds(page)).join());

    // a summary pick
    await setFilters(page, []);
    await editr(page, (g, field) => {
      g.picks[field] = { kind: "missing" };
      g.table.refreshFilter();
    }, f.n);
    await L.settle(page, 500);
    await openFilters(page);
    check((await cardsNow(page)).join() === `${f.n} missing`, "summary pick listed: " + (await cardsNow(page)).join());
    check(/From the column summary/.test(await page.$eval("#edit1-filters", (e) => e.textContent)), "says it is a pick");
    st = await editr(page, (g, field) => ({ pick: !!g.picks[field], conditions: g.conditions.length }), f.n);
    check(st.pick && st.conditions === 0, "summary pick stays: " + JSON.stringify(st));
    await setFilters(page, []);
    check((await shownIds(page)).length === 8, "cleared through the panel");

    // the row names column has a name; chips stop at a few
    const names = await editr(page, (g) => Object.keys(g.colMap).find((k) => g.colMap[k].frozen));
    await setFilters(page, [[names, "contain", ["r1"]]]);
    check((await shownIds(page)).join() === "1", "row names can be filtered");
    check(/^row names contains/.test((await chips()).join()), "row names in the chip: " + (await chips()).join());
    await setFilters(page, [[f.n, "gte", [0]], [f.ok, "present"], [f.grp, "present"], [f.d, "present"], [f.name, "present"]]);
    check((await chips()).length === 3, "three chips, then the rest: " + (await chips()).join("|"));
    check(await page.$eval("#edit1-filter_count", (e) => e.textContent) === "5", "the button counts them all");
    await page.click(".editr-chip-more");
    await page.waitForSelector("#edit1-filters .editr-fp-add", { visible: true });
    await sleep(200);
    check(await cardCount(page) === 5, "more opens the panel with all of them");
    await closeFilters(page);

    // chips follow renames; a deleted column takes its filter, undo restores it
    await setFilters(page, [[f.extra, "gt", [5]]]);
    check((await shownIds(page)).join() === "5,6,7,8", "filter on a column to change");
    await L.renameColumn(page, f.extra, "more");
    check((await chips()).join() === "more > 5", "chip follows a rename: " + (await chips()).join());
    await openFilters(page);
    check(/more/.test(await page.$eval("#edit1-filters .editr-fp-col", (e) => e.textContent)), "and so does the panel");
    await closeFilters(page);
    await L.headerMenu(page, f.extra, "Delete column");
    check((await shownIds(page)).length === 8 && !(await chips()).length, "deleted column takes its filter");
    await page.click("#edit1-undo");
    await L.settle(page, 1000);
    check((await shownIds(page)).join() === "5,6,7,8", "undo restores the column and its filter");
    await page.click("#edit1-undo");
    await L.settle(page, 1000);
    await L.headerMenu(page, f.extra, "Delete column");
    await setFilters(page, []);

    // rows added while filtered stay until the filters change
    await setFilters(page, [[f.n, "gte", [4]], [f.name, "not_contain", ["epsilon"]]]);
    check((await shownIds(page)).join() === "4,6,7,8", "filtered for edits");
    const added = await addRow(page);
    check((await shownIds(page)).includes(added), "added row stays visible");
    const pinned = await codeNow(page);
    check(/picked by position/.test(pinned) && /slice\(/.test(pinned), "rows the filters do not explain are picked by position");
    await L.rowMenu(page, added, f.n, "Delete row");
    check(!(await shownIds(page)).includes(added), "added row removed again");

    // edits reach the data through the filter, and Done returns the view
    await setFilters(page, [[f.n, "gte", [4]], [f.grp, "not_equal", ["a", "b"]],
                            [f.name, "not_contain", ["epsilon"]]]);
    check((await shownIds(page)).join() === "6,8", "rows for Done: " + (await shownIds(page)).join());
    await L.editCell(page, 6, f.n, "66");
    await done(page, "view");
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
    // browsers that make a size container a stacking context put the grid over
    // the toolbar's menus; current Chrome does not, so ask for it here
    await page.addStyleTag({ content: ".editr-toolbar { contain: layout; }" });
    const exportBtn = await page.evaluateHandle(() => [...document.querySelectorAll("button.editr-tool")]
      .find((b) => b.textContent.includes("Export")));
    await exportBtn.click();
    await sleep(500);
    check(await onTop(page, ".editr-toolbar .dropdown-menu.show"), "Export menu over the grid");
    await page.keyboard.press("Escape");
    await sleep(300);
    // filters belong to the data they were set on
    const mpg = await fieldOf(page, "mpg");
    await setFilters(page, [[mpg, "gt", [20]]]);
    await page.click("#open_toggle");
    await sleep(500);
    check(await onTop(page, ".editr-open-menu.show"), "Open menu over the grid");
    await page.click("#open_object", { clickCount: 3 });
    await page.keyboard.type("iris");
    await page.click("#open_load");
    await L.settle(page, 1500);
    check((await L.gridData(page)).length === 150, "iris loaded");
    check((await shownIds(page)).length === 150, "filters do not follow to new data");
    check(await page.$eval("#edit1-filter_count", (e) => e.textContent) === "", "and the button is reset");
    const sl = await fieldOf(page, "Sepal.Length");
    await L.editCell(page, 1, sl, "9.9");
    await done(page);
  },

  // none of these may stop the editor, whatever the settings of the session
  async open_settings(page) {
    const alive = () => page.evaluate(() => Shiny.shinyapp.isConnected());
    const load = async (text) => {
      if (!(await page.$(".editr-open-menu.show"))) {
        await page.click("#open_toggle");
        await sleep(400);
      }
      await page.click("#open_object", { clickCount: 3 });
      await page.keyboard.type(text);
      await page.click("#open_load");
      await L.settle(page, 1200);
    };
    const title = () => page.$eval("#dataset_name", (e) => e.textContent);
    const notes = () => page.$$eval(".shiny-notification-content-text", (n) => n.map((e) => e.textContent).join(" "));
    const started = await title();
    for (const [bad, why] of [["setNames(data.frame(1, 2), c('a', NA))", "without a name"],
                              ["local({d <- data.frame(a = 1:2); d$m <- data.frame(z = 1:2); d})", "hold tables"],
                              ["letters", "not a data frame"]]) {
      await load(bad);
      check(await alive(), "still running after " + bad);
      check((await notes()).includes(why), "says why: " + (await notes()).slice(-70));
      check(await title() === started, "keeps the current data");
    }
    await page.evaluate(() => document.querySelectorAll(".shiny-notification-close").forEach((b) => b.click()));
    await page.click("#open_rows", { clickCount: 3 });
    await page.keyboard.type("2");
    await page.click("#open_new");
    await L.settle(page, 1500);
    check(await alive() && (await L.gridData(page)).length === 2, "a new table opens");
    await load("iris");
    check(await alive() && (await L.gridData(page)).length === 150, "other data opens");
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
    // #35 a paste bigger than the table is trimmed when rows and columns are fixed
    await L.selectCell(page, 2, "c1");
    await L.paste(page, "z\tFALSE\textra\nw\tTRUE\tmore\n");
    await L.settle(page, 800);
    const d = await L.gridData(page);
    check(d.length === 2 && Object.keys(d[0]).filter((k) => /^c\d+$/.test(k)).length === 2,
          "paste trimmed to the table: " + JSON.stringify(d));
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

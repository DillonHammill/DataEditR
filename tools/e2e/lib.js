// Shared helpers for driving the DataEditR grid in headless Chrome.
const puppeteer = require("puppeteer-core");

const GRID = "edit1-grid";
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

let failures = 0;
function check(cond, msg) {
  if (cond) {
    console.log("  ok   " + msg);
  } else {
    failures += 1;
    console.log("  FAIL " + msg);
  }
}

async function launch(port, readyTimeout = 30000) {
  const browser = await puppeteer.launch({
    executablePath: process.env.CHROME_PATH || "/usr/bin/google-chrome",
    headless: true,
    args: ["--no-sandbox", "--disable-gpu", "--window-size=1400,900"],
    defaultViewport: { width: 1400, height: 900 },
  });
  const page = await browser.newPage();
  page.on("pageerror", (e) => { failures += 1; console.log("  PAGE ERROR " + e.message); });
  page.on("console", (m) => {
    if (m.type() === "error" && !/favicon|logo|ERR_|Failed to load resource/.test(m.text())) {
      console.log("  console error: " + m.text());
    }
  });
  for (let i = 0; i < 60; i++) {
    try {
      await page.goto(`http://127.0.0.1:${port}`, { waitUntil: "networkidle2", timeout: 5000 });
      break;
    } catch (e) {
      await sleep(1000);
    }
  }
  await page.waitForFunction(
    (g) => document.getElementById(g) && document.getElementById(g).__editr &&
      document.getElementById(g).__editr.ready, { timeout: readyTimeout }, GRID);
  await sleep(500);
  return { browser, page };
}

// let Shiny round-trip
async function settle(page, ms = 600) {
  await sleep(ms);
  await page.waitForFunction(() => !document.documentElement.classList.contains("shiny-busy"),
                             { timeout: 10000 });
  await sleep(150);
}

async function gridData(page) {
  return page.evaluate((g) => document.getElementById(g).__editr.table.getData(), GRID);
}

async function columns(page) {
  return page.evaluate((g) => document.getElementById(g).__editr.table.getColumns()
    .filter((c) => c.getField() !== undefined && c.getField() !== "__editr_add__")
    .map((c) => ({ field: c.getField(), title: c.getDefinition().title })), GRID);
}

async function cellCentre(page, id, field) {
  return page.evaluate(async (g, id, field) => {
    const t = document.getElementById(g).__editr.table;
    const row = t.getRow(id);
    await t.scrollToRow(id, "center", false); // virtual DOM: render the row first
    const el = row.getCell(field).getElement();
    el.scrollIntoView({ block: "nearest", inline: "center" });
    const b = el.getBoundingClientRect();
    return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
  }, GRID, id, field);
}

async function headerCentre(page, field) {
  return page.evaluate((g, field) => {
    const t = document.getElementById(g).__editr.table;
    const el = t.getColumn(field).getElement().querySelector(".tabulator-col-title");
    const b = el.getBoundingClientRect();
    return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
  }, GRID, field);
}

async function editCell(page, id, field, text) {
  const p = await cellCentre(page, id, field);
  await page.mouse.click(p.x, p.y, { clickCount: 2 });
  await page.waitForSelector(".tabulator-cell.tabulator-editing input", { timeout: 3000 });
  await page.keyboard.down("Control");
  await page.keyboard.press("KeyA");
  await page.keyboard.up("Control");
  await page.keyboard.type(text);
  await page.keyboard.press("Enter");
  await settle(page);
}

async function menuClick(page, label) {
  await page.waitForSelector(".tabulator-menu", { timeout: 3000 });
  const items = await page.$$(".tabulator-menu .tabulator-menu-item");
  for (const it of items) {
    const txt = await page.evaluate((e) => e.textContent.trim(), it);
    if (txt.startsWith(label)) {
      await it.click();
      await settle(page);
      return true;
    }
  }
  throw new Error("menu item not found: " + label);
}

async function rowMenu(page, id, field, label) {
  const p = await cellCentre(page, id, field);
  await page.mouse.click(p.x, p.y, { button: "right" });
  return menuClick(page, label);
}

async function headerMenu(page, field, label) {
  const p = await headerCentre(page, field);
  await page.mouse.click(p.x, p.y, { button: "right" });
  return menuClick(page, label);
}

async function renameColumn(page, field, name) {
  const p = await headerCentre(page, field);
  await page.mouse.click(p.x, p.y, { clickCount: 2 });
  await page.waitForSelector("input.editr-rename", { timeout: 3000 });
  await page.keyboard.down("Control");
  await page.keyboard.press("KeyA");
  await page.keyboard.up("Control");
  await page.keyboard.type(name);
  await page.keyboard.press("Enter");
  await settle(page);
}

async function selectCell(page, id, field, shift = false) {
  const p = await cellCentre(page, id, field);
  if (shift) await page.keyboard.down("Shift");
  await page.mouse.click(p.x, p.y);
  if (shift) await page.keyboard.up("Shift");
  await sleep(100);
}

async function paste(page, text) {
  await page.evaluate((g, text) => {
    const t = document.getElementById(g).__editr.table;
    const dt = new DataTransfer();
    dt.setData("text/plain", text);
    t.element.dispatchEvent(new ClipboardEvent("paste", { clipboardData: dt, bubbles: true, cancelable: true }));
  }, GRID, text);
  await settle(page);
}

async function key(page, k, ctrl = false, shift = false) {
  if (ctrl) await page.keyboard.down("Control");
  if (shift) await page.keyboard.down("Shift");
  await page.keyboard.press(k);
  if (shift) await page.keyboard.up("Shift");
  if (ctrl) await page.keyboard.up("Control");
  await settle(page);
}

async function changedKeys(page) {
  return page.evaluate((g) => Object.keys(document.getElementById(g).__editr.changed).sort(), GRID);
}

module.exports = {
  launch, settle, gridData, columns, editCell, rowMenu, headerMenu, renameColumn,
  selectCell, paste, key, changedKeys, check, sleep, cellCentre,
  failures: () => failures,
};

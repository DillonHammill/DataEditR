const puppeteer = require("puppeteer-core");
const [port, out] = process.argv.slice(2);
const G = "cars-grid";
const W = 1200, H = 640;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
async function settle(page, ms = 700) {
  await sleep(ms);
  await page.waitForFunction(() => !document.documentElement.classList.contains("shiny-busy"), { timeout: 10000 });
  await sleep(200);
}
async function cell(page, id, field) {
  return page.evaluate(async (g, id, field) => {
    const t = document.getElementById(g).__editr.table;
    await t.scrollToRow(id, "nearest", false);
    const b = t.getRow(id).getCell(field).getElement().getBoundingClientRect();
    return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
  }, G, id, field);
}
async function edit(page, id, field, text) {
  const p = await cell(page, id, field);
  await page.mouse.click(p.x, p.y, { clickCount: 2 });
  await page.waitForSelector(".tabulator-cell.tabulator-editing input");
  await page.keyboard.down("Control"); await page.keyboard.press("KeyA"); await page.keyboard.up("Control");
  await page.keyboard.type(text);
  await page.keyboard.press("Enter");
  await settle(page);
}
(async () => {
  const browser = await puppeteer.launch({ executablePath: process.env.CHROME_PATH || "/usr/bin/google-chrome", headless: true,
    args: ["--no-sandbox", "--font-render-hinting=none"], defaultViewport: { width: W, height: H, deviceScaleFactor: 1.25 } });
  const page = await browser.newPage();
  page.on("pageerror", (e) => console.log("PAGE ERROR " + e.message));
  for (let i = 0; i < 30; i++) { try { await page.goto(`http://127.0.0.1:${port}`, { waitUntil: "networkidle2", timeout: 5000 }); break; } catch (e) { await sleep(1000); } }
  await page.waitForFunction((g) => document.getElementById(g) && document.getElementById(g).__editr && document.getElementById(g).__editr.ready, { timeout: 30000 }, G);
  await settle(page, 1000);
  await page.screenshot({ path: `${out}/shiny-save-disabled.png` });
  await edit(page, 1, "c3", "21.5");
  await edit(page, 3, "c5", "95");
  const p = await cell(page, 5, "c2");
  await page.mouse.click(p.x, p.y, { button: "right" });
  await page.waitForSelector(".tabulator-menu");
  const items = await page.$$(".tabulator-menu .tabulator-menu-item");
  for (const it of items) { if ((await page.evaluate((e) => e.textContent.trim(), it)).startsWith("Delete row")) { await it.click(); break; } }
  await settle(page);
  await page.click(".editr-add-btn");
  if (await page.waitForSelector('.editr-add-menu [data-add="row"]', { timeout: 2000 }).catch(() => null)) {
    await page.click('.editr-add-menu [data-add="row"]');
  }
  await settle(page);
  const id = await page.evaluate((g) => document.getElementById(g).__editr.nextId - 1, G);
  await edit(page, id, "c2", "Mini Cooper");
  await edit(page, id, "c3", "34");
  await page.click("#save");
  await page.waitForSelector("#shiny-modal .modal-dialog");
  await sleep(800);
  await page.screenshot({ path: `${out}/shiny-save-confirm.png` });
  await page.click("#confirm");
  await settle(page, 1500);
  await page.waitForSelector(".shiny-notification");
  await sleep(400);
  await page.screenshot({ path: `${out}/shiny-save-done.png` });
  const ids = await page.evaluate((g) => document.getElementById(g).__editr.table.getData().slice(-1)[0].c1, G);
  console.log("new row id after save:", ids);
  await browser.close();
})();

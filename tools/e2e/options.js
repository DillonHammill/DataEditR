const L = require("./lib");
const { check } = L;
(async () => {
  const port = Number(process.argv[2]);
  const { browser, page } = await L.launch(port);
  try {
    const cols = await L.columns(page);
    check(cols.map((c) => c.title).join(",") === "name,ok,grp,pw,locked", "hidden column not shown: " + cols.map((c) => c.title).join(","));
    const fieldOf = (t) => cols.find((c) => c.title === t).field;

    // password cells are masked
    const pwText = await page.evaluate((f) => document.getElementById("edit1-grid").__editr.table
      .getRow(1).getCell(f).getElement().textContent, fieldOf("pw"));
    check(pwText === "••", "password masked: '" + pwText + "'");

    // read-only column cannot be edited
    const p = await L.cellCentre(page, 1, fieldOf("locked"));
    await page.mouse.click(p.x, p.y, { clickCount: 2 });
    await L.sleep(400);
    check(!(await page.$(".tabulator-cell.tabulator-editing")), "no editor on read-only cell");
    await L.key(page, "Delete");
    let data = await L.gridData(page);
    check(data[0][fieldOf("locked")] === 1, "Delete on read-only cell is reverted");

    // checkbox
    const c = await L.cellCentre(page, 3, fieldOf("ok"));
    await page.mouse.click(c.x, c.y, { clickCount: 2 });
    await page.waitForSelector(".tabulator-cell.tabulator-editing input[type=checkbox]", { timeout: 3000 });
    await page.click(".tabulator-cell.tabulator-editing input[type=checkbox]");
    await page.keyboard.press("Enter");
    await L.settle(page);
    data = await L.gridData(page);
    check(data[2][fieldOf("ok")] === true, "checkbox ticked (" + JSON.stringify(data[2][fieldOf("ok")]) + ")");

    // dropdown
    const d = await L.cellCentre(page, 1, fieldOf("grp"));
    await page.mouse.click(d.x, d.y, { clickCount: 2 });
    await page.waitForSelector(".tabulator-edit-list-item", { timeout: 3000 });
    const opts = await page.$$eval(".tabulator-edit-list-item", (e) => e.map((x) => x.textContent.trim()));
    check(opts.join(",") === "x,y,z", "dropdown options x,y,z");
    const items = await page.$$(".tabulator-edit-list-item");
    for (const it of items) {
      if ((await page.evaluate((e) => e.textContent.trim(), it)) === "z") { await it.click(); break; }
    }
    await L.settle(page);
    data = await L.gridData(page);
    check(data[0][fieldOf("grp")] === "z", "dropdown value z selected");

    // password editor uses a password input
    const w = await L.cellCentre(page, 2, fieldOf("pw"));
    await page.mouse.click(w.x, w.y, { clickCount: 2 });
    await page.waitForSelector(".tabulator-cell.tabulator-editing input", { timeout: 3000 });
    const type = await page.$eval(".tabulator-cell.tabulator-editing input", (e) => e.type);
    check(type === "password", "password editor input type=" + type);
    await page.keyboard.down("Control"); await page.keyboard.press("KeyA"); await page.keyboard.up("Control");
    await page.keyboard.type("newpass");
    await page.keyboard.press("Enter");
    await L.settle(page);

    await page.screenshot({ path: process.argv[3] + "/options.png" });
    await page.click("#done");
    await L.sleep(1500);
  } catch (e) {
    console.log("  FAIL exception: " + e.stack);
    process.exitCode = 1;
  } finally {
    await browser.close();
    console.log(L.failures() === 0 ? "BROWSER CHECKS PASSED" : L.failures() + " BROWSER CHECK(S) FAILED");
    if (L.failures()) process.exitCode = 1;
  }
})();

const L = require("./lib");
const { check } = L;
(async () => {
  const port = Number(process.argv[2]);
  const { browser, page } = await L.launch(port);
  try {
    let data = await L.gridData(page);
    let cols = await L.columns(page);
    check(data.length === 5 && cols.map((c) => c.title).join(",") === "id,model,mpg", "database rows loaded");
    // schema edits are not offered
    const p = await page.evaluate(() => {
      const t = document.getElementById("edit1-grid").__editr.table;
      const b = t.getColumn("c2").getElement().getBoundingClientRect();
      return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
    });
    await page.mouse.click(p.x, p.y, { button: "right" });
    await page.waitForSelector(".tabulator-menu");
    const items = await page.$$eval(".tabulator-menu .tabulator-menu-item", (e) => e.map((x) => x.textContent.trim()));
    check(!items.some((t) => /insert column|delete column|rename/i.test(t)), "no column add/delete/rename in header menu: " + items.join(" | "));
    await L.sleep(300); // Tabulator binds its outside-click handler after 100ms
    // close the menu by clicking a neutral spot (the toolbar status text)
    const sb = await page.$eval("#edit1-status", (e) => { const b = e.getBoundingClientRect(); return { x: b.x + 40, y: b.y + b.height / 2 }; });
    await page.mouse.click(sb.x, sb.y);
    await L.sleep(300);
    check(!(await page.$(".tabulator-menu")), "menu closed");

    await L.editCell(page, 2, "c2", "Edited");
    await L.editCell(page, 1, "c3", "50.5");
    await L.rowMenu(page, 3, "c2", "Delete row(s)");
    await L.rowMenu(page, 5, "c2", "Insert row below");
    await L.editCell(page, 6, "c2", "New car");
    await L.editCell(page, 6, "c3", "30");
    data = await L.gridData(page);
    check(data.length === 5, "4 existing + 1 new row in grid");

    await page.click("#done");
    await page.waitForSelector("#shiny-modal .modal-dialog", { visible: true, timeout: 5000 });
    await L.sleep(600);
    const modal = await page.$eval("#shiny-modal .modal-body", (e) => e.textContent.replace(/\s+/g, " ").trim());
    check(/2 row\(s\) updated, 1 inserted and 1 deleted/.test(modal), "commit summary: " + modal);
    await page.screenshot({ path: process.argv[3] + "/db-confirm.png" });
    await page.click("#db_commit");
    await L.sleep(2000);
  } catch (e) {
    console.log("  FAIL exception: " + e.stack);
    process.exitCode = 1;
  } finally {
    await browser.close();
    console.log(L.failures() === 0 ? "BROWSER CHECKS PASSED" : L.failures() + " BROWSER CHECK(S) FAILED");
    if (L.failures()) process.exitCode = 1;
  }
})();

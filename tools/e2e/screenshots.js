const L = require("./lib");
const G = "edit1-grid";
(async () => {
  const port = Number(process.argv[2]);
  const out = process.argv[3];
  const { browser, page } = await L.launch(port);
  const T = (fn, ...a) => page.evaluate(fn, G, ...a);
  try {
    // edits
    await L.editCell(page, 2, "c2", "21.5");
    await L.editCell(page, 3, "c5", "95");
    await L.rowMenu(page, 7, "c2", "Delete row(s)");
    // hide columns through the menu
    await page.click("#edit1-columns");
    await page.waitForSelector(".editr-columns-menu");
    for (const name of ["drat", "qsec", "vs", "am", "gear", "carb"]) {
      await page.evaluate((n) => {
        const l = [...document.querySelectorAll(".editr-columns-list label")].find((x) => x.textContent.trim() === n);
        l.querySelector("input").click();
      }, name);
    }
    await page.screenshot({ path: out + "/02-columns-menu.png" });
    await page.mouse.click(700, 600);
    await L.settle(page);
    // filter cyl to 4 or 6
    await T((g) => document.getElementById(g).__editr.table.setHeaderFilterValue("c3", [4, 6]));
    await L.settle(page, 800);
    // new row
    await page.click(".editr-add-btn");
    if (await page.waitForSelector('.editr-add-menu [data-add="row"]', { timeout: 2000 }).catch(() => null)) {
      await page.click('.editr-add-menu [data-add="row"]');
    }
    await L.settle(page);
    const newId = await T((g) => document.getElementById(g).__editr.nextId - 1);
    await L.editCell(page, newId, "c1", "My car");
    await L.editCell(page, newId, "c2", "26");
    await L.editCell(page, newId, "c3", "4");
    // range selection on wt
    await L.selectCell(page, 8, "c7");
    await L.selectCell(page, 10, "c7", true);
    await L.settle(page);
    // type guard on Valiant mpg
    const p = await L.cellCentre(page, 6, "c2");
    await page.mouse.click(p.x, p.y, { clickCount: 2 });
    await page.waitForSelector(".tabulator-cell.tabulator-editing input");
    await page.keyboard.down("Control"); await page.keyboard.press("KeyA"); await page.keyboard.up("Control");
    await page.keyboard.type("n/a");
    await page.keyboard.press("Enter");
    await page.waitForSelector(".editr-guard");
    await L.sleep(300);
    await page.screenshot({ path: out + "/03-editing.png" });
    await page.click('.editr-guard [data-act="cancel"]');
    await L.settle(page);
    // review panel
    await page.click("#edit1-open_changes");
    await L.settle(page, 1000);
    await page.screenshot({ path: out + "/04-review.png" });
    await page.click("#edit1-tab_code");
    await L.settle(page, 1000);
    await page.screenshot({ path: out + "/05-code.png" });
    await page.click("#edit1-open_code");
    await L.settle(page);
    // done menu
    await page.click("#done_toggle");
    await L.sleep(400);
    await page.screenshot({ path: out + "/06-done-menu.png", clip: { x: 900, y: 0, width: 500, height: 330 } });
    await page.click("#done_toggle");
    await L.sleep(300);
    await page.click("#open_toggle");
    await L.sleep(400);
    await page.screenshot({ path: out + "/07-open-menu.png", clip: { x: 0, y: 40, width: 520, height: 420 } });
    await page.click("#open_toggle");
    await L.sleep(300);
    await page.keyboard.press("?");
    await L.sleep(600);
    await page.screenshot({ path: out + "/08-shortcuts.png" });
    const status = await page.evaluate(() => ({
      rows: document.getElementById("edit1-rows").textContent,
      sel: document.getElementById("edit1-selection").textContent,
      st: document.getElementById("edit1-status").textContent,
      chips: document.getElementById("edit1-chips").textContent,
      badge: document.getElementById("edit1-review_count").textContent,
      cols: document.getElementById("edit1-columns_count").textContent
    }));
    console.log(JSON.stringify(status, null, 1));
    await page.keyboard.press("Escape");
    await L.sleep(500);
    await page.click("#done_toggle");
    await L.sleep(400);
    await page.click('input[name="return_what"][value="view"]');
    await L.sleep(300);
    await page.click("#done");
    await L.sleep(1500);
  } catch (e) {
    console.log("FAIL " + e.stack);
    await page.screenshot({ path: out + "/error.png" });
  } finally {
    await browser.close();
  }
})();

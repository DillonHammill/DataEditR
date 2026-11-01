const L = require("./lib");
const { check } = L;
(async () => {
  const port = Number(process.argv[2]);
  const t0 = Date.now();
  const { browser, page } = await L.launch(port, 180000);
  console.log("  100k rows x 8 cols ready in " + ((Date.now() - t0) / 1000).toFixed(1) + " s (includes R startup)");
  const times = [];
  for (let k = 0; k < 5; k++) {
    const id = 50000 + k;
    const p = await L.cellCentre(page, id, "c4");
    await page.mouse.click(p.x, p.y, { clickCount: 2 });
    await page.waitForSelector(".tabulator-cell.tabulator-editing input");
    await page.keyboard.down("Control"); await page.keyboard.press("KeyA"); await page.keyboard.up("Control");
    await page.keyboard.type("1.50");
    const s = Date.now();
    await page.keyboard.press("Enter");
    // R echoes 1.5 back once the edit has been applied
    await page.waitForFunction((id) => document.getElementById("edit1-grid").__editr.table.getRow(id).getData().c4 === 1.5,
                               { timeout: 10000, polling: 5 }, id);
    times.push(Date.now() - s);
  }
  console.log("  edit -> R -> grid round trip (ms): " + times.join(", "));
  check(Math.max(...times) < 1000, "edits stay interactive on 100k rows");
  await page.click("#done");
  await L.sleep(3000);
  await browser.close();
  console.log(L.failures() === 0 ? "BROWSER CHECKS PASSED" : "FAILED");
})();

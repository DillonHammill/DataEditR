const L = require("./lib");
const { check } = L;

(async () => {
  const port = Number(process.argv[2]);
  const { browser, page } = await L.launch(port);
  try {
    let data = await L.gridData(page);
    let cols = await L.columns(page);
    console.log("initial render");
    check(data.length === 8, "8 rows rendered");
    check(JSON.stringify(cols.map((c) => c.title)) === JSON.stringify([" ", "mpg", "cyl", "disp", "hp"]),
          "row names column + 4 columns: " + cols.map((c) => c.title).join(","));

    console.log("cell editing");
    await L.editCell(page, 1, "c2", "30.5");
    await L.editCell(page, 2, "c2", "21.50");
    data = await L.gridData(page);
    check(data[0].c2 === "30.5" || data[0].c2 === 30.5, "typed value kept");
    check(data[1].c2 === 21.5, "R echoed normalised number 21.50 -> 21.5 (got " + JSON.stringify(data[1].c2) + ")");
    let keys = await L.changedKeys(page);
    check(keys.includes("1|c2") && keys.includes("2|c2"), "tracked cells highlighted: " + keys.join(","));
    const highlighted = await page.$$eval(".tabulator-cell.editr-changed", (e) => e.length);
    check(highlighted >= 2, "highlight class applied in DOM (" + highlighted + ")");

    console.log("rename column");
    await L.renameColumn(page, "c3", "cylinders");
    cols = await L.columns(page);
    check(cols[2].title === "cylinders", "header renamed");

    console.log("insert / delete rows");
    await L.rowMenu(page, 3, "c2", "Insert row below");
    data = await L.gridData(page);
    check(data.length === 9 && data[3].__editr_id__ === 9, "row id 9 inserted below row 3");
    await L.editCell(page, 9, "c1", "My car");
    await L.editCell(page, 9, "c2", "40");
    await L.rowMenu(page, 8, "c2", "Delete row(s)");
    data = await L.gridData(page);
    check(data.length === 8 && !data.some((r) => r.__editr_id__ === 8), "row 8 deleted");

    console.log("insert / delete columns");
    await L.headerMenu(page, "c5", "Insert column right");
    cols = await L.columns(page);
    check(cols.length === 6 && cols[5].title === "V1", "column V1 added at end");
    await L.editCell(page, 1, cols[5].field, "hello");
    await L.headerMenu(page, "c4", "Delete column");
    cols = await L.columns(page);
    check(!cols.some((c) => c.title === "disp"), "disp deleted");

    console.log("range paste");
    await L.selectCell(page, 4, "c2");
    await L.paste(page, "11\t5\n12\t6");
    data = await L.gridData(page);
    const r4 = data.find((r) => r.__editr_id__ === 4), r5 = data.find((r) => r.__editr_id__ === 5);
    check(Number(r4.c2) === 11 && Number(r4.c3) === 5 && Number(r5.c2) === 12 && Number(r5.c3) === 6,
          "2x2 block pasted into rows 4-5");

    console.log("range clear + undo/redo");
    await L.selectCell(page, 6, "c2");
    await L.selectCell(page, 7, "c2", true);
    await L.key(page, "Delete");
    data = await L.gridData(page);
    const cleared = data.filter((r) => [6, 7].includes(r.__editr_id__)).map((r) => r.c2);
    check(cleared.every((v) => v === null || v === undefined || v === ""), "range cleared: " + JSON.stringify(cleared));
    await L.key(page, "z", true);
    data = await L.gridData(page);
    const restored = data.filter((r) => [6, 7].includes(r.__editr_id__)).map((r) => r.c2);
    check(restored.every((v) => typeof v === "number"), "Ctrl+Z restored both cells in one step: " + JSON.stringify(restored));
    await L.key(page, "y", true);
    data = await L.gridData(page);
    check(data.filter((r) => [6, 7].includes(r.__editr_id__)).every((r) => r.c2 == null), "Ctrl+Y re-applied the clear");
    await L.key(page, "z", true);

    console.log("fill down");
    await L.selectCell(page, 4, "c5");
    await L.selectCell(page, 6, "c5", true);
    await L.key(page, "d", true);
    data = await L.gridData(page);
    const hp = [4, 5, 6].map((id) => data.find((r) => r.__editr_id__ === id).c5);
    check(hp.every((v) => v === hp[0]), "Ctrl+D filled rows 5-6 from row 4: " + JSON.stringify(hp));
    await L.key(page, "z", true);
    data = await L.gridData(page);
    const hp2 = [4, 5, 6].map((id) => data.find((r) => r.__editr_id__ === id).c5);
    check(hp2[1] !== hp2[0], "fill down undone in one step: " + JSON.stringify(hp2));

    console.log("undo structural change");
    await L.rowMenu(page, 1, "c2", "Delete row(s)");
    data = await L.gridData(page);
    check(data.length === 7, "row 1 deleted");
    await page.click("#edit1-undo");
    await L.settle(page);
    data = await L.gridData(page);
    check(data.length === 8 && data[0].__editr_id__ === 1 && Number(data[0].c2) === 30.5,
          "undo button restored row 1 in place with its edit");
    const status = await page.$eval("#edit1-status", (e) => e.textContent);
    check(/edited/.test(status), "status shows change summary: '" + status + "'");

    console.log("column select modal");
    await page.click("#select1-select");
    await page.waitForSelector("#select1-button-1", { visible: true });
    await page.click("#select1-button-1");
    await L.sleep(200);
    await page.click("#select1-button-2");
    await L.sleep(200);
    await page.click("#select1-close");
    await L.settle(page, 1200);
    cols = await L.columns(page);
    check(JSON.stringify(cols.map((c) => c.title)) === JSON.stringify([" ", "mpg", "cylinders"]),
          "only selected columns shown (plus row names): " + cols.map((c) => c.title).join(","));

    console.log("row filter modal");
    const before = await L.gridData(page);
    const six = before.filter((r) => Number(r.c3) === 6).map((r) => r.__editr_id__);
    await page.click("#filter1-filter");
    await page.waitForSelector("#filter1-filter-1-col", { timeout: 5000 });
    await L.sleep(500);
    await page.evaluate(() => {
      $("#filter1-filter-1-col")[0].selectize.setValue("cylinders");
    });
    await L.sleep(800);
    await page.evaluate(() => {
      $("#filter1-filter-1-logic")[0].selectize.setValue("equal");
      const s = $("#filter1-filter-1-levels")[0].selectize;
      s.addOption({ value: "6", label: "6" });
      s.setValue(["6"]);
    });
    await L.sleep(800);
    await page.click("#filter1-close");
    await L.settle(page, 1500);
    data = await L.gridData(page);
    check(JSON.stringify(data.map((r) => r.__editr_id__)) === JSON.stringify(six),
          "filtered to cyl == 6 rows " + JSON.stringify(six) + " got " + JSON.stringify(data.map((r) => r.__editr_id__)));

    console.log("editing inside the filtered view");
    await L.editCell(page, six[0], "c2", "99");
    await L.rowMenu(page, six[0], "c2", "Insert row below");
    data = await L.gridData(page);
    check(data.length === six.length + 1, "new row visible in filtered view");
    await page.click("#edit1-undo");
    await L.settle(page);
    data = await L.gridData(page);
    check(data.length === six.length, "undo removed it again");

    console.log("clear view");
    await page.click("#select1-select");
    await page.waitForSelector("#select1-select_none", { visible: true });
    await page.click("#select1-select_none");
    await L.sleep(200);
    await page.click("#select1-close");
    await L.settle(page, 1000);
    await page.click("#filter1-filter");
    await page.waitForSelector("#filter1-filter_reset", { visible: true });
    await L.sleep(800); // modal fade-in
    await page.click("#filter1-filter_reset");
    await L.sleep(500);
    await page.click("#filter1-close");
    await L.settle(page, 1200);
    data = await L.gridData(page);
    cols = await L.columns(page);
    check(data.length === 8 && cols.length === 5 && JSON.stringify(data.map((r) => r.__editr_id__)) === JSON.stringify([1, 2, 3, 9, 4, 5, 6, 7]), "full data shown again (" + data.length + " rows, " + cols.length + " cols)");

    await page.screenshot({ path: process.argv[3] + "/memory.png" });
    console.log("done");
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

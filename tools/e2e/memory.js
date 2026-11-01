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
    // punctuation must not move the editor ("(" used to act as ArrowDown,
    // "'" as ArrowRight, "&" as ArrowUp and "%" as ArrowLeft)
    await L.editCell(page, 1, cols[5].field, "hello (it's #1 & 100%!)");
    data = await L.gridData(page);
    check(data[0][cols[5].field] === "hello (it's #1 & 100%!)" && data[1][cols[5].field] == null,
          "punctuation typed into one cell: " + JSON.stringify(data[0][cols[5].field]));
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
    await L.sleep(300);
    const sel = await page.$eval("#edit1-selection", (e) => e.textContent);
    check(/Selection hp . Sum 390 . Mean 130 . Count 3/.test(sel), "status bar sums the selection: " + sel);
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

    console.log("columns menu");
    const fieldOf = async (title) => (await L.columns(page)).find((c) => c.title === title).field;
    const selected = async () => page.evaluate(() => {
      const c = document.getElementById("edit1-grid").__editr.table.getRanges()[0].getCells();
      return c.length && c[0].length ? c[0][0].getRow().getIndex() + "|" + c[0][0].getField() : null;
    });
    const v1 = await fieldOf("V1");
    await L.selectCell(page, 2, v1);
    await page.click("#edit1-columns");
    await page.waitForSelector(".editr-columns-menu");
    // the selection stays on its cell when a column to its left is hidden,
    // and moves to the nearest visible column when its own column is hidden
    const expectSel = { hp: "2|" + v1, V1: "2|c3" };
    for (const name of ["hp", "V1"]) {
      await page.evaluate((n) => {
        const l = [...document.querySelectorAll(".editr-columns-list label")].find((x) => x.textContent.trim() === n);
        l.querySelector("input").click();
      }, name);
      await L.sleep(150);
      const sel = await selected();
      check(sel === expectSel[name], "selection after hiding " + name + ": " + sel);
    }
    await page.mouse.click(900, 700);
    await L.settle(page);
    const visibleTitles = async () => page.evaluate(() => document.getElementById("edit1-grid").__editr.table.getColumns()
      .filter((c) => c.getField() && c.getField() !== "__editr_add__" && c.isVisible()).map((c) => c.getDefinition().title));
    let vis = await visibleTitles();
    check(JSON.stringify(vis) === JSON.stringify([" ", "mpg", "cylinders"]), "hp and V1 hidden: " + vis.join(","));
    check((await page.$eval("#edit1-columns_count", (e) => e.textContent)) === "2 of 4", "toolbar shows 2 of 4 columns");

    console.log("column layout survives undo/redo of a structural edit");
    const mpgWidth = async () => page.evaluate(() => document.getElementById("edit1-grid").__editr.table.getColumn("c2").getWidth());
    const edge = await page.evaluate(() => {
      const b = document.getElementById("edit1-grid").__editr.table.getColumn("c2").getElement().getBoundingClientRect();
      return { x: b.right - 2, y: b.top + 12 };
    });
    await page.mouse.move(edge.x, edge.y);
    await page.mouse.down();
    await page.mouse.move(edge.x + 60, edge.y, { steps: 6 });
    await page.mouse.up();
    await L.settle(page);
    const resized = await mpgWidth();
    await L.renameColumn(page, "c2", "miles");
    await page.click("#edit1-undo");
    await L.settle(page);
    await page.click("#edit1-redo");
    await L.settle(page);
    await page.click("#edit1-undo");
    await L.settle(page);
    vis = await visibleTitles();
    check(JSON.stringify(vis) === JSON.stringify([" ", "mpg", "cylinders"]), "hidden columns stay hidden: " + vis.join(","));
    const after = await mpgWidth();
    check(Math.abs(after - resized) < 2, "resized width kept: " + resized + " -> " + after);

    console.log("header filter (values list)");
    const before = await L.gridData(page);
    const six = before.filter((r) => Number(r.c3) === 6).map((r) => r.__editr_id__);
    const fbox = await page.evaluate(() => {
      const t = document.getElementById("edit1-grid").__editr.table;
      const i = t.getColumn("c3").getElement().querySelector(".tabulator-header-filter input");
      const b = i.getBoundingClientRect();
      return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
    });
    await page.mouse.click(fbox.x, fbox.y);
    await page.waitForSelector(".tabulator-edit-list .tabulator-edit-list-item", { timeout: 3000 });
    const opts = await page.$$eval(".tabulator-edit-list .tabulator-edit-list-item", (e) => e.map((x) => x.textContent.trim()));
    check(opts.includes("6") && opts.includes("4"), "filter offers the column's values: " + opts.join(","));
    await page.evaluate(() => {
      [...document.querySelectorAll(".tabulator-edit-list .tabulator-edit-list-item")].find((x) => x.textContent.trim() === "6").dispatchEvent(new MouseEvent("click", { bubbles: true }));
    });
    await L.sleep(300);
    await page.mouse.click(900, 700);
    await L.settle(page, 800);
    data = await page.evaluate(() => document.getElementById("edit1-grid").__editr.table.getData("active"));
    check(JSON.stringify(data.map((r) => r.__editr_id__)) === JSON.stringify(six),
          "filtered to cylinders == 6 rows " + JSON.stringify(six) + " got " + JSON.stringify(data.map((r) => r.__editr_id__)));
    const chip = await page.$eval("#edit1-chips", (e) => e.textContent.trim());
    check(chip === "cylinders is 6", "filter chip shown: '" + chip + "'");
    const rowsText = await page.$eval("#edit1-rows", (e) => e.textContent);
    check(rowsText.startsWith(six.length + " of 8 rows"), "status bar row count: " + rowsText);

    console.log("editing inside the filtered view");
    await L.editCell(page, six[0], "c2", "99");
    await L.rowMenu(page, six[0], "c2", "Insert row below");
    data = await page.evaluate(() => document.getElementById("edit1-grid").__editr.table.getData("active"));
    check(data.length === six.length + 1, "new row stays visible under the filter");
    await page.click("#edit1-undo");
    await L.settle(page);
    data = await page.evaluate(() => document.getElementById("edit1-grid").__editr.table.getData("active"));
    check(data.length === six.length, "undo removed it again");

    console.log("find");
    await page.click("#edit1-find");
    await page.keyboard.type("hornet");
    await L.sleep(500);
    let found = await page.$eval("#edit1-find_count", (e) => e.textContent);
    check(found === "1 of 1", "find under the filter: " + found);
    await page.keyboard.press("Escape");

    console.log("clear view");
    await page.click("#edit1-chips button");
    await L.settle(page, 800);
    await page.click("#edit1-columns");
    await page.waitForSelector(".editr-columns-menu");
    await page.evaluate(() => [...document.querySelectorAll(".editr-columns-head button")].find((b) => b.textContent === "Show all").click());
    await page.mouse.click(900, 700);
    await L.settle(page, 800);
    data = await page.evaluate(() => document.getElementById("edit1-grid").__editr.table.getData("active"));
    vis = await visibleTitles();
    check(data.length === 8 && vis.length === 5 && JSON.stringify(data.map((r) => r.__editr_id__)) === JSON.stringify([1, 2, 3, 9, 4, 5, 6, 7]), "full data shown again (" + data.length + " rows, " + vis.length + " cols)");
    check((await page.$eval("#edit1-chips", (e) => e.textContent.trim())) === "", "no chips left");

    console.log("find across all rows");
    await page.click("#edit1-find");
    await page.keyboard.type("hornet");
    await L.sleep(500);
    found = await page.$eval("#edit1-find_count", (e) => e.textContent);
    check(found === "1 of 2", "two matches: " + found);
    await page.keyboard.press("Enter");
    await L.sleep(300);
    found = await page.$eval("#edit1-find_count", (e) => e.textContent);
    check(found === "2 of 2", "Enter moves to the next match: " + found);
    await page.keyboard.press("Escape");

    console.log("type guard");
    const g = await L.cellCentre(page, 5, "c2");
    await page.mouse.click(g.x, g.y, { clickCount: 2 });
    await page.waitForSelector(".tabulator-cell.tabulator-editing input");
    await page.keyboard.down("Control"); await page.keyboard.press("KeyA"); await page.keyboard.up("Control");
    await page.keyboard.type("abc");
    await page.keyboard.press("Enter");
    await page.waitForSelector(".editr-guard", { timeout: 3000 });
    check(true, "guard asks before turning mpg into text");
    await page.click('.editr-guard [data-act="cancel"]');
    await L.settle(page);
    data = await L.gridData(page);
    check(Number(data.find((r) => r.__editr_id__ === 5).c2) === 12, "cancel restored the value");

    console.log("review panel");
    await page.click("#edit1-open_changes");
    await page.waitForSelector("#edit1-changes .editr-change", { timeout: 5000 });
    const items = await page.$$eval("#edit1-changes .editr-change", (e) => e.map((x) => x.textContent.replace(/\s+/g, " ").trim()));
    check(items.length >= 4, items.length + " changes listed");
    check(items.some((t) => /Mazda RX4 Wag/.test(t) && /21 . 21.5/.test(t)), "edit listed with old and new value");
    const badge = await page.$eval("#edit1-review_count", (e) => e.textContent);
    check(badge === String(items.length), "toolbar badge matches (" + badge + ")");
    // revert the Mazda RX4 Wag edit, then undo the revert
    await page.evaluate(() => {
      const li = [...document.querySelectorAll("#edit1-changes .editr-change")].find((x) => /Mazda RX4 Wag/.test(x.textContent));
      li.querySelector(".btn").click();
    });
    await L.settle(page, 800);
    data = await L.gridData(page);
    check(Number(data.find((r) => r.__editr_id__ === 2).c2) === 21, "revert put mpg back to 21");
    await page.click("#edit1-undo");
    await L.settle(page, 800);
    data = await L.gridData(page);
    check(Number(data.find((r) => r.__editr_id__ === 2).c2) === 21.5, "undo brought the edit back");
    await page.click("#edit1-tab_code");
    await page.waitForFunction(() => /add_row/.test(document.getElementById("edit1-code").textContent), { timeout: 5000 });
    check(true, "code tab shows generated code");
    await page.click("#edit1-open_code");

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

// Copy and paste with spreadsheet clipboard formats.
const L = require("./lib");
const { check } = L;

const GRID = "edit1-grid";
const rowOf = (data, id) => data.find((r) => r.__editr_id__ === id);
const fieldOf = async (page, title) =>
  (await L.columns(page)).find((c) => c.title === title).field;

async function guard(page) {
  await page.waitForSelector(".editr-guard", { timeout: 3000 });
  return page.$eval(".editr-guard", (e) => ({
    text: e.textContent,
    acts: [...e.querySelectorAll("[data-act]")].map((b) => b.getAttribute("data-act")),
  }));
}

async function copySelection(page) {
  await page.evaluate(() => {
    window.__copied = null;
    if (!window.__copyHook) {
      window.__copyHook = true;
      document.addEventListener("copy", (e) => {
        const plain = e.clipboardData.getData("text/plain");
        if (plain) window.__copied = { plain, html: e.clipboardData.getData("text/html") };
      });
    }
  });
  await L.key(page, "c", true);
  return page.evaluate(() => window.__copied);
}

(async () => {
  const port = Number(process.argv[2]);
  const { browser, page } = await L.launch(port);
  try {
    const n = await fieldOf(page, "n"), d = await fieldOf(page, "d");
    const name = await fieldOf(page, "name"), ok = await fieldOf(page, "ok");
    const grp = await fieldOf(page, "grp");

    console.log("Excel text: CRLF line endings and a trailing line break");
    await L.selectCell(page, 1, n);
    await L.paste(page, "10\t2024-02-01\r\n20\t2024-02-02\r\n");
    let data = await L.gridData(page);
    check(Number(rowOf(data, 1)[n]) === 10 && Number(rowOf(data, 2)[n]) === 20,
          "two rows pasted: " + rowOf(data, 1)[n] + ", " + rowOf(data, 2)[n]);
    check(rowOf(data, 2)[d] === "2024-02-02", "no carriage return left in the last cell");
    check(Number(rowOf(data, 3)[n]) === 3 && rowOf(data, 3)[d] === "2024-01-03",
          "the trailing line break does not blank the next row");
    check(data.length === 3, "no rows added");

    console.log("quoted cells with line breaks, tabs and quotes");
    await L.selectCell(page, 1, name);
    await L.paste(page, '"two\r\nlines"\t5\r\n"say ""hi""\tthere"\t6\r\n');
    data = await L.gridData(page);
    check(rowOf(data, 1)[name] === "two\nlines", "line break kept inside the cell");
    check(rowOf(data, 2)[name] === 'say "hi"\tthere', "quotes and tab kept inside the cell");
    check(Number(rowOf(data, 2)[n]) === 6, "the cell after a quoted cell lands in its column");

    console.log("formatted numbers and day-first dates");
    await L.selectCell(page, 1, n);
    await L.paste(page, "1,234.5\t15/03/2024\n45%\t3/4/2024\n$12\t1.4.2024\n");
    data = await L.gridData(page);
    check([1, 2, 3].map((i) => Number(rowOf(data, i)[n])).join() === "1234.5,0.45,12",
          "1,234.5 / 45% / $12 -> " + [1, 2, 3].map((i) => rowOf(data, i)[n]).join(" / "));
    check([1, 2, 3].map((i) => rowOf(data, i)[d]).join() === "2024-03-15,2024-04-03,2024-04-01",
          "15/03/2024 makes the block day-first: " + [1, 2, 3].map((i) => rowOf(data, i)[d]).join(" "));

    console.log("logicals");
    await L.selectCell(page, 1, ok);
    await L.paste(page, "no\nyes\n");
    data = await L.gridData(page);
    check(rowOf(data, 1)[ok] === "FALSE" || rowOf(data, 1)[ok] === false,
          "no -> FALSE (" + rowOf(data, 1)[ok] + ")");

    console.log("one value fills a selected range");
    await L.selectCell(page, 1, grp);
    await L.selectCell(page, 3, grp, true);
    await L.paste(page, "z\r\n");
    data = await L.gridData(page);
    check([1, 2, 3].every((i) => rowOf(data, i)[grp] === "z"), "z pasted into all three rows");

    console.log("more rows than the table has");
    await L.selectCell(page, 3, name);
    await L.paste(page, "c2\t30\nd\t40\ne\t50\n");
    data = await L.gridData(page);
    check(data.length === 5, "two rows added (" + data.length + ")");
    const added = data.filter((r) => r.__editr_id__ > 3);
    check(added.map((r) => r[name] + "=" + r[n]).join() === "d=40,e=50", "added rows hold the values");
    await page.click("#edit1-undo");
    await L.settle(page);
    data = await L.gridData(page);
    check(data.length === 3 && rowOf(data, 3)[name] === "c", "one undo removes the rows and the edit");

    console.log("values that do not fit");
    await L.selectCell(page, 1, n);
    await L.paste(page, "abc\n7\n");
    let g = await guard(page);
    check(/1 pasted value does not fit/.test(g.text) && /abc/.test(g.text), "asks about “abc”");
    check(g.acts.join() === "na,text,cancel", "NA, convert to text or cancel: " + g.acts.join());
    await page.click('.editr-guard [data-act="na"]');
    await L.settle(page);
    data = await L.gridData(page);
    check(rowOf(data, 1)[n] === null && Number(rowOf(data, 2)[n]) === 7, "NA stored, the rest pasted");

    await L.selectCell(page, 2, grp);
    await L.paste(page, "w\n");
    g = await guard(page);
    check(g.acts.join() === "na,cancel", "dropdown values cannot become text: " + g.acts.join());
    await page.click('.editr-guard [data-act="cancel"]');
    await L.settle(page);
    data = await L.gridData(page);
    check(rowOf(data, 2)[grp] === "z", "cancel leaves the cell alone");

    console.log("copy");
    await L.selectCell(page, 1, name);
    await L.selectCell(page, 2, ok, true);
    const copied = await copySelection(page);
    check(copied && copied.plain.split("\n").length === 3 &&
          copied.plain.startsWith('"two\nlines"\t\t2024-03-15\tFALSE'),
          "quoted multi-line cell, empty NA, TRUE/FALSE: " + JSON.stringify(copied && copied.plain));
    check(copied && /<table>/.test(copied.html) && /two<br>lines/.test(copied.html),
          "HTML table for spreadsheets");

    console.log("Ctrl+Z in a filter box edits the text");
    const before = await page.$eval("#edit1-review_count", (e) => e.textContent);
    await page.evaluate((f) => {
      const t = document.getElementById("edit1-grid").__editr.table;
      t.getColumn(f).getElement().querySelector(".tabulator-header-filter input").focus();
    }, name);
    await L.key(page, "z", true);
    const after = await page.$eval("#edit1-review_count", (e) => e.textContent);
    check(before === after, "no undo of the data (" + before + " -> " + after + ")");

    console.log("done");
    await page.click("#done");
    await L.sleep(1500);
  } catch (e) {
    console.log("ERROR " + e.stack);
    L.check(false, "script error");
  }
  await browser.close();
  const f = L.failures();
  console.log(f ? f + " BROWSER CHECK(S) FAILED" : "BROWSER CHECKS PASSED");
  process.exit(f ? 1 : 0);
})();

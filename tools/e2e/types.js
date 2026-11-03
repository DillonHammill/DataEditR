// Column types from col_options: editors, formatters and checks.
const L = require("./lib");
const { check } = L;

const rowOf = (data, id) => data.find((r) => r.__editr_id__ === id);
const fieldOf = async (page, title) =>
  (await L.columns(page)).find((c) => c.title === title).field;

async function openEditor(page, id, field) {
  const p = await L.cellCentre(page, id, field);
  await page.mouse.click(p.x, p.y, { clickCount: 2 });
  await page.waitForSelector(".tabulator-cell.tabulator-editing", { timeout: 3000 });
  return page.$eval(".tabulator-cell.tabulator-editing", (e) => {
    const i = e.querySelector("input, textarea");
    return i ? (i.tagName === "TEXTAREA" ? "textarea" : i.type) : e.innerHTML.slice(0, 40);
  });
}

async function setEditorValue(page, value) {
  await page.$eval(".tabulator-cell.tabulator-editing input", (i, v) => {
    i.value = v;
    i.dispatchEvent(new Event("input", { bubbles: true }));
  }, value);
}

(async () => {
  const port = Number(process.argv[2]);
  const { browser, page } = await L.launch(port);
  try {
    const f = {};
    for (const t of ["notes", "when", "at", "score", "pct", "stars", "site", "col", "d", "ok"]) {
      f[t] = await fieldOf(page, t);
    }
    let data;

    console.log("textarea");
    check(await openEditor(page, 2, f.notes) === "textarea", "textarea editor");
    await page.keyboard.type("line one");
    await page.keyboard.down("Shift");
    await page.keyboard.press("Enter");
    await page.keyboard.up("Shift");
    await page.keyboard.type("line two");
    await page.keyboard.press("Enter");
    await L.settle(page);
    data = await L.gridData(page);
    check(rowOf(data, 2)[f.notes] === "line one\nline two", "Shift+Enter adds a line, Enter saves");

    console.log("datetime");
    check(await openEditor(page, 2, f.when) === "datetime-local", "date-time picker");
    await setEditorValue(page, "2024-05-06T07:08");
    await page.keyboard.press("Enter");
    await L.settle(page);
    data = await L.gridData(page);
    check(rowOf(data, 2)[f.when] === "2024-05-06 07:08:00", "stored as " + rowOf(data, 2)[f.when]);

    console.log("time");
    check(await openEditor(page, 2, f.at) === "time", "time picker");
    await setEditorValue(page, "14:45");
    await page.keyboard.press("Enter");
    await L.settle(page);
    data = await L.gridData(page);
    check(rowOf(data, 2)[f.at] === "14:45", "time stored as " + rowOf(data, 2)[f.at]);

    console.log("number with limits");
    check(await openEditor(page, 2, f.score) === "number", "number editor");
    await page.keyboard.type("11");
    await page.keyboard.press("Enter");
    await page.waitForSelector(".editr-guard", { timeout: 3000 });
    const g = await page.$eval(".editr-guard", (e) => ({
      text: e.textContent, acts: [...e.querySelectorAll("[data-act]")].map((b) => b.getAttribute("data-act")) }));
    check(/is more than 10/.test(g.text) && /0 to 10/.test(g.text), "11 is more than 10");
    check(g.acts.join() === "na,cancel", "no convert to text: " + g.acts.join());
    await page.click('.editr-guard [data-act="cancel"]');
    await L.settle(page);
    await L.editCell(page, 2, f.score, "7.5");
    data = await L.gridData(page);
    check(Number(rowOf(data, 2)[f.score]) === 7.5, "7.5 accepted");

    console.log("slider");
    check(await openEditor(page, 2, f.pct) === "range", "slider editor");
    await setEditorValue(page, "60");
    await page.keyboard.press("Enter");
    await L.settle(page);
    data = await L.gridData(page);
    check(Number(rowOf(data, 2)[f.pct]) === 60, "slider value " + rowOf(data, 2)[f.pct]);
    // dragging the handle sets the value instead of selecting cells
    await openEditor(page, 1, f.pct);
    const r = await page.$eval(".tabulator-cell.tabulator-editing input[type=range]", (i) => {
      const b = i.getBoundingClientRect();
      return { x: b.x, y: b.y + b.height / 2, w: b.width };
    });
    await page.mouse.move(r.x + r.w * 0.25, r.y);
    await page.mouse.down();
    await page.mouse.move(r.x + r.w - 1, r.y, { steps: 8 });
    await page.mouse.up();
    await page.keyboard.press("Enter");
    await L.settle(page);
    data = await L.gridData(page);
    check(Number(rowOf(data, 1)[f.pct]) === 100, "dragged to 100 (" + rowOf(data, 1)[f.pct] + ")");
    const bar = await page.evaluate((field) => {
      const t = document.getElementById("edit1-grid").__editr.table;
      return !!t.getRow(1).getCell(field).getElement().querySelector("div[data-max]");
    }, f.pct);
    check(bar, "progress bar shown");
    const blank = await page.evaluate((field) => {
      const t = document.getElementById("edit1-grid").__editr.table;
      return t.getRow(2).getCell(field).getElement().textContent;
    }, f.pct);
    check(blank === "60", "bar legend shows the value");

    console.log("rating");
    const stars = await page.evaluate((field) => {
      const t = document.getElementById("edit1-grid").__editr.table;
      return t.getRow(1).getCell(field).getElement().querySelectorAll("svg").length;
    }, f.stars);
    check(stars === 5, "five stars drawn (" + stars + ")");
    await openEditor(page, 2, f.stars);
    const star = await page.evaluate(() => {
      const s = document.querySelectorAll(".tabulator-cell.tabulator-editing svg")[3].getBoundingClientRect();
      return { x: s.x + s.width / 2, y: s.y + s.height / 2 };
    });
    await page.mouse.click(star.x, star.y);
    await L.settle(page);
    data = await L.gridData(page);
    check(Number(rowOf(data, 2)[f.stars]) === 4, "fourth star gives 4 (" + rowOf(data, 2)[f.stars] + ")");

    console.log("url");
    const links = await page.evaluate((field) => {
      const t = document.getElementById("edit1-grid").__editr.table;
      return [1, 2].map((id) => {
        const a = t.getRow(id).getCell(field).getElement().querySelector("a");
        return a ? a.getAttribute("href") + "|" + a.target : null;
      });
    }, f.site);
    check(links[0] === "https://example.org|_blank", "web address gets an open button");
    check(links[1] === null, "javascript: is shown as text only");

    console.log("colour");
    const swatches = await page.evaluate((field) => {
      const t = document.getElementById("edit1-grid").__editr.table;
      return [1, 2].map((id) => {
        const s = t.getRow(id).getCell(field).getElement().querySelector(".editr-swatch");
        if (!s) return null;
        return s.classList.contains("editr-swatch-none") ? "none" : getComputedStyle(s).backgroundColor;
      });
    }, f.col);
    check(swatches[0] === "rgb(255, 0, 0)", "red swatch");
    check(swatches[1] === "rgb(127, 127, 127)", "R colour names get their swatch (grey50): " + swatches[1]);

    console.log("typed values in other formats");
    await L.editCell(page, 2, f.d, "15/03/2024");
    await L.editCell(page, 2, f.ok, "yes");
    data = await L.gridData(page);
    check(rowOf(data, 2)[f.d] === "2024-03-15", "15/03/2024 -> " + rowOf(data, 2)[f.d]);
    check(rowOf(data, 2)[f.ok] === "TRUE" || rowOf(data, 2)[f.ok] === true,
          "yes -> TRUE (" + rowOf(data, 2)[f.ok] + ")");
    const badges = await page.evaluate(() => [...document.querySelectorAll(".editr-badge")].map((b) => b.textContent));
    check(badges.includes("dttm") && badges.includes("time"), "badges: " + badges.join(" "));

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

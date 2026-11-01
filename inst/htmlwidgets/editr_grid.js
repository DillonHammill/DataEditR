/*
 * DataEditR grid: a thin htmlwidget binding around Tabulator.
 *
 * Contract with R:
 *   - rows carry a stable id in the "__editr_id__" field (the Tabulator index)
 *   - columns are keyed by stable column ids ("c1", "c2", ...); the column
 *     name is only the title, so renames never touch the data
 *   - every user change is reported as an op on Shiny input `<id>_ops`,
 *     batched per event loop tick: {seq, ops: [...]}
 *   - R pushes changes back with the "editr_grid" custom message
 *     ({id, method, args}); those changes never emit ops
 */
(function () {
  "use strict";

  var ID = "__editr_id__";
  var ADD_FIELD = "__editr_add__";
  var pending = {}; // messages that arrive before a grid is built

  function registerHandler() {
    if (window.__editrGridHandler || !window.Shiny ||
        !Shiny.addCustomMessageHandler) {
      return;
    }
    window.__editrGridHandler = true;
    Shiny.addCustomMessageHandler("editr_grid", function (msg) {
      var el = document.getElementById(msg.id);
      if (el && el.__editr && el.__editr.ready) {
        el.__editr.handle(msg.method, msg.args || {});
      } else {
        (pending[msg.id] = pending[msg.id] || []).push(msg);
      }
    });
    // R sends either plain text or HTML it has escaped itself (entities keep
    // the R sources ASCII and survive non-UTF-8 locales)
    Shiny.addCustomMessageHandler("editr_status", function (msg) {
      var el = document.getElementById(msg.id);
      if (!el) return;
      if (msg.html !== undefined && msg.html !== null) {
        el.innerHTML = msg.html;
      } else {
        el.textContent = msg.text;
      }
    });
  }

  registerHandler();

  function toArray(x) {
    if (x === null || x === undefined) return [];
    return Array.isArray(x) ? x : [x];
  }

  function jsonValue(v) {
    return v === undefined ? null : v;
  }

  function escapeHTML(x) {
    return String(x).replace(/[&<>"']/g, function (ch) {
      return {"&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;"}[ch];
    });
  }

  function byId(id) {
    return id ? document.getElementById(id) : null;
  }

  function isBlank(v) {
    return v === null || v === undefined ||
      (typeof v === "string" && (v.trim() === "" || v.trim() === "NA"));
  }

  function formatNumber(x) {
    if (!isFinite(x)) return String(x);
    var a = Math.abs(x);
    if (a !== 0 && (a >= 1e6 || a < 1e-3)) return x.toExponential(3);
    return String(Number(x.toPrecision(6)));
  }

  /* values typed or pasted in other common formats ----------------------- */

  // "1,234.5", "$12.50", "45%" (as copied from formatted spreadsheet cells)
  // become plain numbers; anything else is returned unchanged
  function normalizeNumber(v) {
    if (typeof v !== "string") return v;
    var t = v.trim();
    if (t === "" || !isNaN(Number(t))) return v;
    var pct = /%$/.test(t);
    if (pct) t = t.slice(0, -1).trim();
    var sign = "";
    if (/^[-+]/.test(t)) {
      sign = t.charAt(0) === "-" ? "-" : "";
      t = t.slice(1).trim();
    }
    t = t.replace(/^[$€£¥]\s?/, "");
    if (/^\d{1,3}(,\d{3})+(\.\d+)?$/.test(t)) t = t.replace(/,/g, "");
    if (!/^(\d+(\.\d*)?|\.\d+)$/.test(t)) return v;
    var x = Number(sign + t);
    if (pct) x = Number((x / 100).toPrecision(15));
    return String(x);
  }

  var YMD = /^(\d{4})[\/.\-](\d{1,2})[\/.\-](\d{1,2})(?:[ T](\d{1,2}):(\d{2})(?::(\d{2}(?:\.\d+)?))?)?$/;
  var DMY = /^(\d{1,2})[\/.\-](\d{1,2})[\/.\-](\d{4})(?:[ T](\d{1,2}):(\d{2})(?::(\d{2}(?:\.\d+)?))?)?$/;

  function pad2(x) {
    x = String(x);
    return x.length < 2 ? "0" + x : x;
  }

  function validDate(s) {
    var d = new Date(s + "T00:00:00Z");
    return !isNaN(d.getTime()) && d.toISOString().slice(0, 10) === s;
  }

  function validDateTime(s) {
    var m = /^(\d{4}-\d{2}-\d{2})[ T](\d{2}):(\d{2})(?::(\d{2})(\.\d+)?)?$/.exec(s);
    if (!m) return validDate(s);
    return validDate(m[1]) && +m[2] < 24 && +m[3] < 60 &&
      (m[4] === undefined || +m[4] < 60);
  }

  // day-first or month-first for d/m/y style dates: decided by values that
  // can only be one of them, otherwise by the browser's language (US English
  // is month-first)
  function dayFirst(values) {
    var dmy = null;
    values.forEach(function (v) {
      var m = typeof v === "string" ? DMY.exec(v.trim()) : null;
      if (!m) return;
      if (+m[1] > 12) dmy = true;
      else if (+m[2] > 12 && dmy === null) dmy = false;
    });
    if (dmy === null) {
      var lang = (navigator.languages && navigator.languages[0]) ||
        navigator.language || "";
      dmy = !/^en-US$/i.test(lang);
    }
    return dmy;
  }

  // "15/03/2024", "2024/3/15" or "15.03.2024 14:30" -> ISO date or date-time;
  // anything else is returned unchanged
  function normalizeDate(v, kind, dmy) {
    if (typeof v !== "string") return v;
    var t = v.trim(), m, y, mo, d;
    if ((m = YMD.exec(t))) {
      y = m[1]; mo = m[2]; d = m[3];
    } else if ((m = DMY.exec(t))) {
      y = m[3]; mo = dmy ? m[2] : m[1]; d = dmy ? m[1] : m[2];
    } else {
      return v;
    }
    var date = y + "-" + pad2(mo) + "-" + pad2(d);
    if (!validDate(date)) return v;
    var h = m[4], mi = m[5], s = m[6];
    if (kind === "date") {
      // a time of midnight is how spreadsheets show plain dates
      if (h !== undefined && (+h !== 0 || +mi !== 0 || (s && +s !== 0))) return v;
      return date;
    }
    if (h === undefined) return date + " 00:00:00";
    return date + " " + pad2(h) + ":" + mi + ":" + (s ? pad2(s) : "00");
  }

  var TRUE_WORDS = /^(true|t|yes|y|1)$/i;
  var FALSE_WORDS = /^(false|f|no|n|0)$/i;

  // checkboxes and R need real logicals ("TRUE" would show an empty box)
  function normalizeLogical(v) {
    if (typeof v !== "string") return v;
    var t = v.trim();
    if (TRUE_WORDS.test(t)) return true;
    if (FALSE_WORDS.test(t)) return false;
    return v;
  }

  // a typed or pasted value in the format R expects for this column
  function normalizeValue(c, v, dmy) {
    if (isBlank(v) || !c) return v;
    var o = c.option || {};
    if (c.kind === "numeric") return normalizeNumber(v);
    if (c.kind === "date" || c.kind === "datetime") {
      return normalizeDate(v, c.kind, dmy === undefined ? dayFirst([v]) : dmy);
    }
    if (c.kind === "logical" || o.type === "checkbox") return normalizeLogical(v);
    if (typeof v === "string" && o.type === "time") {
      var m = /^(\d{1,2}):(\d{2})(?::(\d{2}))?$/.exec(v.trim());
      if (m) return pad2(m[1]) + ":" + m[2] + (m[3] ? ":" + m[3] : "");
    }
    return v;
  }

  // why a value cannot be stored in a column (or null)
  function invalidReason(c, v) {
    if (isBlank(v) || !c) return null;
    var o = c.option || {};
    var s = typeof v === "string" ? v.trim() : v;
    if (o.type === "dropdown") {
      return toArray(o.values).indexOf(String(s)) === -1 ?
        "is not one of the options" : null;
    }
    if (c.kind === "numeric") {
      var x = typeof s === "number" ? s : Number(s);
      if (typeof s === "string" && isNaN(x)) return "is not a number";
      if (o.min !== undefined && o.min !== null && x < o.min) {
        return "is less than " + o.min;
      }
      if (o.max !== undefined && o.max !== null && x > o.max) {
        return "is more than " + o.max;
      }
      return null;
    }
    if (typeof s !== "string") return null;
    if (c.kind === "date" && !(/^\d{4}-\d{2}-\d{2}$/.test(s) && validDate(s))) {
      return "is not a date (YYYY-MM-DD)";
    }
    if (c.kind === "datetime" && !validDateTime(s)) {
      return "is not a date-time (YYYY-MM-DD HH:MM:SS)";
    }
    if ((c.kind === "logical" || o.type === "checkbox") &&
        !/^(true|false|t|f)$/i.test(s)) {
      return "is not TRUE or FALSE";
    }
    if (o.type === "time" && !/^([01]\d|2[0-3]):[0-5]\d(:[0-5]\d)?$/.test(s)) {
      return "is not a time (HH:MM)";
    }
    return null;
  }

  function kindPhrase(c) {
    var o = c.option || {};
    var has = function (x) { return x !== undefined && x !== null; };
    if (o.type === "dropdown") return "a dropdown column";
    if (o.type === "rating") return "a rating column (0 to " + o.max + " stars)";
    if (o.type === "time") return "a time column";
    if (c.kind === "numeric") {
      if (has(o.min) && has(o.max)) {
        return "a numeric column (" + o.min + " to " + o.max + ")";
      }
      if (has(o.min)) return "a numeric column (at least " + o.min + ")";
      if (has(o.max)) return "a numeric column (at most " + o.max + ")";
      return "a numeric column";
    }
    if (c.kind === "date") return "a date column";
    if (c.kind === "datetime") return "a date-time column";
    if (c.kind === "logical") return "a TRUE/FALSE column";
    return "a text column";
  }

  // can a column whose values do not fit be turned into a text column?
  function canBecomeText(c) {
    var o = c.option || {};
    return !o.type || o.type === "date" || o.type === "datetime";
  }

  /* clipboard text --------------------------------------------------------- */

  // Spreadsheet clipboard text (Excel, LibreOffice, Google Sheets): cells
  // separated by tabs, rows by CRLF or LF, usually a trailing line break,
  // and cells containing tabs, line breaks or quotes wrapped in quotes with
  // doubled inner quotes. Returns an array of rows of strings.
  function parseClipboardText(text) {
    var rows = [], row = [], field = "", start = true, i = 0;
    var n = typeof text === "string" ? text.length : 0;
    while (i < n) {
      var ch = text.charAt(i);
      if (start && ch === '"') {
        var j = i + 1, buf = "", closed = false;
        while (j < n) {
          if (text.charAt(j) === '"') {
            if (text.charAt(j + 1) === '"') {
              buf += '"';
              j += 2;
              continue;
            }
            closed = true;
            j += 1;
            break;
          }
          buf += text.charAt(j);
          j += 1;
        }
        var next = text.charAt(j);
        if (closed && (j >= n || next === "\t" || next === "\n" || next === "\r")) {
          field = buf.replace(/\r\n?/g, "\n");
          start = false;
          i = j;
          continue;
        }
        // not a quoted cell: read the quote as text
      }
      if (ch === "\t") {
        row.push(field);
        field = "";
        start = true;
        i += 1;
      } else if (ch === "\n" || ch === "\r") {
        row.push(field);
        rows.push(row);
        row = [];
        field = "";
        start = true;
        i += ch === "\r" && text.charAt(i + 1) === "\n" ? 2 : 1;
      } else {
        field += ch;
        start = false;
        i += 1;
      }
    }
    if (!start || field !== "" || row.length) {
      row.push(field);
      rows.push(row);
    }
    return rows;
  }

  // a cell for clipboard text: quoted when it contains tabs, line breaks or
  // quotes, logicals as TRUE/FALSE and missing values empty
  function clipboardCell(v) {
    if (v === null || v === undefined) return "";
    if (v === true) return "TRUE";
    if (v === false) return "FALSE";
    var s = String(v);
    if (/[\t\n\r"]/.test(s)) return '"' + s.replace(/"/g, '""') + '"';
    return s;
  }

  var ICONS = {
    lock: '<svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" role="img" aria-label="Read-only"><rect x="5" y="11" width="14" height="10" rx="2"/><path d="M8 11V7a4 4 0 0 1 8 0v4"/></svg>',
    plus: '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M12 5v14M5 12h14"/></svg>',
    x: '<svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M18 6 6 18M6 6l12 12"/></svg>',
    external: '<svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M14 4h6v6"/><path d="M20 4 10 14"/><path d="M18 14v5a1 1 0 0 1-1 1H5a1 1 0 0 1-1-1V7a1 1 0 0 1 1-1h5"/></svg>',
    alert: '<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M12 3 2 21h20z"/><path d="M12 10v5M12 18h.01"/></svg>'
  };

  var BADGES = {numeric: "123", character: "abc", logical: "T/F",
                date: "date", datetime: "dttm"};

  function uniqueName(base, existing) {
    var i = 1, name = base + i;
    while (existing.indexOf(name) !== -1) {
      i += 1;
      name = base + i;
    }
    return name;
  }

  // Tabulator 6.7.1 finds a row by position with a linear scan that calls
  // isDisplayed() (itself a linear scan) on every row. Range selection does
  // this on every layout, which costs ~1s per edit deep inside a 100k-row
  // table. Display rows are stored in position order, so look up directly
  // and fall back to Tabulator's implementation if that ever disagrees.
  function patchRowLookup(table) {
    var rm = table.rowManager;
    if (!rm || typeof rm.getRowFromPosition !== "function") return;
    var original = rm.getRowFromPosition;
    rm.getRowFromPosition = function (position) {
      var rows = this.getDisplayRows();
      var row = rows[position - 1];
      if (row && row.type === "row" && row.position === position) return row;
      return original.call(this, position);
    };
  }

  // Shiny can take input only while the session is open (a gadget that has
  // just closed would otherwise throw on a pending view report)
  function shinyOpen() {
    if (!window.Shiny || !Shiny.setInputValue) return false;
    var app = Shiny.shinyapp;
    return !app || typeof app.isConnected !== "function" || app.isConnected();
  }

  // Tabulator 6.7.1 turns a typed character into a key code with
  // key.toUpperCase().charCodeAt(0), so while editing a cell "(" (code 40)
  // acted as ArrowDown, "'" (39) as ArrowRight, "&" (38) as ArrowUp,
  // "%" (37) as ArrowLeft and "!", '"', "#", "$" scrolled the table. Only
  // letters and digits can be part of a shortcut, so ignore other characters.
  function patchKeyCodes(table) {
    var kb = table.modules && table.modules.keybindings;
    if (!kb || typeof kb.getKeyCode !== "function") return;
    var original = kb.getKeyCode;
    kb.getKeyCode = function (e) {
      if (e.key && e.key.length === 1 && !/^[A-Za-z0-9]$/.test(e.key)) return 0;
      return original.call(this, e);
    };
  }

  // Row.isDisplayed() is also a linear scan, and range selection calls it for
  // every rendered cell on every layout. Displayed rows carry their 1-based
  // display position (regenerated whenever the display changes), so check
  // that slot directly - exact for ungrouped tables like ours.
  function patchRowDisplayed(table) {
    var rows = table.rowManager && table.rowManager.rows;
    if (!rows || !rows.length) return;
    var proto = Object.getPrototypeOf(rows[0]);
    if (!proto || proto.__editrPatched || typeof proto.isDisplayed !== "function") return;
    proto.__editrPatched = true;
    proto.isDisplayed = function () {
      var display = this.table.rowManager.getDisplayRows();
      return !!this.position && display[this.position - 1] === this;
    };
  }

  // Range selection re-styles every row in the table after each edit or
  // keyboard move (layoutElement() with no arguments). Rows outside the
  // virtual viewport are styled when they render, so only lay out the
  // rendered rows.
  function patchRangeLayout(table) {
    var mod = table.modules && table.modules.selectRange;
    if (!mod || mod.__editrPatched || typeof mod.layoutElement !== "function") return;
    mod.__editrPatched = true;
    var original = mod.layoutElement;
    mod.layoutElement = function () {
      return original.call(this, true);
    };
  }

  HTMLWidgets.widget({
    name: "editr_grid",
    type: "output",

    factory: function (el, width, height) {
      var grid = {
        table: null,
        ready: false,
        suppress: 0,
        queue: [],
        timer: null,
        seq: 0,
        nextId: 1,
        nextCol: 1,
        colMap: {},
        changed: {},
        widths: {}, // columns the user resized
        pinned: {}, // new rows stay visible whatever the filters say
        opts: {},
        ui: {},
        find: {query: "", matches: [], index: -1},
        viewTimer: null,
        popup: null
      };
      el.__editr = grid;
      registerHandler();

      /* ---------------------------------------------------------------- */
      /* ops to R                                                         */
      /* ---------------------------------------------------------------- */

      function emit(op) {
        if (grid.suppress > 0) return;
        grid.queue.push(op);
        if (!grid.timer) grid.timer = setTimeout(flush, 0);
      }

      // merge runs of single-cell updates to the same column so that a paste
      // or a range clear arrives in R as one vectorised update
      function coalesce(ops) {
        var out = [];
        ops.forEach(function (op) {
          var last = out[out.length - 1];
          if (op.type === "update" && last && last.type === "update" &&
              last.cid === op.cid) {
            last.ids = last.ids.concat(op.ids);
            last.values = last.values.concat(op.values);
          } else if (op.type === "update") {
            out.push({type: "update", cid: op.cid, ids: op.ids.slice(),
                      values: op.values.slice()});
          } else {
            out.push(op);
          }
        });
        return out;
      }

      function flush() {
        grid.timer = null;
        var ops = coalesce(grid.queue);
        grid.queue = [];
        if (!ops.length || !shinyOpen()) return;
        grid.seq += 1;
        Shiny.setInputValue(el.id + "_ops", {seq: grid.seq, ops: ops},
                            {priority: "event"});
      }

      /* ---------------------------------------------------------------- */
      /* formatting                                                        */
      /* ---------------------------------------------------------------- */

      function builtinFormatter(name) {
        var fmt = grid.table && grid.table.modules.format;
        return fmt && fmt.constructor.formatters[name];
      }

      function makeFormatter(base, params) {
        return function (cell, formatterParams, onRendered) {
          var element = cell.getElement();
          var key = cell.getRow().getIndex() + "|" + cell.getField();
          element.classList.toggle("editr-changed", !!grid.changed[key]);
          var v = cell.getValue();
          if (base === "password") {
            return document.createTextNode(
              v === null || v === undefined ? "" :
                new Array(String(v).length + 1).join("•"));
          }
          if (base === "url") return linkNode(v);
          if (base === "progress" && (v === null || v === undefined || v === "")) {
            return document.createTextNode("");
          }
          if (base === "colour") return colourNode(v);
          if (base) {
            var f = builtinFormatter(base);
            if (f) {
              return f.call(grid.table.modules.format, cell, params || {},
                            onRendered);
            }
          }
          return document.createTextNode(
            v === null || v === undefined ? "" : String(v));
        };
      }

      // web addresses get a button that opens them in a new tab (the text
      // itself is not a link, so double-clicking still edits the cell)
      function linkNode(v) {
        if (v === null || v === undefined || v === "") return document.createTextNode("");
        var s = String(v).trim();
        var wrap = document.createElement("span");
        wrap.className = "editr-url";
        var text = document.createElement("span");
        text.className = "editr-url-text";
        text.textContent = s;
        wrap.appendChild(text);
        if (/^(https?:\/\/|mailto:)/i.test(s)) {
          var a = document.createElement("a");
          a.href = s;
          a.target = "_blank";
          a.rel = "noopener noreferrer";
          a.className = "editr-url-open";
          a.title = "Open " + s;
          a.setAttribute("aria-label", "Open " + s);
          a.innerHTML = ICONS.external;
          a.addEventListener("dblclick", function (e) { e.stopPropagation(); });
          wrap.appendChild(a);
        }
        return wrap;
      }

      // a swatch next to the colour (hex codes and CSS colour names)
      function colourNode(v) {
        var wrap = document.createElement("span");
        wrap.className = "editr-colour";
        if (v === null || v === undefined || v === "") return wrap;
        var s = String(v).trim();
        var ok = window.CSS && CSS.supports ? CSS.supports("color", s) : true;
        if (ok) {
          var sw = document.createElement("span");
          sw.className = "editr-swatch";
          sw.style.background = s;
          wrap.appendChild(sw);
        }
        wrap.appendChild(document.createTextNode(s));
        return wrap;
      }

      // date-time picker; values are "YYYY-MM-DD HH:MM:SS" like R prints them
      function datetimeEditor(cell, onRendered, success, cancel) {
        var old = cell.getValue();
        var input = document.createElement("input");
        input.type = "datetime-local";
        input.step = "1";
        input.className = "editr-input";
        input.value = old ? String(old).replace(" ", "T") : "";
        var done = false;
        var commit = function () {
          if (done) return;
          done = true;
          var v = input.value ? input.value.replace("T", " ") : null;
          if (v && v.length === 16) v += ":00";
          if ((v || null) === (old || null)) cancel(); else success(v);
        };
        input.addEventListener("blur", commit);
        input.addEventListener("keydown", function (e) {
          if (e.key === "Enter") {
            commit();
          } else if (e.key === "Escape") {
            done = true;
            cancel();
          } else if (/^Arrow/.test(e.key)) {
            e.stopPropagation(); // arrows change the date, not the cell
          }
        });
        // clicks inside the picker must not start a range selection
        input.addEventListener("mousedown", function (e) { e.stopPropagation(); });
        onRendered(function () { input.focus({preventScroll: true}); });
        return input;
      }

      // slider with the current value next to it; dragging the handle does
      // not select cells (Tabulator's range editor lost the drag to the range
      // selection)
      function sliderEditor(cell, onRendered, success, cancel, params) {
        var old = cell.getValue();
        var wrap = document.createElement("div");
        wrap.className = "editr-slider";
        var input = document.createElement("input");
        input.type = "range";
        input.min = params.min;
        input.max = params.max;
        input.step = params.step;
        input.value = old === null || old === undefined ? params.min : old;
        var label = document.createElement("span");
        label.className = "editr-slider-value";
        label.textContent = input.value;
        wrap.appendChild(input);
        wrap.appendChild(label);
        var done = false;
        var commit = function () {
          if (done) return;
          done = true;
          var v = Number(input.value);
          if (v === old) cancel(); else success(v);
        };
        input.addEventListener("input", function () { label.textContent = input.value; });
        input.addEventListener("blur", commit);
        input.addEventListener("change", function () { input.focus(); });
        input.addEventListener("keydown", function (e) {
          if (e.key === "Enter") {
            commit();
          } else if (e.key === "Escape") {
            done = true;
            cancel();
          } else if (/^Arrow|^Page|^Home$|^End$/.test(e.key)) {
            e.stopPropagation();
          }
        });
        ["mousedown", "mousemove", "mouseup", "pointerdown", "pointermove",
         "click", "dblclick"].forEach(function (type) {
          wrap.addEventListener(type, function (e) { e.stopPropagation(); });
        });

        onRendered(function () { input.focus({preventScroll: true}); });
        return wrap;
      }

      /* ---------------------------------------------------------------- */
      /* columns                                                           */
      /* ---------------------------------------------------------------- */

      function titleFormatter(cell) {
        var c = grid.colMap[cell.getColumn().getField()] || {};
        var name = c.frozen ? "row names" : c.title;
        var badge = c.option && c.option.type === "time" ? "time" :
          BADGES[c.kind] || "abc";
        return '<span class="editr-title"><span class="editr-badge">' +
          badge + '</span><span class="editr-name">' + escapeHTML(name) +
          '</span>' + (c.readonly ? '<span class="editr-lock">' + ICONS.lock +
          '</span>' : '') + '</span>';
      }

      /* header filters ------------------------------------------------- */

      function pinnedRow(rowData) {
        return !!grid.pinned[rowData[ID]];
      }

      function rangeEditor(cell, onRendered, success, cancel) {
        var value = cell.getValue() || {};
        var wrap = document.createElement("div");
        wrap.className = "editr-range";
        var timer = null;
        var make = function (key, label) {
          var input = document.createElement("input");
          input.type = "text";
          input.inputMode = "decimal";
          input.placeholder = key;
          input.setAttribute("aria-label", c(cell) + " " + label);
          input.value = value[key] === undefined ? "" : value[key];
          input.addEventListener("mousedown", function (e) { e.stopPropagation(); });
          input.addEventListener("keydown", function (e) { e.stopPropagation(); });
          input.addEventListener("input", function () {
            clearTimeout(timer);
            timer = setTimeout(function () {
              var lo = wrap.children[0].value.trim();
              var hi = wrap.children[1].value.trim();
              success(lo === "" && hi === "" ? "" : {min: lo, max: hi});
            }, 250);
          });
          wrap.appendChild(input);
        };
        var c = function (cl) {
          var col = grid.colMap[cl.getField()];
          return col ? col.title : "";
        };
        make("min", "minimum");
        make("max", "maximum");
        return wrap;
      }

      function rangeFilter(value, rowValue, rowData) {
        if (pinnedRow(rowData)) return true;
        if (rowValue === null || rowValue === undefined || rowValue === "") {
          return false;
        }
        var x = Number(rowValue);
        if (isNaN(x)) return false;
        var lo = value.min === "" ? null : Number(value.min);
        var hi = value.max === "" ? null : Number(value.max);
        if (lo !== null && !isNaN(lo) && x < lo) return false;
        if (hi !== null && !isNaN(hi) && x > hi) return false;
        return true;
      }

      function valuesFilter(value, rowValue, rowData) {
        if (pinnedRow(rowData)) return true;
        var selected = toArray(value).map(String);
        var v = rowValue === null || rowValue === undefined ? "NA" : String(rowValue);
        return selected.indexOf(v) !== -1;
      }

      function textFilter(value, rowValue, rowData) {
        if (pinnedRow(rowData)) return true;
        if (rowValue === null || rowValue === undefined) return false;
        return String(rowValue).toLowerCase()
          .indexOf(String(value).toLowerCase()) !== -1;
      }

      function addHeaderFilter(def, c) {
        var f = c.filter || {type: "text"};
        if (f.type === "range") {
          def.minWidth = 110;
          def.headerFilter = rangeEditor;
          def.headerFilterFunc = rangeFilter;
          def.headerFilterLiveFilter = false;
          def.headerFilterEmptyCheck = function (v) {
            return !v || (v.min === "" && v.max === "");
          };
        } else if (f.type === "values") {
          def.headerFilter = "list";
          def.headerFilterParams = {valuesLookup: "all", multiselect: true,
                                    clearable: true, sort: "asc"};
          def.headerFilterFunc = valuesFilter;
          def.headerFilterPlaceholder = "Any";
          def.headerFilterEmptyCheck = function (v) {
            return !v || !toArray(v).length;
          };
        } else {
          def.headerFilter = "input";
          def.headerFilterFunc = textFilter;
          def.headerFilterPlaceholder = "Contains\u2026";
        }
      }

      function buildColumn(c) {
        var def = {
          title: c.title,
          titleFormatter: titleFormatter,
          field: c.field,
          headerSort: false,
          minWidth: c.empty ? 110 : 60,
          frozen: !!c.frozen,
          editor: c.readonly ? false : "input",
          formatter: makeFormatter(null),
          headerContextMenu: headerMenu,
          cssClass: c.readonly ? "editr-readonly" : ""
        };
        if (grid.opts.filters !== false) addHeaderFilter(def, c);
        if (c.kind === "numeric") def.hozAlign = "right";
        var o = c.option;
        if (o && o.type === "checkbox") {
          def.editor = c.readonly ? false : "tickCross";
          def.editorParams = {tristate: false};
          def.formatter = makeFormatter("tickCross",
                                        {allowEmpty: true, crossElement: false});
          def.hozAlign = "center";
        } else if (o && o.type === "dropdown") {
          def.editor = c.readonly ? false : "list";
          def.editorParams = {values: toArray(o.values), autocomplete: true,
                              listOnEmpty: true, allowEmpty: true,
                              clearable: true};
        } else if (o && o.type === "date") {
          def.editor = c.readonly ? false : "date";
        } else if (o && o.type === "password") {
          def.editorParams = {elementAttributes: {type: "password"}};
          def.formatter = makeFormatter("password");
        } else if (o && o.type === "datetime") {
          def.editor = c.readonly ? false : datetimeEditor;
          def.minWidth = Math.max(def.minWidth, 170);
        } else if (o && o.type === "time") {
          def.editor = c.readonly ? false : "time";
        } else if (o && o.type === "textarea") {
          // Enter saves like any other cell; Shift+Enter or Alt+Enter adds a
          // line break
          def.editor = c.readonly ? false : "textarea";
          def.editorParams = {enterSubmit: true, shiftEnterNewLine: true,
                              altEnterNewLine: true, verticalNavigation: "editor"};
          def.formatter = makeFormatter("textarea");
          def.variableHeight = true;
          def.minWidth = Math.max(def.minWidth, 180);
          def.maxInitialWidth = 360;
        } else if (o && o.type === "url") {
          def.formatter = makeFormatter("url");
        } else if (o && o.type === "colour") {
          def.formatter = makeFormatter("colour");
        } else if (o && o.type === "number") {
          def.editor = c.readonly ? false : "number";
          def.editorParams = {selectContents: true};
          if (o.min !== undefined && o.min !== null) def.editorParams.min = o.min;
          if (o.max !== undefined && o.max !== null) def.editorParams.max = o.max;
          if (o.step !== undefined && o.step !== null) def.editorParams.step = o.step;
        } else if (o && o.type === "rating") {
          def.editor = c.readonly ? false : "star";
          def.formatter = makeFormatter("star", {stars: o.max});
          def.hozAlign = "left";
          def.minWidth = Math.max(def.minWidth, 20 * o.max + 24);
        } else if (o && o.type === "slider") {
          def.editor = c.readonly ? false : sliderEditor;
          def.editorParams = {min: o.min, max: o.max, step: o.step};
          def.formatter = makeFormatter("progress",
                                        {min: o.min, max: o.max, legend: true,
                                         color: "#93B4F5", legendColor: "#1F2933",
                                         legendAlign: "center"});
          def.minWidth = Math.max(def.minWidth, 120);
        }
        return def;
      }

      // a narrow last column whose header adds a column at the end
      function addColumnDef() {
        return {
          field: ADD_FIELD, title: "", width: 44, minWidth: 44,
          resizable: false, headerSort: false, editor: false, clipboard: false,
          cssClass: "editr-addcol",
          titleFormatter: function () {
            return '<button type="button" class="editr-addcol-btn" ' +
              'aria-label="Add column" title="Add column">' + ICONS.plus +
              '</button>';
          },
          headerClick: function () {
            var cols = realColumns();
            if (cols.length) insertColumn(cols[cols.length - 1], true);
          },
          formatter: function () { return ""; }
        };
      }

      function setColumns(cols) {
        grid.colMap = {};
        toArray(cols).forEach(function (c) { grid.colMap[c.field] = c; });
        var defs = toArray(cols).map(buildColumn);
        if (grid.opts.col_edit) defs.push(addColumnDef());
        return defs;
      }

      // column definitions for the same data sent again from R (undo/redo
      // of structural edits): columns the user hid stay hidden and columns
      // the user resized keep their width
      function sameLayout(cols) {
        var hidden = {};
        if (grid.table) {
          grid.table.getColumns().forEach(function (col) {
            if (!col.isVisible()) hidden[col.getField()] = true;
          });
        }
        var defs = setColumns(cols);
        defs.forEach(function (d) {
          if (hidden[d.field]) d.visible = false;
          if (grid.widths[d.field]) d.width = grid.widths[d.field];
        });
        return defs;
      }

      function realColumns() {
        return grid.table.getColumns().filter(function (col) {
          return !!grid.colMap[col.getField()];
        });
      }

      // Tabulator ranges remember column positions, so hiding or showing a
      // column made the selection jump to another column or outline empty
      // space. Run fn() and then put each range back on the same cells, or
      // on the nearest visible column when all of its columns were hidden.
      function keepRanges(fn) {
        var table = grid.table;
        var saved = [];
        if (table && typeof table.getRanges === "function") {
          table.getRanges().forEach(function (r) {
            // the anchor and end cells (cheap, unlike listing every cell of
            // a whole-column selection)
            var a = null, b = null;
            try {
              a = r._range.getStartCell();
              b = r._range.getEndCell();
            } catch (e) {}
            if (!a || !b || !a.column || !b.column) return;
            saved.push({range: r, a: a.getComponent(), b: b.getComponent()});
          });
        }
        fn();
        if (!saved.length) return;
        var cols = realColumns();
        var fields = cols.map(function (col) { return col.getField(); });
        var shown = cols.map(function (col) { return col.isVisible(); });
        saved.forEach(function (s) {
          var ia = fields.indexOf(s.a.getField());
          var ib = fields.indexOf(s.b.getField());
          if (ia < 0 || ib < 0) return;
          var from = Math.min(ia, ib), to = Math.max(ia, ib);
          var lo = -1, hi = -1, i;
          for (i = from; i <= to; i++) {
            if (shown[i]) { if (lo < 0) lo = i; hi = i; }
          }
          if (lo < 0) {
            for (i = 1; i < fields.length && lo < 0; i++) {
              if (shown[from + i]) lo = from + i;
              else if (shown[from - i]) lo = from - i;
            }
            hi = lo;
          }
          if (lo < 0) return;
          try {
            s.range.setBounds(s.a.getRow().getCell(fields[ia <= ib ? lo : hi]),
                              s.b.getRow().getCell(fields[ia <= ib ? hi : lo]));
          } catch (e) {}
        });
      }

      function currentTitles() {
        return Object.keys(grid.colMap).map(function (k) {
          return grid.colMap[k].title;
        });
      }

      /* ---------------------------------------------------------------- */
      /* menus                                                             */
      /* ---------------------------------------------------------------- */

      function historyItems() {
        return [
          {label: "Undo  (Ctrl+Z)", action: function () { emit({type: "undo"}); }},
          {label: "Redo  (Ctrl+Y)", action: function () { emit({type: "redo"}); }}
        ];
      }

      function rowsForAction(row) {
        var rows = [];
        var ranges = grid.table.getRanges ? grid.table.getRanges() : [];
        ranges.forEach(function (r) { rows = rows.concat(r.getRows()); });
        var ids = rows.map(function (r) { return r.getIndex(); });
        if (ids.indexOf(row.getIndex()) === -1) return [row];
        return rows;
      }

      function insertRow(anchorRow, below) {
        var id = grid.nextId++;
        var data = {};
        data[ID] = id;
        grid.pinned[id] = true;
        var anchor = anchorRow ? anchorRow.getIndex() : null;
        var p = anchorRow ? grid.table.addRow(data, !below, anchorRow)
                          : grid.table.addRow(data, false);
        emit({type: "insert_rows", ids: [id], anchor: anchor, below: below});
        return p;
      }

      // Excel-style fill down: copy the top row of the selected range into
      // the rows below it (one undo step)
      function fillDown() {
        var ranges = grid.table.getRanges ? grid.table.getRanges() : [];
        var ops = [];
        grid.suppress += 1;
        try {
          ranges.forEach(function (r) {
            var cells = r.getStructuredCells();
            if (cells.length < 2) return;
            var top = cells[0];
            for (var i = 1; i < cells.length; i++) {
              cells[i].forEach(function (cell, j) {
                var field = cell.getField();
                var c = grid.colMap[field];
                if (!c || c.readonly || !top[j]) return;
                var v = top[j].getValue();
                if (cell.getValue() === v) return;
                cell.setValue(v);
                ops.push({type: "update", cid: field,
                          ids: [cell.getRow().getIndex()],
                          values: [jsonValue(v)]});
              });
            }
          });
        } finally {
          grid.suppress -= 1;
        }
        ops.sort(function (a, b) { return a.cid < b.cid ? -1 : a.cid > b.cid ? 1 : 0; });
        ops.forEach(emit);
      }

      function rowMenu(e, row) {
        var items = [{label: "Fill down  (Ctrl+D)", action: fillDown},
                     {separator: true}];
        if (grid.opts.row_edit) {
          items.push({label: "Insert row above",
                      action: function (e, r) { insertRow(r, false); }});
          items.push({label: "Insert row below",
                      action: function (e, r) { insertRow(r, true); }});
          items.push({label: "Delete row(s)", action: function (e, r) {
            var rows = rowsForAction(r);
            var ids = rows.map(function (x) { return x.getIndex(); });
            if (grid.table.deselectRange) {
              grid.table.getRanges().forEach(function (rg) { rg.remove(); });
            }
            rows.forEach(function (x) { x.delete(); });
            emit({type: "delete_rows", ids: ids});
          }});
          items.push({separator: true});
        }
        return items.concat(historyItems());
      }

      function insertColumn(column, right) {
        var cid = "c" + grid.nextCol++;
        var name = uniqueName("V", currentTitles());
        var c = {field: cid, title: name, kind: "character", readonly: false,
                 rename: true, frozen: false, empty: true,
                 filter: {type: "text"}};
        grid.colMap[cid] = c;
        grid.table.addColumn(buildColumn(c), !right, column);
        updateStatus();
        emit({type: "insert_col", cid: cid, name: name,
              anchor: column.getField(), right: right});
      }

      function headerMenu(e, column) {
        var c = grid.colMap[column.getField()] || {};
        var items = [];
        if (c.rename) {
          items.push({label: "Rename column",
                      action: function (e, col) { startRename(col); }});
        }
        if (grid.opts.col_edit) {
          items.push({label: "Insert column left",
                      action: function (e, col) { insertColumn(col, false); }});
          items.push({label: "Insert column right",
                      action: function (e, col) { insertColumn(col, true); }});
          if (!c.frozen) {
            items.push({label: "Delete column", action: function (e, col) {
              var cid = col.getField();
              delete grid.colMap[cid];
              col.delete();
              emit({type: "delete_col", cid: cid});
              updateStatus();
              reportView();
            }});
          }
        }
        if (!c.frozen) {
          items.push({label: "Hide column", action: function (e, col) {
            keepRanges(function () { col.hide(); });
            updateStatus();
            reportView();
          }});
        }
        if (grid.opts.row_edit) {
          items.push({label: "Add row at end",
                      action: function () { addRowAtEnd(); }});
        }
        if (items.length) items.push({separator: true});
        return items.concat(historyItems());
      }

      /* ---------------------------------------------------------------- */
      /* renaming                                                          */
      /* ---------------------------------------------------------------- */

      function startRename(column) {
        var c = grid.colMap[column.getField()];
        if (!c || !c.rename) return;
        var header = column.getElement();
        var rect = header.getBoundingClientRect();
        var input = document.createElement("input");
        input.type = "text";
        input.value = c.title;
        input.className = "editr-rename";
        input.style.left = (rect.left + window.scrollX) + "px";
        input.style.top = (rect.top + window.scrollY) + "px";
        input.style.width = Math.max(rect.width, 80) + "px";
        input.style.height = rect.height + "px";
        document.body.appendChild(input);
        input.focus();
        input.select();
        var done = false;
        function finish(commit) {
          if (done) return;
          done = true;
          var value = input.value;
          if (input.parentNode) input.parentNode.removeChild(input);
          if (!commit || value === c.title) return;
          setTitle(c.field, value);
          emit({type: "rename_col", cid: c.field, name: value});
        }
        input.addEventListener("keydown", function (e) {
          if (e.key === "Enter") finish(true);
          if (e.key === "Escape") finish(false);
          e.stopPropagation();
        });
        input.addEventListener("blur", function () { finish(true); });
      }

      function setTitle(field, title) {
        var c = grid.colMap[field];
        if (c) c.title = title;
        var col = grid.table.getColumn(field);
        if (col) col.updateDefinition({title: title});
      }

      /* ---------------------------------------------------------------- */
      /* popups (columns menu, type guard)                                 */
      /* ---------------------------------------------------------------- */

      function closePopup() {
        if (grid.popup) {
          var p = grid.popup;
          grid.popup = null;
          if (p.onClose) p.onClose();
          if (p.el.parentNode) p.el.parentNode.removeChild(p.el);
          document.removeEventListener("mousedown", p.outside, true);
          document.removeEventListener("keydown", p.escape, true);
        }
      }

      function openPopup(anchor, el, onClose) {
        closePopup();
        var rect = anchor.getBoundingClientRect();
        el.style.position = "absolute";
        el.style.top = (rect.bottom + window.scrollY + 4) + "px";
        el.style.left = (rect.left + window.scrollX) + "px";
        document.body.appendChild(el);
        // keep inside the window
        var w = el.getBoundingClientRect().width;
        var maxLeft = window.scrollX + document.documentElement.clientWidth - w - 8;
        if (rect.left + window.scrollX > maxLeft) el.style.left = Math.max(8, maxLeft) + "px";
        var popup = {el: el, onClose: onClose};
        popup.outside = function (e) {
          if (!el.contains(e.target) && !anchor.contains(e.target)) closePopup();
        };
        popup.escape = function (e) {
          if (e.key === "Escape") {
            e.stopPropagation();
            closePopup();
          }
        };
        document.addEventListener("mousedown", popup.outside, true);
        document.addEventListener("keydown", popup.escape, true);
        grid.popup = popup;
        return popup;
      }

      function openColumnsMenu(anchor) {
        if (grid.popup && grid.popup.kind === "columns") {
          closePopup();
          return;
        }
        var menu = document.createElement("div");
        menu.className = "editr-popup editr-columns-menu";
        menu.setAttribute("role", "dialog");
        menu.setAttribute("aria-label", "Show or hide columns");
        var head = document.createElement("div");
        head.className = "editr-columns-head";
        var all = document.createElement("button");
        all.type = "button";
        all.textContent = "Show all";
        var none = document.createElement("button");
        none.type = "button";
        none.textContent = "Hide all";
        head.appendChild(all);
        head.appendChild(none);
        menu.appendChild(head);
        var list = document.createElement("div");
        list.className = "editr-columns-list";
        var boxes = [];
        realColumns().forEach(function (col) {
          var c = grid.colMap[col.getField()];
          if (c.frozen) return;
          var label = document.createElement("label");
          var box = document.createElement("input");
          box.type = "checkbox";
          box.checked = col.isVisible();
          box.addEventListener("change", function () {
            keepRanges(function () {
              if (box.checked) col.show(); else col.hide();
            });
            updateStatus();
            reportView();
          });
          boxes.push(box);
          label.appendChild(box);
          var name = document.createElement("span");
          name.textContent = c.title;
          label.appendChild(name);
          list.appendChild(label);
        });
        menu.appendChild(list);
        var setAll = function (show) {
          boxes.forEach(function (b) {
            if (b.checked !== show) {
              b.checked = show;
              b.dispatchEvent(new Event("change"));
            }
          });
        };
        all.addEventListener("click", function () { setAll(true); });
        none.addEventListener("click", function () { setAll(false); });
        var popup = openPopup(anchor, menu);
        popup.kind = "columns";
        anchor.setAttribute("aria-expanded", "true");
        popup.onClose = function () { anchor.setAttribute("aria-expanded", "false"); };
      }

      // ask before a typed value changes a column's type
      function guardEdit(cell, reason) {
        var field = cell.getField();
        var c = grid.colMap[field];
        var id = cell.getRow().getIndex();
        var raw = cell.getValue();
        var old = cell.getOldValue();
        var box = document.createElement("div");
        box.className = "editr-popup editr-guard";
        box.setAttribute("role", "alertdialog");
        box.setAttribute("aria-label", "Value does not match the column type");
        box.innerHTML =
          '<div class="editr-guard-msg"><span class="editr-guard-icon">' + ICONS.alert +
          '</span><div><div class="editr-guard-title">\u201c' + escapeHTML(raw) +
          '\u201d ' + reason + '</div><div class="editr-guard-sub">' +
          escapeHTML(c.title) + ' is ' + kindPhrase(c) +
          '. What should happen?</div></div></div>' +
          '<div class="editr-guard-actions">' +
          '<button type="button" class="btn btn-primary btn-sm" data-act="na">Store as missing (NA)</button>' +
          (canBecomeText(c) ?
            '<button type="button" class="btn btn-outline-secondary btn-sm" data-act="text">Convert ' +
            escapeHTML(c.title) + ' to text</button>' : '') +
          '<button type="button" class="btn btn-link btn-sm" data-act="cancel">Cancel edit</button></div>';
        var decided = false;
        var restore = function (v) {
          grid.suppress += 1;
          var row = grid.table.getRow(id);
          if (row) row.getCell(field).setValue(v);
          grid.suppress -= 1;
        };
        var decide = function (act) {
          decided = true;
          closePopup();
          if (act === "na") {
            restore(null);
            emit({type: "update", cid: field, ids: [id], values: [null]});
          } else if (act === "text") {
            c.kind = "character";
            c.option = null;
            c.filter = {type: "text"};
            emit({type: "update", cid: field, ids: [id], values: [raw]});
            var col = grid.table.getColumn(field);
            if (col) col.updateDefinition(buildColumn(c));
          } else {
            restore(old);
          }
        };
        box.addEventListener("click", function (e) {
          var act = e.target.closest && e.target.closest("[data-act]");
          if (act) decide(act.getAttribute("data-act"));
        });
        var popup = openPopup(cell.getElement(), box, function () {
          if (!decided) restore(old);
        });
        popup.kind = "guard";
        var first = box.querySelector("button");
        if (first) first.focus();
      }

      /* ---------------------------------------------------------------- */
      /* clipboard                                                         */
      /* ---------------------------------------------------------------- */

      // Paste is ours rather than Tabulator's: its parser splits on "\n" and
      // "\t" only, so the line break that spreadsheets put after the last row
      // pasted an empty row over the next row, CRLF left "\r" in the values
      // and quoted cells with line breaks were split up.
      function parsePaste(text) {
        var rows = parseClipboardText(text);
        return rows.length ? {rows: rows} : false;
      }

      // Excel-like: a single selected cell takes the whole block; a larger
      // selection that is a multiple of the block is filled by repeating it.
      // Rows past the end are added when rows can be added.
      function pasteRange(parsed) {
        var table = grid.table;
        var mod = table.modules.selectRange;
        var range = mod && mod.activeRange;
        var block = parsed && parsed.rows;
        if (!range || !block || !block.length) return [];
        var bounds = range.getBounds();
        if (!bounds.start || !bounds.end) return [];
        var rows = table.rowManager.activeRows.slice();
        var r0 = rows.indexOf(bounds.start.row);
        var r1 = rows.indexOf(bounds.end.row);
        var cols = table.columnManager.getVisibleColumnsByIndex();
        var c0 = cols.indexOf(bounds.start.column);
        var c1 = cols.indexOf(bounds.end.column);
        if (r0 < 0 || c0 < 0) return [];
        var dataRows = block.length;
        var dataCols = Math.max.apply(null, block.map(function (r) { return r.length; }));
        var selRows = Math.max(1, r1 - r0 + 1), selCols = Math.max(1, c1 - c0 + 1);
        var nRows = selRows > dataRows && selRows % dataRows === 0 ? selRows : dataRows;
        var nCols = selCols > dataCols && selCols % dataCols === 0 ? selCols : dataCols;

        // target columns (the "+" column and anything past it are ignored)
        var targets = [];
        for (var j = 0; j < nCols; j++) {
          var col = cols[c0 + j];
          var c = col && grid.colMap[col.getField()];
          targets.push(c ? c : null);
        }
        var existing = rows.slice(r0, r0 + nRows);
        var extra = grid.opts.row_edit ? nRows - existing.length : 0;

        // cell values by column, normalised to the format R expects
        var cells = [];
        targets.forEach(function (c, j) {
          if (!c || c.readonly) return;
          var vals = [];
          for (var i = 0; i < existing.length + extra; i++) {
            var src = block[i % dataRows];
            var v = src[j % dataCols];
            if (v === undefined) v = src.length < dataCols ? undefined : "";
            vals.push(v);
          }
          var dmy = c.kind === "date" || c.kind === "datetime" ? dayFirst(vals) : undefined;
          vals = vals.map(function (v) {
            return v === undefined ? v : normalizeValue(c, v, dmy);
          });
          cells.push({c: c, values: vals});
        });

        var problems = [];
        cells.forEach(function (col) {
          var bad = col.values.filter(function (v) {
            return v !== undefined && invalidReason(col.c, v);
          });
          if (bad.length) {
            problems.push({c: col.c, n: bad.length, example: bad[0],
                           reason: invalidReason(col.c, bad[0])});
          }
        });

        var apply = function (act) {
          applyPaste(existing, extra, cells, act);
        };
        if (problems.length) {
          pasteGuard(bounds.start.getComponent(), problems, apply);
        } else {
          apply("paste");
        }
        return [];
      }

      // write a checked paste: add rows, update cells, report one batch of ops
      // (one undo step) and select the pasted block
      function applyPaste(existing, extra, cells, act) {
        var table = grid.table;
        if (act === "cancel") return;
        if (act === "text") {
          cells.forEach(function (col) {
            var bad = col.values.some(function (v) {
              return v !== undefined && invalidReason(col.c, v);
            });
            if (!bad || !canBecomeText(col.c)) return;
            col.c.kind = "character";
            col.c.option = null;
            col.c.filter = {type: "text"};
            var column = table.getColumn(col.c.field);
            if (column) column.updateDefinition(buildColumn(col.c));
          });
        }
        cells.forEach(function (col) {
          col.values = col.values.map(function (v) {
            if (v === undefined) return v;
            if (isBlank(v)) return null;
            return invalidReason(col.c, v) ? null : v;
          });
        });
        var ops = [];
        var newIds = [];
        var newRows = [];
        for (var k = 0; k < extra; k++) {
          var id = grid.nextId++;
          var data = {};
          data[ID] = id;
          grid.pinned[id] = true;
          newIds.push(id);
          newRows.push(data);
        }
        var ids = existing.map(function (row) { return row.getData()[ID]; }).concat(newIds);
        cells.forEach(function (col) {
          var f = col.c.field, upIds = [], upVals = [];
          col.values.forEach(function (v, i) {
            if (v === undefined) return;
            if (i < existing.length) {
              var o = existing[i].getData()[f];
              if (o === v || ((o === null || o === undefined) && v === null)) return;
            } else {
              newRows[i - existing.length][f] = v;
              if (v === null) return;
            }
            upIds.push(ids[i]);
            upVals.push(v);
          });
          if (upIds.length) ops.push({type: "update", cid: f, ids: upIds, values: upVals});
        });
        table.blockRedraw();
        existing.forEach(function (row, i) {
          var upd = {}, any = false;
          cells.forEach(function (col) {
            var v = col.values[i];
            if (v === undefined) return;
            var o = row.getData()[col.c.field];
            if (o === v || ((o === null || o === undefined) && v === null)) return;
            upd[col.c.field] = v;
            any = true;
          });
          if (any) row.updateData(upd);
        });
        table.restoreRedraw();
        if (newIds.length) {
          emit({type: "insert_rows", ids: newIds, anchor: null, below: true});
        }
        ops.forEach(emit);
        var done = newRows.length ? table.addData(newRows, false) : Promise.resolve();
        Promise.resolve(done).then(function () {
          var fields = cells.map(function (col) { return col.c.field; });
          if (!fields.length || !ids.length || !table.addRange) return;
          var first = table.getRow(ids[0]), last = table.getRow(ids[ids.length - 1]);
          if (!first || !last) return;
          table.getRanges().forEach(function (r) { r.remove(); });
          table.addRange(first.getCell(fields[0]), last.getCell(fields[fields.length - 1]));
          updateStatus();
          selectionSummary();
        });
      }

      // one question for all the pasted values that do not fit their columns
      function pasteGuard(anchorCell, problems, decide) {
        var total = problems.reduce(function (a, p) { return a + p.n; }, 0);
        var text = problems.every(function (p) { return canBecomeText(p.c); });
        var box = document.createElement("div");
        box.className = "editr-popup editr-guard";
        box.setAttribute("role", "alertdialog");
        box.setAttribute("aria-label", "Pasted values do not match the column types");
        box.innerHTML =
          '<div class="editr-guard-msg"><span class="editr-guard-icon">' + ICONS.alert +
          '</span><div><div class="editr-guard-title">' + total + ' pasted value' +
          (total === 1 ? " does" : "s do") + ' not fit ' +
          (problems.length === 1 ? "the column" : "their columns") + '</div>' +
          problems.map(function (p) {
            return '<div class="editr-guard-sub">' + escapeHTML(p.c.title) + ' is ' +
              kindPhrase(p.c) + ': ' + p.n + ' value' + (p.n === 1 ? "" : "s") +
              ', e.g. “' + escapeHTML(p.example) + '” ' + p.reason + '</div>';
          }).join("") + '</div></div>' +
          '<div class="editr-guard-actions">' +
          '<button type="button" class="btn btn-primary btn-sm" data-act="na">Store as missing (NA)</button>' +
          (text ? '<button type="button" class="btn btn-outline-secondary btn-sm" data-act="text">Convert ' +
            (problems.length === 1 ? escapeHTML(problems[0].c.title) : "those columns") +
            ' to text</button>' : '') +
          '<button type="button" class="btn btn-link btn-sm" data-act="cancel">Cancel paste</button></div>';
        var decided = false;
        box.addEventListener("click", function (e) {
          var act = e.target.closest && e.target.closest("[data-act]");
          if (!act) return;
          decided = true;
          closePopup();
          decide(act.getAttribute("data-act"));
        });
        var popup = openPopup(anchorCell.getElement(), box, function () {
          if (!decided) decide("cancel");
        });
        popup.kind = "guard";
        var first = box.querySelector("button");
        if (first) first.focus();
      }

      // Copy: Tabulator's plain text does not quote cells with tabs or line
      // breaks; rebuild it (and the HTML version) from the selected range
      function copyFormatter(type, output) {
        var table = grid.table;
        var ranges = table && table.getRanges ? table.getRanges() : [];
        if (!ranges.length) return output;
        var rows = ranges[0].getStructuredCells().map(function (cells) {
          return cells.filter(function (cell) {
            return !!grid.colMap[cell.getField()];
          }).map(function (cell) { return cell.getValue(); });
        });
        if (type === "html") {
          return "<table>" + rows.map(function (r) {
            return "<tr>" + r.map(function (v) {
              var s = v === null || v === undefined ? "" :
                v === true ? "TRUE" : v === false ? "FALSE" : String(v);
              return "<td>" + escapeHTML(s).replace(/\r?\n/g, "<br>") + "</td>";
            }).join("") + "</tr>";
          }).join("") + "</table>";
        }
        return rows.map(function (r) { return r.map(clipboardCell).join("\t"); }).join("\n");
      }

      /* ---------------------------------------------------------------- */
      /* status bar, filter chips, find                                    */
      /* ---------------------------------------------------------------- */

      function setText(id, text) {
        var node = byId(id);
        if (node) node.textContent = text;
      }

      function activeFilters() {
        if (!grid.table || !grid.table.getHeaderFilters) return [];
        return grid.table.getHeaderFilters().filter(function (f) {
          var v = f.value;
          if (v === "" || v === null || v === undefined) return false;
          if (Array.isArray(v)) return v.length > 0;
          if (typeof v === "object") return v.min !== "" || v.max !== "";
          return true;
        });
      }

      function updateStatus() {
        var table = grid.table;
        if (!table || !grid.ready) return;
        var total = table.getDataCount();
        var shown = table.getDataCount("active");
        var cols = realColumns().filter(function (col) {
          return !grid.colMap[col.getField()].frozen;
        });
        var visible = cols.filter(function (col) { return col.isVisible(); });
        var plural = function (n, w) { return n + " " + w + (n === 1 ? "" : "s"); };
        var rows = shown === total ? plural(total, "row") :
          shown + " of " + plural(total, "row");
        var columns = visible.length === cols.length ? plural(cols.length, "column") :
          visible.length + " of " + plural(cols.length, "column");
        setText(grid.ui.rows, rows + " \u00b7 " + columns);
        setText(grid.ui.columns_count,
                visible.length === cols.length ? "" :
                  visible.length + " of " + cols.length);
      }

      function chipText(f) {
        var c = grid.colMap[f.field] || {title: f.field};
        var v = f.value;
        if (Array.isArray(v)) {
          var vals = v.map(String);
          if (vals.length === 1) return c.title + " is " + vals[0];
          return c.title + " is " + vals.slice(0, -1).join(", ") + " or " +
            vals[vals.length - 1];
        }
        if (typeof v === "object") {
          if (v.min !== "" && v.max !== "") return c.title + " " + v.min + "\u2013" + v.max;
          if (v.min !== "") return c.title + " \u2265 " + v.min;
          return c.title + " \u2264 " + v.max;
        }
        return c.title + " contains \u201c" + v + "\u201d";
      }

      function renderChips() {
        var box = byId(grid.ui.chips);
        if (!box) return;
        box.innerHTML = "";
        var filters = activeFilters();
        filters.forEach(function (f) {
          var chip = document.createElement("span");
          chip.className = "editr-chip";
          var text = document.createElement("span");
          text.textContent = chipText(f);
          chip.appendChild(text);
          var x = document.createElement("button");
          x.type = "button";
          x.setAttribute("aria-label", "Remove filter " + chipText(f));
          x.innerHTML = ICONS.x;
          x.addEventListener("click", function () {
            grid.table.setHeaderFilterValue(f.field, "");
          });
          chip.appendChild(x);
          box.appendChild(chip);
        });
        if (filters.length > 1) {
          var clear = document.createElement("button");
          clear.type = "button";
          clear.className = "editr-chip-clear";
          clear.textContent = "Clear filters";
          clear.addEventListener("click", function () {
            grid.table.clearHeaderFilter();
          });
          box.appendChild(clear);
        }
      }

      // tell R which rows/columns are on screen (for "return what's on
      // screen" and exports); null means everything
      function reportView() {
        if (!window.Shiny || !Shiny.setInputValue) return;
        clearTimeout(grid.viewTimer);
        grid.viewTimer = setTimeout(function () {
          if (!grid.table || !shinyOpen()) return;
          var rows = null, cols = null;
          if (activeFilters().length) {
            rows = grid.table.getRows("active").map(function (r) { return r.getIndex(); });
          }
          var all = realColumns();
          var vis = all.filter(function (col) { return col.isVisible(); });
          if (vis.length !== all.length) {
            cols = vis.map(function (col) { return col.getField(); });
          }
          Shiny.setInputValue(el.id + "_view", {rows: rows, cols: cols},
                              {priority: "event"});
        }, 200);
      }

      function selectionSummary() {
        var table = grid.table;
        var ranges = table.getRanges ? table.getRanges() : [];
        var cells = [];
        ranges.forEach(function (r) {
          r.getStructuredCells().forEach(function (row) {
            row.forEach(function (cell) {
              if (grid.colMap[cell.getField()]) cells.push(cell);
            });
          });
        });
        if (cells.length < 2) {
          setText(grid.ui.selection, "");
          return;
        }
        var nums = [];
        var fields = {};
        cells.forEach(function (cell) {
          fields[cell.getField()] = true;
          var v = cell.getValue();
          if (v === null || v === undefined || v === "" || typeof v === "boolean") return;
          var x = Number(v);
          if (!isNaN(x)) nums.push(x);
        });
        var keys = Object.keys(fields);
        var label = keys.length === 1 ? (grid.colMap[keys[0]].title + " \u00b7 ") :
          (cells.length + " cells \u00b7 ");
        if (!nums.length) {
          setText(grid.ui.selection, "Selection " + cells.length + " cells");
          return;
        }
        var sum = nums.reduce(function (a, b) { return a + b; }, 0);
        setText(grid.ui.selection, "Selection " + label + "Sum " + formatNumber(sum) +
                " \u00b7 Mean " + formatNumber(sum / nums.length) +
                " \u00b7 Count " + nums.length);
      }

      function selectCell(id, field) {
        var table = grid.table;
        var row = table.getRow(id);
        if (!row) return;
        return table.scrollToRow(id, "center", false).then(function () {
          return table.scrollToColumn(field, "middle", false).catch(function () {});
        }).then(function () {
          var cell = row.getCell(field);
          if (!cell) return;
          if (table.getRanges) {
            table.getRanges().forEach(function (r) { r.remove(); });
          }
          if (table.addRange) table.addRange(cell, cell);
          var cellEl = cell.getElement();
          cellEl.classList.add("editr-flash");
          setTimeout(function () { cellEl.classList.remove("editr-flash"); }, 900);
        }).catch(function () {});
      }

      function runFind(query) {
        var f = grid.find;
        f.query = query;
        f.matches = [];
        f.index = -1;
        if (query) {
          var q = query.toLowerCase();
          var fields = realColumns().filter(function (col) {
            return col.isVisible();
          }).map(function (col) { return col.getField(); });
          grid.table.getRows("active").forEach(function (row) {
            var d = row.getData();
            fields.forEach(function (field) {
              var v = d[field];
              if (v !== null && v !== undefined &&
                  String(v).toLowerCase().indexOf(q) !== -1) {
                f.matches.push({id: d[ID], field: field});
              }
            });
          });
        }
        stepFind(1);
      }

      function stepFind(dir) {
        var f = grid.find;
        if (!f.query) {
          setText(grid.ui.find_count, "");
          return;
        }
        if (!f.matches.length) {
          setText(grid.ui.find_count, "No matches");
          return;
        }
        f.index = (f.index + dir + f.matches.length) % f.matches.length;
        setText(grid.ui.find_count, (f.index + 1) + " of " + f.matches.length);
        var m = f.matches[f.index];
        selectCell(m.id, m.field);
      }

      function bindUI() {
        var find = byId(grid.ui.find);
        if (find && !find.__editrBound) {
          find.__editrBound = true;
          var timer = null;
          find.addEventListener("input", function () {
            clearTimeout(timer);
            timer = setTimeout(function () { runFind(find.value.trim()); }, 150);
          });
          find.addEventListener("keydown", function (e) {
            if (e.key === "Enter") {
              e.preventDefault();
              stepFind(e.shiftKey ? -1 : 1);
            } else if (e.key === "Escape") {
              find.value = "";
              runFind("");
              find.blur();
            }
          });
        }
        var cols = byId(grid.ui.columns_btn);
        if (cols && !cols.__editrBound) {
          cols.__editrBound = true;
          cols.addEventListener("click", function () {
            el.__editr.openColumns(cols);
          });
        }
      }

      function addRowAtEnd() {
        var p = insertRow(null, true);
        var id = grid.nextId - 1;
        Promise.resolve(p).then(function () {
          var first = realColumns().filter(function (col) {
            var c = grid.colMap[col.getField()];
            return col.isVisible() && !c.readonly;
          })[0];
          if (first) {
            selectCell(id, first.getField());
          }
        });
      }

      grid.openColumns = function (anchor) { openColumnsMenu(anchor); };

      /* ---------------------------------------------------------------- */
      /* commands from R                                                   */
      /* ---------------------------------------------------------------- */

      function rowsFromColumns(cols) {
        if (!cols || !cols[ID]) return [];
        return HTMLWidgets.dataframeToD3(cols);
      }

      function reformatRows(ids) {
        ids.forEach(function (id) {
          var row = grid.table.getRow(id);
          if (row) row.reformat();
        });
      }

      grid.handle = function (method, args) {
        var table = grid.table;
        if (!table) return;
        if (args.next_id) grid.nextId = Math.max(grid.nextId, args.next_id);
        if (args.next_col) grid.nextCol = Math.max(grid.nextCol, args.next_col);
        grid.suppress += 1;
        try {
          if (method === "update_cells") {
            var ids = toArray(args.ids), fields = toArray(args.fields);
            var values = toArray(args.values);
            var byRow = {};
            ids.forEach(function (id, i) {
              if (!table.getRow(id)) return;
              byRow[id] = byRow[id] || {};
              byRow[id][ID] = id;
              byRow[id][fields[i]] = jsonValue(values[i]);
            });
            var upd = Object.keys(byRow).map(function (k) { return byRow[k]; });
            if (upd.length) table.updateData(upd);
          } else if (method === "replace_data") {
            if (args.columns) table.setColumns(sameLayout(args.columns));
            table.replaceData(rowsFromColumns(args.data)).then(function () {
              updateStatus();
              renderChips();
              reportView();
            });
          } else if (method === "scroll_to") {
            selectCell(args.id, args.field || (realColumns()[0] || {getField: function () { return null; }}).getField());
          } else if (method === "set_title") {
            setTitle(args.field, args.title);
          } else if (method === "set_changed") {
            var next = {};
            toArray(args.keys).forEach(function (k) { next[k] = true; });
            var touched = {};
            Object.keys(grid.changed).concat(Object.keys(next)).forEach(function (k) {
              if (!!grid.changed[k] !== !!next[k]) touched[k.split("|")[0]] = true;
            });
            grid.changed = next;
            reformatRows(Object.keys(touched).map(Number));
          }
        } finally {
          grid.suppress -= 1;
        }
      };

      /* ---------------------------------------------------------------- */
      /* build                                                             */
      /* ---------------------------------------------------------------- */

      function build(x) {
        var opts = x.options || {};
        grid.opts = opts;
        grid.ui = opts.ui || {};
        grid.pinned = {};
        grid.widths = {};
        grid.nextId = opts.next_id || 1;
        grid.nextCol = opts.next_col || 1;
        grid.changed = {};
        toArray(opts.changed).forEach(function (k) { grid.changed[k] = true; });
        if (opts.track_color) {
          el.style.setProperty("--editr-track-color", opts.track_color);
        }

        // Tabulator sizes its own element; give it a child so the widget
        // element keeps its fixed height (needed for the virtual DOM)
        var holder = document.createElement("div");
        holder.style.height = "100%";
        el.appendChild(holder);

        var table = new Tabulator(holder, {
          data: rowsFromColumns(x.data),
          columns: setColumns(x.columns),
          index: ID,
          height: "100%",
          layout: opts.stretch ? "fitColumns" : "fitData",
          placeholder: "No data",
          reactiveData: false,
          columnDefaults: {headerSort: false},
          rowHeader: {
            resizable: false, frozen: true, headerSort: false,
            hozAlign: "center", headerHozAlign: "center", width: 50,
            cssClass: "editr-rowheader",
            formatter: function (cell) { return cell.getRow().getIndex(); }
          },
          rowContextMenu: rowMenu,
          selectableRange: 1,
          selectableRangeColumns: true,
          selectableRangeRows: true,
          selectableRangeClearCells: true,
          selectableRangeClearCellsValue: null,
          editTriggerEvent: "dblclick",
          clipboard: true,
          clipboardCopyStyled: false,
          clipboardCopyConfig: {rowHeaders: false, columnHeaders: false},
          clipboardCopyRowRange: "range",
          clipboardCopyFormatter: copyFormatter,
          clipboardPasteParser: parsePaste,
          clipboardPasteAction: pasteRange,
          headerFilterLiveFilterDelay: 250,
          footerElement: opts.row_edit ?
            '<button type="button" class="editr-addrow">' + ICONS.plus +
              '<span>Add row</span></button>' : undefined
        });
        grid.table = table;
        patchKeyCodes(table);
        patchRowLookup(table);
        patchRangeLayout(table);

        table.on("tableBuilt", function () {
          grid.ready = true;
          var queued = pending[el.id] || [];
          delete pending[el.id];
          queued.forEach(function (m) { grid.handle(m.method, m.args || {}); });
          patchRowDisplayed(table);
          var add = holder.querySelector(".editr-addrow");
          if (add) add.addEventListener("click", addRowAtEnd);
          bindUI();
          updateStatus();
          renderChips();
          reportView();
        });

        // rows added while a filter is active stay visible until the user
        // changes the filters; then every row is judged afresh
        table.on("dataFiltering", function () {
          var key = JSON.stringify(activeFilters().map(function (f) {
            return [f.field, f.value];
          }));
          if (key !== grid.filterKey) {
            grid.filterKey = key;
            grid.pinned = {};
          }
        });

        table.on("dataFiltered", function () {
          if (!grid.ready) return;
          setTimeout(function () {
            updateStatus();
            renderChips();
            reportView();
          }, 0);
        });
        table.on("rowAdded", function () {
          patchRowDisplayed(table);
          updateStatus();
          reportView();
        });
        table.on("rowDeleted", function () { updateStatus(); reportView(); });
        table.on("rangeChanged", selectionSummary);
        table.on("rangeAdded", selectionSummary);
        table.on("rangeRemoved", selectionSummary);

        table.on("cellEdited", function (cell) {
          if (grid.suppress > 0) return;
          var field = cell.getField();
          var c = grid.colMap[field];
          if (c && c.readonly) {
            // range clears bypass the editor check - put the value back
            grid.suppress += 1;
            cell.setValue(cell.getOldValue());
            grid.suppress -= 1;
            return;
          }
          // "15/03/2024" -> "2024-03-15", "1,234" -> "1234", "yes" -> "TRUE"
          var value = cell.getValue();
          var normal = normalizeValue(c, value);
          if (normal !== value) {
            grid.suppress += 1;
            cell.setValue(normal);
            grid.suppress -= 1;
            value = normal;
          }
          var reason = invalidReason(c, value);
          if (reason) {
            guardEdit(cell, reason);
            return;
          }
          emit({type: "update", cid: field, ids: [cell.getRow().getIndex()],
                values: [jsonValue(value)]});
          setTimeout(selectionSummary, 0);
        });

        table.on("headerDblClick", function (e, column) {
          startRename(column);
        });

        table.on("columnResized", function (column) {
          grid.widths[column.getField()] = column.getWidth();
        });
      }

      // "?" opens the shortcuts sheet, Ctrl+F jumps to "Find in data"
      document.addEventListener("keydown", function (e) {
        if (!document.body.contains(el)) return;
        var t = e.target;
        var typing = t && (t.tagName === "INPUT" || t.tagName === "TEXTAREA" ||
                           t.tagName === "SELECT" || t.isContentEditable);
        if (e.key === "?" && !typing && !e.ctrlKey && !e.metaKey) {
          var m = byId(grid.ui.shortcuts);
          if (m && window.bootstrap && bootstrap.Modal) {
            bootstrap.Modal.getOrCreateInstance(m).show();
            e.preventDefault();
          }
        } else if ((e.ctrlKey || e.metaKey) && e.key.toLowerCase() === "f" &&
                   (!typing || el.contains(t))) {
          var find = byId(grid.ui.find);
          if (find) {
            find.focus();
            find.select();
            e.preventDefault();
          }
        }
      });

      // the double-click that opens an editor selects the word under the
      // pointer; pressing inside the editor would then drag that selection
      // instead of moving a slider or selecting text
      el.addEventListener("dragstart", function (e) {
        if (e.target && e.target.closest && e.target.closest(".tabulator-editing")) {
          e.preventDefault();
        }
      }, true);

      el.addEventListener("keydown", function (e) {
        if (!(e.ctrlKey || e.metaKey)) return;
        var key = e.key.toLowerCase();
        var editing = grid.table && grid.table.modules.edit &&
          grid.table.modules.edit.currentCell;
        var t = e.target;
        // shortcuts in filter boxes act on the text, not on the data
        var typing = t && (t.tagName === "INPUT" || t.tagName === "TEXTAREA" ||
                           t.tagName === "SELECT" || t.isContentEditable);
        if (editing || typing) return;
        if (key === "c") {
          // Tabulator has copied the selection; stop the browser's own copy
          // (of an empty selection) from running after it
          e.preventDefault();
        } else if (key === "d") {
          fillDown();
          e.preventDefault();
        } else if (key === "z" && !e.shiftKey) {
          emit({type: "undo"});
          e.preventDefault();
        } else if (key === "y" || (key === "z" && e.shiftKey)) {
          emit({type: "redo"});
          e.preventDefault();
        }
      });

      return {
        renderValue: function (x) {
          closePopup();
          if (grid.table) {
            grid.ready = false;
            grid.table.destroy();
            grid.table = null;
            el.innerHTML = "";
          }
          build(x);
        },
        resize: function (width, height) {
          if (grid.table) grid.table.redraw();
        },
        getTable: function () { return grid.table; }
      };
    }
  });
})();

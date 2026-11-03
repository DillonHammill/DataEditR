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
  /* labelled data (see R/labelled.R) ----------------------------------- */

  // value labels of a column: {list: [{value, label}], map: value -> label}
  function labelsOf(c) {
    var l = c && c.labels;
    if (!l || !l.values || (c.option && c.option.type)) return null;
    if (!l.map) {
      var names = toArray(l.names);
      l.list = toArray(l.values).map(function (v, i) {
        return {value: v, label: String(names[i])};
      });
      l.map = {};
      l.list.forEach(function (x) { l.map[String(x.value)] = x.label; });
    }
    return l;
  }

  function valueLabel(c, v) {
    var l = labelsOf(c);
    if (!l || v === null || v === undefined || v === "") return null;
    var hit = l.map[String(v)];
    return hit === undefined ? null : hit;
  }

  // dropdowns given as a named list (as for shiny::selectInput()) show the
  // names and store the values
  function choiceLabel(c, v) {
    var o = c && c.option;
    if (!o || o.type !== "dropdown" || !o.labels || v === null || v === undefined) return null;
    var i = toArray(o.values).map(String).indexOf(String(v));
    return i === -1 ? null : String(toArray(o.labels)[i]);
  }

  function choiceValue(c, s) {
    var o = c && c.option;
    if (!o || o.type !== "dropdown" || !o.labels || typeof s !== "string") return undefined;
    var t = s.trim();
    var values = toArray(o.values).map(String);
    if (values.indexOf(t) !== -1) return t;
    var low = t.toLowerCase();
    var hits = toArray(o.labels).map(String).reduce(function (acc, l, i) {
      if (l.toLowerCase() === low) acc.push(values[i]);
      return acc;
    }, []);
    return hits.length === 1 ? hits[0] : undefined;
  }

  // a value with its label, as the cell editor offers it: "1 \u00b7 Male"
  function labelledText(c, v) {
    var lab = valueLabel(c, v);
    return lab === null ? String(v) : v + " \u00b7 " + lab;
  }

  // a value as the cells show it: with its value label, the name of a
  // dropdown choice, or the text of HTML without its tags
  function shownText(c, v) {
    var s = typeof v === "boolean" ? (v ? "TRUE" : "FALSE") : String(v);
    var choice = choiceLabel(c, v);
    if (choice !== null) return choice;
    if (c && c.option && c.option.type === "html") {
      return s.replace(/<[^>]*>/g, " ").replace(/&nbsp;/g, " ").replace(/\s+/g, " ").trim();
    }
    return labelledText(c, s);
  }

  // SPSS user-defined missing values
  function userMissing(c, v) {
    var l = c && c.labels;
    if (!l || v === null || v === undefined || v === "") return false;
    var s = String(v);
    if (toArray(l.na).some(function (x) { return String(x) === s; })) return true;
    var r = toArray(l.range);
    var x = Number(v);
    return r.length === 2 && !isNaN(x) && x >= r[0] && x <= r[1];
  }

  // the value of a label typed or pasted into a labelled column ("Male" or
  // "1 \u00b7 Male"), or undefined
  function labelValue(c, s) {
    var l = labelsOf(c);
    if (!l || typeof s !== "string") return undefined;
    var t = s.trim();
    var m = /^(.*?)\s*\u00b7\s*(.*)$/.exec(t);
    if (m && l.map[m[1]] !== undefined && l.map[m[1]] === m[2]) {
      return l.list.filter(function (x) { return String(x.value) === m[1]; })[0].value;
    }
    // a value is itself (text codes such as "AU")
    if (l.map[t] !== undefined) {
      return l.list.filter(function (x) { return String(x.value) === t; })[0].value;
    }
    var low = t.toLowerCase();
    var hit = l.list.filter(function (x) { return x.label.toLowerCase() === low; });
    return hit.length === 1 ? hit[0].value : undefined;
  }

  function normalizeValue(c, v, dmy) {
    if (isBlank(v) || !c) return v;
    var o = c.option || {};
    if (typeof v === "string" && labelsOf(c)) {
      var lv = labelValue(c, v);
      if (lv !== undefined) return lv;
    }
    if (typeof v === "string" && o.type === "dropdown" && o.labels) {
      var cv = choiceValue(c, v);
      if (cv !== undefined) return cv;
    }
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
      if (typeof s === "string" && isNaN(x)) {
        return labelsOf(c) ? "is not a number or one of the value labels" : "is not a number";
      }
      if (o.min !== undefined && o.min !== null && x < o.min) {
        return "is less than " + o.min;
      }
      if (o.max !== undefined && o.max !== null && x > o.max) {
        return "is more than " + o.max;
      }
      if (offStep(o, x)) return "is not " + stepPhrase(o);
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

  // number inputs with a step take values on it (counted from the minimum)
  function hasStep(o) {
    return o.type === "number" && typeof o.step === "number" && o.step > 0;
  }
  function offStep(o, x) {
    if (!hasStep(o) || !isFinite(x)) return false;
    var k = (x - (typeof o.min === "number" ? o.min : 0)) / o.step;
    return Math.abs(k - Math.round(k)) > 1e-9 * Math.max(1, Math.abs(k));
  }
  function stepPhrase(o) {
    var from = typeof o.min === "number" ? o.min : 0;
    if (o.step === 1 && from % 1 === 0) return "a whole number";
    return "in steps of " + o.step + (from !== 0 ? " from " + from : "");
  }

  function kindPhrase(c) {
    var o = c.option || {};
    var has = function (x) { return x !== undefined && x !== null; };
    if (o.type === "dropdown") return "a dropdown column";
    if (o.type === "rating") return "a rating column (0 to " + o.max + " stars)";
    if (o.type === "time") return "a time column";
    if (labelsOf(c)) {
      return c.kind === "numeric" ? "a numeric column with value labels" :
        "a text column with value labels";
    }
    if (c.kind === "numeric") {
      var parts = [];
      if (hasStep(o)) {
        parts.push(stepPhrase(o) === "a whole number" ? "whole numbers" : stepPhrase(o));
      }
      if (has(o.min) && has(o.max)) parts.push(o.min + " to " + o.max);
      else if (has(o.min)) parts.push("at least " + o.min);
      else if (has(o.max)) parts.push("at most " + o.max);
      return "a numeric column" + (parts.length ? " (" + parts.join(", ") + ")" : "");
    }
    if (c.kind === "date") return "a date column";
    if (c.kind === "datetime") return "a date-time column";
    if (c.kind === "logical") return "a TRUE/FALSE column";
    return "a text column";
  }

  // can a column whose values do not fit be turned into a text column?
  function canBecomeText(c) {
    var o = c.option || {};
    if (labelsOf(c)) return false; // the codes would lose their labels
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
    alert: '<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M12 3 2 21h20z"/><path d="M12 10v5M12 18h.01"/></svg>',
    more: '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" role="img" aria-label="Column menu"><circle cx="12" cy="5" r="1"/><circle cx="12" cy="12" r="1"/><circle cx="12" cy="19" r="1"/></svg>'
  };

  var BADGES = {numeric: "123", character: "abc", logical: "T/F",
                date: "date", datetime: "dttm"};

  // filters listed in the toolbar before the rest are left to the Filters panel
  var MAX_CHIPS = 4;

  // logics of the Filters panel (the order offered is FP_ORDER). input is
  // what a filter asks for: tags (any of several values), one or two values, or
  // none; order: only for values that have an order (not plain text);
  // suggest: the values of the column are listed to pick from
  var FILTER_LOGICS = {
    equal: {label: "is any of", input: "tags", suggest: true},
    not_equal: {label: "is none of", input: "tags", suggest: true},
    gt: {label: "is above", input: "one", order: true},
    lt: {label: "is below", input: "one", order: true},
    gte: {label: "is at least", input: "one", order: true},
    lte: {label: "is at most", input: "one", order: true},
    between: {label: "is between", input: "two", order: true},
    not_between: {label: "is not between", input: "two", order: true},
    contain: {label: "contains", input: "tags"},
    not_contain: {label: "does not contain", input: "tags"},
    regex: {label: "matches the pattern", input: "one", raw: true},
    not_regex: {label: "does not match the pattern", input: "one", raw: true},
    missing: {label: "is missing", input: "none"},
    present: {label: "is not missing", input: "none"}
  };

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

  // scrollToRow() with "center" or "bottom" checks whether the table is
  // scrolled to the end with an exact comparison, which fails when the scroll
  // position is fractional (browser zoom or screen scaling), and a row near
  // the end (e.g. a new row) can be left below the visible area. Bring the
  // row into view afterwards.
  function patchScrollToRow(table) {
    var r = table.rowManager && table.rowManager.renderer;
    if (!r || r.__editrPatched || typeof r.scrollToRowPosition !== "function") return;
    r.__editrPatched = true;
    var orig = r.scrollToRowPosition;
    r.scrollToRowPosition = function (row, position, ifVisible) {
      var ev = this.elementVertical;
      return orig.call(this, row, position, ifVisible).then(function () {
        var el = row.getElement();
        if (!ev || !el || !el.isConnected) return;
        var box = ev.getBoundingClientRect();
        var b = el.getBoundingClientRect();
        var top = box.top + ev.clientTop;
        var bottom = top + ev.clientHeight;
        if (b.bottom > bottom + 1) ev.scrollTop += b.bottom - bottom;
        else if (b.top < top - 1) ev.scrollTop -= top - b.top;
      });
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
        comments: {}, // "id|field" -> {text, user, time}
        rColours: {}, // R colour name (lower case, no spaces) -> hex
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

      // label: what made the ops ("paste", "fill"), for the change log
      function emit(op, label) {
        if (grid.suppress > 0) return;
        grid.queue.push(op);
        if (typeof label === "string") grid.queueLabel = label;
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
        var label = grid.queueLabel || null;
        grid.queue = [];
        grid.queueLabel = null;
        if (!ops.length || !shinyOpen()) return;
        grid.seq += 1;
        Shiny.setInputValue(el.id + "_ops", {seq: grid.seq, ops: ops, label: label},
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
          // values already in the data that do not fit the column's input
          // (outside a slider's limits, not one of the options, ...)
          var bad = invalidReason(grid.colMap[cell.getField()], v);
          element.classList.toggle("editr-invalid", !!bad);
          // cells that break a rule (see R/rules.R)
          var problem = grid.problems[key];
          element.classList.toggle("editr-problem", !!problem);
          // comments on cells (see R/comments.R)
          var comment = grid.comments[key];
          element.classList.toggle("editr-commented", !!comment);
          // conditional formatting (see R/formats.R)
          applyFormat(element, formatStyle(cell.getRow().getIndex(), cell.getField(), v));
          var tips = [];
          if (bad) tips.push("\u201c" + v + "\u201d " + bad);
          else if (problem) tips.push(problem);
          if (comment) tips.push("\ud83d\udcac " + comment.text);
          if (tips.length) {
            element.title = tips.join("\n");
          } else if (element.title) {
            element.removeAttribute("title");
          }
          if (base === "password") {
            return document.createTextNode(
              v === null || v === undefined ? "" :
                new Array(String(v).length + 1).join("•"));
          }
          if (base === "labelled") return labelledNode(grid.colMap[cell.getField()], v, element, bad || problem);
          if (base === "url") return linkNode(v);
          if (base === "progress" && (v === null || v === undefined || v === "")) {
            return document.createTextNode("");
          }
          if (base === "colour") return colourNode(v);
          if (base === "html") return safeHTML(v);
          if (base === "choice") {
            var shown = choiceLabel(grid.colMap[cell.getField()], v);
            if (shown !== null && !tips.length) element.title = String(v);
            return document.createTextNode(shown !== null ? shown :
              (v === null || v === undefined ? "" : String(v)));
          }
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

      // the value of a labelled column with its label; SPSS user-defined
      // missing values are muted
      function labelledNode(c, v, element, titled) {
        var na = userMissing(c, v);
        element.classList.toggle("editr-user-na", na);
        if (na && !titled) element.title = "User-defined missing value";
        if (v === null || v === undefined || v === "") return document.createTextNode("");
        var wrap = document.createElement("span");
        wrap.className = "editr-lbl";
        var code = document.createElement("span");
        code.className = "editr-lbl-code";
        code.textContent = String(v);
        wrap.appendChild(code);
        var lab = valueLabel(c, v);
        if (lab !== null) {
          wrap.appendChild(document.createTextNode(" "));
          var text = document.createElement("span");
          text.className = "editr-lbl-text";
          text.textContent = lab;
          wrap.appendChild(text);
        }
        return wrap;
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

      // a colour as CSS can draw it: R colour names first (R's "green" and
      // "gray" are not the CSS ones, and "grey50" is not CSS at all), then
      // hex codes and CSS names; null when it is not a colour
      function colourCss(v) {
        if (v === null || v === undefined) return null;
        var s = String(v).trim();
        if (!s) return null;
        var r = grid.rColours[s.toLowerCase().replace(/\s+/g, "")];
        if (r) return r;
        if (/^#([0-9a-f]{3}|[0-9a-f]{4}|[0-9a-f]{6}|[0-9a-f]{8})$/i.test(s)) return s;
        return window.CSS && CSS.supports && CSS.supports("color", s) ? s : null;
      }

      // a colour as a hex code for <input type="color"> (#RRGGBB)
      function colourHex(v) {
        var css = colourCss(v);
        if (!css) return null;
        if (/^#[0-9a-f]{6}/i.test(css)) return css.slice(0, 7).toLowerCase();
        if (/^#[0-9a-f]{3,4}$/i.test(css)) {
          return ("#" + css[1] + css[1] + css[2] + css[2] + css[3] + css[3]).toLowerCase();
        }
        var probe = document.createElement("span");
        probe.style.color = css;
        document.body.appendChild(probe);
        var m = /rgba?\((\d+),\s*(\d+),\s*(\d+)/.exec(getComputedStyle(probe).color);
        probe.remove();
        if (!m) return null;
        return "#" + [m[1], m[2], m[3]].map(function (x) {
          return ("0" + Number(x).toString(16)).slice(-2);
        }).join("");
      }

      function swatch(v) {
        var sw = document.createElement("span");
        sw.className = "editr-swatch";
        var css = colourCss(v);
        if (css) {
          sw.style.background = css;
        } else {
          sw.className += " editr-swatch-none";
          sw.title = "Not a colour R knows";
        }
        return sw;
      }

      // "html" columns: the HTML of a cell, kept to text formatting, links
      // (opening in a new tab) and images; scripts, styles, forms, frames and
      // event handlers are removed
      var HTML_KEEP = {A: 1, B: 1, STRONG: 1, I: 1, EM: 1, U: 1, S: 1, BR: 1,
                       P: 1, DIV: 1, SPAN: 1, SMALL: 1, SUB: 1, SUP: 1, CODE: 1,
                       PRE: 1, UL: 1, OL: 1, LI: 1, IMG: 1, MARK: 1, HR: 1,
                       H1: 1, H2: 1, H3: 1, H4: 1, H5: 1, H6: 1, BLOCKQUOTE: 1};
      var HTML_DROP = {SCRIPT: 1, STYLE: 1, IFRAME: 1, FRAME: 1, OBJECT: 1,
                       EMBED: 1, LINK: 1, META: 1, BASE: 1, FORM: 1, INPUT: 1,
                       BUTTON: 1, TEXTAREA: 1, SELECT: 1, SVG: 1, MATH: 1,
                       NOSCRIPT: 1, TEMPLATE: 1, AUDIO: 1, VIDEO: 1, SOURCE: 1};
      function safeHTML(v) {
        var tpl = document.createElement("template");
        tpl.innerHTML = v === null || v === undefined ? "" : String(v);
        var clean = function (node) {
          Array.prototype.slice.call(node.childNodes).forEach(function (ch) {
            if (ch.nodeType === 3) return;
            if (ch.nodeType !== 1 || HTML_DROP[ch.tagName.toUpperCase()]) {
              node.removeChild(ch);
              return;
            }
            var tag = ch.tagName.toUpperCase();
            clean(ch);
            if (!HTML_KEEP[tag]) {
              // other elements: keep what is inside them
              while (ch.firstChild) node.insertBefore(ch.firstChild, ch);
              node.removeChild(ch);
              return;
            }
            Array.prototype.slice.call(ch.attributes).forEach(function (a) {
              var name = a.name.toLowerCase(), val = String(a.value).trim();
              var ok = name === "title" || name === "alt" ||
                (name === "href" && tag === "A" && /^(https?:|mailto:)/i.test(val)) ||
                (name === "src" && tag === "IMG" && /^(https?:|data:image\/)/i.test(val));
              if (!ok) ch.removeAttribute(a.name);
            });
            if (tag === "A") {
              ch.setAttribute("target", "_blank");
              ch.setAttribute("rel", "noopener noreferrer");
            }
          });
        };
        clean(tpl.content);
        var out = document.createElement("span");
        out.className = "editr-html";
        out.appendChild(tpl.content);
        // links open without selecting or editing the cell
        out.addEventListener("mousedown", function (e) {
          if (e.target.closest && e.target.closest("a")) e.stopPropagation();
        });
        return out;
      }

      // a swatch next to the colour
      function colourNode(v) {
        var wrap = document.createElement("span");
        wrap.className = "editr-colour";
        if (v === null || v === undefined || v === "") return wrap;
        var s = String(v).trim();
        wrap.appendChild(swatch(s));
        wrap.appendChild(document.createTextNode(s));
        return wrap;
      }

      // R's default palette and the Okabe-Ito palette (safe for colour
      // blindness), offered by the colour picker after the column's colours
      var PALETTE = ["black", "#DF536B", "#61D04F", "#2297E6", "#28E2E5", "#CD0BBC",
                     "#F5C710", "gray62", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                     "#0072B2", "#D55E00", "#CC79A7", "white"];

      // colours offered for a column: its palette (col_options), or the
      // colours already in it and then the default palette
      function colourChoices(c) {
        var o = c.option || {};
        if (o.palette && toArray(o.palette).length) return toArray(o.palette).map(String);
        var seen = {}, out = [];
        var add = function (v) {
          if (v === null || v === undefined || out.length >= 24) return;
          var s = String(v).trim();
          var key = s.toLowerCase().replace(/\s+/g, "");
          if (!s || seen[key]) return;
          seen[key] = true;
          if (colourCss(s)) out.push(s);
        };
        // the first colours of the column (looking at up to 5,000 rows)
        var used = 0;
        (grid.table ? grid.table.getData() : []).slice(0, 5000).some(function (row) {
          var before = out.length;
          add(row[c.field]);
          if (out.length > before) used += 1;
          return used >= 8;
        });
        PALETTE.forEach(add);
        return out;
      }

      // colour columns: type a colour, or pick one of the swatches below the
      // cell, or any colour with the browser's colour picker
      function colourEditor(cell, onRendered, success, cancel) {
        var old = cell.getValue();
        var c = grid.colMap[cell.getField()] || {};
        var wrap = document.createElement("div");
        wrap.className = "editr-colour-edit";
        var sw = document.createElement("span");
        var input = document.createElement("input");
        input.type = "text";
        input.className = "editr-input";
        input.value = old === null || old === undefined ? "" : String(old);
        input.setAttribute("aria-label", "Colour");
        input.setAttribute("autocomplete", "off");
        wrap.appendChild(sw);
        wrap.appendChild(input);
        var paint = function () {
          var next = swatch(input.value);
          sw.className = next.className;
          sw.style.background = next.style.background;
          sw.title = next.title;
        };
        paint();
        var box = document.createElement("div");
        box.className = "editr-popup editr-colour-box";
        box.setAttribute("role", "listbox");
        box.setAttribute("aria-label", "Colours");
        colourChoices(c).forEach(function (v) {
          var b = document.createElement("button");
          b.type = "button";
          b.className = "editr-colour-pick";
          b.setAttribute("role", "option");
          b.style.background = colourCss(v);
          b.title = v;
          b.setAttribute("aria-label", v);
          b.setAttribute("data-value", v);
          if (String(old) === v) b.setAttribute("aria-selected", "true");
          box.appendChild(b);
        });
        var custom = document.createElement("label");
        custom.className = "editr-colour-custom";
        var picker = document.createElement("input");
        picker.type = "color";
        picker.value = colourHex(input.value) || "#2975dd";
        custom.appendChild(picker);
        custom.appendChild(document.createTextNode("Other colour\u2026"));
        box.appendChild(custom);

        var done = false, picking = false;
        var close = function () {
          box.remove();
          window.removeEventListener("scroll", place, true);
        };
        var finish = function (v) {
          if (done) return;
          done = true;
          close();
          v = v === null || v === undefined ? "" : String(v).trim();
          var before = old === null || old === undefined ? "" : String(old);
          if (v === before) cancel(); else success(v === "" ? null : v);
        };
        var stop = function () {
          if (done) return;
          done = true;
          close();
          cancel();
        };
        var place = function () {
          var r = cell.getElement().getBoundingClientRect();
          box.style.top = (r.bottom + window.scrollY + 4) + "px";
          var left = r.left + window.scrollX;
          var maxLeft = window.scrollX + document.documentElement.clientWidth -
            box.getBoundingClientRect().width - 8;
          box.style.left = Math.max(8, Math.min(left, maxLeft)) + "px";
        };
        input.addEventListener("input", paint);
        input.addEventListener("blur", function () {
          if (!picking) finish(input.value);
        });
        input.addEventListener("keydown", function (e) {
          if (e.key === "Enter") {
            e.preventDefault();
            finish(input.value);
          } else if (e.key === "Escape") {
            e.preventDefault();
            stop();
          }
        });
        input.addEventListener("mousedown", function (e) { e.stopPropagation(); });
        // clicks in the box keep the focus in the text box
        box.addEventListener("mousedown", function (e) {
          if (e.target === picker || e.target === custom) {
            picking = true;
          } else {
            e.preventDefault();
          }
        });
        box.addEventListener("click", function (e) {
          var b = e.target.closest && e.target.closest(".editr-colour-pick");
          if (b) finish(b.getAttribute("data-value"));
        });
        picker.addEventListener("input", function () {
          input.value = picker.value;
          paint();
        });
        picker.addEventListener("change", function () { finish(picker.value); });
        picker.addEventListener("blur", function () {
          picking = false;
          if (!done) input.focus({preventScroll: true});
        });
        onRendered(function () {
          box.style.position = "absolute";
          document.body.appendChild(box);
          place();
          window.addEventListener("scroll", place, true);
          input.focus({preventScroll: true});
          input.select();
        });
        return wrap;
      }

      // date-time and time pickers; date-times are "YYYY-MM-DD HH:MM:SS" like
      // R prints them and times "HH:MM". A partly filled picker keeps the
      // old value (the browser reports it as empty), an emptied one clears it.
      function pickerEditor(type) {
        return function (cell, onRendered, success, cancel) {
          return pickerInput(type, cell, onRendered, success, cancel);
        };
      }

      function pickerInput(type, cell, onRendered, success, cancel) {
        var old = cell.getValue();
        var input = document.createElement("input");
        input.type = type;
        input.className = "editr-input";
        if (type === "datetime-local") {
          input.step = "1";
          input.value = old ? String(old).replace(" ", "T") : "";
        } else {
          input.value = old ? String(old) : "";
        }
        var done = false;
        var commit = function () {
          if (done) return;
          done = true;
          if (input.validity && input.validity.badInput) {
            cancel();
            return;
          }
          var v = input.value || null;
          if (v && type === "datetime-local") {
            v = v.replace("T", " ");
            if (v.length === 16) v += ":00";
          }
          if (v === (old || null)) cancel(); else success(v);
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
          labelsOf(c) ? "lbl" : BADGES[c.kind] || "abc";
        var varLabel = c.labels && c.labels.label ? String(c.labels.label) : "";
        return '<span class="editr-title"><span class="editr-badge"' +
          (badge === "lbl" ? ' title="' + (c.kind === "numeric" ? "Numbers" : "Text") +
           ' with value labels"' : '') + '>' +
          badge + '</span><span class="editr-name"' +
          (varLabel ? ' title="' + escapeHTML(varLabel) + '"' : '') + '>' + escapeHTML(name) +
          '</span>' + (c.readonly ? '<span class="editr-lock">' + ICONS.lock +
          '</span>' : '') + '</span>' +
          (grid.opts.summary !== false && !c.frozen ? summaryHTML(c.field) : "");
      }

      /* column summaries (see R/summary.R) ------------------------------ */

      function plural(n, w) { return n + " " + w + (n === 1 ? "" : "s"); }

      function percent(part, total) {
        if (!total) return "0%";
        var p = 100 * part / total;
        return (p > 0 && p < 1 ? p.toFixed(1) : Math.round(p)) + "%";
      }

      function summaryHTML(field) {
        var s = grid.summary[field];
        var html = '<div class="editr-summary" data-field="' + escapeHTML(field) + '">';
        if (!s) return html + '</div>';
        if (s.type === "hist" || s.type === "dates") {
          var bins = toArray(s.bins);
          var max = Math.max.apply(null, bins.map(function (b) { return b.n; }).concat([1]));
          html += '<svg class="editr-sum-chart" viewBox="0 0 ' + bins.length +
            ' 20" preserveAspectRatio="none" role="img" aria-label="Distribution">';
          bins.forEach(function (b, i) {
            var h = b.n ? Math.max(1.5, 20 * b.n / max) : 0;
            var tip = escapeHTML(b.label + ": " + plural(b.n, "row"));
            html += '<g data-act="bin" data-i="' + i + '"><title>' + tip + '</title>' +
              '<rect class="editr-sum-hit" x="' + i + '" y="0" width="1" height="20"/>' +
              '<rect class="editr-sum-bar-v" x="' + (i + 0.1) + '" y="' + (20 - h) +
              '" width="0.8" height="' + h + '"/></g>';
          });
          html += '</svg>';
        } else if (s.type === "values" && s.unique > 20 && s.unique === s.n - s.missing) {
          // identifiers: a bar of values that each appear once says nothing
          html += '<div class="editr-sum-chart editr-sum-empty">all unique</div>';
        } else if (s.type === "values") {
          var total = Math.max(1, s.n - s.missing);
          html += '<div class="editr-sum-values">';
          var lc = grid.colMap[field];
          toArray(s.top).forEach(function (t, i) {
            var tip = escapeHTML(labelledText(lc, t.value) + ": " + plural(t.n, "row") + " (" + percent(t.n, total) + ")");
            html += '<span class="editr-sum-seg editr-sum-seg-' + (i % 5) + '" data-act="value" data-i="' + i +
              '" style="flex-grow:' + t.n + '" title="' + tip + '"></span>';
          });
          if (s.other > 0) {
            html += '<span class="editr-sum-seg editr-sum-seg-other" style="flex-grow:' + s.other +
              '" title="' + escapeHTML("other values: " + plural(s.other, "row")) + '"></span>';
          }
          html += '</div>';
        } else {
          html += '<div class="editr-sum-chart editr-sum-empty">no values</div>';
        }
        html += '<div class="editr-sum-foot">';
        if (s.missing > 0) {
          html += '<button type="button" class="editr-sum-missing" data-act="missing" title="' + plural(s.missing, "missing value") + ' (show these rows)">' +
            percent(s.missing, s.n) + '<span class="editr-sum-na"> NA</span></button>';
        } else {
          html += '<span></span>';
        }
        html += '<button type="button" class="editr-sum-more" data-act="details" title="' +
          plural(s.unique, "distinct value") + ' (details)">' + s.unique +
          '<span class="editr-sum-word"> distinct</span></button></div>';
        return html + '</div>';
      }

      function fillSummaries() {
        if (grid.opts.summary === false) return;
        el.querySelectorAll(".editr-summary[data-field]").forEach(function (box) {
          var tmp = document.createElement("div");
          tmp.innerHTML = summaryHTML(box.getAttribute("data-field"));
          box.innerHTML = tmp.firstChild.innerHTML;
        });
      }

      function setSummaryOn(on, tell) {
        grid.summaryOn = on;
        el.classList.toggle("editr-summaries-off", !on);
        var btn = byId(grid.ui.summary_btn);
        if (btn) btn.setAttribute("aria-pressed", on ? "true" : "false");
        try { window.localStorage.setItem("DataEditR.summary", on ? "on" : "off"); } catch (e) {}
        if (grid.table && grid.ready) grid.table.redraw(true);
        if (tell && shinyOpen()) Shiny.setInputValue(el.id + "_summary", on, {priority: "event"});
      }

      // clicking a bar filters the rows, the missing count shows the rows
      // where the value is missing, the count of values opens the details.
      // The header filter of the column is used when it can express the bar
      // (a range for numbers, "contains" for dates, a list of values);
      // otherwise the bar becomes a filter of its own (grid.picks), shown as
      // a chip like the others
      function summaryClick(target) {
        var box = target.closest(".editr-summary");
        var act = target.closest("[data-act]");
        if (!box || !act) return;
        var field = box.getAttribute("data-field");
        var s = grid.summary[field];
        var c = grid.colMap[field];
        if (!s || !c) return;
        var kind = act.getAttribute("data-act");
        var i = Number(act.getAttribute("data-i"));
        var header = c.filter ? c.filter.type : null;
        var useHeader = function (value) {
          delete grid.picks[field];
          grid.table.setHeaderFilterValue(field, value);
          grid.table.refreshFilter();
        };
        var usePick = function (pick) {
          grid.picks[field] = pick;
          if (header) grid.table.setHeaderFilterValue(field, "");
          grid.table.refreshFilter();
        };
        if (kind === "bin") {
          var bins = toArray(s.bins);
          var b = bins[i];
          if (s.type === "hist") {
            // bins hold values from lo up to (not including) hi, but the last
            // bin includes its upper end
            var below = i < bins.length - 1;
            if (header === "range") {
              var range = {min: String(b.lo), max: String(b.hi)};
              if (below) range.below = true;
              useHeader(range);
            } else {
              usePick({kind: "range", lo: b.lo, hi: b.hi, below: below});
            }
          } else if (header === "text") {
            useHeader(b.label);
          } else {
            usePick({kind: "prefix", value: b.label});
          }
        } else if (kind === "value") {
          var v = toArray(s.top)[i].value;
          if (header === "values") {
            useHeader([c.kind === "logical" ? v === "TRUE" : c.kind === "numeric" ? Number(v) : v]);
          } else {
            usePick({kind: "equals", value: v});
          }
        } else if (kind === "missing") {
          if (grid.picks[field] && grid.picks[field].kind === "missing") {
            delete grid.picks[field];
            grid.table.refreshFilter();
          } else {
            usePick({kind: "missing"});
          }
        } else if (kind === "details") {
          openSummaryDetails(act, c, s);
        }
      }

      function openSummaryDetails(anchor, c, s) {
        var box = document.createElement("div");
        box.className = "editr-popup editr-sum-details";
        box.setAttribute("role", "dialog");
        box.setAttribute("aria-label", "Summary of " + c.title);
        var rows = [["Rows", s.n], ["Missing", s.missing + " (" + percent(s.missing, s.n) + ")"],
                    ["Distinct values", s.unique]];
        var st = s.stats || {};
        if (s.type === "hist") {
          rows = rows.concat([["Smallest", st.min], ["Largest", st.max], ["Mean", st.mean],
                              ["Median", st.median], ["Standard deviation", st.sd]]);
        } else if (s.type === "dates") {
          rows = rows.concat([["Earliest", st.min], ["Latest", st.max]]);
        }
        var html = '<div class="editr-sum-title">' + escapeHTML(c.title) + '</div>' +
          (c.labels && c.labels.label ? '<div class="editr-sum-label">' + escapeHTML(c.labels.label) + '</div>' : '') +
          '<table>';
        rows.forEach(function (r) {
          html += '<tr><th>' + r[0] + '</th><td>' + escapeHTML(r[1] === undefined ? "" : r[1]) + '</td></tr>';
        });
        html += '</table>';
        if (s.type === "values") {
          var total = Math.max(1, s.n - s.missing);
          html += '<div class="editr-sum-title">Most common</div><table>';
          toArray(s.top).forEach(function (t) {
            html += '<tr><th>' + escapeHTML(labelledText(c, t.value)) + '</th><td>' + t.n + ' (' + percent(t.n, total) + ')</td></tr>';
          });
          html += '</table>';
        }
        box.innerHTML = html;
        openPopup(anchor, box);
      }

      // rows that match the filters set from the column summaries (added
      // rows stay)
      function pickFilter(data) {
        if (pinnedRow(data)) return true;
        for (var f in grid.picks) {
          var c = grid.colMap[f];
          if (!c) continue;
          var p = grid.picks[f];
          var v = data[f];
          var empty = v === null || v === undefined || v === "";
          if (p.kind === "missing") {
            if (userMissing(c, v)) continue;
            if (!empty) return false;
            continue;
          }
          if (empty) return false;
          if (p.kind === "range") {
            var x = Number(v);
            if (isNaN(x) || x < p.lo || (p.below ? x >= p.hi : x > p.hi)) return false;
          } else if (p.kind === "prefix") {
            if (String(v).indexOf(p.value) !== 0) return false;
          } else if (typeof v === "boolean") {
            if ((v ? "TRUE" : "FALSE") !== p.value) return false;
          } else if (typeof v === "number") {
            var w = Number(p.value);
            if (Math.abs(v - w) > 1e-12 * Math.max(1, Math.abs(w))) return false;
          } else if (String(v) !== p.value) {
            return false;
          }
        }
        return true;
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
        if (hi !== null && !isNaN(hi) && (value.below ? x >= hi : x > hi)) return false;
        return true;
      }

      function valuesFilter(value, rowValue, rowData) {
        if (pinnedRow(rowData)) return true;
        var selected = toArray(value).map(String);
        var v = rowValue === null || rowValue === undefined ? "NA" : String(rowValue);
        return selected.indexOf(v) !== -1;
      }

      // "set" keeps values containing set; "!set" those that do not
      function textFilter(value, rowValue, rowData) {
        if (pinnedRow(rowData)) return true;
        var q = String(value);
        var not = q.charAt(0) === "!" && q.length > 1;
        if (not) q = q.slice(1);
        if (rowValue === null || rowValue === undefined) return not;
        var has = String(rowValue).toLowerCase().indexOf(q.toLowerCase()) !== -1;
        return not ? !has : has;
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
          if (labelsOf(c)) {
            def.headerFilterParams.itemFormatter = function (label, value) {
              return escapeHTML(labelledText(c, value));
            };
          }
          def.headerFilterFunc = valuesFilter;
          def.headerFilterPlaceholder = "Any";
          def.headerFilterEmptyCheck = function (v) {
            return !v || !toArray(v).length;
          };
        } else {
          def.headerFilter = "input";
          def.headerFilterFunc = function (value, rowValue, rowData) {
            var shown = rowValue === null || rowValue === undefined ? rowValue :
              shownText(c, rowValue);
            return textFilter(value, shown, rowData);
          };
          def.headerFilterPlaceholder = "Contains\u2026";
          def.headerFilterParams = {elementAttributes: {
            title: "Rows containing this text; start with ! for rows that do not"
          }};
        }
      }

      /* ---------------------------------------------------------------- */
      /* filters: the logics, tests and text of a filter                   */
      /* ---------------------------------------------------------------- */

      // A filter (a "rule") is {field, logic, values}. grid.conditions holds
      // the filters set in the Filters panel (see "filter panel" below);
      // filterRows() turns a header box or summary pick into rules.

      function filterType(c) {
        var o = c.option || {};
        if (c.kind === "logical" || o.type === "checkbox") return "logical";
        if (c.kind === "date" || o.type === "date") return "date";
        if (c.kind === "datetime" || o.type === "datetime") return "datetime";
        if (o.type === "time") return "time";
        if (c.kind === "numeric") return "number";
        return "text";
      }

      function filterLogics(c) {
        var type = filterType(c);
        return Object.keys(FILTER_LOGICS).filter(function (k) {
          if (type === "logical") {
            return k === "equal" || k === "not_equal" || k === "missing" || k === "present";
          }
          return type !== "text" || !FILTER_LOGICS[k].order;
        });
      }

      // a value typed into the dialog, as the column holds it
      function filterValue(c, s) {
        var v = normalizeValue(c, s);
        return typeof v === "boolean" ? (v ? "TRUE" : "FALSE") : String(v);
      }

      function filterNumber(v) {
        return v === "Inf" ? Infinity : v === "-Inf" ? -Infinity : Number(v);
      }

      function ruleKey(r) {
        return JSON.stringify([r.field, r.logic, r.values]);
      }

      function ruleComplete(r) {
        var input = FILTER_LOGICS[r.logic].input;
        var has = function (x) { return x !== undefined && x !== null && x !== ""; };
        if (input === "none") return true;
        return has(r.values[0]) && (input !== "two" || has(r.values[1]));
      }

      // what stops a row from being applied (or null)
      function ruleProblem(r) {
        var c = grid.colMap[r.field];
        var logic = FILTER_LOGICS[r.logic];
        if (!c || logic.input === "none") return null;
        if (logic.raw) {
          try {
            new RegExp(r.values[0] || "");
          } catch (e) {
            return "This is not a valid regular expression";
          }
          return null;
        }
        if (filterType(c) === "number" && (logic.order || logic.suggest)) {
          var bad = r.values.filter(function (v) {
            return v !== "" && v !== undefined && v !== "NA" && isNaN(filterNumber(v));
          });
          if (bad.length) return "“" + bad[0] + "” is not a number";
        }
        return null;
      }

      // {field, test}: does a value of the column pass a row of the dialog?
      function conditionTest(r) {
        var c = grid.colMap[r.field];
        if (!c) return null;
        var number = filterType(c) === "number";
        var vals = r.values.map(String);
        var blank = function (v) { return v === null || v === undefined || v === ""; };
        var text = function (v) {
          return typeof v === "boolean" ? (v ? "TRUE" : "FALSE") : String(v);
        };
        var shown = function (v) { return shownText(c, v); };
        // NaN when a cell of a number column does not hold a number
        var order = function (v, x) {
          if (!number) {
            var s = text(v);
            return s < x ? -1 : s > x ? 1 : 0;
          }
          var a = filterNumber(v), b = filterNumber(x);
          return a < b ? -1 : a > b ? 1 : a === b ? 0 : NaN;
        };
        var nums = vals.map(filterNumber);
        var low = vals.map(function (v) { return v.toLowerCase(); });
        var hasNA = vals.indexOf("NA") !== -1;
        var equal = function (v) {
          if (blank(v)) return hasNA;
          if (!number) return vals.indexOf(text(v)) !== -1;
          var x = filterNumber(v);
          return nums.some(function (w) {
            return x === w || Math.abs(x - w) <= 1e-12 * Math.max(1, Math.abs(w));
          });
        };
        var compare = function (ok) {
          return function (v) { return !blank(v) && ok(order(v, vals[0])); };
        };
        // the two values can be given in either order
        var between = function (v) {
          if (blank(v)) return false;
          var a = order(v, vals[0]), b = order(v, vals[1]);
          return (a >= 0 && b <= 0) || (a <= 0 && b >= 0);
        };
        var contain = function (v) {
          if (blank(v)) return false;
          var s = shown(v).toLowerCase();
          return low.some(function (x) { return s.indexOf(x) !== -1; });
        };
        var regex = function (v) {
          if (blank(v)) return false;
          regex.re = regex.re || new RegExp(vals[0]);
          return regex.re.test(shown(v));
        };
        var missing = function (v) { return blank(v) || userMissing(c, v); };
        var not = function (f) { return function (v) { return !f(v); }; };
        var tests = {
          equal: equal, not_equal: not(equal),
          gt: compare(function (o) { return o > 0; }),
          lt: compare(function (o) { return o < 0; }),
          gte: compare(function (o) { return o >= 0; }),
          lte: compare(function (o) { return o <= 0; }),
          between: between, not_between: not(between),
          contain: contain, not_contain: not(contain),
          regex: regex, not_regex: not(regex),
          missing: missing, present: not(missing)
        };
        return {field: r.field, test: tests[r.logic]};
      }

      function prepareConditions() {
        grid.conditionTests = (grid.conditions || []).map(conditionTest).filter(Boolean);
      }

      // rows that match the filters of the panel: all of them, or any with
      // grid.matchAny (added rows stay)
      function conditionFilter(data) {
        if (pinnedRow(data)) return true;
        var tests = grid.conditionTests || [];
        if (!tests.length) return true;
        var i;
        if (grid.matchAny) {
          for (i = 0; i < tests.length; i++) {
            if (tests[i].test(data[tests[i].field])) return true;
          }
          return false;
        }
        for (i = 0; i < tests.length; i++) {
          if (!tests[i].test(data[tests[i].field])) return false;
        }
        return true;
      }

      // the column of row names has no title of its own
      function columnName(c) {
        return c.frozen ? "row names" : c.title;
      }

      function conditionText(r) {
        var c = grid.colMap[r.field] || {title: r.field};
        var vals = r.values.map(function (x) {
          return valueLabel(c, x) || choiceLabel(c, x) || String(x);
        });
        var list = vals.length < 2 ? vals.join("") :
          vals.slice(0, -1).join(", ") + " or " + vals[vals.length - 1];
        var t = columnName(c);
        switch (r.logic) {
          case "equal": return t + " is " + list;
          case "not_equal": return t + " is not " + list;
          case "gt": return t + " > " + vals[0];
          case "lt": return t + " < " + vals[0];
          case "gte": return t + " ≥ " + vals[0];
          case "lte": return t + " ≤ " + vals[0];
          case "between": return t + " " + vals[0] + "–" + vals[1];
          case "not_between": return t + " not " + vals[0] + "–" + vals[1];
          case "contain": return t + " contains “" + list + "”";
          case "not_contain": return t + " does not contain “" + list + "”";
          case "regex": return t + " matches /" + vals[0] + "/";
          case "not_regex": return t + " does not match /" + vals[0] + "/";
          case "missing": return t + " is missing";
          default: return t + " is not missing";
        }
      }

      // the rows of the dialog for a header filter or summary pick
      function filterRows(f) {
        var out = [];
        var add = function (logic, values) {
          out.push({field: f.field, logic: logic, values: values.map(String)});
        };
        var range = function (lo, hi, below) {
          if (below) {
            add("gte", [lo]);
            add("lt", [hi]);
          } else if (lo !== "" && hi !== "") {
            add("between", [lo, hi]);
          } else if (lo !== "") {
            add("gte", [lo]);
          } else {
            add("lte", [hi]);
          }
        };
        var v = f.value, p = f.pick;
        if (p) {
          if (p.kind === "missing") add("missing", []);
          else if (p.kind === "range") range(p.lo, p.hi, p.below);
          else if (p.kind === "prefix") add("contain", [p.value]);
          else add("equal", [p.value]);
        } else if (Array.isArray(v)) {
          add("equal", v.map(function (x) {
            return typeof x === "boolean" ? (x ? "TRUE" : "FALSE") : x;
          }));
        } else if (typeof v === "object") {
          range(v.min, v.max, v.below);
        } else if (String(v).charAt(0) === "!" && String(v).length > 1) {
          // "!text" in a header box keeps the rows without the text
          add("not_contain", [String(v).slice(1)]);
        } else {
          add("contain", [v]);
        }
        return out;
      }

      /* ---------------------------------------------------------------- */
      /* filter panel: every filter in the Filters tab of the side panel  */
      /* ---------------------------------------------------------------- */

      // The panel shows a card per filter. Cards without a source are the
      // filters of the panel: they apply (as grid.conditions) as soon as they
      // are complete, so the grid follows the panel as it changes. Cards with
      // a source stand for a header box or a summary pick; changing one makes
      // it (and the other cards of that box) a filter of the panel and clears
      // the box. Filters of the panel match all (AND) or any (OR,
      // grid.matchAny) of them; header boxes and picks always apply as well.

      var FP_ORDER = {
        range: ["between", "not_between", "gte", "lte", "gt", "lt", "equal", "not_equal",
                "contain", "not_contain", "regex", "not_regex", "missing", "present"],
        text: ["contain", "not_contain", "equal", "not_equal", "regex", "not_regex",
               "missing", "present"],
        logical: ["equal", "not_equal", "missing", "present"]
      };
      var FP_FEW = 8; // values listed before "Show more"

      function fpBox() { return byId(grid.ui.filters); }

      function fpVisible() {
        var box = fpBox();
        return !!box && !!box.offsetParent;
      }

      function fpPrefix() {
        return grid.ui.filters ? grid.ui.filters.replace(/filters$/, "") : null;
      }

      // open the Filters tab of the side panel
      function fpOpenPanel() {
        var prefix = fpPrefix();
        if (prefix && window.DataEditR && DataEditR.panel) DataEditR.panel(prefix, "filters", false);
      }

      function fpEl(tag, props, kids) {
        var e = document.createElement(tag);
        Object.keys(props || {}).forEach(function (k) {
          var v = props[k];
          if (v === null || v === undefined || v === false) return;
          if (k === "class") e.className = v;
          else if (k === "text") e.textContent = v;
          else if (k === "html") e.innerHTML = v;
          else if (k.slice(0, 2) === "on") e.addEventListener(k.slice(2), v);
          else e.setAttribute(k, v === true ? "" : String(v));
        });
        (kids || []).forEach(function (k) {
          if (k === null || k === undefined || k === false) return;
          e.appendChild(typeof k === "string" ? document.createTextNode(k) : k);
        });
        return e;
      }

      function fpId() {
        grid.fp.seq += 1;
        return "f" + grid.fp.seq;
      }

      function fpBadge(c) {
        return c.option && c.option.type === "time" ? "time" :
          labelsOf(c) ? "lbl" : BADGES[c.kind] || "abc";
      }

      function fpLogics(c) {
        var allowed = filterLogics(c);
        var type = filterType(c);
        var order = type === "text" ? FP_ORDER.text : type === "logical" ?
          FP_ORDER.logical : FP_ORDER.range;
        return order.filter(function (k) { return allowed.indexOf(k) !== -1; });
      }

      function fpLogicLabel(c, k) {
        if (filterType(c) === "logical" && (k === "equal" || k === "not_equal")) {
          return k === "equal" ? "is" : "is not";
        }
        return FILTER_LOGICS[k].label;
      }

      // the logic a new card starts with
      function fpDefaultLogic(c) {
        var type = filterType(c);
        if (type === "logical") return "equal";
        if (c.filter && c.filter.type === "values") return "equal";
        if (type === "text") return "contain";
        return "between";
      }

      // passwords are never listed: their values are typed
      function fpSecret(c) {
        return !!(c && c.option && c.option.type === "password");
      }

      // what a card asks for
      function fpKind(card, c) {
        var logic = FILTER_LOGICS[card.logic];
        if (logic.input === "none") return "none";
        if (logic.raw) return "pattern";
        if (card.logic === "equal" || card.logic === "not_equal") {
          return filterType(c) === "logical" ? "bool" : fpSecret(c) ? "words" : "list";
        }
        if (card.logic === "contain" || card.logic === "not_contain") return "words";
        return "range";
      }

      // values of a column on a number line: numbers, days or minutes
      function fpScale(c) {
        var type = filterType(c);
        if (type === "number") {
          return {
            to: function (v) {
              var x = typeof v === "number" ? v : filterNumber(String(v).trim());
              return isFinite(x) ? x : NaN;
            },
            from: function (x, step) { return fpRound(x, step); },
            text: function (x, step) { return fpRound(x, step); }
          };
        }
        if (type === "date") {
          return {
            to: function (v) {
              var m = /^(\d{4})-(\d{2})-(\d{2})/.exec(String(v));
              return m ? Date.UTC(+m[1], +m[2] - 1, +m[3]) / 864e5 : NaN;
            },
            from: function (x) { return new Date(Math.round(x) * 864e5).toISOString().slice(0, 10); },
            text: function (x) { return new Date(Math.round(x) * 864e5).toISOString().slice(0, 10); },
            whole: true
          };
        }
        if (type === "datetime") {
          return {
            to: function (v) {
              var t = Date.parse(String(v).trim().replace(" ", "T").slice(0, 19) + "Z");
              return isFinite(t) ? t / 6e4 : NaN;
            },
            from: function (x) {
              return new Date(Math.round(x) * 6e4).toISOString().slice(0, 16).replace("T", " ") + ":00";
            },
            text: function (x) {
              return new Date(Math.round(x) * 6e4).toISOString().slice(0, 16).replace("T", " ");
            },
            whole: true
          };
        }
        return null;
      }

      function fpStep(span, whole) {
        if (!(span > 0)) return 1;
        var raw = span / 200;
        var p = Math.pow(10, Math.floor(Math.log(raw) / Math.LN10));
        var m = raw / p;
        var step = (m < 2 ? 1 : m < 5 ? 2 : 5) * p;
        return whole ? Math.max(1, Math.round(step)) : step;
      }

      function fpRound(x, step) {
        var d = Math.max(0, Math.min(10, -Math.floor(Math.log(step) / Math.LN10 + 1e-9)));
        return String(Number((Math.round(x / step) * step).toFixed(d)));
      }

      // the column on its number line: range, histogram and a step
      function fpStats(c, scale) {
        var xs = [];
        grid.table.getData().forEach(function (row) {
          var v = row[c.field];
          if (v === null || v === undefined || v === "") return;
          var x = scale.to(v);
          if (isFinite(x)) xs.push(x);
        });
        if (!xs.length) return null;
        var lo = Infinity, hi = -Infinity, ints = true;
        xs.forEach(function (x) {
          if (x < lo) lo = x;
          if (x > hi) hi = x;
          if (x !== Math.round(x)) ints = false;
        });
        var nb = hi > lo ? Math.max(6, Math.min(24, Math.round(Math.sqrt(xs.length)) + 4)) : 1;
        var width = hi > lo ? (hi - lo) / nb : 1;
        var bins = [];
        for (var i = 0; i < nb; i++) bins.push(0);
        xs.forEach(function (x) {
          bins[Math.min(nb - 1, Math.floor((x - lo) / width))] += 1;
        });
        return {lo: lo, hi: hi, bins: bins, width: width,
                step: fpStep(hi - lo, scale.whole || ints)};
      }

      // counts of the values of a column, most common first when there are many
      function fpValues(c) {
        var counts = Object.create(null), keys = [], missing = 0;
        grid.table.getData().forEach(function (row) {
          var v = row[c.field];
          if (v === null || v === undefined || v === "") {
            missing += 1;
            return;
          }
          var k = typeof v === "boolean" ? (v ? "TRUE" : "FALSE") : String(v);
          if (counts[k] === undefined) {
            counts[k] = 0;
            keys.push(k);
          }
          counts[k] += 1;
        });
        var number = filterType(c) === "number";
        var natural = function (a, b) {
          if (number) return filterNumber(a) - filterNumber(b);
          return a.localeCompare(b, undefined, {numeric: true, sensitivity: "base"});
        };
        keys.sort(keys.length > FP_FEW ? function (a, b) {
          return counts[b] - counts[a] || natural(a, b);
        } : natural);
        return {keys: keys, counts: counts, missing: missing};
      }

      // how many rows a card keeps (null when it is not complete)
      function fpKeeps(card) {
        if (!ruleComplete(card) || ruleProblem(card)) return null;
        var t = conditionTest(card);
        if (!t) return null;
        var n = 0;
        grid.table.getData().forEach(function (row) {
          if (t.test(row[t.field])) n += 1;
        });
        return n;
      }

      // what a card does, after the column name
      function fpSummary(card, c) {
        var shown = function (v) {
          return v === "NA" ? "missing" : valueLabel(c, v) || choiceLabel(c, v) || String(v);
        };
        var vals = card.values.filter(function (v) { return v !== "" && v !== undefined; });
        var list = function (sep) {
          var s = vals.map(shown);
          return s.length < 2 ? s.join("") : s.slice(0, -1).join(", ") + " " + sep + " " + s[s.length - 1];
        };
        var quoted = vals.map(function (v) { return "“" + v + "”"; });
        var words = quoted.length < 2 ? quoted.join("") :
          quoted.slice(0, -1).join(", ") + " or " + quoted[quoted.length - 1];
        var label = fpLogicLabel(c, card.logic);
        if (!ruleComplete(card)) return label + " …";
        switch (card.logic) {
          case "between": return "is between " + vals[0] + " and " + vals[1];
          case "not_between": return "is not between " + vals[0] + " and " + vals[1];
          case "equal": return filterType(c) === "logical" ? "is " + list("or") : "is " + list("or");
          case "not_equal": return "is not " + list("or");
          case "contain": return "contains " + words;
          case "not_contain": return "does not contain " + words;
          case "regex": return "matches /" + vals[0] + "/";
          case "not_regex": return "does not match /" + vals[0] + "/";
          case "missing": case "present": return label;
          default: return label + " " + vals[0];
        }
      }

      // the text of header boxes and picks, to see when they change
      function fpExternalKey() {
        return JSON.stringify(activeFilters().filter(function (f) { return !f.cond; }).map(function (f) {
          return [f.field, f.value === undefined ? null : f.value, f.pick || null];
        }));
      }

      // rebuild the cards of header boxes and picks (and render) when they
      // changed outside the panel
      function fpSync(force) {
        if (!grid.table) return;
        var key = fpExternalKey();
        if (!force && key === grid.fp.key) {
          fpUpdateCounts();
          return;
        }
        grid.fp.key = key;
        var old = grid.fp.cards;
        var mine = old.filter(function (c) { return !c.source; });
        var sources = [];
        activeFilters().forEach(function (f) {
          if (f.cond) return;
          var source = (f.pick ? "pick:" : "header:") + f.field;
          var prev = old.filter(function (c) { return c.source === source; });
          filterRows(f).forEach(function (r, i) {
            sources.push({id: prev[i] ? prev[i].id : fpId(), field: r.field, logic: r.logic,
                          values: r.values, source: source, original: ruleKey(r),
                          open: prev[i] ? prev[i].open : false});
          });
        });
        grid.fp.cards = sources.concat(mine);
        fpRender();
      }

      function fpSyncSoon() {
        clearTimeout(grid.fp.syncTimer);
        grid.fp.syncTimer = setTimeout(function () {
          if (fpVisible()) fpSync(false);
        }, 0);
      }

      // the panel was opened: list every filter, open while they are few
      function fpShow() {
        fpSync(true);
        if (grid.fp.cards.length <= 3) {
          grid.fp.cards.forEach(function (c) { c.open = true; });
          fpRender();
        }
      }

      // a card of a header box or pick is about to change: all cards of that
      // box become filters of the panel, and the box is cleared
      function fpTouch(card) {
        if (!card.source) return;
        var source = card.source;
        grid.fp.cards.forEach(function (c) {
          if (c.source === source) {
            delete c.source;
            delete c.original;
          }
        });
        fpCommit(true);
        clearFilterSource(source);
        if (source.indexOf("pick:") === 0) grid.table.refreshFilter();
        grid.fp.key = fpExternalKey();
        var ui = grid.fp.ui[card.id];
        if (ui) {
          var note = ui.root.querySelector(".editr-fp-src");
          if (note) note.parentNode.removeChild(note);
        }
      }

      // apply the cards of the panel to the grid (at once, or after typing)
      function fpCommit(now) {
        clearTimeout(grid.fp.timer);
        var run = function () {
          if (!grid.table) return;
          grid.conditions = grid.fp.cards.filter(function (c) {
            return !c.source && grid.colMap[c.field] && ruleComplete(c) && !ruleProblem(c);
          }).map(function (c) {
            return {field: c.field, logic: c.logic, values: c.values.slice(), cardId: c.id};
          });
          grid.table.refreshFilter();
          setTimeout(fpUpdateCounts, 0);
        };
        if (now) run();
        else grid.fp.timer = setTimeout(run, 200);
      }

      function fpChanged(card, now) {
        var ui = grid.fp.ui[card.id];
        if (ui && ui.update) ui.update();
        fpCommit(now);
      }

      function fpUpdateCounts() {
        if (!grid.table || !grid.fp.ui) return;
        var total = grid.table.getDataCount();
        grid.fp.cards.forEach(function (card) {
          var ui = grid.fp.ui[card.id];
          if (!ui) return;
          var c = grid.colMap[card.field];
          var p = ruleProblem(card);
          ui.msg.textContent = p || "";
          var n = fpKeeps(card);
          ui.keeps.innerHTML = "";
          if (n === null) {
            ui.keeps.textContent = p ? "" : "not set yet";
          } else {
            ui.keeps.appendChild(document.createTextNode("keeps "));
            ui.keeps.appendChild(fpEl("strong", {text: n.toLocaleString("en-US")}));
            ui.keeps.appendChild(document.createTextNode(" of " + total.toLocaleString("en-US")));
          }
          if (ui.sum && c) ui.sum.textContent = fpSummary(card, c);
        });
        var box = fpBox();
        if (!box) return;
        var shown = grid.table.getDataCount("active");
        var count = box.querySelector(".editr-fp-count");
        if (count) {
          count.innerHTML = "";
          count.appendChild(fpEl("strong", {text: shown.toLocaleString("en-US")}));
          count.appendChild(document.createTextNode(" of " + total.toLocaleString("en-US") +
                                                    (total === 1 ? " row shown" : " rows shown")));
        }
        var bar = box.querySelector(".editr-fp-bar > span");
        if (bar) bar.style.width = (total ? 100 * shown / total : 0) + "%";
      }

      function fpRemove(card) {
        fpTouch(card);
        grid.fp.cards.splice(grid.fp.cards.indexOf(card), 1);
        fpCommit(true);
        fpRender();
      }

      // a filter of the panel removed from its chip
      function fpRemoveById(id) {
        var card = grid.fp.cards.filter(function (c) { return c.id === id; })[0];
        if (card) {
          grid.fp.cards.splice(grid.fp.cards.indexOf(card), 1);
          fpCommit(true);
          if (fpVisible()) fpRender();
        }
      }

      function fpClearAll() {
        grid.fp.cards = [];
        grid.picks = {};
        grid.conditions = [];
        grid.matchAny = false;
        // one box at a time: Tabulator's clearHeaderFilter() rebuilds every
        // box and fails on a column deleted after it had one
        grid.table.getHeaderFilters().forEach(function (f) {
          if (grid.colMap[f.field]) grid.table.setHeaderFilterValue(f.field, "");
        });
        grid.table.refreshFilter();
        grid.fp.key = fpExternalKey();
        if (fpVisible()) fpRender();
      }

      // match all or any: with any, the header boxes and picks become filters
      // of the panel so that they are part of the "or"
      function fpSetMatch(any) {
        grid.matchAny = !!any;
        if (any) {
          var sources = [];
          grid.fp.cards.forEach(function (c) {
            if (c.source && sources.indexOf(c.source) === -1) sources.push(c.source);
          });
          sources.forEach(function (s) {
            fpTouch(grid.fp.cards.filter(function (c) { return c.source === s; })[0]);
          });
        }
        fpCommit(true);
        fpRender();
      }

      function fpAdd(field) {
        var c = grid.colMap[field];
        if (!c) return;
        grid.fp.cards.forEach(function (x) { x.open = false; });
        var card = {id: fpId(), field: field, logic: fpDefaultLogic(c), values: [], open: true};
        grid.fp.cards.push(card);
        fpRender();
        var ui = grid.fp.ui[card.id];
        var first = ui && ui.root.querySelector(".editr-fp-body input, .editr-fp-body button, .editr-fp-body [tabindex]");
        if (first) first.focus();
      }

      /* the parts of a card ------------------------------------------- */

      function fpRangeUI(card, c, ui) {
        var scale = fpScale(c);
        var stats = scale ? fpStats(c, scale) : null;
        var logic = card.logic;
        var two = FILTER_LOGICS[logic].input === "two";
        var body = fpEl("div", {class: "editr-fp-range"});
        var type = filterType(c);
        var inputs = [];
        var pos = function (x) {
          return stats.hi > stats.lo ? Math.max(0, Math.min(100, 100 * (x - stats.lo) / (stats.hi - stats.lo))) : 50;
        };
        var valueAt = function (i) {
          var x = scale.to(card.values[i]);
          if (isFinite(x)) return x;
          if (two) return i === 0 ? stats.lo : stats.hi;
          return logic === "lte" || logic === "lt" ? stats.hi : stats.lo;
        };
        var bars = [], handles = [], fill = null, track = null;
        if (stats) {
          var max = Math.max.apply(null, stats.bins);
          var hist = fpEl("div", {class: "editr-fp-hist", "aria-hidden": "true"});
          stats.bins.forEach(function (n) {
            var b = fpEl("span", {class: "editr-fp-bin"});
            b.style.height = (n ? Math.max(6, 100 * n / max) : 2) + "%";
            bars.push(b);
            hist.appendChild(b);
          });
          body.appendChild(hist);
          track = fpEl("div", {class: "editr-fp-track"});
          fill = fpEl("span", {class: "editr-fp-fill"});
          track.appendChild(fill);
          var n = two ? 2 : 1;
          for (var i = 0; i < n; i++) {
            (function (i) {
              var hd = fpEl("span", {
                class: "editr-fp-handle", role: "slider", tabindex: "0",
                "aria-label": two ? (i === 0 ? "From" : "To") :
                  (logic === "lte" || logic === "lt" ? "Highest kept" : "Lowest kept")
              });
              hd.setAttribute("aria-valuemin", scale.text(stats.lo, stats.step));
              hd.setAttribute("aria-valuemax", scale.text(stats.hi, stats.step));
              var set = function (x) {
                fpTouch(card);
                x = Math.max(stats.lo, Math.min(stats.hi, x));
                // the ends of a range do not pass each other
                if (two && isFinite(scale.to(card.values[1 - i]))) {
                  var other = scale.to(card.values[1 - i]);
                  x = i === 0 ? Math.min(x, other) : Math.max(x, other);
                }
                card.values[i] = scale.from(x, stats.step);
                if (two && (card.values[1 - i] === undefined || card.values[1 - i] === "")) {
                  card.values[1 - i] = scale.from(i === 0 ? stats.hi : stats.lo, stats.step);
                }
                fpChanged(card, false);
              };
              hd.addEventListener("pointerdown", function (e) {
                e.preventDefault();
                hd.focus();
                hd.setPointerCapture(e.pointerId);
                var move = function (ev) {
                  var r = track.getBoundingClientRect();
                  set(stats.lo + (stats.hi - stats.lo) * (ev.clientX - r.left) / r.width);
                };
                var up = function () {
                  hd.removeEventListener("pointermove", move);
                  hd.removeEventListener("pointerup", up);
                  fpCommit(true);
                };
                hd.addEventListener("pointermove", move);
                hd.addEventListener("pointerup", up);
              });
              hd.addEventListener("keydown", function (e) {
                var x = valueAt(i), d = stats.step * (e.shiftKey ? 10 : 1);
                if (e.key === "ArrowRight" || e.key === "ArrowUp") set(x + d);
                else if (e.key === "ArrowLeft" || e.key === "ArrowDown") set(x - d);
                else if (e.key === "PageUp") set(x + 10 * d);
                else if (e.key === "PageDown") set(x - 10 * d);
                else if (e.key === "Home") set(stats.lo);
                else if (e.key === "End") set(stats.hi);
                else return;
                e.preventDefault();
              });
              handles.push(hd);
              track.appendChild(hd);
            })(i);
          }
          // a click on the line moves the nearest end there
          track.addEventListener("pointerdown", function (e) {
            if (e.target !== track && e.target !== fill) return;
            var r = track.getBoundingClientRect();
            var x = stats.lo + (stats.hi - stats.lo) * (e.clientX - r.left) / r.width;
            var i = two && Math.abs(x - valueAt(1)) < Math.abs(x - valueAt(0)) ? 1 : 0;
            fpTouch(card);
            card.values[i] = scale.from(Math.max(stats.lo, Math.min(stats.hi, x)), stats.step);
            if (two && (card.values[1 - i] === undefined || card.values[1 - i] === "")) {
              card.values[1 - i] = scale.from(i === 0 ? stats.hi : stats.lo, stats.step);
            }
            fpChanged(card, true);
          });
          body.appendChild(track);
          body.appendChild(fpEl("div", {class: "editr-fp-axis", "aria-hidden": "true"}, [
            fpEl("span", {text: scale.text(stats.lo, stats.step)}),
            fpEl("span", {text: scale.text(stats.hi, stats.step)})
          ]));
        }
        // the ends as text
        var row = fpEl("div", {class: "editr-fp-ends"});
        var n = two ? 2 : 1;
        for (var k = 0; k < n; k++) {
          (function (i) {
            var input = fpEl("input", {
              type: type === "date" ? "date" : type === "datetime" ? "datetime-local" : "text",
              class: "form-control form-control-sm",
              "aria-label": two ? (i === 0 ? "From" : "To") : "Value",
              inputmode: type === "number" ? "decimal" : null,
              placeholder: stats && type !== "date" && type !== "datetime" ?
                scale.text(two ? (i === 0 ? stats.lo : stats.hi) :
                             (logic === "lte" || logic === "lt" ? stats.hi : stats.lo), stats.step) : null
            });
            input.addEventListener("input", function () {
              fpTouch(card);
              var x = input.value.trim();
              if (type === "datetime" && x) x = x.replace("T", " ") + (x.length === 16 ? ":00" : "");
              card.values[i] = x ? filterValue(c, x) : "";
              fpChanged(card, false);
            });
            inputs.push(input);
            if (i === 1) row.appendChild(fpEl("span", {class: "editr-fp-to", text: "to"}));
            row.appendChild(input);
          })(k);
        }
        body.appendChild(row);
        ui.update = function () {
          var x0 = scale ? scale.to(card.values[0]) : NaN;
          var x1 = scale ? scale.to(card.values[1]) : NaN;
          inputs.forEach(function (input, i) {
            if (document.activeElement === input) return;
            var v = card.values[i];
            input.value = v === undefined || v === null ? "" :
              type === "datetime" ? String(v).replace(" ", "T").slice(0, 16) : String(v);
          });
          if (!stats) return;
          var set = two ? isFinite(x0) && isFinite(x1) : isFinite(x0);
          var a = Math.min(valueAt(0), two ? valueAt(1) : valueAt(0));
          var b = Math.max(valueAt(0), two ? valueAt(1) : valueAt(0));
          // does the bar (from x to y) hold values that are kept?
          var keep = function (x, y) {
            if (!set) return null;
            switch (logic) {
              case "between": return y >= a && x <= b;
              case "not_between": return x < a || y > b;
              case "gte": case "gt": return y >= x0;
              default: return x <= x0;
            }
          };
          bars.forEach(function (bar, i) {
            var from = stats.lo + stats.width * i;
            var on = keep(from, from + stats.width);
            bar.classList.toggle("editr-fp-on", on === true);
            bar.classList.toggle("editr-fp-off", on === false);
          });
          handles.forEach(function (hd, i) {
            var x = valueAt(i);
            hd.style.left = pos(x) + "%";
            hd.setAttribute("aria-valuenow", scale.text(x, stats.step));
            hd.setAttribute("aria-valuetext", scale.text(x, stats.step));
          });
          fill.classList.toggle("editr-fp-unset", !set);
          fill.classList.toggle("editr-fp-outside", logic === "not_between");
          if (two) {
            fill.style.left = pos(a) + "%";
            fill.style.width = (pos(b) - pos(a)) + "%";
          } else if (logic === "gte" || logic === "gt") {
            fill.style.left = pos(valueAt(0)) + "%";
            fill.style.width = (100 - pos(valueAt(0))) + "%";
          } else {
            fill.style.left = "0%";
            fill.style.width = pos(valueAt(0)) + "%";
          }
        };
        return body;
      }

      function fpListUI(card, c, ui) {
        var vals = fpValues(c);
        var entries = vals.keys.map(function (k) {
          return {value: k, n: vals.counts[k], label: shownText(c, k)};
        });
        if (vals.missing) entries.push({value: "NA", n: vals.missing, label: "missing", missing: true});
        var max = Math.max.apply(null, entries.map(function (e) { return e.n; }).concat(1));
        var many = vals.keys.length > FP_FEW;
        var state = {query: "", more: false};
        var body = fpEl("div", {class: "editr-fp-values"});
        var opts = fpEl("div", {class: "editr-fp-opts", role: "group",
                                "aria-label": "Values of " + columnName(c)});
        var footer = fpEl("div", {class: "editr-fp-optfoot"});
        if (many) {
          var search = fpEl("input", {
            type: "search", class: "form-control form-control-sm editr-fp-search",
            placeholder: "Search " + entries.length.toLocaleString("en-US") + " values",
            "aria-label": "Search the values of " + columnName(c)
          });
          search.addEventListener("input", function () {
            state.query = search.value.trim().toLowerCase();
            draw();
          });
          body.appendChild(search);
        }
        body.appendChild(opts);
        body.appendChild(footer);
        var option = function (e) {
          var checked = card.values.indexOf(e.value) !== -1;
          var box = fpEl("input", {type: "checkbox", checked: checked});
          box.addEventListener("change", function () {
            fpTouch(card);
            var at = card.values.indexOf(e.value);
            if (box.checked && at === -1) card.values.push(e.value);
            if (!box.checked && at !== -1) card.values.splice(at, 1);
            fpChanged(card, true);
            label.classList.toggle("editr-fp-picked", box.checked);
          });
          var meter = fpEl("span", {class: "editr-fp-meter", "aria-hidden": "true"},
                           [fpEl("span", {})]);
          meter.firstChild.style.width = (100 * e.n / max) + "%";
          var label = fpEl("label", {class: "editr-fp-opt" + (checked ? " editr-fp-picked" : "")}, [
            box,
            fpEl("span", {class: "editr-fp-val" + (e.missing ? " editr-fp-missing" : ""),
                          text: e.label, title: e.label}),
            meter,
            fpEl("span", {class: "editr-fp-n", text: e.n.toLocaleString("en-US")})
          ]);
          return label;
        };
        var draw = function () {
          opts.innerHTML = "";
          footer.innerHTML = "";
          var picked = entries.filter(function (e) { return card.values.indexOf(e.value) !== -1; });
          var rest = entries.filter(function (e) { return card.values.indexOf(e.value) === -1; });
          if (state.query) {
            rest = rest.filter(function (e) { return e.label.toLowerCase().indexOf(state.query) !== -1; });
          }
          var limit = state.more || state.query ? 1000 : Math.max(0, FP_FEW - (many ? picked.length : 0));
          if (!many) limit = 1000;
          picked.concat(rest.slice(0, limit)).forEach(function (e) { opts.appendChild(option(e)); });
          if (!picked.length && !rest.length) {
            opts.appendChild(fpEl("div", {class: "editr-fp-hint", text: "No values match."}));
          }
          if (rest.length > limit) {
            footer.appendChild(fpEl("button", {
              type: "button", class: "editr-link",
              text: "Show " + (rest.length - limit).toLocaleString("en-US") + " more",
              onclick: function () { state.more = true; draw(); }
            }));
          }
          if (card.values.length) {
            footer.appendChild(fpEl("button", {
              type: "button", class: "editr-link", text: "Clear",
              onclick: function () {
                fpTouch(card);
                card.values = [];
                fpChanged(card, true);
                draw();
              }
            }));
          }
        };
        draw();
        ui.update = function () {};
        return body;
      }

      function fpBoolUI(card, c, ui) {
        var vals = fpValues(c);
        var body = fpEl("div", {class: "editr-fp-bool", role: "group",
                                "aria-label": "Values of " + columnName(c)});
        var buttons = [];
        [["TRUE", vals.counts.TRUE || 0], ["FALSE", vals.counts.FALSE || 0],
         ["NA", vals.missing]].forEach(function (x) {
          var b = fpEl("button", {type: "button", class: "editr-fp-choice"}, [
            fpEl("span", {class: x[0] === "NA" ? "editr-fp-missing" : null,
                          text: x[0] === "NA" ? "missing" : x[0]}),
            fpEl("small", {text: x[1].toLocaleString("en-US") + (x[1] === 1 ? " row" : " rows")})
          ]);
          b.addEventListener("click", function () {
            fpTouch(card);
            var at = card.values.indexOf(x[0]);
            if (at === -1) card.values.push(x[0]);
            else card.values.splice(at, 1);
            fpChanged(card, true);
          });
          buttons.push([b, x[0]]);
          body.appendChild(b);
        });
        ui.update = function () {
          buttons.forEach(function (x) {
            x[0].setAttribute("aria-pressed", card.values.indexOf(x[1]) !== -1 ? "true" : "false");
          });
        };
        return body;
      }

      function fpWordsUI(card, c, ui) {
        var box = fpEl("div", {class: "editr-fp-words"});
        var input = fpEl("input", {type: "text", "aria-label": "Text to look for",
                                   placeholder: "Type a word and press Enter"});
        var add = function () {
          var s = input.value.trim();
          input.value = "";
          if (!s || card.values.indexOf(s) !== -1) return;
          fpTouch(card);
          card.values.push(s);
          fpChanged(card, true);
        };
        input.addEventListener("keydown", function (e) {
          if ((e.key === "Enter" || e.key === ",") && input.value.trim()) {
            e.preventDefault();
            add();
          } else if (e.key === "Backspace" && !input.value && card.values.length) {
            fpTouch(card);
            card.values.pop();
            fpChanged(card, true);
          }
        });
        input.addEventListener("blur", add);
        ui.update = function () {
          Array.prototype.slice.call(box.querySelectorAll(".editr-chip")).forEach(function (t) {
            box.removeChild(t);
          });
          card.values.forEach(function (v, i) {
            var chip = fpEl("span", {class: "editr-chip"}, [
              fpEl("span", {text: v}),
              fpEl("button", {type: "button", "aria-label": "Remove " + v, html: ICONS.x,
                              onclick: function () {
                                fpTouch(card);
                                card.values.splice(i, 1);
                                fpChanged(card, true);
                              }})
            ]);
            box.insertBefore(chip, input);
          });
          input.placeholder = card.values.length ? "" : "Type a word and press Enter";
        };
        box.appendChild(input);
        box.addEventListener("click", function (e) { if (e.target === box) input.focus(); });
        return box;
      }

      function fpPatternUI(card, c, ui) {
        var input = fpEl("input", {type: "text", class: "form-control form-control-sm editr-fp-pattern",
                                   "aria-label": "Pattern (regular expression)",
                                   placeholder: "e.g. ^Merc [0-9]+$", spellcheck: "false"});
        input.value = card.values[0] || "";
        input.addEventListener("input", function () {
          fpTouch(card);
          card.values = [input.value];
          fpChanged(card, false);
        });
        ui.update = function () {};
        return input;
      }

      function fpCardUI(card) {
        var c = grid.colMap[card.field];
        var ui = {};
        grid.fp.ui[card.id] = ui;
        var kind = fpKind(card, c);
        var root = fpEl("section", {class: "editr-fp-card" + (card.open ? " editr-fp-open" : ""),
                                    "data-card": card.id, "data-field": card.field,
                                    "aria-label": "Filter on " + columnName(c)});
        ui.root = root;
        var name = fpEl("button", {
          type: "button", class: "editr-fp-name", "aria-expanded": card.open ? "true" : "false",
          title: card.open ? "Fold this filter" : "Open this filter",
          onclick: function () {
            card.open = !card.open;
            fpRender();
            var again = grid.fp.ui[card.id];
            if (again) again.root.querySelector(".editr-fp-name").focus();
          }
        }, [fpEl("span", {class: "editr-badge", text: fpBadge(c)}),
            fpEl("span", {class: "editr-fp-col", text: columnName(c)})]);
        var head = fpEl("div", {class: "editr-fp-head"}, [name]);
        if (card.open) {
          var sel = fpEl("select", {class: "form-select form-select-sm editr-fp-logic",
                                    "aria-label": "Condition on " + columnName(c)});
          var keys = fpLogics(c);
          if (keys.indexOf(card.logic) === -1) keys.unshift(card.logic);
          keys.forEach(function (k) {
            sel.appendChild(fpEl("option", {value: k, text: fpLogicLabel(c, k)}));
          });
          sel.value = card.logic;
          sel.addEventListener("change", function () {
            fpTouch(card);
            var before = fpKind(card, c), had = FILTER_LOGICS[card.logic].input;
            card.logic = sel.value;
            var now = FILTER_LOGICS[card.logic].input;
            if (fpKind(card, c) !== before || now === "none") {
              card.values = now === "none" ? [] : before === "range" && fpKind(card, c) === "range" ?
                card.values.slice(0, now === "two" ? 2 : 1) : [];
            } else if (now !== had) {
              card.values = card.values.slice(0, now === "two" ? 2 : 1);
            }
            fpCommit(true);
            fpRender();
            var again = grid.fp.ui[card.id];
            if (again) again.root.querySelector(".editr-fp-logic").focus();
          });
          head.appendChild(sel);
        } else {
          ui.sum = fpEl("span", {class: "editr-fp-sum"});
          head.appendChild(ui.sum);
        }
        head.appendChild(fpEl("button", {
          type: "button", class: "editr-fp-remove", "aria-label": "Remove the filter on " + columnName(c),
          title: "Remove this filter", html: ICONS.x,
          onclick: function () { fpRemove(card); }
        }));
        root.appendChild(head);
        if (card.open && kind !== "none") {
          var body = kind === "range" ? fpRangeUI(card, c, ui) :
            kind === "list" ? fpListUI(card, c, ui) :
            kind === "bool" ? fpBoolUI(card, c, ui) :
            kind === "words" ? fpWordsUI(card, c, ui) : fpPatternUI(card, c, ui);
          root.appendChild(fpEl("div", {class: "editr-fp-body"}, [body]));
        }
        ui.msg = fpEl("div", {class: "editr-fp-msg", role: "status"});
        ui.keeps = fpEl("span", {class: "editr-fp-keeps"});
        var hint = card.source ? (card.source.indexOf("pick:") === 0 ?
                                  "From the column summary" : "From the box under the column name") :
          kind === "words" && /contain/.test(card.logic) ? "Ignores case" :
          kind === "words" || kind === "pattern" ? "Respects case" : "";
        root.appendChild(ui.msg);
        root.appendChild(fpEl("div", {class: "editr-fp-foot"}, [
          fpEl("span", {class: card.source ? "editr-fp-src" : "editr-fp-hint", text: hint}),
          ui.keeps
        ]));
        if (ui.update) ui.update();
        return root;
      }

      /* adding a filter: pick a column ----------------------------------- */

      function fpHint(c) {
        if (fpSecret(c)) return null;
        var type = filterType(c);
        var scale = (c.filter && c.filter.type === "values") || type === "logical" ? null : fpScale(c);
        if (scale) {
          var stats = fpStats(c, scale);
          if (stats && stats.bins.length > 1) {
            var spark = fpEl("span", {class: "editr-fp-spark", "aria-hidden": "true"});
            var bins = stats.bins, k = Math.ceil(bins.length / 8), sums = [];
            for (var i = 0; i < bins.length; i += k) {
              sums.push(bins.slice(i, i + k).reduce(function (a, b) { return a + b; }, 0));
            }
            var max = Math.max.apply(null, sums);
            sums.forEach(function (n) {
              var s = fpEl("span", {});
              s.style.height = (n ? Math.max(12, 100 * n / max) : 4) + "%";
              spark.appendChild(s);
            });
            return spark;
          }
        }
        var seen = Object.create(null), n = 0;
        var data = grid.table.getData();
        for (var r = 0; r < data.length && n <= 1000; r++) {
          var v = data[r][c.field];
          if (v === null || v === undefined || v === "") continue;
          var key = String(v);
          if (!seen[key]) {
            seen[key] = true;
            n += 1;
          }
        }
        return fpEl("span", {class: "editr-fp-pickhint",
                             text: n > 1000 ? "1000+ values" : n + (n === 1 ? " value" : " values")});
      }

      function fpClosePicker() {
        var p = grid.fp.picker;
        if (!p) return;
        grid.fp.picker = null;
        if (p.parentNode) p.parentNode.removeChild(p);
        document.removeEventListener("mousedown", grid.fp.pickerOutside, true);
        var add = fpBox() && fpBox().querySelector(".editr-fp-add");
        if (add) add.setAttribute("aria-expanded", "false");
      }

      function fpOpenPicker(add) {
        if (grid.fp.picker) {
          fpClosePicker();
          return;
        }
        var box = fpBox();
        var filtered = {};
        grid.fp.cards.forEach(function (x) { filtered[x.field] = true; });
        var cols = realColumns().map(function (col) {
          var c = grid.colMap[col.getField()];
          return {c: c, name: columnName(c), hidden: !col.isVisible()};
        });
        var picker = fpEl("div", {class: "editr-fp-picker", role: "dialog",
                                  "aria-label": "Choose a column to filter"});
        var search = fpEl("input", {type: "search", class: "form-control form-control-sm",
                                    placeholder: "Find a column", "aria-label": "Find a column"});
        var list = fpEl("div", {class: "editr-fp-cols", role: "listbox", "aria-label": "Columns"});
        picker.appendChild(search);
        picker.appendChild(list);
        var shown = [], active = 0;
        var draw = function () {
          list.innerHTML = "";
          var q = search.value.trim().toLowerCase();
          shown = cols.filter(function (x) { return !q || x.name.toLowerCase().indexOf(q) !== -1; });
          active = Math.min(active, Math.max(0, shown.length - 1));
          shown.forEach(function (x, i) {
            var opt = fpEl("button", {
              type: "button", role: "option", class: "editr-fp-colopt",
              "aria-selected": i === active ? "true" : "false", "data-field": x.c.field,
              onclick: function () {
                fpClosePicker();
                fpAdd(x.c.field);
              }
            }, [fpEl("span", {class: "editr-badge", text: fpBadge(x.c)}),
                fpEl("span", {class: "editr-fp-col", text: x.name}),
                x.hidden ? fpEl("span", {class: "editr-fp-pickhint", text: "hidden"}) : null,
                filtered[x.c.field] ? fpEl("span", {class: "editr-fp-pickhint editr-fp-isfiltered",
                                                    text: "filtered"}) : null,
                fpEl("span", {class: "editr-spacer"}),
                fpHint(x.c)]);
            list.appendChild(opt);
          });
          if (!shown.length) list.appendChild(fpEl("div", {class: "editr-fp-hint", text: "No column matches."}));
        };
        search.addEventListener("input", function () {
          active = 0;
          draw();
        });
        search.addEventListener("keydown", function (e) {
          if (e.key === "ArrowDown" || e.key === "ArrowUp") {
            e.preventDefault();
            active = Math.max(0, Math.min(shown.length - 1, active + (e.key === "ArrowDown" ? 1 : -1)));
            draw();
            var cur = list.querySelector('[aria-selected="true"]');
            if (cur && cur.scrollIntoView) cur.scrollIntoView({block: "nearest"});
          } else if (e.key === "Enter") {
            e.preventDefault();
            if (shown[active]) {
              var field = shown[active].c.field;
              fpClosePicker();
              fpAdd(field);
            }
          } else if (e.key === "Escape") {
            e.preventDefault();
            e.stopPropagation();
            fpClosePicker();
            add.focus();
          }
        });
        add.parentNode.insertBefore(picker, add.nextSibling);
        add.setAttribute("aria-expanded", "true");
        grid.fp.picker = picker;
        grid.fp.pickerOutside = function (e) {
          if (!picker.contains(e.target) && e.target !== add && !add.contains(e.target)) fpClosePicker();
        };
        document.addEventListener("mousedown", grid.fp.pickerOutside, true);
        draw();
        search.focus({preventScroll: true});
        if (picker.scrollIntoView) picker.scrollIntoView({block: "nearest"});
      }

      /* the panel ------------------------------------------------------ */

      function fpRender() {
        var box = fpBox();
        if (!box || !grid.table) return;
        fpClosePicker();
        grid.fp.ui = {};
        box.innerHTML = "";
        var cards = grid.fp.cards.filter(function (c) { return grid.colMap[c.field]; });
        var main = cards.filter(function (c) { return !(grid.matchAny && c.source); });
        var extra = grid.matchAny ? cards.filter(function (c) { return c.source; }) : [];
        var wrap = fpEl("div", {class: "editr-fp"});
        if (main.length >= 2 || grid.matchAny) {
          var seg = function (any, label) {
            return fpEl("button", {
              type: "button", role: "radio", class: "editr-fp-seg",
              "aria-checked": !!grid.matchAny === any ? "true" : "false", text: label,
              onclick: function () { if (!!grid.matchAny !== any) fpSetMatch(any); }
            });
          };
          wrap.appendChild(fpEl("div", {class: "editr-fp-match"}, [
            fpEl("span", {text: "Show rows that match"}),
            fpEl("span", {class: "editr-fp-segs", role: "radiogroup", "aria-label": "Match"},
                 [seg(false, "all"), seg(true, "any")]),
            fpEl("span", {text: "of these"})
          ]));
        }
        var list = fpEl("div", {class: "editr-fp-list"});
        main.forEach(function (card, i) {
          if (i) list.appendChild(fpEl("div", {class: "editr-fp-join", text: grid.matchAny ? "or" : "and"}));
          list.appendChild(fpCardUI(card));
        });
        if (extra.length) {
          list.appendChild(fpEl("div", {class: "editr-fp-also",
                                        text: "And also, from the boxes under the column names:"}));
          extra.forEach(function (card, i) {
            if (i) list.appendChild(fpEl("div", {class: "editr-fp-join", text: "and"}));
            list.appendChild(fpCardUI(card));
          });
        }
        if (!cards.length) {
          list.appendChild(fpEl("div", {class: "editr-fp-empty"}, [
            fpEl("strong", {text: "No filters"}),
            fpEl("span", {text: "Filters choose the rows to show. The data is not changed."})
          ]));
        }
        wrap.appendChild(list);
        var add = fpEl("button", {type: "button", class: "editr-fp-add", "aria-haspopup": "dialog",
                                  "aria-expanded": "false",
                                  html: ICONS.plus + "<span>Add filter</span>"});
        add.addEventListener("click", function () { fpOpenPicker(add); });
        wrap.appendChild(add);
        box.appendChild(wrap);
        box.appendChild(fpEl("div", {class: "editr-fp-total"}, [
          fpEl("div", {class: "editr-fp-totalrow"}, [
            fpEl("span", {class: "editr-fp-count", "aria-live": "polite"}),
            cards.length ? fpEl("button", {type: "button", class: "editr-link editr-fp-clear",
                                           text: "Clear all", onclick: fpClearAll}) : null
          ]),
          fpEl("div", {class: "editr-fp-bar", "aria-hidden": "true"}, [fpEl("span", {})])
        ]));
        fpUpdateCounts();
      }

      function clearFilterSource(source) {
        var kind = source.split(":")[0];
        var field = source.slice(kind.length + 1);
        if (kind === "pick") delete grid.picks[field];
        else grid.table.setHeaderFilterValue(field, "");
      }

      grid.filterPanel = {
        show: fpShow, add: fpAdd, clear: fpClearAll, match: fpSetMatch, sync: fpSync
      };

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
          headerMenu: c.frozen ? undefined : headerMenu,
          headerMenuIcon: ICONS.more,
          cssClass: c.readonly ? "editr-readonly" : ""
        };
        if (grid.opts.filters !== false) addHeaderFilter(def, c);
        if (c.kind === "numeric") def.hozAlign = "right";
        var o = c.option;
        var lab = labelsOf(c);
        if (lab) {
          // pick a label, or type a value or a label
          def.editor = c.readonly ? false : "list";
          def.editorParams = {
            values: lab.list.map(function (x) {
              return {label: x.value + " \u00b7 " + x.label, value: x.value};
            }),
            autocomplete: true, freetext: true, listOnEmpty: true,
            allowEmpty: true
          };
          def.formatter = makeFormatter("labelled");
          def.hozAlign = "left";
          def.minWidth = Math.max(def.minWidth, 110);
        }
        if (o && o.type === "checkbox") {
          def.editor = c.readonly ? false : "tickCross";
          def.editorParams = {tristate: false};
          def.formatter = makeFormatter("tickCross",
                                        {allowEmpty: true, crossElement: false});
          def.hozAlign = "center";
        } else if (o && o.type === "dropdown") {
          def.editor = c.readonly ? false : "list";
          var choiceLabels = toArray(o.labels);
          def.editorParams = {
            values: choiceLabels.length ? toArray(o.values).map(function (v, i) {
              return {label: String(choiceLabels[i]), value: v};
            }) : toArray(o.values),
            autocomplete: true, listOnEmpty: true, allowEmpty: true,
            clearable: true
          };
          if (choiceLabels.length) {
            def.formatter = makeFormatter("choice");
            if (def.headerFilter === "list") {
              def.headerFilterParams.itemFormatter = function (label, value) {
                return escapeHTML(choiceLabel(c, value) || String(value));
              };
            }
          }
        } else if (o && o.type === "date") {
          def.editor = c.readonly ? false : "date";
        } else if (o && o.type === "password") {
          def.editorParams = {elementAttributes: {type: "password"}};
          def.formatter = makeFormatter("password");
        } else if (o && o.type === "datetime") {
          def.editor = c.readonly ? false : pickerEditor("datetime-local");
          def.minWidth = Math.max(def.minWidth, 170);
        } else if (o && o.type === "time") {
          def.editor = c.readonly ? false : pickerEditor("time");
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
        } else if (o && o.type === "html") {
          // edited as HTML text over several lines
          def.editor = c.readonly ? false : "textarea";
          def.editorParams = {enterSubmit: true, shiftEnterNewLine: true,
                              altEnterNewLine: true, verticalNavigation: "editor"};
          def.formatter = makeFormatter("html");
          def.variableHeight = true;
          def.minWidth = Math.max(def.minWidth, 140);
        } else if (o && o.type === "colour") {
          def.formatter = makeFormatter("colour");
          def.editor = c.readonly ? false : colourEditor;
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
        // widths set with col_widths; other columns fit their values but
        // long text starts at most 320 px wide (drag the header to widen it)
        if (c.width) {
          def.width = Math.max(Number(c.width), 40);
          def.minWidth = Math.min(def.minWidth, def.width);
        } else if (!def.maxInitialWidth) {
          def.maxInitialWidth = 320;
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
        ops.forEach(function (op) { emit(op, "fill"); });
      }

      function rowMenu(e, row) {
        var items = [];
        var cellEl = e && e.target && e.target.closest ? e.target.closest(".tabulator-cell") : null;
        var field = cellEl ? cellEl.getAttribute("tabulator-field") : null;
        if (field && grid.colMap[field] && grid.opts.comments !== false) {
          var cell = row.getCell(field);
          var has = !!grid.comments[row.getIndex() + "|" + field];
          items.push({label: (has ? "Edit comment\u2026" : "Add comment\u2026") + "  (Shift+F2)",
                      action: function () { openComment(cell); }});
          if (has) {
            items.push({label: "Delete comment", action: function () {
              setComment(row.getIndex(), field, "");
            }});
          }
        }
        var nComments = Object.keys(grid.comments).length;
        if (nComments && grid.opts.comments !== false) {
          items.push({label: "Delete all comments (" + nComments + ")\u2026",
                      action: function () {
                        confirmDeleteComments(cellEl || row.getElement());
                      }});
        }
        items.push({label: "Fill down  (Ctrl+D)", action: fillDown});
        // move the cells below the selection down (or up), in its columns only
        var filtered = activeFilters().length > 0;
        var shiftNote = filtered ? "  (clear the filters first)" : "";
        items.push({label: "Insert cells, shift down" + shiftNote, disabled: filtered,
                    action: function () { shiftCells(true); }});
        items.push({label: "Delete cells, shift up" + shiftNote, disabled: filtered,
                    action: function () { shiftCells(false); }});
        items.push({separator: true});
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
        // columns next to the clicked cell (as in the column menu)
        var column = field && grid.colMap[field] ? grid.table.getColumn(field) : null;
        if (grid.opts.col_edit && column) {
          if (!grid.colMap[field].frozen) {
            items.push({label: "Insert column left",
                        action: function () { insertColumn(column, false); }});
          }
          items.push({label: "Insert column right",
                      action: function () { insertColumn(column, true); }});
          items.push({separator: true});
        }
        return items.concat(historyItems());
      }

      /* comments on cells ---------------------------------------------- */

      function commentSet(args) {
        var out = {};
        var keys = toArray(args && args.keys);
        var texts = toArray(args && args.texts);
        var users = toArray(args && args.users);
        var times = toArray(args && args.times);
        keys.forEach(function (k, i) {
          out[k] = {text: String(texts[i]), user: users[i] || "", time: times[i] || ""};
        });
        return out;
      }

      function openComment(cell) {
        if (!cell) return;
        var id = cell.getRow().getIndex();
        var field = cell.getField();
        var c = grid.colMap[field];
        if (!c) return;
        var cur = grid.comments[id + "|" + field];
        var box = document.createElement("div");
        box.className = "editr-popup editr-comment-box";
        box.setAttribute("role", "dialog");
        box.setAttribute("aria-label", "Comment on " + c.title);
        var meta = cur && (cur.user || cur.time) ?
          [cur.user, cur.time].filter(function (x) { return x; }).join(" \u00b7 ") : "";
        box.innerHTML = '<div class="editr-comment-title">Comment on ' + escapeHTML(c.title) +
          '</div>' + (meta ? '<div class="editr-comment-meta">' + escapeHTML(meta) + '</div>' : '') +
          '<textarea class="form-control form-control-sm" rows="3" aria-label="Comment"' +
          ' placeholder="Why the value is what it is, what to check\u2026"></textarea>' +
          '<div class="editr-comment-actions">' +
          (cur ? '<button type="button" class="btn btn-link btn-sm editr-comment-delete" data-act="delete">Delete</button>' : '') +
          '<span class="editr-spacer"></span>' +
          '<button type="button" class="btn btn-outline-secondary btn-sm" data-act="cancel">Cancel</button>' +
          '<button type="button" class="btn btn-primary btn-sm" data-act="save">Save</button></div>';
        var ta = box.querySelector("textarea");
        ta.value = cur ? cur.text : "";
        var finish = function (act) {
          if (act === "save") setComment(id, field, ta.value.trim());
          else if (act === "delete") setComment(id, field, "");
          closePopup();
        };
        box.addEventListener("click", function (e) {
          var a = e.target.closest && e.target.closest("[data-act]");
          if (a) finish(a.getAttribute("data-act"));
        });
        ta.addEventListener("keydown", function (e) {
          e.stopPropagation();
          if (e.key === "Enter" && (e.ctrlKey || e.metaKey)) {
            e.preventDefault();
            finish("save");
          } else if (e.key === "Escape") {
            e.preventDefault();
            finish("cancel");
          }
        });
        var popup = openPopup(cell.getElement(), box);
        if (popup) popup.kind = "comment";
        // at once, so typing straight away goes into the box (and again
        // after a menu click has finished)
        ta.focus();
        setTimeout(function () {
          if (document.activeElement !== ta && document.body.contains(ta)) ta.focus();
        }, 0);
      }

      // R keeps the comments (with who wrote them and when) and sends them
      // back with set_comments
      function setComment(id, field, text) {
        var key = id + "|" + field;
        if (text) {
          var cur = grid.comments[key];
          grid.comments[key] = {text: text, user: cur ? cur.user : "", time: cur ? cur.time : ""};
        } else {
          delete grid.comments[key];
        }
        reformatRows([id]);
        if (formVisible()) refreshForm();
        if (shinyOpen()) {
          Shiny.setInputValue(el.id + "_comment",
                              {id: id, field: field, text: text, nonce: Math.random()},
                              {priority: "event"});
        }
      }

      // comments cannot be undone, so deleting them all is confirmed first
      function confirmDeleteComments(anchor) {
        var n = Object.keys(grid.comments).length;
        if (!n) return;
        var box = document.createElement("div");
        box.className = "editr-popup editr-guard editr-comments-clear";
        box.setAttribute("role", "alertdialog");
        box.setAttribute("aria-label", "Delete all comments");
        box.innerHTML =
          '<div class="editr-guard-msg"><span class="editr-guard-icon">' + ICONS.alert +
          '</span><div><div class="editr-guard-title">Delete all ' + n + ' comment' +
          (n === 1 ? "" : "s") + '?</div>' +
          '<div class="editr-guard-sub">Deleted comments cannot be brought back with undo.</div>' +
          '</div></div><div class="editr-guard-actions">' +
          '<button type="button" class="btn btn-danger btn-sm" data-act="delete">Delete all comments</button>' +
          '<button type="button" class="btn btn-link btn-sm" data-act="cancel">Cancel</button></div>';
        box.addEventListener("click", function (e) {
          var a = e.target.closest && e.target.closest("[data-act]");
          if (!a) return;
          closePopup();
          if (a.getAttribute("data-act") === "delete") deleteAllComments();
        });
        var popup = openPopup(anchor, box);
        if (popup) popup.kind = "guard";
        var cancel = box.querySelector('[data-act="cancel"]');
        if (cancel) cancel.focus();
      }

      function deleteAllComments() {
        var ids = {};
        Object.keys(grid.comments).forEach(function (k) { ids[k.split("|")[0]] = true; });
        grid.comments = {};
        reformatRows(Object.keys(ids).map(Number));
        if (formVisible()) refreshForm();
        if (shinyOpen()) {
          Shiny.setInputValue(el.id + "_comment", {all: true, nonce: Math.random()},
                              {priority: "event"});
        }
      }

      function activeCell() {
        var ranges = grid.table && grid.table.getRanges ? grid.table.getRanges() : [];
        if (!ranges.length) return null;
        var cells = ranges[0].getCells();
        var first = Array.isArray(cells[0]) ? cells[0][0] : cells[0];
        return first || null;
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
          // nothing goes before the row names (frozen on the left)
          if (!c.frozen) {
            items.push({label: "Insert column left",
                        action: function (e, col) { insertColumn(col, false); }});
          }
          items.push({label: "Insert column right",
                      action: function (e, col) { insertColumn(col, true); }});
          if (!c.frozen) {
            items.push({label: "Delete column", action: function (e, col) {
              var cid = col.getField();
              delete grid.colMap[cid];
              col.delete();
              if (grid.picks[cid] || grid.conditions.some(function (r) { return r.field === cid; })) {
                grid.table.refreshFilter();
              }
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
        var actions = actionItems(c);
        if (actions.length) {
          if (items.length) items.push({separator: true});
          items = items.concat(actions);
        }
        if (items.length) items.push({separator: true});
        return items.concat(historyItems());
      }

      // whole-column actions run in R and are recorded as one dplyr verb
      function actionItems(c) {
        var items = [];
        if (grid.opts.actions === false || c.frozen) return items;
        var ask = function (kind, extra) {
          return function () { requestAction(kind, c.field, extra); };
        };
        if (grid.opts.col_edit || !c.readonly) {
          items.push({label: "Formula\u2026", action: ask("formula")});
        }
        if (!c.readonly) {
          items.push({label: "Find and replace\u2026", action: ask("replace")});
          items.push({label: "Recode values\u2026", action: ask("recode")});
          if (!grid.opts.db) {
            items.push({label: "Change type", menu: [
              {label: "Number", action: ask("type", {to: "numeric"})},
              {label: "Whole number", action: ask("type", {to: "integer"})},
              {label: "Text", action: ask("type", {to: "character"})},
              {label: "TRUE / FALSE", action: ask("type", {to: "logical"})},
              {label: "Date", action: ask("type", {to: "Date"})},
              {label: "Factor", action: ask("type", {to: "factor"})}
            ]});
          }
        }
        if (!grid.opts.db) {
          items.push({label: "Sort ascending", action: ask("sort", {desc: false})});
          items.push({label: "Sort descending", action: ask("sort", {desc: true})});
        }
        if (!c.readonly && !grid.opts.db) {
          items.push({label: "Labels\u2026", action: ask("labels")});
        }
        items.push({label: "Rules\u2026", action: ask("rules")});
        items.push({label: "Conditional formatting\u2026", action: ask("formats")});
        return items;
      }

      function requestAction(kind, cid, extra) {
        if (!shinyOpen()) return;
        // R must have every edit before it changes the whole column
        if (grid.timer) {
          clearTimeout(grid.timer);
          flush();
        }
        var msg = {kind: kind, cid: cid, nonce: Math.random()};
        Object.keys(extra || {}).forEach(function (k) { msg[k] = extra[k]; });
        setTimeout(function () {
          Shiny.setInputValue(el.id + "_action", msg, {priority: "event"});
        }, 0);
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
        renderChips();
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
      // Rows past the end are added when rows can be added, and columns
      // past the last column when columns can be added.
      function pasteRange(parsed) {
        var table = grid.table;
        var mod = table.modules.selectRange;
        var range = mod && mod.activeRange;
        var block = parsed && parsed.rows;
        // Ctrl+Shift+V: the first row names the columns
        var names = null;
        if (grid.pasteHeaders && Date.now() - grid.pasteHeaders < 3000 &&
            block && block.length) {
          names = block[0];
          block = block.slice(1);
        }
        grid.pasteHeaders = 0;
        if (!range || !block || (!block.length && !names)) return [];
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
        var dataCols = Math.max.apply(null, (names ? [names] : []).concat(block)
          .map(function (r) { return r.length; }));
        var selRows = Math.max(1, r1 - r0 + 1), selCols = Math.max(1, c1 - c0 + 1);
        var nRows = selRows > dataRows && selRows % dataRows === 0 ? selRows : dataRows;
        var nCols = selCols > dataCols && selCols % dataCols === 0 ? selCols : dataCols;

        // target columns: the columns from the selection on, then new
        // columns (V1, V2, ...) for the rest of a wider block, or nothing
        // when columns cannot be added
        var room = 0;
        for (var q = c0; q < cols.length; q++) {
          if (grid.colMap[cols[q].getField()]) room += 1;
        }
        var titles = currentTitles();
        var targets = [];
        for (var j = 0; j < nCols; j++) {
          var col = j < room ? cols[c0 + j] : null;
          var c = col && grid.colMap[col.getField()];
          if (!c && j >= room && grid.opts.col_edit) {
            var title = uniqueName("V", titles);
            titles.push(title);
            c = {field: null, title: title, kind: "character", readonly: false,
                 rename: true, frozen: false, empty: true,
                 filter: {type: "text"}, added: true};
          }
          targets.push(c ? c : null);
        }
        var existing = rows.slice(r0, r0 + nRows);
        var extra = grid.opts.row_edit ? nRows - existing.length : 0;

        // column names from the first row: new columns are created with them,
        // other columns are renamed (when their names can be edited)
        var renames = [];
        if (names) {
          targets.forEach(function (c, j) {
            var nm = j < names.length ? String(names[j]).trim() : "";
            if (!c || !nm) return;
            if (c.added) c.title = nm;
            else if (c.rename && nm !== c.title) renames.push({c: c, name: nm});
          });
        }

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
        writeBlock(bounds.start.getComponent(), existing, extra, cells,
                   {label: "paste", renames: renames});
        return [];
      }

      // check the values going into each column, then write them, asking
      // first when some do not fit their column
      function writeBlock(anchor, existing, extra, cells, how) {
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
          applyPaste(existing, extra, cells, act, how);
        };
        if (problems.length) {
          pasteGuard(anchor, problems, apply, how.label);
        } else {
          apply(how.label);
        }
      }

      // write a checked paste or fill: add columns and rows, update cells,
      // report one batch of ops (one undo step) and select what was written
      // (how.select: [first row, last row, first field, last field])
      function applyPaste(existing, extra, cells, act, how) {
        var table = grid.table;
        how = how || {label: "paste"};
        var label = how.label;
        if (act === "cancel") return;
        // NEW COLUMNS AFTER THE LAST COLUMN (A PASTE WIDER THAN THE TABLE)
        var after = null;
        cells.forEach(function (col) {
          if (!col.c.added || col.c.field) return;
          if (!after) {
            var real = realColumns();
            after = real.length ? real[real.length - 1].getField() : null;
          }
          var cid = "c" + grid.nextCol++;
          col.c.field = cid;
          grid.colMap[cid] = col.c;
          if (after) {
            table.addColumn(buildColumn(col.c), false, after);
          } else {
            table.addColumn(buildColumn(col.c), true);
          }
          emit({type: "insert_col", cid: cid, name: col.c.title,
                anchor: after, right: true}, label);
          after = cid;
        });
        // COLUMNS RENAMED FROM THE FIRST ROW OF A PASTE (Ctrl+Shift+V)
        (how.renames || []).forEach(function (r) {
          setTitle(r.c.field, r.name);
          emit({type: "rename_col", cid: r.c.field, name: r.name}, label);
        });
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
          emit({type: "insert_rows", ids: newIds, anchor: null, below: true}, label);
        }
        ops.forEach(function (op) { emit(op, label); });
        var done = newRows.length ? table.addData(newRows, false) : Promise.resolve();
        Promise.resolve(done).then(function () {
          var fields = cells.map(function (col) { return col.c.field; });
          var sel = how.select;
          if (!sel) {
            if (!fields.length || !ids.length) return;
            sel = [table.getRow(ids[0]), table.getRow(ids[ids.length - 1]),
                   fields[0], fields[fields.length - 1]];
          }
          if (!table.addRange || !sel[0] || !sel[1]) return;
          table.getRanges().forEach(function (r) { r.remove(); });
          table.addRange(sel[0].getCell(sel[2]), sel[1].getCell(sel[3]));
          updateStatus();
          selectionSummary();
        });
      }

      // Insert empty cells where the selection is and move the cells below
      // down (down = TRUE), or delete the selected cells and move the cells
      // below up, in the selected columns only (Excel's "shift cells").
      // Values pushed past the last row get new rows (when rows can be
      // added). One undo step.
      function shiftCells(down) {
        var table = grid.table;
        var mod = table.modules.selectRange;
        var range = mod && mod.activeRange;
        if (!range || activeFilters().length) return;
        var bounds = range.getBounds();
        if (!bounds.start || !bounds.end) return;
        var rows = table.rowManager.activeRows.slice();
        var cols = table.columnManager.getVisibleColumnsByIndex();
        var r0 = rows.indexOf(bounds.start.row), r1 = rows.indexOf(bounds.end.row);
        var c0 = cols.indexOf(bounds.start.column), c1 = cols.indexOf(bounds.end.column);
        if (r0 < 0 || r1 < 0 || c0 < 0 || c1 < 0) return;
        if (r1 < r0) { var t = r0; r0 = r1; r1 = t; }
        if (c1 < c0) { var u = c0; c0 = c1; c1 = u; }
        var k = r1 - r0 + 1;
        var below = rows.slice(r0);
        var cells = [], extra = 0;
        for (var j = c0; j <= c1; j++) {
          var c = grid.colMap[cols[j].getField()];
          if (!c || c.readonly) continue;
          var vals = below.map(function (row) {
            var v = row.getData()[c.field];
            return v === undefined ? null : v;
          });
          var blanks = vals.slice(0, k).map(function () { return null; });
          var out = down ? blanks.concat(vals) : vals.slice(k).concat(blanks);
          if (down) {
            // values pushed past the last row
            var over = out.slice(below.length);
            var last = -1;
            over.forEach(function (v, i) { if (!isBlank(v)) last = i; });
            extra = Math.max(extra, last + 1);
          }
          cells.push({c: c, values: out});
        }
        if (!cells.length) return;
        if (extra > 0 && !grid.opts.row_edit) {
          shiftRefused(bounds.start.getComponent(), extra);
          return;
        }
        cells.forEach(function (col) {
          col.values = col.values.slice(0, below.length + extra);
          while (col.values.length < below.length + extra) col.values.push(null);
        });
        var first = rows[r0].getComponent(), lastRow = rows[r1].getComponent();
        writeBlock(bounds.start.getComponent(), below, extra, cells, {
          label: "shift",
          select: [first, lastRow, cols[c0].getField(), cols[c1].getField()]
        });
      }

      function shiftRefused(anchor, n) {
        var box = document.createElement("div");
        box.className = "editr-popup editr-guard";
        box.setAttribute("role", "alertdialog");
        box.setAttribute("aria-label", "Cells cannot be inserted");
        box.innerHTML =
          '<div class="editr-guard-msg"><span class="editr-guard-icon">' + ICONS.alert +
          '</span><div><div class="editr-guard-title">No room to move the cells down</div>' +
          '<div class="editr-guard-sub">' + n + ' value' + (n === 1 ? "" : "s") +
          ' would move past the last row, and rows cannot be added.</div></div></div>' +
          '<div class="editr-guard-actions"><button type="button" class="btn btn-primary btn-sm" data-act="ok">OK</button></div>';
        box.addEventListener("click", function (e) {
          if (e.target.closest && e.target.closest("[data-act]")) closePopup();
        });
        openPopup(anchor.getElement(), box);
        var ok = box.querySelector("button");
        if (ok) ok.focus();
      }

      /* ---------------------------------------------------------------- */
      /* fill handle                                                       */
      /* ---------------------------------------------------------------- */

      // Values for the cells a selection is dragged over (pos: positions
      // counted from the first selected cell; -1 is the cell before it):
      // two or more numbers or dates a fixed step apart, or text ending in
      // such numbers (Week 1, Week 2), carry on as a series; anything else
      // is repeated. copy = TRUE repeats series too.
      function fillValues(src, pos, copy) {
        var k = src.length;
        var series = copy || k < 2 ? null : fillSeries(src);
        return pos.map(function (p) {
          return series ? series(p) : src[((p % k) + k) % k];
        });
      }

      function fillSeries(src) {
        var steady = function (xs) {
          var step = xs[1] - xs[0];
          if (!isFinite(step) || step === 0) return null;
          for (var i = 2; i < xs.length; i++) {
            if (Math.abs(xs[i] - xs[i - 1] - step) > 1e-9 * Math.max(1, Math.abs(step))) {
              return null;
            }
          }
          return step;
        };
        var decimals = function (x) {
          var m = /\.(\d+)$/.exec(String(x));
          return m ? m[1].length : 0;
        };
        var round = function (x, d) {
          return Number(x.toFixed(Math.min(d, 15)));
        };
        // numbers
        if (src.every(function (v) { return typeof v === "number" && isFinite(v); })) {
          var step = steady(src);
          if (step === null) return null;
          var d = Math.max.apply(null, src.map(decimals));
          return function (p) { return round(src[0] + step * p, d); };
        }
        if (!src.every(function (v) { return typeof v === "string"; })) return null;
        // dates: months apart on the same day, or days apart
        var dates = src.map(function (v) {
          return /^\d{4}-\d{2}-\d{2}$/.test(v) && validDate(v) ? v : null;
        });
        if (dates.every(function (v) { return v; })) {
          var ymd = dates.map(function (v) { return v.split("-").map(Number); });
          var months = ymd.map(function (x) { return x[0] * 12 + x[1] - 1; });
          var mstep = steady(months);
          var lastDay = function (y, mo) { return new Date(Date.UTC(y, mo + 1, 0)).getUTCDate(); };
          var ends = ymd.every(function (x) { return x[2] === lastDay(x[0], x[1] - 1); });
          if (mstep !== null && (ends || ymd.every(function (x) { return x[2] === ymd[0][2]; }))) {
            return function (p) {
              var m = months[0] + mstep * p;
              var y = Math.floor(m / 12), mo = m - y * 12;
              // month ends stay month ends; the 31st becomes the last day of
              // shorter months
              var day = ends ? lastDay(y, mo) : Math.min(ymd[0][2], lastDay(y, mo));
              return y + "-" + pad2(mo + 1) + "-" + pad2(day);
            };
          }
          var days = ymd.map(function (x) { return Date.UTC(x[0], x[1] - 1, x[2]) / 864e5; });
          var dstep = steady(days);
          if (dstep === null) return null;
          return function (p) {
            return new Date((days[0] + dstep * p) * 864e5).toISOString().slice(0, 10);
          };
        }
        // text ending in a number
        var parts = src.map(function (v) { return /^(.*?)(\d+)$/.exec(v); });
        if (parts.every(function (m) { return m && m[1] === parts[0][1]; })) {
          var nums = parts.map(function (m) { return Number(m[2]); });
          var nstep = steady(nums);
          if (nstep === null) return null;
          var width = parts[0][2].charAt(0) === "0" ? parts[0][2].length : 0;
          return function (p) {
            var n = nums[0] + nstep * p;
            if (n < 0) return src[((p % src.length) + src.length) % src.length];
            var t = String(n);
            while (t.length < width) t = "0" + t;
            return parts[0][1] + t;
          };
        }
        return null;
      }

      // text of a value for another column (filling across columns)
      function fillText(v) {
        if (v === null || v === undefined) return v;
        if (typeof v === "boolean") return v ? "TRUE" : "FALSE";
        return String(v);
      }

      // Tabulator draws the fill handle (selectableRangeFill) and the
      // preview of the cells it is dragged over; we work out the values and
      // write them like a paste: one undo step, logged as a fill, values
      // that do not fit a column are asked about. Hold Ctrl to copy without
      // carrying on a series.
      function patchFillHandle(table) {
        var mod = table.modules.selectRange;
        var fh = mod && mod.fillHandle;
        if (!fh) return;
        fh.element.setAttribute("title", "Drag to fill (Ctrl: copy without a series)");
        fh.handleMouseUp = function (e) {
          fh.isActive = false;
          fh.unsubscribe("cell-mousemove", fh.handleCellMouseMove);
          document.removeEventListener("mouseup", fh.handleMouseUp);
          var src = fh.source;
          var target = fh.constructor.extendAlongAxis(src, fh.pointerRow, fh.pointerCol);
          if (fh.preview) {
            fh.preview.destroy();
            fh.preview = null;
          }
          fillArea(src, target, !!(e && (e.ctrlKey || e.metaKey)));
        };
      }

      function fillArea(src, target, copy) {
        var mod = grid.table.modules.selectRange;
        var rows = mod.getTableRows();
        var cols = mod.getTableColumns();
        var down = target.top !== src.top || target.bottom !== src.bottom;
        var across = target.left !== src.left || target.right !== src.right;
        if (!down && !across) return;
        var fieldAt = function (j) { return cols[j] && cols[j].getField(); };
        var defAt = function (j) { return grid.colMap[fieldAt(j)]; };
        var outside = function (lo, hi, a, b) {
          var out = [];
          for (var i = lo; i <= hi; i++) if (i < a || i > b) out.push(i);
          return out;
        };
        var existing, cells = [];
        if (down) {
          var tRows = outside(target.top, target.bottom, src.top, src.bottom);
          existing = tRows.map(function (r) { return rows[r]; });
          var pos = tRows.map(function (r) { return r - src.top; });
          for (var j = src.left; j <= src.right; j++) {
            var c = defAt(j);
            if (!c || c.readonly) continue;
            var vals = [];
            for (var r = src.top; r <= src.bottom; r++) vals.push(rows[r].getData()[c.field]);
            cells.push({c: c, values: fillValues(vals, pos, copy)});
          }
        } else {
          existing = [];
          var srcRows = [];
          for (var r2 = src.top; r2 <= src.bottom; r2++) {
            existing.push(rows[r2]);
            var v2 = [];
            for (var j2 = src.left; j2 <= src.right; j2++) {
              var d2 = defAt(j2);
              v2.push(d2 ? rows[r2].getData()[d2.field] : null);
            }
            srcRows.push(v2);
          }
          outside(target.left, target.right, src.left, src.right).forEach(function (tc) {
            var c = defAt(tc);
            if (!c || c.readonly) return;
            var values = srcRows.map(function (v) {
              var x = fillText(fillValues(v, [tc - src.left], copy)[0]);
              return isBlank(x) ? null : normalizeValue(c, x);
            });
            cells.push({c: c, values: values});
          });
        }
        if (!cells.length || !existing.length) return;
        // SELECT THE SELECTION AND WHAT WAS FILLED (REAL COLUMNS ONLY)
        var lo = target.left, hi = target.right;
        while (lo < hi && !defAt(lo)) lo++;
        while (hi > lo && !defAt(hi)) hi--;
        writeBlock(existing[0].getComponent().getCell(cells[0].c.field), existing, 0, cells, {
          label: "fill",
          select: [rows[target.top].getComponent(), rows[target.bottom].getComponent(),
                   fieldAt(lo), fieldAt(hi)]
        });
      }

      // one question for all the pasted values that do not fit their columns
      function pasteGuard(anchorCell, problems, decide, label) {
        var fill = label === "fill";
        var total = problems.reduce(function (a, p) { return a + p.n; }, 0);
        var text = problems.every(function (p) { return canBecomeText(p.c); });
        var box = document.createElement("div");
        box.className = "editr-popup editr-guard";
        box.setAttribute("role", "alertdialog");
        box.setAttribute("aria-label", (fill ? "Filled" : "Pasted") +
                         " values do not match the column types");
        box.innerHTML =
          '<div class="editr-guard-msg"><span class="editr-guard-icon">' + ICONS.alert +
          '</span><div><div class="editr-guard-title">' + total +
          (fill ? ' filled value' : ' pasted value') +
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
          '<button type="button" class="btn btn-link btn-sm" data-act="cancel">Cancel ' +
          (fill ? "fill" : "paste") + '</button></div>';
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
        var picks = Object.keys(grid.picks || {}).filter(function (f) {
          return grid.colMap[f];
        }).map(function (f) { return {field: f, pick: grid.picks[f]}; });
        var conditions = (grid.conditions || []).filter(function (r) {
          return grid.colMap[r.field];
        }).map(function (r) { return {field: r.field, cond: r}; });
        return picks.concat(grid.table.getHeaderFilters().filter(function (f) {
          var v = f.value;
          if (v === "" || v === null || v === undefined) return false;
          if (Array.isArray(v)) return v.length > 0;
          if (typeof v === "object") return v.min !== "" || v.max !== "";
          return true;
        }), conditions);
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
        if (f.cond) return conditionText(f.cond);
        var c = grid.colMap[f.field] || {title: f.field};
        var name = columnName(c);
        var v = f.value;
        if (f.pick) {
          var p = f.pick;
          if (p.kind === "missing") return name + " is missing";
          if (p.kind === "range") {
            return p.below ? p.lo + " \u2264 " + name + " < " + p.hi :
              name + " " + p.lo + "\u2013" + p.hi;
          }
          if (p.kind === "prefix") return name + " in " + p.value;
          return name + " is " + (valueLabel(c, p.value) || choiceLabel(c, p.value) || p.value);
        }
        if (Array.isArray(v)) {
          var vals = v.map(function (x) {
            return valueLabel(c, x) || choiceLabel(c, x) || String(x);
          });
          if (vals.length === 1) return name + " is " + vals[0];
          return name + " is " + vals.slice(0, -1).join(", ") + " or " +
            vals[vals.length - 1];
        }
        if (typeof v === "object") {
          if (v.min !== "" && v.max !== "" && v.below) {
            return v.min + " \u2264 " + name + " < " + v.max;
          }
          if (v.min !== "" && v.max !== "") return name + " " + v.min + "\u2013" + v.max;
          if (v.min !== "") return name + " \u2265 " + v.min;
          return name + " \u2264 " + v.max;
        }
        var sv = String(v);
        if (sv.charAt(0) === "!" && sv.length > 1) {
          return name + " does not contain \u201c" + sv.slice(1) + "\u201d";
        }
        return name + " contains \u201c" + v + "\u201d";
      }

      function renderChips() {
        var box = byId(grid.ui.chips);
        if (!box) return;
        box.innerHTML = "";
        var filters = activeFilters();
        // many filters would push the grid down: the Filters panel lists the rest
        var few = filters.length > MAX_CHIPS ? filters.slice(0, MAX_CHIPS - 1) : filters;
        var nConds = filters.filter(function (f) { return f.cond; }).length;
        var anyShown = false;
        few.forEach(function (f) {
          // the filters of the panel match any of them: say so before them
          if (f.cond && grid.matchAny && nConds > 1 && !anyShown) {
            anyShown = true;
            box.appendChild(fpEl("span", {class: "editr-chip-label", text: "Any of:"}));
          }
          var chip = document.createElement("span");
          chip.className = "editr-chip";
          var text = document.createElement("span");
          text.textContent = chipText(f);
          text.title = chipText(f);
          chip.appendChild(text);
          var x = document.createElement("button");
          x.type = "button";
          x.setAttribute("aria-label", "Remove filter " + chipText(f));
          x.innerHTML = ICONS.x;
          x.addEventListener("click", function () {
            if (f.cond) {
              fpRemoveById(f.cond.cardId);
            } else if (f.pick) {
              delete grid.picks[f.field];
              grid.table.refreshFilter();
            } else {
              grid.table.setHeaderFilterValue(f.field, "");
            }
          });
          chip.appendChild(x);
          box.appendChild(chip);
        });
        if (few.length < filters.length) {
          var more = document.createElement("button");
          more.type = "button";
          more.className = "editr-chip-clear editr-chip-more";
          more.textContent = "+" + (filters.length - few.length) + " more";
          more.title = "See every filter";
          more.addEventListener("click", fpOpenPanel);
          box.appendChild(more);
        }
        if (filters.length > 1) {
          var clear = document.createElement("button");
          clear.type = "button";
          clear.className = "editr-chip-clear";
          clear.textContent = "Clear filters";
          clear.addEventListener("click", fpClearAll);
          box.appendChild(clear);
        }
        setText(grid.ui.filter_count, filters.length ? filters.length : "");
        var btn = byId(grid.ui.filter_btn);
        if (btn) btn.classList.toggle("editr-tool-on", filters.length > 0);
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
        // Tabulator's scroll promises do not always settle (e.g. when the row
        // is already on screen), so never wait for them for long
        var briefly = function (p) {
          return Promise.race([
            Promise.resolve(p).catch(function () {}),
            new Promise(function (r) { setTimeout(r, 250); })
          ]);
        };
        return briefly(table.scrollToRow(id, "center", false)).then(function () {
          return briefly(table.scrollToColumn(field, "middle", false));
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
        if (!el.__editrPanelBound) {
          el.__editrPanelBound = true;
          window.addEventListener("editr-panel", function (e) {
            if (e.detail && e.detail.tab === "record" && grid.ui.record &&
                e.detail.prefix && grid.ui.record.indexOf(e.detail.prefix) === 0) {
              renderForm(selectedRowId() !== null ? selectedRowId() : grid.formRow);
            }
            var mine = e.detail && e.detail.prefix && grid.ui.filters &&
              grid.ui.filters.indexOf(e.detail.prefix) === 0;
            if (mine && e.detail.tab === "filters") el.__editr.filterPanel.show();
            else if (mine) fpClosePicker();
          });
        }
        var sum = byId(grid.ui.summary_btn);
        if (sum) {
          sum.hidden = grid.opts.summary === false;
          sum.setAttribute("aria-pressed", grid.summaryOn ? "true" : "false");
          if (!sum.__editrBound) {
            sum.__editrBound = true;
            sum.addEventListener("click", function () {
              setSummaryOn(!el.__editr.summaryOn, true);
            });
          }
        }
        // database tables: how many rows are loaded, and a button to load
        // others (see R/db_ui.R)
        var rowsBtn = byId(grid.ui.rows_btn);
        if (rowsBtn) {
          var dbRows = grid.opts.db_rows;
          rowsBtn.hidden = !dbRows;
          if (dbRows) {
            var num = function (n) { return Number(n).toLocaleString("en-US"); };
            setText(grid.ui.rows_info, dbRows.total > dbRows.loaded ?
              num(dbRows.loaded) + " of " + num(dbRows.total) : num(dbRows.total));
            rowsBtn.classList.toggle("editr-tool-on", !!dbRows.filtered);
            rowsBtn.title = (dbRows.readonly ? "Read-only. " : "") +
              "Load other rows from the database";
          }
          if (!rowsBtn.__editrBound) {
            rowsBtn.__editrBound = true;
            rowsBtn.addEventListener("click", function () {
              if (shinyOpen()) {
                Shiny.setInputValue(el.id + "_load_rows", Math.random(), {priority: "event"});
              }
            });
          }
        }
        var cols = byId(grid.ui.columns_btn);
        if (cols && !cols.__editrBound) {
          cols.__editrBound = true;
          cols.addEventListener("click", function () {
            el.__editr.openColumns(cols);
          });
        }
        var filt = byId(grid.ui.filter_btn);
        if (filt) filt.hidden = grid.opts.filters === false;
      }

      /* ---------------------------------------------------------------- */
      /* record form: one row as a form in the side panel                  */
      /* ---------------------------------------------------------------- */

      function formBox() { return byId(grid.ui.record); }

      function formVisible() {
        var box = formBox();
        return !!box && !!box.offsetParent;
      }

      function selectedRowId() {
        var ranges = grid.table && grid.table.getRanges ? grid.table.getRanges() : [];
        if (ranges.length) {
          var cells = ranges[0].getCells();
          var first = Array.isArray(cells[0]) ? cells[0][0] : cells[0];
          if (first) return first.getRow().getIndex();
        }
        return null;
      }

      function activeRows() {
        return grid.table.getRows("active");
      }

      function formValue(c, v) {
        if (v === null || v === undefined) return "";
        if (labelsOf(c)) return labelledText(c, v);
        if (c.option && c.option.type === "datetime") return String(v).replace(" ", "T").slice(0, 16);
        return String(v);
      }

      function formInput(c, v) {
        var o = c.option || {};
        var input;
        if (o.type === "dropdown" || c.kind === "logical" || o.type === "checkbox") {
          input = document.createElement("select");
          var opts = o.type === "dropdown" ? toArray(o.values).map(String) : ["TRUE", "FALSE"];
          [""].concat(opts).forEach(function (x) {
            var op = document.createElement("option");
            op.value = x;
            op.textContent = x === "" ? "\u2014" : (choiceLabel(c, x) || x);
            input.appendChild(op);
          });
          var current = v === null || v === undefined ? "" :
            (typeof v === "boolean" ? (v ? "TRUE" : "FALSE") : String(v));
          // a value that is not one of the options is still shown
          if (current !== "" && opts.indexOf(current) === -1) {
            var extra = document.createElement("option");
            extra.value = current;
            extra.textContent = current;
            input.appendChild(extra);
          }
          input.value = current;
        } else if (o.type === "textarea") {
          input = document.createElement("textarea");
          input.rows = 3;
          input.value = formValue(c, v);
        } else if (labelsOf(c)) {
          // the labels are suggested; other values can be typed
          input = document.createElement("input");
          input.type = "text";
          input.value = formValue(c, v);
          var dl = document.createElement("datalist");
          dl.id = el.id + "-form-list-" + c.field;
          labelsOf(c).list.forEach(function (x) {
            var op = document.createElement("option");
            op.value = x.value + " \u00b7 " + x.label;
            dl.appendChild(op);
          });
          input.setAttribute("list", dl.id);
          input.editrList = dl;
        } else if (o.type === "colour") {
          // the text box with a swatch that opens the colour picker
          input = document.createElement("input");
          input.type = "text";
          input.value = formValue(c, v);
          var pick = document.createElement("input");
          pick.type = "color";
          pick.className = "editr-form-colour";
          pick.setAttribute("aria-label", "Pick a colour for " + c.title);
          pick.value = colourHex(input.value) || "#000000";
          pick.disabled = !!c.readonly;
          pick.addEventListener("change", function () {
            input.value = pick.value;
            input.dispatchEvent(new Event("change"));
          });
          input.addEventListener("input", function () {
            var hex = colourHex(input.value);
            if (hex) pick.value = hex;
          });
          input.editrList = pick;
        } else {
          input = document.createElement("input");
          input.type = o.type === "password" ? "password" :
            c.kind === "date" || o.type === "date" ? "date" :
            o.type === "datetime" ? "datetime-local" :
            o.type === "time" ? "time" : "text";
          if (c.kind === "numeric" || o.type === "number" || o.type === "slider" ||
              o.type === "rating") input.inputMode = "decimal";
          input.value = formValue(c, v);
        }
        input.className = "form-control form-control-sm editr-form-input";
        input.disabled = !!c.readonly;
        return input;
      }

      // read a form input as a cell value (the grid normalises it further)
      function formRead(c, input) {
        var x = input.value;
        if (x === "") return null;
        if (labelsOf(c)) {
          var lv = labelValue(c, x);
          if (lv !== undefined) return lv;
        }
        if (input.tagName === "SELECT" && (c.kind === "logical" ||
            (c.option && c.option.type === "checkbox"))) return x === "TRUE";
        if (c.option && c.option.type === "datetime") return x.replace("T", " ") + (x.length === 16 ? ":00" : "");
        return x;
      }

      function renderForm(id) {
        var box = formBox();
        if (!box || !grid.table) return;
        var table = grid.table;
        var rows = activeRows();
        var row = id !== null && id !== undefined ? table.getRow(id) : null;
        if (!row) row = rows[0] || null;
        box.innerHTML = "";
        var head = document.createElement("div");
        head.className = "editr-pane-head editr-form-head";
        box.appendChild(head);
        if (!row) {
          grid.formRow = null;
          head.innerHTML = '<h2>Record</h2>';
          var empty = document.createElement("div");
          empty.className = "editr-empty";
          empty.textContent = "No rows to show.";
          box.appendChild(empty);
          return;
        }
        grid.formRow = row.getIndex();
        var pos = rows.indexOf(row);
        var data = row.getData();
        var label = function () {
          var rn = realColumns().filter(function (col) { return grid.colMap[col.getField()].frozen; })[0];
          var v = rn ? data[rn.getField()] : null;
          return v ? String(v) : "Row " + data[ID];
        };
        head.innerHTML = '<h2 class="editr-form-title">' + escapeHTML(label()) + '</h2>' +
          '<span class="editr-muted small">' + (pos + 1) + ' of ' + rows.length + '</span>' +
          '<span class="editr-spacer"></span>';
        var nav = function (text, title, step, disabled) {
          var b = document.createElement("button");
          b.type = "button";
          b.className = "btn btn-sm btn-outline-secondary";
          b.textContent = text;
          b.title = title;
          b.disabled = disabled;
          b.addEventListener("click", function () {
            var next = rows[pos + step];
            if (next) showRecord(next.getIndex());
          });
          head.appendChild(b);
        };
        nav("\u2039", "Previous row", -1, pos <= 0);
        nav("\u203A", "Next row", 1, pos >= rows.length - 1);
        if (grid.opts.row_edit) {
          var add = document.createElement("button");
          add.type = "button";
          add.className = "btn btn-sm btn-primary";
          add.textContent = "New";
          add.title = "Add a row at the end";
          add.addEventListener("click", function () {
            var p = insertRow(null, true);
            var nid = grid.nextId - 1;
            Promise.resolve(p).then(function () {
              updateStatus();
              showRecord(nid, true);
            });
          });
          head.appendChild(add);
        }
        var form = document.createElement("form");
        form.className = "editr-form";
        form.addEventListener("submit", function (e) { e.preventDefault(); });
        realColumns().forEach(function (col) {
          var field = col.getField();
          var c = grid.colMap[field];
          if (!col.isVisible()) return;
          var wrap = document.createElement("div");
          wrap.className = "editr-form-field";
          wrap.setAttribute("data-field", field);
          var lab = document.createElement("label");
          var input = formInput(c, data[field]);
          input.id = el.id + "-form-" + field;
          lab.htmlFor = input.id;
          lab.innerHTML = '<span class="editr-badge">' + (c.option && c.option.type === "time" ? "time" : labelsOf(c) ? "lbl" : BADGES[c.kind] || "abc") +
            '</span> ' + escapeHTML(c.frozen ? "row name" : c.title);
          if (c.labels && c.labels.label) {
            var hint = document.createElement("span");
            hint.className = "editr-form-varlabel";
            hint.textContent = c.labels.label;
            lab.appendChild(hint);
          }
          wrap.appendChild(lab);
          wrap.appendChild(input);
          if (input.editrList) wrap.appendChild(input.editrList);
          var msg = document.createElement("div");
          msg.className = "editr-form-msg";
          wrap.appendChild(msg);
          var commit = function () {
            var r = grid.table.getRow(grid.formRow);
            if (!r) return;
            var cell = r.getCell(field);
            var v = formRead(c, input);
            var old = cell.getValue();
            if (v === old || ((old === null || old === undefined || old === "") && v === null)) return;
            cell.setValue(v);
            setTimeout(refreshForm, 0);
          };
          input.addEventListener("change", commit);
          input.addEventListener("keydown", function (e) {
            if (e.key === "Enter" && input.tagName !== "TEXTAREA") {
              e.preventDefault();
              commit();
              var fields = form.querySelectorAll(".editr-form-input:not([disabled])");
              var i = Array.prototype.indexOf.call(fields, input);
              if (fields[i + 1]) fields[i + 1].focus();
            }
          });
          form.appendChild(wrap);
        });
        box.appendChild(form);
        refreshForm();
      }

      // put the current values and rule messages in the form (not into the
      // box being typed in)
      function refreshForm() {
        var box = formBox();
        if (!box || grid.formRow === null || grid.formRow === undefined) return;
        var row = grid.table.getRow(grid.formRow);
        if (!row) {
          renderForm(null);
          return;
        }
        var data = row.getData();
        box.querySelectorAll(".editr-form-field").forEach(function (wrap) {
          var field = wrap.getAttribute("data-field");
          var c = grid.colMap[field];
          if (!c) return;
          var input = wrap.querySelector(".editr-form-input");
          var v = data[field];
          if (input && document.activeElement !== input) {
            if (input.tagName === "SELECT") {
              input.value = v === null || v === undefined ? "" :
                (typeof v === "boolean" ? (v ? "TRUE" : "FALSE") : String(v));
            } else {
              input.value = formValue(c, v);
            }
          }
          var pick = wrap.querySelector(".editr-form-colour");
          if (pick && input) pick.value = colourHex(input.value) || "#000000";
          var key = data[ID] + "|" + field;
          var bad = invalidReason(c, v);
          var problem = grid.problems[key];
          var msg = wrap.querySelector(".editr-form-msg");
          msg.textContent = bad ? "\u201c" + v + "\u201d " + bad : (problem || "");
          var note = wrap.querySelector(".editr-form-comment");
          var comment = grid.comments[key];
          if (!note && comment) {
            note = document.createElement("div");
            note.className = "editr-form-comment";
            wrap.appendChild(note);
          }
          if (note) note.textContent = comment ? "\ud83d\udcac " + comment.text : "";
          wrap.classList.toggle("editr-form-bad", !!(bad || problem));
          wrap.classList.toggle("editr-form-changed", !!grid.changed[key]);
        });
      }

      function showRecord(id, focus) {
        renderForm(id);
        if (grid.formRow !== null && grid.formRow !== undefined) {
          var first = realColumns().filter(function (col) { return col.isVisible(); })[0];
          if (first) selectCell(grid.formRow, first.getField());
        }
        if (focus) {
          var input = formBox() && formBox().querySelector(".editr-form-input:not([disabled])");
          if (input) input.focus();
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

      function problemMap(args) {
        var out = {};
        var keys = toArray(args && args.keys), msgs = toArray(args && args.messages);
        keys.forEach(function (k, i) { out[k] = msgs[i] || "Breaks a rule"; });
        return out;
      }

      /* conditional formatting (see R/formats.R) ----------------------- */

      // formats in order (later ones win): rules with the rows (and columns)
      // they colour, and colour scales of a column with their limits
      function formatList(arr) {
        return toArray(arr).map(function (f) {
          if (f.type === "scale") {
            var cols = toArray(f.colours).map(hexRgb);
            return {type: "scale", cid: f.cid, colours: cols, min: Number(f.min),
                    mid: f.mid === undefined || f.mid === null ? null : Number(f.mid),
                    max: Number(f.max),
                    sig: JSON.stringify([f.cid, f.colours, f.min, f.mid, f.max])};
          }
          var ids = {};
          toArray(f.ids).forEach(function (i) { ids[i] = true; });
          var cids = {};
          toArray(f.cids).forEach(function (c) { cids[c] = true; });
          return {type: "rule", ids: ids, all: !!f.all, cids: cids,
                  fill: f.fill || null, color: f.color || null, bold: !!f.bold,
                  fillText: f.fill ? textOn(hexRgb(f.fill)) : null,
                  sig: JSON.stringify([f.all, f.cids, f.fill, f.color, f.bold])};
        });
      }

      function hexRgb(h) {
        var m = /^#?([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})/i.exec(String(h));
        return m ? [parseInt(m[1], 16), parseInt(m[2], 16), parseInt(m[3], 16)] : [255, 255, 255];
      }

      // the colour of a value on a scale (two colours, or three with a middle)
      function scaleColour(f, x) {
        var lo = f.min, hi = f.max, cs = f.colours;
        var clamp = function (t) { return isFinite(t) ? Math.max(0, Math.min(1, t)) : 0; };
        if (cs.length === 3) {
          var mid = f.mid === null ? (lo + hi) / 2 : f.mid;
          if (x <= mid) {
            return {rgb: cs[0], to: cs[1], t: mid > lo ? clamp((x - lo) / (mid - lo)) : 1};
          }
          return {rgb: cs[1], to: cs[2], t: hi > mid ? clamp((x - mid) / (hi - mid)) : 0};
        }
        return {rgb: cs[0], to: cs[1], t: hi > lo ? clamp((x - lo) / (hi - lo)) : 0};
      }

      // dark text on light fills, white text on dark ones (also in dark
      // themes, where the text of other cells is light)
      function textOn(rgb) {
        var lum = (0.299 * rgb[0] + 0.587 * rgb[1] + 0.114 * rgb[2]) / 255;
        return lum < 0.5 ? "#FFFFFF" : "#1F2933";
      }

      function formatStyle(id, field, v) {
        var fs = grid.formats;
        if (!fs || !fs.length) return null;
        var st = null;
        fs.forEach(function (f) {
          if (f.type === "scale") {
            if (f.cid !== field || v === null || v === undefined || v === "") return;
            var x = typeof v === "number" ? v : Number(v);
            if (!isFinite(x)) return;
            var s = scaleColour(f, x);
            var rgb = [0, 1, 2].map(function (i) {
              return Math.round(s.rgb[i] + (s.to[i] - s.rgb[i]) * s.t);
            });
            st = st || {};
            st.fill = "rgb(" + rgb.join(",") + ")";
            st.color = textOn(rgb);
          } else if (f.ids[id] && (f.all || f.cids[field])) {
            st = st || {};
            if (f.fill) {
              st.fill = f.fill;
              st.color = f.fillText;
            }
            if (f.color) st.color = f.color;
            if (f.bold) st.bold = true;
          }
        });
        return st;
      }

      function applyFormat(element, st) {
        var fill = st && st.fill, color = st && st.color, bold = !!(st && st.bold);
        element.classList.toggle("editr-fmt-fill", !!fill);
        element.classList.toggle("editr-fmt-color", !!color);
        element.classList.toggle("editr-fmt-bold", bold);
        if (fill) element.style.setProperty("--editr-fmt-fill", fill);
        else element.style.removeProperty("--editr-fmt-fill");
        if (color) element.style.setProperty("--editr-fmt-color", color);
        else element.style.removeProperty("--editr-fmt-color");
      }

      // redraw the rows whose formatting changed
      function setFormats(next) {
        var before = grid.formats || [];
        grid.formats = next;
        var same = before.length === next.length && before.every(function (f, i) {
          return f.type === next[i].type && f.sig === next[i].sig;
        });
        if (!same) {
          reformatRows(grid.table.getRows().map(function (r) { return r.getIndex(); }));
          return;
        }
        var touched = {};
        next.forEach(function (f, i) {
          if (f.type !== "rule") return;
          var a = before[i].ids, b = f.ids;
          Object.keys(a).forEach(function (k) { if (!b[k]) touched[k] = true; });
          Object.keys(b).forEach(function (k) { if (!a[k]) touched[k] = true; });
        });
        reformatRows(Object.keys(touched).map(Number));
      }

      // redraw rows; a row with a cell being edited is redrawn when the edit
      // ends (redrawing it would remove the open editor)
      function reformatRows(ids) {
        var cur = grid.table.modules.edit && grid.table.modules.edit.currentCell;
        ids.forEach(function (id) {
          var row = grid.table.getRow(id);
          if (!row) return;
          if (cur && row._getSelf && row._getSelf() === cur.row) {
            grid.reformatLater[row.getIndex()] = true;
            return;
          }
          row.reformat();
        });
      }

      function reformatPending() {
        var ids = Object.keys(grid.reformatLater || {}).map(Number);
        grid.reformatLater = {};
        if (ids.length) setTimeout(function () { reformatRows(ids); }, 0);
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
            if (formVisible()) setTimeout(refreshForm, 0);
          } else if (method === "replace_data") {
            if (args.columns) table.setColumns(sameLayout(args.columns));
            table.replaceData(rowsFromColumns(args.data)).then(function () {
              updateStatus();
              renderChips();
              reportView();
              if (formVisible()) renderForm(grid.formRow);
            });
          } else if (method === "scroll_to") {
            selectCell(args.id, args.field || (realColumns()[0] || {getField: function () { return null; }}).getField());
          } else if (method === "set_title") {
            setTitle(args.field, args.title);
          } else if (method === "set_summary") {
            grid.summary = args.summary || {};
            fillSummaries();
          } else if (method === "set_comments") {
            var before = grid.comments;
            grid.comments = commentSet(args);
            var touchedC = {};
            Object.keys(before).concat(Object.keys(grid.comments)).forEach(function (k) {
              var a = before[k], b = grid.comments[k];
              if (!a || !b || a.text !== b.text) touchedC[k.split("|")[0]] = true;
            });
            reformatRows(Object.keys(touchedC).map(Number));
            if (formVisible()) refreshForm();
          } else if (method === "set_formats") {
            setFormats(formatList(args.formats));
          } else if (method === "set_problems") {
            var nextP = problemMap(args);
            var touchedP = {};
            Object.keys(grid.problems).concat(Object.keys(nextP)).forEach(function (k) {
              if (grid.problems[k] !== nextP[k]) touchedP[k.split("|")[0]] = true;
            });
            grid.problems = nextP;
            reformatRows(Object.keys(touchedP).map(Number));
            if (formVisible()) refreshForm();
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
        grid.problems = problemMap(opts.problems);
        grid.formats = formatList(opts.formats);
        grid.reformatLater = {};
        grid.comments = commentSet(opts.comments);
        grid.rColours = {};
        if (opts.colours) {
          var hex = toArray(opts.colours.hex);
          toArray(opts.colours.names).forEach(function (n, i) {
            grid.rColours[String(n).toLowerCase().replace(/\s+/g, "")] = hex[i];
          });
        }
        grid.summary = opts.summary_data || {};
        grid.picks = {};
        grid.conditions = [];
        grid.matchAny = false;
        grid.fp = {cards: [], seq: 0, key: null, ui: {}, picker: null};
        var pref = null;
        try { pref = window.localStorage.getItem("DataEditR.summary"); } catch (e) {}
        grid.summaryOn = opts.summary !== false && pref !== "off";
        el.classList.toggle("editr-summaries-off", !grid.summaryOn);
        if (opts.summary !== false && !grid.summaryOn && shinyOpen()) {
          Shiny.setInputValue(el.id + "_summary", false, {priority: "event"});
        }
        // summaries handle their own clicks (no column selection or rename)
        if (!el.__editrSummaryBound) {
          el.__editrSummaryBound = true;
          ["mousedown", "dblclick", "click"].forEach(function (type) {
            el.addEventListener(type, function (e) {
              if (!e.target.closest || !e.target.closest(".editr-summary")) return;
              e.stopPropagation();
              if (type === "click") summaryClick(e.target);
            }, true);
          });
        }
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
          selectableRangeFill: true,
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
          table.addFilter(pickFilter);
          table.addFilter(conditionFilter);
          var queued = pending[el.id] || [];
          delete pending[el.id];
          queued.forEach(function (m) { grid.handle(m.method, m.args || {}); });
          patchRowDisplayed(table);
          patchScrollToRow(table);
          patchFillHandle(table);
          var add = holder.querySelector(".editr-addrow");
          if (add) add.addEventListener("click", addRowAtEnd);
          bindUI();
          updateStatus();
          renderChips();
          reportView();
          if (fpVisible()) fpShow();
        });

        // rows added while a filter is active stay visible until the user
        // changes the filters; then every row is judged afresh
        table.on("dataFiltering", function () {
          prepareConditions();
          var key = JSON.stringify(activeFilters().map(function (f) {
            return [f.field, f.value, f.pick, f.cond];
          }));
          if (key !== grid.filterKey) {
            grid.filterKey = key;
            grid.pinned = {};
          }
        });

        table.on("cellClick", function (e, cell) {
          if (formVisible() && cell.getRow().getIndex() !== grid.formRow) {
            renderForm(cell.getRow().getIndex());
          }
        });
        table.on("cellEdited", function () {
          if (formVisible()) setTimeout(refreshForm, 0);
          if (fpVisible()) setTimeout(fpUpdateCounts, 0);
          reformatPending();
        });
        table.on("cellEditCancelled", reformatPending);
        table.on("dataFiltered", function () {
          if (!grid.ready) return;
          setTimeout(function () {
            updateStatus();
            renderChips();
            reportView();
          }, 0);
          fpSyncSoon();
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
          // "15/03/2024" -> "2024-03-15", "1,234" -> "1234", "yes" -> TRUE,
          // and an emptied cell is a missing value like a cleared one
          var value = cell.getValue();
          var normal = typeof value === "string" && value.trim() === "" ? null :
            normalizeValue(c, value);
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

      // Ctrl+Shift+V pastes with the first row as column names (the browser
      // still fires the paste event; pasteRange() checks this flag)
      el.addEventListener("keydown", function (e) {
        if ((e.ctrlKey || e.metaKey) && e.shiftKey && e.key.toLowerCase() === "v") {
          grid.pasteHeaders = Date.now();
        }
      }, true);

      // Shift+F2 comments on the selected cell (as in spreadsheets)
      el.addEventListener("keydown", function (e) {
        if (e.key !== "F2" || !e.shiftKey || grid.opts.comments === false) return;
        var t = e.target;
        if (t && (t.tagName === "INPUT" || t.tagName === "TEXTAREA")) return;
        var cell = activeCell();
        if (cell && grid.colMap[cell.getField()]) {
          e.preventDefault();
          e.stopPropagation();
          openComment(cell);
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

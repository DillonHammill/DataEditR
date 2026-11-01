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

  // why a typed value cannot be stored in a column of this kind (or null)
  function invalidReason(kind, v) {
    if (isBlank(v) || typeof v !== "string") return null;
    var s = v.trim();
    if (kind === "numeric" && isNaN(Number(s))) {
      return "is not a number";
    }
    if (kind === "date" &&
        !(/^\d{4}-\d{2}-\d{2}$/.test(s) && !isNaN(Date.parse(s)))) {
      return "is not a date (YYYY-MM-DD)";
    }
    return null;
  }

  var ICONS = {
    lock: '<svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" role="img" aria-label="Read-only"><rect x="5" y="11" width="14" height="10" rx="2"/><path d="M8 11V7a4 4 0 0 1 8 0v4"/></svg>',
    plus: '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M12 5v14M5 12h14"/></svg>',
    x: '<svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M18 6 6 18M6 6l12 12"/></svg>',
    alert: '<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M12 3 2 21h20z"/><path d="M12 10v5M12 18h.01"/></svg>'
  };

  var BADGES = {numeric: "123", character: "abc", logical: "T/F",
                date: "date", datetime: "time"};

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

      /* ---------------------------------------------------------------- */
      /* columns                                                           */
      /* ---------------------------------------------------------------- */

      function titleFormatter(cell) {
        var c = grid.colMap[cell.getColumn().getField()] || {};
        var name = c.frozen ? "row names" : c.title;
        var badge = BADGES[c.kind] || "abc";
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
          escapeHTML(c.title) + ' is a ' + (c.kind === "date" ? "date" : "numeric") +
          ' column. What should happen?</div></div></div>' +
          '<div class="editr-guard-actions">' +
          '<button type="button" class="btn btn-primary btn-sm" data-act="na">Store as missing (NA)</button>' +
          '<button type="button" class="btn btn-outline-secondary btn-sm" data-act="text">Convert ' +
          escapeHTML(c.title) + ' to text</button>' +
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

      // Tabulator's built-in range paste uses row.updateData(), which does
      // not raise cellEdited, so we diff and report the pasted cells here.
      function pasteRange(data) {
        var mod = this.table.modules.selectRange;
        var range = mod && mod.activeRange;
        if (!range || !data || !data.length) return [];
        var bounds = range.getBounds();
        if (!bounds.start) return [];
        var rows = this.table.rowManager.activeRows.slice();
        var startRow = rows.indexOf(bounds.start.row);
        if (startRow < 0) return [];
        var single = bounds.start === bounds.end;
        var width = single ? data.length :
          rows.indexOf(bounds.end.row) - startRow + 1;
        rows = rows.slice(startRow, startRow + width);
        var ops = [];
        this.table.blockRedraw();
        rows.forEach(function (row, i) {
          var incoming = data[i % data.length];
          var old = row.getData();
          var upd = {};
          Object.keys(incoming).forEach(function (f) {
            var c = grid.colMap[f];
            if (!c || c.readonly || f === ID) return;
            var v = incoming[f];
            var o = old[f];
            if (o === v || ((o === null || o === undefined) && v === "")) return;
            upd[f] = v;
            ops.push({type: "update", cid: f, ids: [old[ID]],
                      values: [jsonValue(v)]});
          });
          if (Object.keys(upd).length) row.updateData(upd);
        });
        this.table.restoreRedraw();
        // group by column so each column is one vectorised update
        ops.sort(function (a, b) { return a.cid < b.cid ? -1 : a.cid > b.cid ? 1 : 0; });
        ops.forEach(emit);
        return rows;
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
          clipboardPasteParser: "range",
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
          var reason = c && !c.option ? invalidReason(c.kind, cell.getValue()) : null;
          if (reason) {
            guardEdit(cell, reason);
            return;
          }
          emit({type: "update", cid: field, ids: [cell.getRow().getIndex()],
                values: [jsonValue(cell.getValue())]});
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

      el.addEventListener("keydown", function (e) {
        if (!(e.ctrlKey || e.metaKey)) return;
        var key = e.key.toLowerCase();
        var editing = grid.table && grid.table.modules.edit &&
          grid.table.modules.edit.currentCell;
        if (editing) return;
        if (key === "d") {
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

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
    Shiny.addCustomMessageHandler("editr_status", function (msg) {
      var el = document.getElementById(msg.id);
      if (el) el.textContent = msg.text;
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
        opts: {}
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
        if (!ops.length || !window.Shiny || !Shiny.setInputValue) return;
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

      function buildColumn(c) {
        var def = {
          title: c.title,
          field: c.field,
          headerSort: false,
          minWidth: 60,
          frozen: !!c.frozen,
          editor: c.readonly ? false : "input",
          formatter: makeFormatter(null),
          headerContextMenu: headerMenu,
          cssClass: c.readonly ? "editr-readonly" : ""
        };
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

      function setColumns(cols) {
        grid.colMap = {};
        toArray(cols).forEach(function (c) { grid.colMap[c.field] = c; });
        return toArray(cols).map(buildColumn);
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
        var c = {field: cid, title: name, kind: "logical", readonly: false,
                 rename: true, frozen: false};
        grid.colMap[cid] = c;
        grid.table.addColumn(buildColumn(c), !right, column);
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
            }});
          }
        }
        if (grid.opts.row_edit) {
          items.push({label: "Add row at end",
                      action: function () { insertRow(null, true); }});
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
            if (args.columns) table.setColumns(setColumns(args.columns));
            table.replaceData(rowsFromColumns(args.data));
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
          clipboardPasteAction: pasteRange
        });
        grid.table = table;
        patchRowLookup(table);

        table.on("tableBuilt", function () {
          grid.ready = true;
          var queued = pending[el.id] || [];
          delete pending[el.id];
          queued.forEach(function (m) { grid.handle(m.method, m.args || {}); });
        });

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
          emit({type: "update", cid: field, ids: [cell.getRow().getIndex()],
                values: [jsonValue(cell.getValue())]});
        });

        table.on("headerDblClick", function (e, column) {
          startRename(column);
        });
      }

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

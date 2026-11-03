/* DataEditR editor layout behaviour: review panel tabs and copy buttons */
(function () {
  "use strict";

  var DataEditR = window.DataEditR = window.DataEditR || {};
  var TABS = ["record", "changes", "history", "problems", "code"];

  // open the review panel on a tab; toggle = true closes it when that tab
  // is already showing
  DataEditR.panel = function (prefix, tab, toggle) {
    var aside = document.getElementById(prefix + "panel");
    if (!aside) return;
    var current = aside.getAttribute("data-open");
    var next = toggle && current === tab ? null : tab;
    if (next) {
      aside.setAttribute("data-open", next);
    } else {
      aside.removeAttribute("data-open");
    }
    TABS.forEach(function (t) {
      var pane = document.getElementById(prefix + "pane_" + t);
      if (pane) pane.hidden = t !== next;
      var tabBtn = document.getElementById(prefix + "tab_" + t);
      if (tabBtn) tabBtn.setAttribute("aria-selected", t === next ? "true" : "false");
      var tool = document.getElementById(prefix + "open_" + t);
      if (tool) tool.setAttribute("aria-pressed", t === next ? "true" : "false");
    });
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(prefix + "panel", next || "");
    }
    // the grid fills the Record tab itself
    window.dispatchEvent(new CustomEvent("editr-panel",
                                         {detail: {prefix: prefix, tab: next}}));
  };

  DataEditR.send = function (id, value) {
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(id, {value: value, nonce: Math.random()},
                          {priority: "event"});
    }
  };

  // edit the note of a step in the History tab: Enter or leaving the box
  // saves it, Escape keeps the old note
  DataEditR.note = function (prefix, step, button) {
    var box = button.closest(".editr-step-note");
    if (!box) return;
    var current = box.getAttribute("data-note") || "";
    var input = document.createElement("input");
    input.type = "text";
    input.className = "form-control form-control-sm editr-note-input";
    input.value = current;
    input.maxLength = 500;
    input.placeholder = "Why was this changed?";
    input.setAttribute("aria-label", "Note for step " + step);
    box.innerHTML = "";
    box.appendChild(input);
    input.focus();
    var done = false;
    var save = function (keep) {
      if (done) return;
      done = true;
      DataEditR.send(prefix + "note", {step: step, text: keep ? input.value : current});
    };
    input.addEventListener("keydown", function (e) {
      if (e.key === "Enter") {
        e.preventDefault();
        save(true);
      } else if (e.key === "Escape") {
        e.preventDefault();
        e.stopPropagation();
        save(false);
      }
    });
    input.addEventListener("blur", function () { save(true); });
  };

  DataEditR.copy = function (id, button) {
    var node = document.getElementById(id);
    if (!node) return;
    var text = node.innerText;
    var label = button && button.querySelector("span");
    var done = function (msg) {
      if (!label) return;
      var old = label.textContent;
      label.textContent = msg;
      setTimeout(function () { label.textContent = old; }, 1200);
    };
    var fallback = function () {
      var range = document.createRange();
      range.selectNodeContents(node);
      var sel = window.getSelection();
      sel.removeAllRanges();
      sel.addRange(range);
      try {
        document.execCommand("copy");
        done("Copied");
      } catch (e) {
        done("Press Ctrl+C");
      }
    };
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(text).then(function () {
        done("Copied");
      }, fallback);
    } else {
      fallback();
    }
  };

  // close a Bootstrap dropdown from R (after loading data, for example)
  var registered = false;
  function register() {
    if (registered || !window.Shiny || !Shiny.addCustomMessageHandler) return;
    registered = true;
    Shiny.addCustomMessageHandler("editr_close_dropdown", function (msg) {
      var toggle = document.getElementById(msg.id);
      if (toggle && window.bootstrap && bootstrap.Dropdown) {
        bootstrap.Dropdown.getOrCreateInstance(toggle).hide();
      }
    });
  }
  register();
  if (window.jQuery) {
    jQuery(document).on("shiny:connected", register);
  }
})();

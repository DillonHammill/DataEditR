/* DataEditR editor layout behaviour: review panel tabs and copy buttons */
(function () {
  "use strict";

  var DataEditR = window.DataEditR = window.DataEditR || {};
  var TABS = ["changes", "code"];

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
  };

  DataEditR.send = function (id, value) {
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(id, {value: value, nonce: Math.random()},
                          {priority: "event"});
    }
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

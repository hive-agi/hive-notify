// =============================================================================
// Hive Ask Inbox GTK regression harness -- shim half.
//
// This file is NOT run on its own. script/ask_inbox_guard_harness.js
// concatenates it IN FRONT of the compiled helper
// (resources/hive_notify/ask_inbox.js) into one temporary gjs program and runs
// that, so the shim and the helper share one program scope and the shim can
// reach hive_notify.ask_inbox.{handle_line_BANG_, app, emit,
// apply_responsive_BANG_}.
//
// Why a shim and not a separate process: the helper's top level calls
// app.run([]) and blocks, so the only place a driver can run is an idle
// callback registered BEFORE that call. The shim registers one here.
//
// It drives the COMPILED receiver exactly as the JVM does (a queue frame on the
// wire), then asserts on the real widget tree:
//   1. a choice ask shows its summary AND its real body text
//   2. exactly the two choice buttons render, labelled Allow and Deny
//   3. clicking Deny emits {type:"answer", id:<ask id>, value:"deny"}
//   4. a simple Markdown-derived body (styled spans) still shows its text
//   5. a text ask shows its body, a Send button and its prefilled default
//   6. a 480px width stacks the Paned (the narrow layout)
// It prints HARNESS lines and a final HARNESS RESULT PASS|FAIL, and exits
// non-zero on failure.
// =============================================================================

imports.gi.versions.Gtk = "4.0";

(function () {
  var Gtk = imports.gi.Gtk;
  var GLib = imports.gi.GLib;
  var system = imports.system;

  var passes = [];
  var fails = [];

  function ok(cond, msg) {
    if (cond) passes.push("PASS " + msg);
    else fails.push("FAIL " + msg);
  }

  function walk(w, pred, out) {
    out = out || [];
    if (!w) return out;
    try { if (pred(w)) out.push(w); } catch (e) {}
    var c = null;
    try { c = w.get_first_child ? w.get_first_child() : null; } catch (e) {}
    while (c) {
      walk(c, pred, out);
      c = c.get_next_sibling();
    }
    return out;
  }

  function buttonLabel(b) {
    var c = null;
    try { c = b.get_first_child(); } catch (e) {}
    if (c && c instanceof Gtk.Label) return c.get_label();
    try { return b.get_label(); } catch (e) { return ""; }
  }

  function bodyTexts(win) {
    var views = walk(win, function (w) { return w instanceof Gtk.TextView; });
    var texts = views.map(function (v) {
      var b = v.get_buffer();
      return b.get_text(b.get_start_iter(), b.get_end_iter(), true);
    });
    var sorted = texts.slice().sort(function (a, b) { return b.length - a.length; });
    return { longest: sorted[0] || "", any: texts, views: views };
  }

  function anyHas(texts, needle) {
    return texts.some(function (t) { return t.indexOf(needle) >= 0; });
  }

  function queueLine(generation, asks) {
    return JSON.stringify({ type: "queue", generation: generation, asks: asks });
  }

  var captured = [];
  var tries = 0;

  function finish() {
    passes.forEach(function (p) { print("HARNESS " + p); });
    fails.forEach(function (f) { print("HARNESS " + f); });
    print("HARNESS SUMMARY pass=" + passes.length + " fail=" + fails.length);
    print("HARNESS RESULT " + (fails.length ? "FAIL" : "PASS"));
    try { cljs.core.deref(hive_notify.ask_inbox.app).quit(); } catch (e) {}
    if (fails.length) system.exit(1);
  }

  function drive() {
    var A = hive_notify.ask_inbox;
    var app = cljs.core.deref(A.app);
    var wins = app ? app.get_windows() : null;
    if (!wins || wins.length === 0) {
      if (++tries > 100) { fails.push("FAIL window never appeared"); finish(); return false; }
      return true;
    }
    var win = wins[0];

    // ---- 1/2/3: a synthetic guard permission ask -------------------------
    A.handle_line_BANG_(queueLine(1, [{
      id: "synthetic",
      summary: "Guard permission request",
      body: [{ text: "Review the command before allowing.", styles: [] }],
      choices: [["allow", "Allow"], ["deny", "Deny"]]
    }]));

    var b1 = bodyTexts(win);
    ok(b1.longest.indexOf("Guard permission request") >= 0,
       "choice ask: body shows the summary heading");
    ok(b1.longest.indexOf("Review the command before allowing.") >= 0,
       "choice ask: body shows the real guard body text");

    var buttons = walk(win, function (w) { return w instanceof Gtk.Button; });
    var labels = buttons.map(buttonLabel);
    ok(labels.indexOf("Allow") >= 0, "choice ask: an Allow button renders");
    ok(labels.indexOf("Deny") >= 0, "choice ask: a Deny button renders");
    var choiceLabels = labels.filter(function (l) { return l === "Allow" || l === "Deny"; });
    ok(choiceLabels.length === 2,
       "choice ask: exactly two choice buttons (got " + choiceLabels.length + ")");

    var deny = buttons.filter(function (b) { return buttonLabel(b) === "Deny"; })[0];
    ok(!!deny, "choice ask: the Deny button is reachable");
    if (deny) {
      var origEmit = A.emit;
      A.emit = function (obj) { captured.push(obj); };
      try { deny.emit("clicked"); }
      catch (e) {
        try { deny.clicked(); }
        catch (e2) { fails.push("FAIL clicking Deny threw: " + e2); }
      }
      A.emit = origEmit;
      var ans = captured[captured.length - 1];
      ok(!!ans, "click Deny: an answer was emitted");
      if (ans) {
        ok(ans.type === "answer", "click Deny: frame type is answer");
        ok(ans.id === "synthetic", "click Deny: id is the ask id (synthetic)");
        ok(ans.value === "deny", "click Deny: value is deny");
      }
    }

    // ---- 4: a simple Markdown-derived body (styled spans) ----------------
    A.handle_line_BANG_(queueLine(2, [{
      id: "md",
      summary: "Markdown check",
      body: [{ text: "Review", styles: [] },
             { text: " carefully", styles: ["strong"] },
             { text: " now", styles: [] }],
      choices: [["ok", "OK"]]
    }]));
    var b2 = bodyTexts(win);
    ok(b2.longest.indexOf("Markdown check") >= 0, "markdown ask: body shows the summary");
    ok(b2.longest.indexOf("Review carefully now") >= 0,
       "markdown ask: body shows the styled text");
    var table = b2.views.length ? b2.views[0].get_buffer().get_tag_table() : null;
    ok(!!(table && table.lookup("strong")), "markdown ask: the strong tag is installed");

    // ---- 5: a text ask ---------------------------------------------------
    A.handle_line_BANG_(queueLine(3, [{
      id: "typed",
      summary: "Type a value",
      body: [{ text: "say something", styles: [] }],
      input: "text",
      "default": "v2"
    }]));
    var b3 = bodyTexts(win);
    ok(anyHas(b3.any, "say something"), "text ask: body shows its text");
    var buttons3 = walk(win, function (w) { return w instanceof Gtk.Button; });
    var send = buttons3.filter(function (b) { return buttonLabel(b) === "Send"; })[0];
    ok(!!send, "text ask: a Send button renders");
    ok(anyHas(b3.any, "v2"), "text ask: the default value is prefilled");

    // ---- 6: the narrow (480px) layout stacks the Paned -------------------
    A.apply_responsive_BANG_.call(null, 480);
    var paned = walk(win, function (w) { return w instanceof Gtk.Paned; })[0];
    ok(!!paned, "narrow layout: a Paned exists");
    if (paned) {
      ok(paned.get_orientation() === Gtk.Orientation.VERTICAL,
         "narrow layout: 480px stacks the Paned vertically");
    }

    finish();
    return false;
  }

  GLib.idle_add(GLib.PRIORITY_DEFAULT_IDLE, function () { return drive(); });
})();

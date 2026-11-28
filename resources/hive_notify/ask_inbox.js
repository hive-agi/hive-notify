#!/usr/bin/env gjs
// Hive Ask Inbox — a native GTK4 (GJS) window for queued Hive Ask prompts.
//
// The JVM owns the queue, the deadlines and the first-answer race; this helper
// owns only presentation and the one explicit answer a person gives. It speaks
// a private UTF-8, newline-delimited JSON protocol over stdin/stdout, and every
// diagnostic goes to stderr so the wire stays clean.
//
// Strata, bottom up (each layer uses only the ones below it):
//   data      the wire codec (`emit`, `startReading`) and the tag/palette tables
//   domain    pure renderers: `spansOf`, `applyBody`, `paletteFor`
//   view      `buildWindow` and the render functions that mutate widgets
//   boundary  the Gtk.Application that wires stdin events to the view
//
// Wire, helper -> JVM:
//   {"type":"ready"}                       after Gtk.init_check and app setup
//   {"type":"answer","id":"uuid","value":"yes"}   choice id, or typed answer
// Wire, JVM -> helper:
//   {"type":"queue","generation":1,"asks":[{...}]}   the whole queue, oldest first
//
// An ask is {id, summary, body:[{text, styles:[...]}], remainingMs, choices:[[id,
// label]]} or, for a typed question, {id, summary, body, remainingMs, input:"text",
// default}. The body styles are heading, strong, emphasis, quote, code,
// code-block, keyword, string, number, comment; several may apply to one span.
// Summary and choice labels are always literal — never parsed as markup.
//
// GJS notes that shaped this file: `insert_with_tags_by_name` is not exposed, so
// text is inserted and tags applied by name over the inserted range; a
// Gtk.Paned switches orientation (horizontal <-> stacked) rather than swapping
// widgets; a non-unique local application keeps the window out of any session
// singleton and out of xmonad's exact `hive ask` popup rule.

imports.gi.versions.Gtk = "4.0";

const GLib = imports.gi.GLib;
const Gio = imports.gi.Gio;
const GioUnix = imports.gi.GioUnix;
const Gtk = imports.gi.Gtk;
const Gdk = imports.gi.Gdk;
const Pango = imports.gi.Pango;

// SPDX-License-Identifier: MIT
// Copyright (c) 2026 hive-agi contributors

const APP_ID = "org.hive.AskInbox";
const WINDOW_TITLE = "Hive Ask Inbox";

// Below this width the queue/details Paned stacks instead of sitting side by
// side. Chosen so two readable columns fit, and below it one column does.
const NARROW_WIDTH = 620;

// Finite minimums: the window can never collapse smaller than this, and neither
// panel can vanish as the divider is dragged.
const WINDOW_MIN = [380, 340];
const QUEUE_MIN = [220, 240];
const DETAILS_MIN = [280, 240];

// A bounded height for the choice list, so eight long labels stay reachable by
// scrolling instead of pushing the details out of the window.
const CHOICES_MAX_HEIGHT = 280;

const STYLE_NAMES = [
  "heading", "strong", "emphasis", "quote",
  "code", "code-block", "keyword", "string", "number", "comment",
];

// =============================================================================
// Data: wire codec
// =============================================================================

function log(message) {
  printerr("[ask-inbox] " + message);
}

let outStream = null;

function wireInit() {
  outStream = GioUnix.OutputStream.new(1, false);
}

function emit(obj) {
  if (!outStream) return;
  try {
    const line = JSON.stringify(obj) + "\n";
    const bytes = new TextEncoder().encode(line);
    outStream.write_bytes(GLib.Bytes.new(bytes), null);
    outStream.flush(null);
  } catch (e) {
    log("emit failed: " + e);
  }
}

// =============================================================================
// Data: palette and tags
// =============================================================================

function paletteFor(dark) {
  if (dark) {
    return {
      quote: "#9aa0a6",
      codeBg: "#2b2b2b",
      keyword: "#c792ea",
      string: "#c3e88d",
      number: "#f78c6c",
      comment: "#7f848e",
    };
  }
  return {
    quote: "#5f6368",
    codeBg: "#f0f0f0",
    keyword: "#8250df",
    string: "#0a7d33",
    number: "#b5560a",
    comment: "#6a737d",
  };
}

function prefersDark() {
  try {
    const settings = Gtk.Settings.get_default();
    if (!settings) return false;
    if (settings.gtk_application_prefer_dark_theme) return true;
    const name = String(settings.gtk_theme_name || "");
    return name.toLowerCase().indexOf("dark") !== -1;
  } catch (e) {
    return false;
  }
}

function installTags(buffer, palette) {
  const table = buffer.get_tag_table();
  const defs = {
    "heading": { weight: Pango.Weight.BOLD, scale: 1.2, pixelsAboveLines: 10, pixelsBelowLines: 6 },
    "strong": { weight: Pango.Weight.BOLD },
    "emphasis": { style: Pango.Style.ITALIC },
    "quote": { style: Pango.Style.ITALIC, leftMargin: 16, foreground: palette.quote },
    "code": { family: "monospace", background: palette.codeBg },
    "code-block": { family: "monospace", background: palette.codeBg, leftMargin: 12, pixelsAboveLines: 6, pixelsBelowLines: 6 },
    "keyword": { family: "monospace", weight: Pango.Weight.BOLD, foreground: palette.keyword },
    "string": { family: "monospace", foreground: palette.string },
    "number": { family: "monospace", foreground: palette.number },
    "comment": { family: "monospace", style: Pango.Style.ITALIC, foreground: palette.comment },
  };
  for (const name of STYLE_NAMES) {
    const tag = new Gtk.TextTag({ name: name });
    const d = defs[name] || {};
    try {
      if (d.family !== undefined) tag.family = d.family;
      if (d.weight !== undefined) tag.weight = d.weight;
      if (d.style !== undefined) tag.style = d.style;
      if (d.scale !== undefined) tag.scale = d.scale;
      if (d.foreground !== undefined) tag.foreground = d.foreground;
      if (d.background !== undefined) tag.background = d.background;
      if (d.leftMargin !== undefined) tag.left_margin = d.leftMargin;
      if (d.pixelsAboveLines !== undefined) tag.pixels_above_lines = d.pixelsAboveLines;
      if (d.pixelsBelowLines !== undefined) tag.pixels_below_lines = d.pixelsBelowLines;
    } catch (e) {
      log("tag " + name + " property failed: " + e);
    }
    table.add(tag);
  }
}

// =============================================================================
// Domain: pure helpers
// =============================================================================

// The body is a vector of {text, styles}; a plain string is tolerated as one
// unstyled span so a malformed snapshot still renders rather than throwing.
function spansOf(body) {
  if (typeof body === "string") return [{ text: body, styles: [] }];
  if (!Array.isArray(body)) return [];
  return body.map(function (span) {
    if (span && typeof span === "object") {
      return {
        text: span.text === null || span.text === undefined ? "" : String(span.text),
        styles: Array.isArray(span.styles) ? span.styles : [],
      };
    }
    return { text: String(span === null || span === undefined ? "" : span), styles: [] };
  });
}

function isTextAsk(ask) {
  return !!ask && ask.input === "text";
}

function choicePairs(ask) {
  if (!ask || !Array.isArray(ask.choices)) return [];
  return ask.choices
    .filter(function (c) { return Array.isArray(c) && c.length >= 2; })
    .map(function (c) { return [String(c[0]), String(c[1])]; });
}

// =============================================================================
// State
// =============================================================================

const state = {
  generation: null,
  presentedGeneration: null,
  asks: [],
  selectedId: null,
  drafts: {},     // ask id -> typed draft text
  sent: {},       // ask id -> true once an answer went out
  window: null,
  paned: null,
  queueScroll: null,
  listbox: null,
  summaryLabel: null,
  bodyScroll: null,
  bodyView: null,
  bodyBuffer: null,
  bodyAdj: null,
  answerBox: null,
  activated: false,
};

// =============================================================================
// View: queue
// =============================================================================

function clearChildren(widget) {
  let child = widget.get_first_child();
  while (child) {
    const next = child.get_next_sibling();
    widget.remove(child);
    child = next;
  }
}

function rebuildQueue() {
  const lb = state.listbox;
  if (!lb) return;
  clearChildren(lb);
  for (let i = 0; i < state.asks.length; i++) {
    const ask = state.asks[i];
    const row = new Gtk.ListBoxRow();
    row.set_activatable(true);
    row.set_selectable(true);
    const label = new Gtk.Label({ label: String(ask.summary || "") });
    label.set_xalign(0);
    label.set_ellipsize(Pango.EllipsizeMode.END);
    label.set_margin_top(6);
    label.set_margin_bottom(6);
    label.set_margin_start(10);
    label.set_margin_end(10);
    if (state.sent[ask.id]) label.add_css_class("dim-label");
    row.set_child(label);
    row.set_tooltip_text(String(ask.summary || ""));
    row.askId = ask.id;
    lb.append(row);
  }
}

function restoreSelection() {
  const lb = state.listbox;
  if (!lb) return;
  let target = state.selectedId;
  const present = state.asks.some(function (a) { return a.id === target; });
  if (!target || !present) {
    // First-pending focus: the oldest pending ask is the first row.
    target = state.asks.length ? state.asks[0].id : null;
  }
  state.selectedId = target;
  let row = null;
  for (let i = 0; i < state.asks.length; i++) {
    if (state.asks[i].id === target) { row = lb.get_row_at_index(i); break; }
  }
  if (row) lb.select_row(row);
  else lb.unselect_all();
}

// =============================================================================
// View: details
// =============================================================================

function applyBody(buffer, spans) {
  for (let i = 0; i < spans.length; i++) {
    const span = spans[i];
    const startOff = buffer.get_char_count();
    buffer.insert(buffer.get_end_iter(), span.text, -1);
    const endOff = buffer.get_char_count();
    const startIter = buffer.get_iter_at_offset(startOff);
    const endIter = buffer.get_iter_at_offset(endOff);
    for (let j = 0; j < span.styles.length; j++) {
      const name = span.styles[j];
      if (STYLE_NAMES.indexOf(name) === -1) continue;
      try {
        buffer.apply_tag_by_name(name, startIter, endIter);
      } catch (e) {
        log("apply tag " + name + " failed: " + e);
      }
    }
  }
}

function renderAnswer(ask) {
  const box = state.answerBox;
  clearChildren(box);
  if (!ask) return;

  const alreadySent = !!state.sent[ask.id];

  if (isTextAsk(ask)) {
    const row = new Gtk.Box({ orientation: Gtk.Orientation.HORIZONTAL, spacing: 6 });
    row.set_margin_top(8);
    const entry = new Gtk.Entry();
    entry.set_hexpand(true);
    entry.set_placeholder_text("Type your answer");
    const draft = state.drafts[ask.id];
    entry.set_text(draft !== undefined && draft !== null ? String(draft)
      : (ask.default !== undefined && ask.default !== null ? String(ask.default) : ""));
    entry.set_sensitive(!alreadySent);
    const send = new Gtk.Button({ label: "Send" });
    send.set_sensitive(!alreadySent && entry.get_text().trim().length > 0);
    entry.connect("changed", function () {
      state.drafts[ask.id] = entry.get_text();
      send.set_sensitive(entry.get_text().trim().length > 0);
    });
    entry.connect("activate", function () {
      const text = entry.get_text().trim();
      if (text.length > 0) sendAnswer(ask.id, text);
    });
    send.connect("clicked", function () {
      const text = entry.get_text().trim();
      if (text.length > 0) sendAnswer(ask.id, text);
    });
    row.append(entry);
    row.append(send);
    box.append(row);
    return;
  }

  const choices = choicePairs(ask);
  if (choices.length === 0) return;

  // Eight long labels must stay usable: a bounded, vertically scrolling stack,
  // each button full width with a wrapped, left-aligned label.
  const scroll = new Gtk.ScrolledWindow();
  scroll.set_policy(Gtk.PolicyType.NEVER, Gtk.PolicyType.AUTOMATIC);
  scroll.set_max_content_height(CHOICES_MAX_HEIGHT);
  scroll.set_propagate_natural_height(true);
  scroll.set_margin_top(8);
  const list = new Gtk.Box({ orientation: Gtk.Orientation.VERTICAL, spacing: 6 });
  for (let i = 0; i < choices.length; i++) {
    const id = choices[i][0];
    const label = choices[i][1];
    const button = new Gtk.Button();
    button.set_hexpand(true);
    button.set_sensitive(!alreadySent);
    const text = new Gtk.Label({ label: label });
    text.set_wrap(true);
    text.set_wrap_mode(Pango.WrapMode.WORD_CHAR);
    text.set_xalign(0);
    text.set_margin_top(4);
    text.set_margin_bottom(4);
    button.set_child(text);
    button.connect("clicked", function () { sendAnswer(ask.id, id); });
    list.append(button);
  }
  scroll.set_child(list);
  box.append(scroll);
}

function renderDetails() {
  const ask = currentAsk();
  const askId = ask ? ask.id : null;
  // The reading position is kept across snapshots of the SAME ask, and reset to
  // the top when the person moves to a different one.
  const sameAsk = askId !== null && askId === state._scrollAskId;
  const keepScroll = sameAsk && state.bodyAdj ? state.bodyAdj.get_value() : 0;
  state._scrollAskId = askId;
  if (state.summaryLabel) state.summaryLabel.set_text(ask ? String(ask.summary || "") : "");
  if (state.bodyBuffer) {
    state.bodyBuffer.set_text("", 0);
    if (ask) applyBody(state.bodyBuffer, spansOf(ask.body));
  }
  renderAnswer(ask);
  // Restore the reading position after the buffer was rebuilt.
  GLib.idle_add(GLib.PRIORITY_DEFAULT_IDLE, function () {
    if (state.bodyAdj) {
      const upper = state.bodyAdj.get_upper() - state.bodyAdj.get_page_size();
      const target = Math.max(0, Math.min(keepScroll, upper));
      state.bodyAdj.set_value(target);
    }
    return false;
  });
}

function currentAsk() {
  if (!state.selectedId) return null;
  for (let i = 0; i < state.asks.length; i++) {
    if (state.asks[i].id === state.selectedId) return state.asks[i];
  }
  return null;
}

// =============================================================================
// Boundary: answering, presentation, and the application
// =============================================================================

function sendAnswer(id, value) {
  if (!id || state.sent[id]) return;
  state.sent[id] = true;
  state.drafts[id] = undefined;
  emit({ type: "answer", id: id, value: String(value) });
  rebuildQueue();
  renderAnswer(currentAsk());
}

function hasPending() {
  for (let i = 0; i < state.asks.length; i++) {
    if (!state.sent[state.asks[i].id]) return true;
  }
  return false;
}

function applyQueue(generation, asks) {
  state.generation = generation;
  state.asks = asks;
  rebuildQueue();
  restoreSelection();
  // A coalesced snapshot that keeps the same row selected fires no row-selected
  // signal, so refresh the details here; the same-ask scroll guard keeps the
  // reading position stable across the rebuild.
  renderDetails();
  if (asks.length === 0) {
    hideWindow();
  } else if (generation !== state.presentedGeneration) {
    // First snapshot of a new generation presents once; later coalesced
    // snapshots update in place without stealing focus again.
    state.presentedGeneration = generation;
    presentWindow();
  }
}

function presentWindow() {
  const win = state.window;
  if (!win) return;
  try {
    if (!win.get_visible()) win.present();
    else win.unminimize();
  } catch (e) {
    log("present failed: " + e);
  }
}

function hideWindow() {
  const win = state.window;
  if (!win) return;
  try { win.set_visible(false); } catch (e) { log("hide failed: " + e); }
}

function initialSize() {
  let width = 940;
  let height = 700;
  try {
    const display = Gdk.Display.get_default();
    if (display) {
      const monitors = display.get_monitors();
      const n = monitors.get_n_items();
      let monitor = null;
      for (let i = 0; i < n; i++) {
        const m = monitors.get_item(i);
        let primary = false;
        try { primary = !!m.is_primary(); } catch (e) { primary = false; }
        if (primary) { monitor = m; break; }
      }
      if (!monitor && n > 0) monitor = monitors.get_item(0);
      if (monitor) {
        const g = monitor.get_geometry();
        width = Math.round(Math.min(Math.max(g.width * 0.45, 640), 1080));
        height = Math.round(Math.min(Math.max(g.height * 0.70, 520), 900));
        width = Math.min(width, Math.max(400, g.width - 80));
        height = Math.min(height, Math.max(360, g.height - 80));
      }
    }
  } catch (e) {
    log("monitor query failed: " + e);
  }
  return [Math.max(width, WINDOW_MIN[0]), Math.max(height, WINDOW_MIN[1])];
}

function applyResponsive() {
  const win = state.window;
  const paned = state.paned;
  if (!win || !paned) return;
  let width = 0;
  try {
    const size = win.get_default_size();
    width = size ? size[0] : 0;
  } catch (e) { width = 0; }
  const narrow = width > 0 && width < NARROW_WIDTH;
  const want = narrow ? Gtk.Orientation.VERTICAL : Gtk.Orientation.HORIZONTAL;
  if (paned.get_orientation() !== want) paned.set_orientation(want);
}

function buildQueuePanel() {
  const scroll = new Gtk.ScrolledWindow();
  scroll.set_policy(Gtk.PolicyType.NEVER, Gtk.PolicyType.AUTOMATIC);
  scroll.set_size_request(QUEUE_MIN[0], QUEUE_MIN[1]);
  scroll.set_vexpand(true);
  scroll.set_hexpand(true);
  const lb = new Gtk.ListBox();
  lb.set_selection_mode(Gtk.SelectionMode.SINGLE);
  lb.set_activate_on_single_click(true);
  lb.connect("row-selected", function (_lb, row) {
    if (!row) return;
    state.selectedId = row.askId;
    renderDetails();
  });
  scroll.set_child(lb);
  state.queueScroll = scroll;
  state.listbox = lb;
  return scroll;
}

function buildDetailsPanel() {
  const box = new Gtk.Box({ orientation: Gtk.Orientation.VERTICAL, spacing: 6 });
  box.set_size_request(DETAILS_MIN[0], DETAILS_MIN[1]);
  box.set_vexpand(true);
  box.set_hexpand(true);
  box.set_margin_top(8);
  box.set_margin_bottom(8);
  box.set_margin_start(8);
  box.set_margin_end(8);

  const summary = new Gtk.Label({ label: "" });
  summary.set_xalign(0);
  summary.set_wrap(true);
  summary.set_wrap_mode(Pango.WrapMode.WORD_CHAR);
  summary.set_selectable(true);
  summary.add_css_class("title-4");
  box.append(summary);
  state.summaryLabel = summary;

  const bodyScroll = new Gtk.ScrolledWindow();
  bodyScroll.set_policy(Gtk.PolicyType.NEVER, Gtk.PolicyType.AUTOMATIC);
  bodyScroll.set_vexpand(true);
  const view = new Gtk.TextView();
  view.set_wrap_mode(Gtk.WrapMode.WORD_CHAR);
  view.set_editable(false);
  view.set_cursor_visible(false);
  view.set_monospace(false);
  view.set_accepts_tab(false);
  view.set_top_margin(6);
  view.set_bottom_margin(6);
  view.set_left_margin(6);
  view.set_right_margin(6);
  view.set_pixels_below_lines(2);
  const buffer = view.get_buffer();
  installTags(buffer, paletteFor(prefersDark()));
  bodyScroll.set_child(view);
  box.append(bodyScroll);
  state.bodyScroll = bodyScroll;
  state.bodyView = view;
  state.bodyBuffer = buffer;
  state.bodyAdj = bodyScroll.get_vadjustment();

  const separator = new Gtk.Separator({ orientation: Gtk.Orientation.HORIZONTAL });
  box.append(separator);

  const answerBox = new Gtk.Box({ orientation: Gtk.Orientation.VERTICAL, spacing: 6 });
  box.append(answerBox);
  state.answerBox = answerBox;

  return box;
}

function buildWindow(app) {
  const win = new Gtk.ApplicationWindow({ application: app, title: WINDOW_TITLE });
  const size = initialSize();
  win.set_default_size(size[0], size[1]);
  win.set_size_request(WINDOW_MIN[0], WINDOW_MIN[1]);
  win.connect("notify::default-width", function () { applyResponsive(); });
  win.connect("close-request", function () {
    if (hasPending()) {
      // The person closed the window while answers are still owed: minimize so
      // the queue stays reachable instead of losing the asks.
      try { win.minimize(); } catch (e) { log("minimize failed: " + e); }
      return true;
    }
    return false;
  });

  const paned = new Gtk.Paned({ orientation: Gtk.Orientation.HORIZONTAL });
  paned.set_wide_handle(false);
  paned.set_position(300);
  paned.set_resize_start_child(false);
  paned.set_shrink_start_child(false);
  paned.set_start_child(buildQueuePanel());
  paned.set_end_child(buildDetailsPanel());
  win.set_child(paned);
  state.paned = paned;

  win.connect("show", function () { applyResponsive(); });
  state.window = win;
  return win;
}

// =============================================================================
// Boundary: stdin events
// =============================================================================

function handleLine(line) {
  let msg = null;
  try {
    msg = JSON.parse(line);
  } catch (e) {
    log("bad JSON on stdin: " + e);
    return;
  }
  if (!msg || typeof msg !== "object") return;
  if (msg.type === "queue") {
    const generation = typeof msg.generation === "number" ? msg.generation : state.generation;
    const asks = Array.isArray(msg.asks) ? msg.asks : [];
    applyQueue(generation, asks);
  } else {
    log("unknown message type: " + String(msg.type));
  }
}

function startReading() {
  let stream = null;
  try {
    stream = GioUnix.InputStream.new(0, false);
  } catch (e) {
    log("stdin unavailable: " + e);
    return;
  }
  const reader = new Gio.DataInputStream({ base_stream: stream });
  function step() {
    reader.read_line_async(GLib.PRIORITY_DEFAULT, null, function (_src, res) {
      let line = null;
      try {
        const r = reader.read_line_finish_utf8(res);
        if (r && r[0] !== null && r[0] !== undefined) line = r[0];
      } catch (e) {
        log("read failed: " + e);
        line = null;
      }
      if (line === null) {
        // stdin EOF: the JVM is gone, so the window has nothing left to serve.
        log("stdin closed");
        if (state.window) state.window.set_visible(false);
        app.quit();
        return;
      }
      if (line.length > 0) handleLine(line);
      step();
    });
  }
  step();
}

// =============================================================================
// Boundary: main
// =============================================================================

let app = null;

function main() {
  if (!Gtk.init_check()) {
    log("no display; exiting");
    return 1;
  }

  app = new Gtk.Application({ application_id: APP_ID, flags: Gio.ApplicationFlags.NON_UNIQUE });
  app.connect("activate", function () {
    if (state.activated) {
      presentWindow();
      return;
    }
    state.activated = true;
    buildWindow(app);
    presentWindow();
    startReading();
  });

  wireInit();
  emit({ type: "ready" });
  return app.run([]);
}

const code = main();
if (code !== undefined && code !== null) {
  imports.system.exit(typeof code === "number" ? code : 0);
}

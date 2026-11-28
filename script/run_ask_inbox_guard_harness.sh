#!/usr/bin/env bash
# =============================================================================
# Hive Ask Inbox GTK regression harness -- runner half.
#
#   script/run_ask_inbox_guard_harness.sh [path/to/ask_inbox.js]
#
# Concatenates the assertions shim (script/ask_inbox_guard_shim.js) IN FRONT of
# the compiled helper (default resources/hive_notify/ask_inbox.js) into one
# temporary gjs program and runs it on the CURRENT display. The shim registers
# an idle callback before the helper's blocking app.run([]) and, once the
# window exists, feeds a synthetic guard ask on the wire and asserts the real
# body, the exact Allow/Deny buttons, and the answer frame a Deny click emits.
#
# stdin is held open on purpose: the helper treats stdin EOF as "the JVM is
# gone" and quits, so a /dev/null stdin would end the run before the shim can
# drive anything. `sleep | gjs` keeps it open; the shim quits the app itself
# once it has asserted.
#
# Exits 0 only when the child prints "HARNESS RESULT PASS". Needs a display
# (DISPLAY or WAYLAND_DISPLAY) and gjs. It uses its own NON_UNIQUE window and
# only synthetic asks, so it never touches a real pending ask in the running
# inbox.
# =============================================================================
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
bundle="${1:-$here/../resources/hive_notify/ask_inbox.js}"

tmp="$(mktemp -t hive-ask-harness-XXXXXX.js)"
trap 'rm -f "$tmp"' EXIT

cat "$here/ask_inbox_guard_shim.js" >"$tmp"
printf '\n;\n' >>"$tmp"
cat "$bundle" >>"$tmp"

set +e
out="$(timeout 40 sh -c "sleep 12 | gjs '$tmp' 2>&1")"
rc=$?
set -e

printf '%s\n' "$out"

if grep -q 'HARNESS RESULT PASS' <<<"$out"; then
  exit 0
fi

echo "harness runner: no PASS line (child exit $rc)"
exit 1

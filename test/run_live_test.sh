#!/usr/bin/env bash
# Starts a throwaway `rayforce -p <port>` server, waits for it to accept
# connections, runs each client's live test against it, and tears the
# server down --
# regardless of whether the test passes. Invoked from dune's runtest alias
# (see test/dune); not meant to be run by hand, though it's harmless to.
set -u

if ! command -v rayforce >/dev/null 2>&1; then
  echo "run_live_test.sh: 'rayforce' not found on PATH -- install it (see" \
       "~/code/aur/rayforce) to run this test." >&2
  exit 1
fi

# Both clients -- blocking and Async -- run against the same server.
test_exes=("$@")

# Pid-derived port to keep parallel dune-cache/CI runs from colliding on a
# fixed port; still not collision-proof against an unrelated listener, but
# good enough for a local smoke test.
port=$((16000 + $$ % 1000))

rayforce -p "$port" >/dev/null 2>&1 &
server_pid=$!

cleanup() {
  kill "$server_pid" 2>/dev/null
  wait "$server_pid" 2>/dev/null
}
trap cleanup EXIT

# Poll for the listener instead of a fixed sleep -- /dev/tcp is a bash
# builtin (no nc/curl dependency).
tries=0
until (exec 3<>"/dev/tcp/127.0.0.1/$port") 2>/dev/null; do
  exec 3<&- 2>/dev/null
  tries=$((tries + 1))
  if [ "$tries" -ge 50 ]; then
    echo "run_live_test.sh: rayforce -p $port never came up" >&2
    exit 1
  fi
  sleep 0.1
done
exec 3<&-

status=0
for exe in "${test_exes[@]}"; do
  "$exe" "$port" || status=$?
done
exit "$status"

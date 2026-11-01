#!/bin/bash
# run.sh <scenario> <port> <outdir> <node-script>
scenario=$1; port=$2; out=$3; script=$4
rm -rf "$out" && mkdir -p "$out"
setsid Rscript app.R "$scenario" "$port" "$out" > "$out/r.log" 2>&1 &
echo $! > "$out/pid"
sleep 4
timeout 240 node "$script" "$port" "$out" 2>&1
sleep 3
kill "$(cat "$out/pid")" 2>/dev/null
tail -3 "$out/r.log"

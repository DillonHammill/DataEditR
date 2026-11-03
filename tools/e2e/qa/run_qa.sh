#!/bin/bash
# Run QA scenarios end to end: start the app, drive it in headless Chrome,
# then check the returned data, written files and generated code.
# Usage: tools/e2e/qa/run_qa.sh [scenario ...]   (default: all)
# Needs DataEditR installed and node with puppeteer-core (see ../README.md).
cd "$(dirname "$0")"
all="df matrix tibble datatable template text empty file binds factor factor_keep subset filter_dialog pages
cancel cancel_value log recover_crash recover actions rules summary_form labelled fill comments formats issues types open open_new open_upload open_settings
review console hide db db_rows db_conflict shiny shiny_db shiny_modal"
scenarios=${*:-$all}
port=${QA_PORT:-8900}
summary=""
for s in $scenarios; do
  out="out/$s"
  rm -rf "$out" && mkdir -p "$out"
  setsid Rscript qa_app.R "$s" "$port" "$out" > "$out/app.log" 2>&1 &
  pid=$!
  sleep 3
  timeout 300 node qa.js "$s" "$port" "$out" > "$out/browser.log" 2>&1
  b=$?
  # shiny apps keep running until they are stopped
  if [[ $s == shiny* ]]; then sleep 1; kill -- -"$pid" 2>/dev/null || kill "$pid" 2>/dev/null; fi
  for i in $(seq 1 20); do kill -0 "$pid" 2>/dev/null || break; sleep 0.5; done
  kill -- -"$pid" 2>/dev/null
  Rscript qa_check.R "$s" "$out" > "$out/check.log" 2>&1
  c=$?
  status="PASS"
  [ $b -ne 0 ] || [ $c -ne 0 ] && status="FAIL"
  echo "== $s: $status"
  grep -h "FAIL\|PAGE ERROR\|console error" "$out/browser.log" "$out/check.log" | sed 's/^/   /'
  summary="$summary$s:$status "
  port=$((port + 1))
done
echo "$summary"

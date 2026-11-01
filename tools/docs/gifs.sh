#!/bin/bash
# Animated demos for the vignettes and the README.
# Usage: tools/docs/gifs.sh [port]
# Needs: DataEditR installed, node with puppeteer-core, Chrome (CHROME_PATH)
# and ffmpeg. Frames and GIFs are written to tools/docs/gifout; copy the GIFs
# to vignettes/images (and readme.gif to
# man/figures/DataEditR/DataEditR-README.gif).
cd "$(dirname "$0")"
port=${1:-8841}
out=gifout
mkdir -p "$out"

# <app scenario> <gif scenario>: app.R starts the editor, gifs.js records
record() {
  local app=$1 scen=$2
  rm -rf "$out/$scen.tmp" && mkdir -p "$out/$scen.tmp"
  setsid Rscript app.R "$app" "$port" "$out/$scen.tmp" > "$out/$scen.log" 2>&1 &
  local pid=$!
  sleep 3
  timeout 600 node gifs.js "$scen" "$port" "$out"
  sleep 1
  kill -- -"$pid" 2>/dev/null || kill "$pid" 2>/dev/null
  port=$((port + 1))
}

record main editing      # edit-cells ranges rows-columns rename-resize find
record main filtering    # filter-rows select-columns done-export open-data
record track review      # review
record template template # template
record options options   # col-checkbox col-dropdown col-date col-password
record db db             # db-commit
record readme readme     # readme

# frames -> GIF (800px wide, 900px for the README)
for list in "$out"/*-frames/list.txt; do
  name=$(basename "$(dirname "$list")" -frames)
  width=800
  [ "$name" = "readme" ] && width=900
  ffmpeg -loglevel error -y -f concat -safe 0 -i "$list" \
    -vf "fps=10,scale=$width:-1:flags=lanczos,split[a][b];[a]palettegen=max_colors=128:stats_mode=diff[p];[b][p]paletteuse=dither=bayer:bayer_scale=4:diff_mode=rectangle" \
    -loop 0 "$out/$name.gif"
done
ls -la "$out"/*.gif

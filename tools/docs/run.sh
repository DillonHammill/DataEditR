#!/bin/bash
# Screenshots and GIFs for the vignettes (vignettes/images).
# Usage: tools/docs/run.sh <scenario> <port>
#   scenarios: main empty size template options custom stretch track actions summary filter labelled db_rows restore db gif
# Animated demos for the user guide and README: tools/docs/gifs.sh
# Needs: DataEditR installed, node with puppeteer-core, Chrome (CHROME_PATH),
# ffmpeg for GIFs. Output goes to tools/docs/out.
cd "$(dirname "$0")"
scenario=$1; port=${2:-8831}; out=out
mkdir -p "$out/$scenario.tmp"
setsid Rscript app.R "$scenario" "$port" "$out/$scenario.tmp" > "$out/$scenario.log" 2>&1 &
pid=$!
sleep 3
timeout 200 node shots.js "$scenario" "$port" "$out" 2>&1
sleep 2
kill $pid 2>/dev/null
if [ "$scenario" = "gif" ]; then
  for n in editing filtering reviewing; do
    ffmpeg -loglevel error -y -f concat -safe 0 -i "$out/$n-frames/list.txt" \
      -vf "fps=10,scale=900:-1:flags=lanczos,split[s0][s1];[s0]palettegen=max_colors=128:stats_mode=diff[p];[s1][p]paletteuse=dither=bayer:bayer_scale=5:diff_mode=rectangle" \
      -loop 0 "$out/$n.gif"
  done
fi
# shrink screenshots: convert x.png -strip -colors 256 PNG8:x.png

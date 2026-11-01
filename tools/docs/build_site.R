# Build the pkgdown site into docs/.
# Usage (from the package root): Rscript tools/docs/build_site.R
#
# The pkgdown packaged by Debian/Ubuntu (r-cran-pkgdown) points the MathJax
# script at file:///usr/share/javascript/mathjax, which does not exist for
# visitors of the website. Point it back at the CDN that upstream pkgdown uses.
options(pkgdown.internet = FALSE)
pkgdown::build_site(preview = FALSE)

local_mathjax <- "file:///usr/share/javascript/mathjax/MathJax.js"
cdn_mathjax <- "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js"
pages <- list.files("docs", pattern = "[.]html$", recursive = TRUE,
                    full.names = TRUE)
for (page in pages) {
  html <- readLines(page, warn = FALSE, encoding = "UTF-8")
  if (any(grepl(local_mathjax, html, fixed = TRUE))) {
    writeLines(gsub(local_mathjax, cdn_mathjax, html, fixed = TRUE), page,
               useBytes = TRUE)
  }
}

###############################################################################
# Build the interactive HTML report from outputs/results.json
# Run from the es_framework folder:  Rscript report/build_report.R
# (run_all.R calls this automatically at the end)
###############################################################################

build_report <- function(results = "outputs/results.json",
                         template = "report/template.html",
                         out = "report/es_sensitivity_report.html") {
  # Read and write raw bytes so UTF-8 text survives in any locale
  read_raw <- function(f) readBin(f, "raw", file.info(f)$size)
  json <- read_raw(results)
  html <- read_raw(template)
  marker <- charToRaw("__DATA__")
  n <- length(marker)
  hits <- which(vapply(seq_len(length(html) - n + 1),
                       function(i) identical(html[i:(i + n - 1)], marker), logical(1)))
  if (length(hits) != 1) stop("Template must contain exactly one __DATA__ placeholder")
  page <- c(html[seq_len(hits - 1)], json, html[(hits + n):length(html)])
  writeBin(page, out)
  message("Report written to ", out)
  invisible(out)
}

if (sys.nframe() == 0) build_report()

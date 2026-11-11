# Publishing the ES sensitivity report on Posit Connect

Two options, each in its own folder with a `manifest.json`.

## Option 1: `static/` (simplest)

The finished report as a single HTML page. Connect serves it as-is and runs no R.

**Publish with Git-backed deployment or the Connect UI:** upload/point at the
`static/` folder. `manifest.json` declares `appmode: static` with `index.html` as
the primary document.

**Or from R:**
```r
rsconnect::deployApp("static", appPrimaryDoc = "index.html", appMode = "static",
                     appTitle = "ES decision trees: sensitivity and pinch points")
```
To refresh it after re-running the analysis, copy the new
`es_framework/report/es_sensitivity_report.html` over `static/index.html`, then
regenerate the manifest (checksums change):
```r
rsconnect::writeManifest("static", appFiles = "index.html",
                         appPrimaryDoc = "index.html", appMode = "static")
```

## Option 2: `rmarkdown/` (re-runs the analysis on Connect)

`es_report.Rmd` runs the whole framework every time Connect renders it:
1. the validation tests (the render **fails** if any test fails),
2. the full sensitivity analysis (`run_all.R`),
3. embeds the interactive report as the rendered page.

This lets you schedule re-renders in Connect, and swap in real (e.g. RSN) data
inside `run_all.R` later.

**Publish from R:**
```r
rsconnect::deployDoc("rmarkdown/es_report.Rmd",
                     appTitle = "ES decision trees: sensitivity and pinch points")
```

**Or with Git-backed deployment:** point Connect at the `rmarkdown/` folder;
`manifest.json` declares `appmode: rmd-static`.

### Regenerate the manifest on your own machine (recommended)
The included manifest was generated on R 4.3.3 with the package versions
available where it was built. Regenerate it with your R and package versions
before deploying with Git so Connect restores the same environment:
```r
setwd("rmarkdown")
rsconnect::writeManifest(appPrimaryDoc = "es_report.Rmd")
```

Packages used: dplyr, tidyr, ggplot2, jsonlite, testthat, rmarkdown, knitr.

## Notes
- The report is fully self-contained (no external data). The static version
  loads the Inter font from Google Fonts; the R Markdown version uses system
  fonts so rendering needs no internet access.
- Navigation uses `#overview`, `#recreation`, `#habitat`, `#water-flow`,
  `#water-quality`, `#water-supply`. These work inside Connect's viewer and in
  links you share.

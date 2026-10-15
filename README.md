# mistralOCR

`mistralOCR` helps you extract tables from PDF files using the Mistral OCR API.
It parses markdown tables into tibbles and can attach confidence information to extracted values.

## Features

- Submit local PDFs or images to Mistral OCR
- Extract markdown tables into R tibbles
- Recover per-cell confidence values (with pagewise fallback support)
- Optionally extract page header/footer text around tables
- Launch an interactive Shiny app for extraction and export

## Installation

```r
# install.packages("pak")
options(pkg.build_vignettes = TRUE)
pak::pak("NWTlter/mistralOCR")
```
Or
```
remotes::install_github("NWTlter/mistralOCR", build_vignettes = TRUE)
```
Or install from a local clone:

```r
devtools::install(".", build_vignettes = TRUE)
```

## Authentication

Set your Mistral API key in an environment variable:

```r
Sys.setenv(MISTRAL_API_KEY = "your-api-key")
```

For persistent setup, add this to your `.Renviron` file:

```r
usethis::edit_r_environ()
# Then add:
# MISTRAL_API_KEY=your-api-key
```

## Basic usage

```r
library(mistralOCR)

result <- mistral_ocr_pdf(
  pdf = "path/to/document.pdf",
  confidence_scores_granularity = "word",
  pagewise_confidence_fallback = TRUE
)

result$pages
result$table_titles
result$table_dfs[[1]]
```

## Shiny app

```r
app <- launch_mistral_ocr_app()
shiny::runApp(app)
```

## Notes

This package and outputs should be reviewed carefully before production use. Claude code was used extensively in creating this package.

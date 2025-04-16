#' Extract R Code from a Quarto (.qmd) File
#'
#' This function uses `knitr::purl()` to extract R code from a Quarto (`.qmd`) file
#' located in the `teaching_materials/` directory. It optionally includes markdown
#' content as comments for better context in the extracted script.
#'
#' @param filename A character string. The name of the `.qmd` file (e.g., `"fetching.qmd"`).
#' @param outname A character string for the output `.R` file name. If `NULL` (default),
#'   a name will be generated automatically by replacing `.qmd` with `_script.R` or `_annotated.R`.
#' @param annotated Logical. If `TRUE`, includes markdown as comments (like a teaching script).
#'   If `FALSE`, extracts only the raw R code.
#'
#' @returns Creates a `.R` script in the `teaching_materials/` directory as a side effect.
#'   Returns the output file path (invisibly).
#'
#' @export
#' @importFrom here here
#' @importFrom stringr str_replace
#' @importFrom knitr purl
#'
#' @examples
#' extract_code("fetching.qmd")
#' extract_code("fetching.qmd", annotated = TRUE)
#' extract_code("reporting.qmd", outname = "reporting_code.R")
extract_code <- function(filename, outname = NULL, annotated = FALSE) {
  if (is.null(outname)) {
    outname <- if (annotated) {
      stringr::str_replace(filename, "\\.qmd$", "_annotated.R")
    } else {
      stringr::str_replace(filename, "\\.qmd$", "_script.R")
    }
  }

  purl(
    input = here::here("teaching_materials", filename),
    output = here::here("teaching_materials", outname),
    documentation = if (annotated) 2 else 0
  )

  invisible(here::here("teaching_materials", outname))
}


#
library(here)
library(knitr)
library(here)

extract_code("fetching.qmd")
extract_code("reporting.qmd", annotated = TRUE)
extract_code("example2_statistics_estonia.qmd", outname = "example2.R")
extract_code("example1_dbnomics.qmd", outname = "example1.R")

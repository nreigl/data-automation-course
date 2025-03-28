#' Generate a BibTeX/BibLaTeX citation for a DBnomics time series
#'
#' Fetches metadata from the DBnomics API for a specific provider/dataset/series
#' and returns a formatted citation as a BibTeX, BibLaTeX, or BibEntry object.
#'
#' @param provider Character string. The DBnomics provider code (e.g., `"ECB"`).
#' @param dataset Character string. The DBnomics dataset code (e.g., `"MIR"`).
#' @param series Character string. The DBnomics series code (e.g., `"M.EE.B.A2C.A.R.A.2250.EUR.N"`).
#' @param format Output format: `"biblatex"` (default), `"bibtex"`, or `"bibentry"` (raw RefManageR object).
#'
#' @return A citation entry as a character string (BibTeX/BibLaTeX) or a BibEntry object.
#'
#' @export
#'
#' @examples
#' get_rdb_bibentry(
#'   provider = "ECB",
#'   dataset = "MIR",
#'   series = "M.EE.B.A2C.A.R.A.2250.EUR.N"
#' )
get_rdb_bibentry <- function(provider, dataset, series, format = "biblatex") {
  # Required packages
  if (!requireNamespace("RefManageR", quietly = TRUE)) {
    stop("Package 'RefManageR' is required.")
  }
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Packages 'httr' and 'jsonlite' are required.")
  }

  # Build metadata URL
  series_url <- paste0("https://api.db.nomics.world/v22/series/", provider, "/", dataset, "/", series)

  # Fetch metadata
  response <- httr::GET(series_url)
  stopifnot(httr::status_code(response) == 200)
  json_text <- httr::content(response, as = "text", encoding = "UTF-8")
  meta <- jsonlite::fromJSON(json_text)$series$docs

  # Extract fields
  title <- meta$title
  last_update <- as.Date(meta$updated_at)
  year <- format(Sys.Date(), "%Y")
  urldate <- format(Sys.Date(), "%Y-%m-%d")
  citation_key <- gsub("_", "-", paste0(dataset, "-", urldate))

  # Create BibEntry object
  entry <- RefManageR::BibEntry(
    bibtype = "misc",
    key     = citation_key,
    title   = paste0(title, " (", series, ")"),
    url     = series_url,
    language = "english",
    year    = year,
    author  = provider,
    urldate = urldate,
    type    = "Dataset",
    note    = paste0("Accessed ", urldate, ", series last updated ", last_update, ".")
  )

  if (format == "bibtex") {
    return(utils::toBibtex(entry))
  } else if (format == "biblatex") {
    return(RefManageR::toBiblatex(entry))
  } else {
    return(entry)
  }
}




#' Generate a BibTeX/BibLaTeX citation for a DBnomics dataset
#'
#' This function generates a citation entry for a full dataset from a DBnomics provider
#' (e.g. "IMF", "ECB") using the DBnomics dataset metadata endpoint.
#'
#' @param provider Character. DBnomics provider code (e.g. "IMF").
#' @param dataset Character. Dataset code (e.g. "BOP").
#' @param format Citation format. One of "biblatex" (default), "bibtex", or "bibentry".
#'
#' @return A citation as a BibLaTeX/BibTeX string or a RefManageR::BibEntry.
#' @export
#'
#' @examples
#' get_rdb_dataset_bibentry("IMF", "BOP")
get_rdb_dataset_bibentry <- function(provider, dataset, format = "biblatex") {
  if (!requireNamespace("RefManageR", quietly = TRUE)) {
    stop("Package 'RefManageR' is required.")
  }

  url <- paste0("https://api.db.nomics.world/v22/datasets/", provider, "/", dataset)
  response <- httr::GET(url)
  stopifnot(httr::status_code(response) == 200)
  json_text <- httr::content(response, as = "text", encoding = "UTF-8")
  meta <- jsonlite::fromJSON(json_text)$dataset$docs

  title <- meta$title
  last_update <- as.Date(meta$updated_at)
  year <- format(Sys.Date(), "%Y")
  urldate <- format(Sys.Date(), "%Y-%m-%d")
  citation_key <- gsub("_", "-", paste0(dataset, "-", urldate))
  dataset_url <- paste0("https://db.nomics.world/", provider, "/", dataset)

  entry <- RefManageR::BibEntry(
    bibtype = "misc",
    key     = citation_key,
    title   = paste0(title, " (", dataset, ")"),
    url     = dataset_url,
    language = "english",
    year    = year,
    author  = provider,
    urldate = urldate,
    type    = "Dataset",
    note    = paste0("Accessed ", urldate, ", dataset last updated ", last_update, ".")
  )

  if (format == "bibtex") {
    return(utils::toBibtex(entry))
  } else if (format == "biblatex") {
    return(RefManageR::toBiblatex(entry))
  } else {
    return(entry)
  }
}

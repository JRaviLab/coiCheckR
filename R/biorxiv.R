#' Look up a bioRxiv (or medRxiv) preprint's author list by DOI
#'
#' The public bioRxiv API (<https://api.biorxiv.org>) has no author- or
#' name-search endpoint -- it only resolves metadata for a DOI you
#' already have. Use this to *enrich* a preprint you already found (e.g.
#' via PubMed's "preprint" linkouts, or a DOI cited in the manuscript's
#' bibliography), not to discover a candidate's preprints from scratch.
#' For preprint discovery, PubMed itself now indexes many bioRxiv/medRxiv
#' records directly, so [pmSearchAuthor()] is often sufficient.
#'
#' @param doi Character scalar, e.g. `"10.1101/2020.05.17.095000"`.
#' @param server One of `"biorxiv"` or `"medrxiv"`.
#'
#' @return A one-row tibble with `doi`, `title`, `authors` (single
#'   semicolon-delimited string, as returned by the API), `date`,
#'   `category`, `published` (linked journal DOI once formally
#'   published, or `NA`). Returns zero rows if the DOI is not found.
#' @examples
#' tryCatch(
#'   biorxivLookup("10.1101/2020.05.17.095000"),
#'   error = function(e) message("bioRxiv API unavailable: ", conditionMessage(e))
#' )
#' @export
biorxivLookup <- function(doi, server = c("biorxiv", "medrxiv")) {
  server <- match.arg(server)
  url <- sprintf("https://api.biorxiv.org/details/%s/%s", server, doi)

  resp <- httr2::request(url) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) >= 400) {
    warning(sprintf(
      "bioRxiv lookup failed (HTTP %s) for doi = '%s'",
      httr2::resp_status(resp), doi
    ))
    return(.empty_biorxiv_tbl())
  }

  parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  coll <- parsed$collection
  if (length(coll) == 0) {
    return(.empty_biorxiv_tbl())
  }

  rec <- coll[[length(coll)]] # last entry = most recent version
  tibble::tibble(
    doi = rec$doi %||% doi,
    title = rec$title %||% NA_character_,
    authors = rec$authors %||% NA_character_,
    date = rec$date %||% NA_character_,
    category = rec$category %||% NA_character_,
    published = rec$published %||% NA_character_
  )
}

.empty_biorxiv_tbl <- function() {
  tibble::tibble(
    doi = character(), title = character(), authors = character(),
    date = character(), category = character(), published = character()
  )
}

#' Split a bioRxiv `authors` string into individual names
#'
#' The API returns authors as a single string, typically
#' `"Last F.; Last2 F.M.; ..."`. This splits it into a character vector
#' for downstream matching against PubMed-style names.
#'
#' @param authors_string A single string as returned in
#'   [biorxivLookup()]'s `authors` column.
#' @return Character vector of individual author names.
#' @examples
#' biorxivSplitAuthors("Smith A.; Lee C.D.; Doe E.")
#' @export
biorxivSplitAuthors <- function(authors_string) {
  if (is.na(authors_string) || !nzchar(authors_string)) {
    return(character())
  }
  trimws(strsplit(authors_string, ";")[[1]])
}

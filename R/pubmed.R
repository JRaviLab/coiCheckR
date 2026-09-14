#' Search PubMed for an author's publications
#'
#' Thin wrapper around [rentrez::entrez_search()] scoped to the `pubmed`
#' database, using the `[Author]` and (optionally) `[Affiliation]` search
#' fields. Affiliation matching in PubMed is a substring match on the
#' recorded affiliation string, so keep it short (e.g. `"Colorado"`
#' rather than a full department name) to avoid false negatives from
#' formatting drift across papers.
#'
#' @param author Character scalar, `"Last FM"` format (PubMed convention),
#'   e.g. `"Smith AB"`. A full forename (e.g. `"Smith Alice"`) is also
#'   accepted and often narrows results *better* than initials for a
#'   common surname -- PubMed's `[Author]` field search is not sensitive
#'   to whether the forename is spelled out or abbreviated.
#' @param affiliation Optional character scalar, or vector, to narrow by
#'   affiliation substring (OR-combined *across* vector elements when
#'   more than one is given). Recommended when the surname is common. A
#'   single affiliation string only matches papers published while at
#'   that institution -- someone with a multi-institution career needs
#'   each affiliation listed as a separate vector element (e.g.
#'   `c("Colorado", "Michigan", "Rutgers")`) to avoid silently dropping
#'   earlier-career publications. *Within* one element, though, multiple
#'   words are matched as a phrase, not OR'd word-by-word -- prefer a
#'   single short, distinctive word (e.g. `"Colorado"`) per institution
#'   over its full official name, since a full name has to match
#'   whatever exact phrasing that specific paper happened to record
#'   (department, campus, etc.), which varies paper to paper, whereas a
#'   short word is robust to that variation.
#' @param min_year,max_year Optional integer bounds on publication year.
#' @param retmax Maximum records to retrieve. Default 300.
#'
#' @return Character vector of PMIDs (possibly empty).
#' @examples
#' \donttest{
#' # Live PubMed call -- \donttest since "Smith AB" is common enough
#' # that this can be slow (many real, unrelated hits) and isn't run
#' # by default during R CMD check or routine CRAN/Bioconductor checks.
#' tryCatch(
#'   pmSearchAuthor("Smith AB", affiliation = "State University"),
#'   error = function(e) message("Live PubMed API unavailable: ", conditionMessage(e))
#' )
#' }
#' @export
pmSearchAuthor <- function(author,
                           affiliation = NULL,
                           min_year = NULL,
                           max_year = NULL,
                           retmax = 300) {
  stopifnot(is.character(author), length(author) == 1)

  term <- stringr::str_glue("{author}[Author]")
  if (!is.null(affiliation)) {
    aff_clause <- stringr::str_c(
      stringr::str_c(affiliation, "[Affiliation]"),
      collapse = " OR "
    )
    term <- stringr::str_glue("{term} AND ({aff_clause})")
  }
  if (!is.null(min_year) || !is.null(max_year)) {
    lo <- if (is.null(min_year)) "1900" else as.character(min_year)
    hi <- if (is.null(max_year)) {
      format(Sys.Date(), "%Y")
    } else {
      as.character(max_year)
    }
    term <- stringr::str_glue("{term} AND ({lo}:{hi}[pdat])")
  }

  res <- tryCatch(
    rentrez::entrez_search(db = "pubmed", term = term, retmax = retmax),
    error = function(e) {
      warning(stringr::str_glue(
        "PubMed search failed for author = '{author}': {conditionMessage(e)}"
      ), call. = FALSE)
      NULL
    }
  )
  if (is.null(res)) {
    return(.markSourceFailed(character()))
  }
  res$ids
}

#' Fetch author lists and metadata for a set of PMIDs
#'
#' Retrieves PubMed XML records in batches (E-utilities caps `efetch` at
#' ~200 IDs per call for unauthenticated use; this function chunks
#' automatically) and parses each record's author list, publication year,
#' and journal.
#'
#' @param PMIDs Character or integer vector of PMIDs.
#' @param batch_size Records per `efetch` call. Default 150 (safely under
#'   the unauthenticated rate cap; raise if you set an NCBI API key via
#'   `rentrez::set_entrez_key()`).
#' @param pause Seconds to sleep between batches, to stay within NCBI's
#'   rate limits (3 req/sec without a key, 10 req/sec with one).
#'
#' @return A [tibble::tibble()] with one row per (PMID, author): columns
#'   `PMID`, `year`, `journal`, `author_last`, `author_fore`,
#'   `affiliation`.
#' @examples
#' \donttest{
#' # Live PubMed calls -- \donttest for the same reason as
#' # pmSearchAuthor()'s example.
#' tryCatch(
#'   {
#'     ids <- pmSearchAuthor("Smith AB")
#'     pmFetchAuthors(ids[seq_len(min(5, length(ids)))])
#'   },
#'   error = function(e) message("Live PubMed API unavailable: ", conditionMessage(e))
#' )
#' }
#' @export
pmFetchAuthors <- function(PMIDs, batch_size = 150, pause = 0.4) {
  PMIDs <- unique(as.character(PMIDs))
  if (length(PMIDs) == 0) {
    return(.pmEmptyAuthorsTbl())
  }

  chunks <- split(PMIDs, ceiling(seq_along(PMIDs) / batch_size))

  rows <- purrr::map(chunks, function(ids) {
    Sys.sleep(pause)
    tryCatch(
      {
        xml_txt <- rentrez::entrez_fetch(db = "pubmed", id = ids, rettype = "xml")
        doc <- xml2::read_xml(xml_txt)
        articles <- xml2::xml_find_all(doc, ".//PubmedArticle")
        .pmParseArticles(articles)
      },
      error = function(e) {
        warning(stringr::str_glue(
          "PubMed fetch failed for a batch of {length(ids)} PMID(s): {conditionMessage(e)}"
        ), call. = FALSE)
        NULL
      }
    )
  })

  is_failed_chunk <- purrr::map_lgl(rows, is.null)
  failed <- any(is_failed_chunk)
  rows <- rows[!is_failed_chunk]
  out <- if (length(rows) == 0) .pmEmptyAuthorsTbl() else dplyr::bind_rows(rows)
  if (failed) out <- .markSourceFailed(out)
  out
}

.pmEmptyAuthorsTbl <- function() {
  tibble::tibble(
    PMID = character(), year = integer(), journal = character(),
    author_last = character(), author_fore = character(),
    affiliation = character()
  )
}

.pmParseArticles <- function(articles) {
  purrr::map_dfr(articles, function(art) {
    PMID <- xml2::xml_text(xml2::xml_find_first(art, ".//PMID"))
    year <- xml2::xml_text(xml2::xml_find_first(
      art, ".//PubDate/Year | .//PubDate/MedlineDate"
    ))
    year_str <- stringr::str_sub(year, 1, 4)
    year <- if (stringr::str_detect(year_str, "^[0-9]{4}$")) as.integer(year_str) else NA_integer_
    journal <- xml2::xml_text(xml2::xml_find_first(art, ".//Journal/Title"))

    auths <- xml2::xml_find_all(art, ".//AuthorList/Author")
    if (length(auths) == 0) {
      return(tibble::tibble(
        PMID = PMID, year = year, journal = journal,
        author_last = NA_character_, author_fore = NA_character_,
        affiliation = NA_character_
      ))
    }

    purrr::map_dfr(auths, function(a) {
      tibble::tibble(
        PMID = PMID, year = year, journal = journal,
        author_last = xml2::xml_text(xml2::xml_find_first(a, ".//LastName")),
        author_fore = xml2::xml_text(xml2::xml_find_first(a, ".//ForeName")),
        affiliation = xml2::xml_text(xml2::xml_find_first(
          a, ".//AffiliationInfo/Affiliation"
        ))
      )
    })
  })
}

#' Get a tidy co-authorship table for one author
#'
#' Convenience wrapper combining [pmSearchAuthor()] and
#' [pmFetchAuthors()].
#'
#' @inheritParams pmSearchAuthor
#' @param ... Passed to [pmFetchAuthors()].
#'
#' @return Tibble as returned by [pmFetchAuthors()], plus a
#'   `query_author` column identifying whose search produced each row.
#' @examples
#' \donttest{
#' # Live PubMed calls -- \donttest since "Smith AB" fetches every
#' # matching record's full XML, which can be slow for a common name.
#' tryCatch(
#'   pmCoauthors("Smith AB"),
#'   error = function(e) message("Live PubMed API unavailable: ", conditionMessage(e))
#' )
#' }
#' @export
pmCoauthors <- function(author, affiliation = NULL, min_year = NULL,
                        max_year = NULL, retmax = 300, ...) {
  PMIDs <- pmSearchAuthor(author, affiliation, min_year, max_year, retmax)
  search_failed <- .sourceFailed(PMIDs)
  out <- pmFetchAuthors(PMIDs, ...)
  fetch_failed <- .sourceFailed(out)
  out <- dplyr::mutate(out, query_author = author)
  if (search_failed || fetch_failed) out <- .markSourceFailed(out)
  out
}

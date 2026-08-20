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
#'   e.g. `"Smith AB"`.
#' @param affiliation Optional character scalar to narrow by affiliation
#'   substring. Recommended when the surname is common.
#' @param min_year,max_year Optional integer bounds on publication year.
#' @param retmax Maximum records to retrieve. Default 300.
#'
#' @return Character vector of PMIDs (possibly empty).
#' @export
pmSearchAuthor <- function(author,
                           affiliation = NULL,
                           min_year = NULL,
                           max_year = NULL,
                           retmax = 300) {
  stopifnot(is.character(author), length(author) == 1)

  term <- sprintf("%s[Author]", author)
  if (!is.null(affiliation)) {
    term <- sprintf("%s AND %s[Affiliation]", term, affiliation)
  }
  if (!is.null(min_year) || !is.null(max_year)) {
    lo <- if (is.null(min_year)) "1900" else as.character(min_year)
    hi <- if (is.null(max_year)) {
      format(Sys.Date(), "%Y")
    } else {
      as.character(max_year)
    }
    term <- sprintf("%s AND (%s:%s[pdat])", term, lo, hi)
  }

  res <- rentrez::entrez_search(db = "pubmed", term = term, retmax = retmax)
  res$ids
}

#' Fetch author lists and metadata for a set of PMIDs
#'
#' Retrieves PubMed XML records in batches (E-utilities caps `efetch` at
#' ~200 IDs per call for unauthenticated use; this function chunks
#' automatically) and parses each record's author list, publication year,
#' and journal.
#'
#' @param pmids Character or integer vector of PMIDs.
#' @param batch_size Records per `efetch` call. Default 150 (safely under
#'   the unauthenticated rate cap; raise if you set an NCBI API key via
#'   `rentrez::set_entrez_key()`).
#' @param pause Seconds to sleep between batches, to stay within NCBI's
#'   rate limits (3 req/sec without a key, 10 req/sec with one).
#'
#' @return A [tibble::tibble()] with one row per (pmid, author): columns
#'   `pmid`, `year`, `journal`, `author_last`, `author_fore`,
#'   `affiliation`.
#' @export
pmFetchAuthors <- function(pmids, batch_size = 150, pause = 0.4) {
  pmids <- unique(as.character(pmids))
  if (length(pmids) == 0) {
    return(tibble::tibble(
      pmid = character(), year = integer(), journal = character(),
      author_last = character(), author_fore = character(),
      affiliation = character()
    ))
  }

  chunks <- split(pmids, ceiling(seq_along(pmids) / batch_size))

  rows <- purrr::map(chunks, function(ids) {
    Sys.sleep(pause)
    xml_txt <- rentrez::entrez_fetch(db = "pubmed", id = ids, rettype = "xml")
    doc <- xml2::read_xml(xml_txt)
    articles <- xml2::xml_find_all(doc, ".//PubmedArticle")

    purrr::map_dfr(articles, function(art) {
      pmid <- xml2::xml_text(xml2::xml_find_first(art, ".//PMID"))
      year <- xml2::xml_text(xml2::xml_find_first(
        art, ".//PubDate/Year | .//PubDate/MedlineDate"
      ))
      year <- suppressWarnings(as.integer(substr(year, 1, 4)))
      journal <- xml2::xml_text(xml2::xml_find_first(art, ".//Journal/Title"))

      auths <- xml2::xml_find_all(art, ".//AuthorList/Author")
      if (length(auths) == 0) {
        return(tibble::tibble(
          pmid = pmid, year = year, journal = journal,
          author_last = NA_character_, author_fore = NA_character_,
          affiliation = NA_character_
        ))
      }

      purrr::map_dfr(auths, function(a) {
        tibble::tibble(
          pmid = pmid, year = year, journal = journal,
          author_last = xml2::xml_text(xml2::xml_find_first(a, ".//LastName")),
          author_fore = xml2::xml_text(xml2::xml_find_first(a, ".//ForeName")),
          affiliation = xml2::xml_text(xml2::xml_find_first(
            a, ".//AffiliationInfo/Affiliation"
          ))
        )
      })
    })
  })

  dplyr::bind_rows(rows)
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
#' @export
pmCoauthors <- function(author, affiliation = NULL, min_year = NULL,
                        max_year = NULL, retmax = 300, ...) {
  pmids <- pmSearchAuthor(author, affiliation, min_year, max_year, retmax)
  out <- pmFetchAuthors(pmids, ...)
  out$query_author <- author
  out
}

#' Screen a candidate reviewer against a manuscript's authors
#'
#' The main entry point. Combines direct co-authorship (PubMed),
#' second-degree co-authorship, and shared NIH funding (RePORTER) into a
#' single tidy report. Returns evidence, not a verdict -- this package
#' deliberately does not emit a binary "conflicted / clear" flag, in
#' line with journal guidance (e.g. JOSS's COI policy) that COI judgment
#' should remain with an editor who can weigh context.
#'
#' @param candidate_name Character scalar, `"Last FM"` PubMed-style name.
#'   A full forename (`"Last Firstname"`) is also fine -- internally
#'   normalized to initials for matching -- and often searches PubMed
#'   more precisely than bare initials for a common surname (see
#'   [pmSearchAuthor()]'s docs).
#' @param author_names Character vector of the manuscript's author names,
#'   same format.
#' @param affiliation Optional affiliation substring, or vector of
#'   substrings (OR-combined), to disambiguate a common candidate
#'   surname (passed to [pmSearchAuthor()]). A single string only
#'   matches papers from that institution -- list every institution for
#'   a candidate with a multi-institution career.
#' @param author_affiliations The `author_names`-side counterpart to
#'   `affiliation`, applied within the second-degree check (direct
#'   co-authorship doesn't independently search each author, so this has
#'   no effect there). `NULL` (default), a plain vector applied to every
#'   author, or a list the same length as `author_names` for per-author
#'   values -- see [secondDegreeConflicts()].
#' @param coauthor_window_years Lookback window in years for *direct*
#'   co-authorship, or `NULL` for unrestricted. Many journals use 3--4
#'   years (e.g. NIH study section policy uses 3); ICMJE-aligned
#'   journals vary.
#' @param check_second_degree Logical; include second-degree
#'   co-authorship screening (slower -- one PubMed query per author).
#' @param check_funding Logical; include NIH RePORTER shared-award
#'   screening.
#' @param funding_fiscal_years Optional integer vector to scope the
#'   RePORTER query.
#' @param candidate_org_names,author_org_names Optional
#'   [reporterSearchPI()] `org_names` filter, analogous to
#'   `affiliation`/`author_affiliations` but for the NIH RePORTER
#'   funding check -- see [reporterSharedAwards()].
#'
#' @return An object of class `coiReport`: a list of tibbles (`direct`,
#'   `second_degree`, `funding`), plus `status` -- one of
#'   `"potential_conflict"`, `"no_conflict_detected"`, or
#'   `"insufficient_evidence"` -- and `sources_failed`, a character
#'   vector naming any of `"pubmed"`, `"pubmed_second_degree"`,
#'   `"nih_reporter"` that errored rather than returning zero rows. A
#'   source failure is never silently treated as "no conflict": if no
#'   evidence was found *and* a source failed, `status` is
#'   `"insufficient_evidence"`, not `"no_conflict_detected"`. Printable
#'   via the package's `print.coiReport` method, which gives a one-line
#'   summary per evidence type.
#' @examples
#' \donttest{
#' # Live PubMed/RePORTER calls -- \donttest since "Smith"/"Lee" are
#' # common enough that this can be slow (many real, unrelated hits)
#' # and isn't run by default during R CMD check or routine
#' # CRAN/Bioconductor checks. Use `affiliation` for a real candidate.
#' tryCatch(
#'   checkCoi("Smith AB", "Lee C"),
#'   error = function(e) message("Live PubMed/RePORTER API unavailable: ", conditionMessage(e))
#' )
#' }
#' @export
checkCoi <- function(candidate_name,
                     author_names,
                     affiliation = NULL,
                     author_affiliations = NULL,
                     coauthor_window_years = NULL,
                     check_second_degree = TRUE,
                     check_funding = TRUE,
                     funding_fiscal_years = NULL,
                     candidate_org_names = NULL,
                     author_org_names = NULL) {
  stopifnot(is.character(candidate_name), length(candidate_name) == 1)
  stopifnot(is.character(author_names), length(author_names) >= 1)

  min_year <- if (!is.null(coauthor_window_years)) {
    as.integer(format(Sys.Date(), "%Y")) - coauthor_window_years
  } else {
    NULL
  }

  sources_failed <- character()

  # --- direct co-authorship ---
  direct <- .directCoauthorship(candidate_name, author_names, affiliation, min_year)
  if (.sourceFailed(direct)) sources_failed <- c(sources_failed, "pubmed")

  # --- second-degree ---
  second_degree <- if (check_second_degree) {
    sd <- secondDegreeConflicts(
      candidate_name, author_names,
      affiliation = affiliation, author_affiliations = author_affiliations,
      min_year = min_year
    )
    if (.sourceFailed(sd)) sources_failed <- c(sources_failed, "pubmed_second_degree")
    sd
  } else {
    tibble::tibble()
  }

  # --- shared funding ---
  funding <- if (check_funding) {
    f <- reporterSharedAwards(
      candidate_name, author_names, fiscal_years = funding_fiscal_years,
      candidate_org_names = candidate_org_names, author_org_names = author_org_names
    )
    if (.sourceFailed(f)) sources_failed <- c(sources_failed, "nih_reporter")
    f
  } else {
    tibble::tibble()
  }

  status <- .classifyCoiStatus(direct, second_degree, funding, sources_failed)

  structure(
    list(
      candidate = candidate_name,
      authors = author_names,
      direct = direct,
      second_degree = second_degree,
      funding = funding,
      status = status,
      sources_failed = sources_failed
    ),
    class = "coiReport"
  )
}

# Direct (first-degree) co-authorship between candidate and manuscript authors.
.directCoauthorship <- function(candidate_name, author_names, affiliation,
                                min_year) {
  cand_pubs <- pmCoauthors(
    candidate_name, affiliation = affiliation, min_year = min_year
  )
  failed <- .sourceFailed(cand_pubs)
  cand_canonical <- .pm_canonical_name(candidate_name)
  author_canonical <- .pm_canonical_name(author_names)
  cand_edges <- buildCoauthorEdges(cand_pubs) |>
    dplyr::filter(
      .pm_name_matches_any(.data$from, cand_canonical) |
        .pm_name_matches_any(.data$to, cand_canonical)
    ) |>
    dplyr::mutate(other = dplyr::if_else(
      .pm_name_matches_any(.data$from, cand_canonical), .data$to, .data$from
    ))

  result <- cand_edges |>
    dplyr::filter(.pm_name_matches_any(.data$other, author_canonical)) |>
    dplyr::transmute(
      candidate = candidate_name,
      author = .data$other,
      PMID = .data$PMID,
      year = .data$year
    ) |>
    dplyr::distinct()

  if (failed) result <- .markSourceFailed(result)
  result
}

#' @export
print.coiReport <- function(x, ...) {
  status <- x$status %||% NA_character_
  # str_glue()/glue() trims a *trailing* newline by default, so the "\n"
  # is appended outside the glue call rather than inside its template --
  # otherwise these lines would all run together with no line breaks.
  cat(stringr::str_glue(
    "COI screen: {x$candidate} vs. {length(x$authors)} author(s)"
  ), "\n", sep = "")
  cat(stringr::str_glue("  status:                  {status}"), "\n", sep = "")
  cat(stringr::str_glue("  direct co-authorship:    {nrow(x$direct)} hit(s)"), "\n", sep = "")
  cat(stringr::str_glue("  second-degree overlap:   {nrow(x$second_degree)} hit(s)"), "\n", sep = "")
  cat(stringr::str_glue("  shared NIH awards:       {nrow(x$funding)} hit(s)"), "\n", sep = "")
  if (length(x$sources_failed) > 0) {
    cat(stringr::str_glue(
      "  ! sources failed (evidence may be incomplete): {stringr::str_c(x$sources_failed, collapse = ', ')}"
    ), "\n", sep = "")
  }
  if (nrow(x$direct) > 0) {
    cat("\n  -- direct --\n")
    print(x$direct)
  }
  if (nrow(x$second_degree) > 0) {
    cat("\n  -- second-degree (review, not auto-exclude) --\n")
    print(x$second_degree)
  }
  if (nrow(x$funding) > 0) {
    cat("\n  -- shared funding --\n")
    print(x$funding)
  }
  invisible(x)
}

#' Screen multiple candidates at once
#'
#' Vectorised convenience wrapper over [checkCoi()].
#'
#' @param candidate_names Character vector of candidates.
#' @param author_names As in [checkCoi()].
#' @param ... Passed to [checkCoi()].
#'
#' @return A named list of `coiReport` objects, one per candidate, plus
#'   a `$summary` tibble (`candidate`, `status`, `n_direct`,
#'   `n_second_degree`, `n_funding`, `sources_failed`) for quick triage.
#' @examples
#' \donttest{
#' # Live PubMed/RePORTER calls (multiplied across every candidate) --
#' # \donttest for the same reason as checkCoi()'s example.
#' tryCatch(
#'   checkCoiBatch(c("Smith AB", "Doe C"), "Lee C"),
#'   error = function(e) message("Live PubMed/RePORTER API unavailable: ", conditionMessage(e))
#' )
#' }
#' @export
checkCoiBatch <- function(candidate_names, author_names, ...) {
  reports <- purrr::map(
    candidate_names,
    ~ checkCoi(.x, author_names, ...)
  )
  names(reports) <- candidate_names

  summary_tbl <- purrr::map_dfr(reports, function(r) {
    tibble::tibble(
      candidate = r$candidate,
      status = r$status %||% NA_character_,
      n_direct = nrow(r$direct),
      n_second_degree = nrow(r$second_degree),
      n_funding = nrow(r$funding),
      sources_failed = stringr::str_c(r$sources_failed, collapse = ", ")
    )
  })

  c(reports, list(summary = summary_tbl))
}

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
#' @param author_names Character vector of the manuscript's author names,
#'   same format.
#' @param affiliation Optional affiliation substring to disambiguate a
#'   common candidate surname (passed to [pmSearchAuthor()]).
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
#' tryCatch(
#'   checkCoi("Smith AB", "Lee C"),
#'   error = function(e) message("Live PubMed/RePORTER API unavailable: ", conditionMessage(e))
#' )
#' @export
checkCoi <- function(candidate_name,
                     author_names,
                     affiliation = NULL,
                     coauthor_window_years = NULL,
                     check_second_degree = TRUE,
                     check_funding = TRUE,
                     funding_fiscal_years = NULL) {
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
      affiliation = affiliation, min_year = min_year
    )
    if (.sourceFailed(sd)) sources_failed <- c(sources_failed, "pubmed_second_degree")
    sd
  } else {
    tibble::tibble()
  }

  # --- shared funding ---
  funding <- if (check_funding) {
    f <- reporterSharedAwards(
      candidate_name, author_names, fiscal_years = funding_fiscal_years
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
  cand_edges <- buildCoauthorEdges(cand_pubs) |>
    dplyr::filter(
      .data$from == candidate_name | .data$to == candidate_name
    ) |>
    dplyr::mutate(other = dplyr::if_else(
      .data$from == candidate_name, .data$to, .data$from
    ))

  result <- cand_edges |>
    dplyr::filter(.data$other %in% author_names) |>
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
  cat(stringr::str_glue(
    "COI screen: {x$candidate} vs. {length(x$authors)} author(s)\n"
  ))
  cat(stringr::str_glue("  status:                  {status}\n"))
  cat(stringr::str_glue("  direct co-authorship:    {nrow(x$direct)} hit(s)\n"))
  cat(stringr::str_glue("  second-degree overlap:   {nrow(x$second_degree)} hit(s)\n"))
  cat(stringr::str_glue("  shared NIH awards:       {nrow(x$funding)} hit(s)\n"))
  if (length(x$sources_failed) > 0) {
    cat(stringr::str_glue(
      "  ! sources failed (evidence may be incomplete): {stringr::str_c(x$sources_failed, collapse = ', ')}\n"
    ))
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
#' tryCatch(
#'   checkCoiBatch(c("Smith AB", "Doe C"), "Lee C"),
#'   error = function(e) message("Live PubMed/RePORTER API unavailable: ", conditionMessage(e))
#' )
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

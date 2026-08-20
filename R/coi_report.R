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
#'   common candidate surname (passed to [pm_search_author()]).
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
#' @return An object of class `coi_report` (a list of tibbles:
#'   `direct`, `second_degree`, `funding`), printable via the package's
#'   `print.coi_report` method, which gives a one-line summary per
#'   evidence type.
#' @export
check_coi <- function(candidate_name,
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

  # --- direct co-authorship ---
  cand_pubs <- pm_coauthors(candidate_name, affiliation = affiliation, min_year = min_year)
  cand_edges <- build_coauthor_edges(cand_pubs) |>
    dplyr::filter(.data$from == candidate_name | .data$to == candidate_name) |>
    dplyr::mutate(other = ifelse(.data$from == candidate_name, .data$to, .data$from))

  direct <- cand_edges |>
    dplyr::filter(.data$other %in% author_names) |>
    dplyr::transmute(
      candidate = candidate_name,
      author = .data$other,
      pmid = .data$pmid,
      year = .data$year
    ) |>
    dplyr::distinct()

  # --- second-degree ---
  second_degree <- if (check_second_degree) {
    second_degree_conflicts(candidate_name, author_names, min_year = min_year)
  } else {
    tibble::tibble()
  }

  # --- shared funding ---
  funding <- if (check_funding) {
    reporter_shared_awards(candidate_name, author_names, fiscal_years = funding_fiscal_years)
  } else {
    tibble::tibble()
  }

  structure(
    list(
      candidate = candidate_name,
      authors = author_names,
      direct = direct,
      second_degree = second_degree,
      funding = funding
    ),
    class = "coi_report"
  )
}

#' @export
print.coi_report <- function(x, ...) {
  cat(sprintf("COI screen: %s vs. %d author(s)\n", x$candidate, length(x$authors)))
  cat(sprintf("  direct co-authorship:    %d hit(s)\n", nrow(x$direct)))
  cat(sprintf("  second-degree overlap:   %d hit(s)\n", nrow(x$second_degree)))
  cat(sprintf("  shared NIH awards:       %d hit(s)\n", nrow(x$funding)))
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
#' Vectorised convenience wrapper over [check_coi()].
#'
#' @param candidate_names Character vector of candidates.
#' @param author_names As in [check_coi()].
#' @param ... Passed to [check_coi()].
#'
#' @return A named list of `coi_report` objects, one per candidate, plus
#'   a `$summary` tibble (`candidate`, `n_direct`, `n_second_degree`,
#'   `n_funding`) for quick triage.
#' @export
check_coi_batch <- function(candidate_names, author_names, ...) {
  reports <- purrr::map(
    candidate_names,
    ~ check_coi(.x, author_names, ...)
  )
  names(reports) <- candidate_names

  summary_tbl <- purrr::map_dfr(reports, function(r) {
    tibble::tibble(
      candidate = r$candidate,
      n_direct = nrow(r$direct),
      n_second_degree = nrow(r$second_degree),
      n_funding = nrow(r$funding)
    )
  })

  c(reports, list(summary = summary_tbl))
}

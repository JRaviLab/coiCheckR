#' Search NIH RePORTER for a principal investigator's awards
#'
#' Queries the NIH RePORTER v2 Projects API
#' (<https://api.reporter.nih.gov/v2/projects/search>), which is free and
#' requires no API key. Returns every project where the queried name
#' appears as a contact PI or co-PI (multi-PI awards list all PIs under
#' `principal_investigators`).
#'
#' NIH RePORTER covers NIH-funded projects only. It will not catch NSF,
#' DOE, or foundation (e.g. Gates) funding; see `coiCheckR` README for
#' notes on extending to `api.nsf.gov` for NSF awards.
#'
#' @param PI_name Character scalar, `"Last, First"` or just `"Last"`. A
#'   `"Last, First"` name is split and matched against RePORTER's
#'   `first_name`/`last_name` fields; the live API's `any_name` field
#'   does not accept the comma form (it returns zero rows for it even
#'   when the name is a real, funded PI) so this parses it instead. A
#'   bare `"Last"` falls back to `any_name`'s substring/fuzzy match.
#' @param fiscal_years Optional integer vector of fiscal years to
#'   restrict the search (recommended -- an unrestricted query against a
#'   common surname can return hundreds of unrelated projects).
#' @param limit Max records per page (API caps at 500).
#'
#' @return A tibble with one row per project: `project_num`,
#'   `fiscal_year`, `org_name`, `project_title`, `contact_PI`,
#'   `all_PIs` (a list-column of every PI/co-PI name on that award).
#' @examples
#' tryCatch(
#'   reporterSearchPI("Smith, Anne", fiscal_years = 2020:2023),
#'   error = function(e) message("NIH RePORTER API unavailable: ", conditionMessage(e))
#' )
#' @export
reporterSearchPI <- function(PI_name, fiscal_years = NULL, limit = 500) {
  stopifnot(is.character(PI_name), length(PI_name) == 1)

  body <- .reporterRequestBody(PI_name, fiscal_years, limit)

  resp <- tryCatch(
    httr2::request("https://api.reporter.nih.gov/v2/projects/search") |>
      httr2::req_method("POST") |>
      httr2::req_body_json(body) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) {
      warning(stringr::str_glue(
        "NIH RePORTER request failed for PI_name = '{PI_name}': {conditionMessage(e)}"
      ), call. = FALSE)
      NULL
    }
  )

  if (is.null(resp)) {
    return(.markSourceFailed(.empty_reporter_tbl()))
  }

  if (httr2::resp_status(resp) >= 400) {
    warning(stringr::str_glue(
      "NIH RePORTER request failed (HTTP {httr2::resp_status(resp)}) for PI_name = '{PI_name}'"
    ), call. = FALSE)
    return(.markSourceFailed(.empty_reporter_tbl()))
  }

  parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  results <- parsed$results
  if (length(results) == 0) {
    return(.empty_reporter_tbl())
  }

  .reporterParseResults(results)
}

.PI_name_query <- function(PI_name) {
  if (stringr::str_detect(PI_name, stringr::fixed(","))) {
    parts <- stringr::str_split(PI_name, stringr::fixed(","))[[1]]
    list(last_name = stringr::str_trim(parts[1]), first_name = stringr::str_trim(parts[2]))
  } else {
    list(any_name = PI_name)
  }
}

.reporterRequestBody <- function(PI_name, fiscal_years, limit) {
  criteria <- list(pi_names = list(.PI_name_query(PI_name)))
  if (!is.null(fiscal_years)) {
    criteria$fiscal_years <- as.list(as.integer(fiscal_years))
  }
  list(
    criteria = criteria,
    include_fields = list(
      "ProjectNum", "FiscalYear", "OrgName", "ProjectTitle",
      "ContactPiName", "PrincipalInvestigators"
    ),
    offset = 0,
    limit = limit
  )
}

.reporterParseResults <- function(results) {
  purrr::map_dfr(results, function(r) {
    PIs <- purrr::map_chr(r$principal_investigators %||% list(), function(p) {
      stringr::str_c(p$first_name %||% "", p$last_name %||% "", sep = " ")
    })
    tibble::tibble(
      project_num = r$project_num %||% NA_character_,
      fiscal_year = r$fiscal_year %||% NA_integer_,
      org_name = r$org_name %||% NA_character_,
      project_title = r$project_title %||% NA_character_,
      contact_PI = r$contact_pi_name %||% NA_character_,
      all_PIs = list(PIs)
    )
  })
}

.empty_reporter_tbl <- function() {
  tibble::tibble(
    project_num = character(), fiscal_year = integer(),
    org_name = character(), project_title = character(),
    contact_PI = character(), all_PIs = list()
  )
}

#' Find NIH awards shared between a candidate and a set of authors
#'
#' Cross-references each name's RePORTER project list for overlapping
#' `project_num`s -- i.e. the candidate and an author are (or were)
#' co-PIs on the same award. This is a funding-level conflict distinct
#' from co-authorship and is easy to miss by literature search alone
#' (e.g. a shared MPI grant with no joint publication yet).
#'
#' @param candidate_name,author_names As in [reporterSearchPI()].
#' @param fiscal_years Optional integer vector; recommended to match your
#'   journal's/funder's COI lookback window (NIH study section rules
#'   typically use a 3-year window).
#'
#' @return A tibble of shared awards, or zero rows if none found.
#' @examples
#' tryCatch(
#'   reporterSharedAwards("Smith, Anne", "Lee, Charles", fiscal_years = 2020:2023),
#'   error = function(e) message("NIH RePORTER API unavailable: ", conditionMessage(e))
#' )
#' @export
reporterSharedAwards <- function(candidate_name, author_names,
                                 fiscal_years = NULL) {
  cand <- reporterSearchPI(candidate_name, fiscal_years)
  cand_failed <- .sourceFailed(cand)
  if (nrow(cand) == 0) {
    return(if (cand_failed) .markSourceFailed(cand[0, ]) else cand[0, ])
  }

  auth_failed <- FALSE
  auth <- purrr::map_dfr(author_names, function(nm) {
    df <- reporterSearchPI(nm, fiscal_years)
    if (.sourceFailed(df)) auth_failed <<- TRUE
    if (nrow(df) > 0) df$queried_author <- nm
    df
  })
  if (nrow(auth) == 0) {
    return(if (cand_failed || auth_failed) .markSourceFailed(auth) else auth)
  }

  shared <- dplyr::inner_join(
    cand, auth,
    by = "project_num",
    suffix = c("_candidate", "_author")
  )
  if (cand_failed || auth_failed) shared <- .markSourceFailed(shared)
  shared
}

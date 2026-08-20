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
#' @param pi_name Character scalar, `"Last, First"` or just `"Last"`
#'   (RePORTER does substring/fuzzy matching on `any_name`).
#' @param fiscal_years Optional integer vector of fiscal years to
#'   restrict the search (recommended -- an unrestricted query against a
#'   common surname can return hundreds of unrelated projects).
#' @param limit Max records per page (API caps at 500).
#'
#' @return A tibble with one row per project: `project_num`,
#'   `fiscal_year`, `org_name`, `project_title`, `contact_pi`,
#'   `all_pis` (a list-column of every PI/co-PI name on that award).
#' @export
reporter_search_pi <- function(pi_name, fiscal_years = NULL, limit = 500) {
  stopifnot(is.character(pi_name), length(pi_name) == 1)

  criteria <- list(pi_names = list(list(any_name = pi_name)))
  if (!is.null(fiscal_years)) {
    criteria$fiscal_years <- as.list(as.integer(fiscal_years))
  }

  body <- list(
    criteria = criteria,
    include_fields = list(
      "ProjectNum", "FiscalYear", "OrgName", "ProjectTitle",
      "ContactPiName", "PrincipalInvestigators"
    ),
    offset = 0,
    limit = limit
  )

  resp <- httr2::request("https://api.reporter.nih.gov/v2/projects/search") |>
    httr2::req_method("POST") |>
    httr2::req_body_json(body) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) >= 400) {
    warning(sprintf(
      "NIH RePORTER request failed (HTTP %s) for pi_name = '%s'",
      httr2::resp_status(resp), pi_name
    ))
    return(.empty_reporter_tbl())
  }

  parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  results <- parsed$results
  if (length(results) == 0) return(.empty_reporter_tbl())

  purrr::map_dfr(results, function(r) {
    pis <- purrr::map_chr(r$principal_investigators %||% list(), function(p) {
      paste(p$first_name %||% "", p$last_name %||% "")
    })
    tibble::tibble(
      project_num = r$project_num %||% NA_character_,
      fiscal_year = r$fiscal_year %||% NA_integer_,
      org_name = r$org_name %||% NA_character_,
      project_title = r$project_title %||% NA_character_,
      contact_pi = r$contact_pi_name %||% NA_character_,
      all_pis = list(pis)
    )
  })
}

.empty_reporter_tbl <- function() {
  tibble::tibble(
    project_num = character(), fiscal_year = integer(),
    org_name = character(), project_title = character(),
    contact_pi = character(), all_pis = list()
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Find NIH awards shared between a candidate and a set of authors
#'
#' Cross-references each name's RePORTER project list for overlapping
#' `project_num`s -- i.e. the candidate and an author are (or were)
#' co-PIs on the same award. This is a funding-level conflict distinct
#' from co-authorship and is easy to miss by literature search alone
#' (e.g. a shared MPI grant with no joint publication yet).
#'
#' @param candidate_name,author_names As in [reporter_search_pi()].
#' @param fiscal_years Optional integer vector; recommended to match your
#'   journal's/funder's COI lookback window (NIH study section rules
#'   typically use a 3-year window).
#'
#' @return A tibble of shared awards, or zero rows if none found.
#' @export
reporter_shared_awards <- function(candidate_name, author_names,
                                    fiscal_years = NULL) {
  cand <- reporter_search_pi(candidate_name, fiscal_years)
  if (nrow(cand) == 0) return(cand[0, ])

  auth <- purrr::map_dfr(author_names, function(nm) {
    df <- reporter_search_pi(nm, fiscal_years)
    if (nrow(df) > 0) df$queried_author <- nm
    df
  })
  if (nrow(auth) == 0) return(auth)

  shared <- dplyr::inner_join(
    cand, auth,
    by = "project_num",
    suffix = c("_candidate", "_author")
  )
  shared
}

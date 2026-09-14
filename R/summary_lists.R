#' Tidy summary of a single COI report
#'
#' @param object A `coiReport`, as returned by [checkCoi()].
#' @param ... Unused; present for S3 consistency.
#' @return A tibble, one row per evidence type present (`relation`,
#'   `n`, `parties` -- a comma-joined list of the authors involved),
#'   zero rows if no conflicts were found and every source ran
#'   successfully. If any evidence source errored (see `sources_failed`
#'   in [checkCoi()]'s return value), an additional
#'   `relation = "sources_failed"` row is included -- a zero-row result
#'   means the search actually completed with nothing found, not that
#'   part of it silently failed to run.
#' @examples
#' report <- structure(
#'   list(
#'     candidate = "Smith AB",
#'     authors = "Lee C",
#'     direct = tibble::tibble(
#'       candidate = "Smith AB", author = "Lee C", PMID = "1", year = 2020L
#'     ),
#'     second_degree = tibble::tibble(),
#'     funding = tibble::tibble()
#'   ),
#'   class = "coiReport"
#' )
#' summary(report)
#' @export
summary.coiReport <- function(object, ...) {
  rows <- list()
  if (nrow(object$direct) > 0) {
    rows$direct <- tibble::tibble(
      relation = "direct", n = nrow(object$direct),
      parties = stringr::str_c(unique(object$direct$author), collapse = ", ")
    )
  }
  if (nrow(object$second_degree) > 0) {
    rows$second_degree <- tibble::tibble(
      relation = "second_degree", n = nrow(object$second_degree),
      parties = stringr::str_c(
        unique(object$second_degree$linked_author), collapse = ", "
      )
    )
  }
  if (nrow(object$funding) > 0) {
    rows$funding <- tibble::tibble(
      relation = "funding", n = nrow(object$funding),
      parties = stringr::str_c(unique(object$funding$queried_author), collapse = ", ")
    )
  }
  if (length(object$sources_failed) > 0) {
    rows$sources_failed <- tibble::tibble(
      relation = "sources_failed", n = NA_integer_,
      parties = stringr::str_c(object$sources_failed, collapse = ", ")
    )
  }
  if (length(rows) == 0) {
    return(tibble::tibble(
      relation = character(), n = integer(), parties = character()
    ))
  }
  dplyr::bind_rows(rows)
}

#' Format a batch COI screen as a readable text list
#'
#' Turns [checkCoiBatch()]'s output into plain-text lines suitable
#' for pasting into an email to an editor -- the kind of summary this
#' package's design doc describes as "evidence, not a verdict": each
#' line states what was found, not a conflicted/clear judgment.
#'
#' @param batch Output of [checkCoiBatch()].
#' @return A character vector, one line per candidate, e.g.
#'   `"Smith AB -- 1 direct (Lee C), 0 second-degree, 0 funding [potential_conflict]"`.
#'   A line ends with `"(sources failed: ...)"` if any evidence source
#'   errored, since that's when a candidate could look clear only
#'   because part of the search didn't run.
#' @examples
#' batch <- list(
#'   `Smith AB` = structure(
#'     list(
#'       candidate = "Smith AB",
#'       authors = "Lee C",
#'       direct = tibble::tibble(
#'         candidate = "Smith AB", author = "Lee C", PMID = "1", year = 2020L
#'       ),
#'       second_degree = tibble::tibble(),
#'       funding = tibble::tibble()
#'     ),
#'     class = "coiReport"
#'   ),
#'   summary = tibble::tibble(
#'     candidate = "Smith AB",
#'     n_direct = 1L, n_second_degree = 0L, n_funding = 0L
#'   )
#' )
#' formatCoiSummary(batch)
#' @export
formatCoiSummary <- function(batch) {
  candidates <- setdiff(names(batch), "summary")
  purrr::map_chr(candidates, function(nm) {
    r <- batch[[nm]]
    direct_parties <- if (nrow(r$direct) > 0) {
      stringr::str_c(" (", stringr::str_c(unique(r$direct$author), collapse = ", "), ")")
    } else {
      ""
    }
    status <- r$status %||% NA_character_
    failed_note <- if (length(r$sources_failed) > 0) {
      stringr::str_glue(" (sources failed: {stringr::str_c(r$sources_failed, collapse = ', ')})")
    } else {
      ""
    }
    stringr::str_glue(
      "{nm} -- {nrow(r$direct)} direct{direct_parties}, {nrow(r$second_degree)} second-degree, ",
      "{nrow(r$funding)} funding [{status}]{failed_note}"
    )
  }) |> unname()
}

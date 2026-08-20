#' Tidy summary of a single COI report
#'
#' @param object A `coiReport`, as returned by [checkCoi()].
#' @param ... Unused; present for S3 consistency.
#' @return A tibble, one row per evidence type present (`relation`,
#'   `n`, `parties` -- a comma-joined list of the authors involved),
#'   zero rows if no conflicts were found.
#' @export
summary.coiReport <- function(object, ...) {
  rows <- list()
  if (nrow(object$direct) > 0) {
    rows$direct <- tibble::tibble(
      relation = "direct", n = nrow(object$direct),
      parties = paste(unique(object$direct$author), collapse = ", ")
    )
  }
  if (nrow(object$second_degree) > 0) {
    rows$second_degree <- tibble::tibble(
      relation = "second_degree", n = nrow(object$second_degree),
      parties = paste(
        unique(object$second_degree$linked_author), collapse = ", "
      )
    )
  }
  if (nrow(object$funding) > 0) {
    rows$funding <- tibble::tibble(
      relation = "funding", n = nrow(object$funding),
      parties = paste(unique(object$funding$queried_author), collapse = ", ")
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
#'   `"Smith AB -- 1 direct (Lee C), 0 second-degree, 0 funding"`.
#' @export
formatCoiSummary <- function(batch) {
  candidates <- setdiff(names(batch), "summary")
  vapply(candidates, function(nm) {
    r <- batch[[nm]]
    direct_parties <- if (nrow(r$direct) > 0) {
      paste0(" (", paste(unique(r$direct$author), collapse = ", "), ")")
    } else {
      ""
    }
    sprintf(
      "%s -- %d direct%s, %d second-degree, %d funding",
      nm, nrow(r$direct), direct_parties, nrow(r$second_degree), nrow(r$funding)
    )
  }, character(1), USE.NAMES = FALSE)
}

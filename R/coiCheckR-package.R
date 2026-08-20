#' coiCheckR: Screen Peer Reviewer Candidates for Conflicts of Interest
#'
#' See the README and `vignette("coiCheckR")` for a walkthrough. Start
#' with [check_coi()] for a single candidate or [check_coi_batch()] for
#' a shortlist.
#'
#' @section Rate limits:
#' PubMed E-utilities allow 3 requests/sec without a key, 10/sec with
#' one. If you're screening more than a handful of names, get a free key
#' at <https://www.ncbi.nlm.nih.gov/account/settings/> and call
#' [set_ncbi_key()] once per session.
#'
#' @keywords internal
"_PACKAGE"

#' Set your NCBI API key for the session
#'
#' Thin wrapper around [rentrez::set_entrez_key()]; raises the PubMed
#' rate limit from 3 to 10 requests/sec, useful when screening several
#' candidates against a multi-author paper (this package issues one
#' PubMed query per candidate/author name, plus one more per
#' second-degree check).
#'
#' @param key Character scalar, your NCBI API key.
#' @export
set_ncbi_key <- function(key) {
  rentrez::set_entrez_key(key)
  invisible(TRUE)
}

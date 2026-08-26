#' Build a co-authorship edge list from a tidy PubMed table
#'
#' Converts the per-(pmid, author) rows from [pmFetchAuthors()] into
#' pairwise co-author edges (every pair of authors sharing a PMID), with
#' the query author's own name excluded as a "co-author of themself".
#'
#' @param pm_tbl Output of [pmFetchAuthors()] or [pmCoauthors()].
#' @return A tibble edge list: `from`, `to` (both `"Last FM"`), `pmid`,
#'   `year`.
#' @examples
#' pm_tbl <- tibble::tibble(
#'   pmid = c("1", "1", "2"),
#'   year = c(2020L, 2020L, 2021L),
#'   author_last = c("Smith", "Lee", "Smith"),
#'   author_fore = c("AB", "CD", "AB")
#' )
#' buildCoauthorEdges(pm_tbl)
#' @export
buildCoauthorEdges <- function(pm_tbl) {
  pm_tbl <- pm_tbl |>
    dplyr::filter(!is.na(.data$author_last)) |>
    dplyr::mutate(author_name = paste(
      .data$author_last, .pm_normalize_forename(.data$author_fore)
    ))

  pm_tbl |>
    dplyr::group_by(.data$pmid, .data$year) |>
    dplyr::group_modify(function(grp, ...) {
      nms <- grp$author_name
      if (length(nms) < 2) {
        return(tibble::tibble(from = character(), to = character()))
      }
      pairs <- utils::combn(nms, 2, simplify = FALSE)
      tibble::tibble(
        from = purrr::map_chr(pairs, 1),
        to = purrr::map_chr(pairs, 2)
      )
    }) |>
    dplyr::ungroup()
}

# PubMed's ForeName field is inconsistently abbreviated -- some records
# carry initials ("MR"), others the full given name ("Monica Rose"). Both
# candidate_name/author_names and the "Last FM" documented input format
# assume initials, so collapse any full given name to its initials here to
# keep matching against user-supplied names reliable.
.pm_normalize_forename <- function(fore) {
  vapply(fore, function(f) {
    if (is.na(f) || !nzchar(f)) {
      return(NA_character_)
    }
    toks <- strsplit(f, "[[:space:].-]+")[[1]]
    toks <- toks[nzchar(toks)]
    if (length(toks) == 0) {
      return(NA_character_)
    }
    initials <- vapply(toks, function(t) {
      if (grepl("^[A-Z]{1,4}$", t)) t else toupper(substr(t, 1, 1))
    }, character(1))
    paste0(initials, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

#' Detect second-degree co-authorship conflicts
#'
#' A second-degree conflict exists when the *candidate* has not
#' published directly with an *author*, but the candidate's frequent
#' collaborator has -- i.e. there is a length-2 path in the co-authorship
#' graph. This is a soft signal for editorial judgment, not an automatic
#' disqualifier (per the RAP / knowledge-graph COI literature this
#' package's design note is based on): a candidate's one-off co-author
#' three years ago being tied to a paper author is much weaker evidence
#' than a recurring collaborator.
#'
#' @param candidate_name,author_names As in [reporterSharedAwards()].
#' @param affiliation Optional affiliation substring to disambiguate the
#'   *candidate* when their surname is common (passed to
#'   [pmSearchAuthor()], same as [checkCoi()]'s `affiliation` argument).
#'   Without this, a common-surname candidate's second-degree results can
#'   include an unrelated same-named person's collaborator network.
#' @param min_year Restrict co-authorship evidence to this year or later
#'   (e.g. `Sys.Date() |> format("%Y") |> as.integer() - 4` for a 4-year
#'   window matching common COI policy).
#' @param min_shared_pubs Minimum number of shared PMIDs between the
#'   candidate's collaborator and the candidate for that collaborator to
#'   count as a "frequent" one. Default 2 (i.e. not a single one-off
#'   co-authorship).
#'
#' @return A tibble: `candidate_collaborator`, `linked_author`,
#'   `n_shared_with_candidate`, `evidence_pmid` (the PMID linking the
#'   collaborator to the author).
#' @examples
#' tryCatch(
#'   secondDegreeConflicts("Smith AB", "Lee C", min_year = 2020),
#'   error = function(e) message("Live PubMed API unavailable: ", conditionMessage(e))
#' )
#' @export
secondDegreeConflicts <- function(candidate_name, author_names,
                                  affiliation = NULL, min_year = NULL,
                                  min_shared_pubs = 2) {
  cand_pubs <- pmCoauthors(
    candidate_name, affiliation = affiliation, min_year = min_year
  )
  cand_edges <- buildCoauthorEdges(cand_pubs)

  cand_collab_counts <- cand_edges |>
    dplyr::filter(
      .data$from == candidate_name | .data$to == candidate_name
    ) |>
    dplyr::mutate(collaborator = dplyr::if_else(
      .data$from == candidate_name, .data$to, .data$from
    )) |>
    dplyr::count(.data$collaborator, name = "n_shared_with_candidate") |>
    dplyr::filter(.data$n_shared_with_candidate >= min_shared_pubs)

  if (nrow(cand_collab_counts) == 0) {
    return(tibble::tibble(
      candidate_collaborator = character(), linked_author = character(),
      n_shared_with_candidate = integer(), evidence_pmid = character()
    ))
  }

  purrr::map_dfr(author_names, function(auth) {
    auth_pubs <- pmCoauthors(auth, min_year = min_year)
    auth_edges <- buildCoauthorEdges(auth_pubs) |>
      dplyr::filter(.data$from == auth | .data$to == auth) |>
      dplyr::mutate(other = dplyr::if_else(
        .data$from == auth, .data$to, .data$from
      ))

    hits <- dplyr::inner_join(
      cand_collab_counts, auth_edges,
      by = c("collaborator" = "other")
    )
    if (nrow(hits) == 0) {
      return(NULL)
    }

    tibble::tibble(
      candidate_collaborator = hits$collaborator,
      linked_author = auth,
      n_shared_with_candidate = hits$n_shared_with_candidate,
      evidence_pmid = hits$pmid
    )
  })
}

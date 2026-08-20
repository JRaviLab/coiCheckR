#' Build a co-authorship edge list from a tidy PubMed table
#'
#' Converts the per-(pmid, author) rows from [pm_fetch_authors()] into
#' pairwise co-author edges (every pair of authors sharing a PMID), with
#' the query author's own name excluded as a "co-author of themself".
#'
#' @param pm_tbl Output of [pm_fetch_authors()] or [pm_coauthors()].
#' @return A tibble edge list: `from`, `to` (both `"Last FM"`), `pmid`,
#'   `year`.
#' @export
build_coauthor_edges <- function(pm_tbl) {
  pm_tbl <- pm_tbl |>
    dplyr::filter(!is.na(.data$author_last)) |>
    dplyr::mutate(author_name = paste(.data$author_last, .data$author_fore))

  pm_tbl |>
    dplyr::group_by(.data$pmid, .data$year) |>
    dplyr::group_modify(function(grp, ...) {
      nms <- grp$author_name
      if (length(nms) < 2) return(tibble::tibble(from = character(), to = character()))
      pairs <- utils::combn(nms, 2, simplify = FALSE)
      tibble::tibble(
        from = purrr::map_chr(pairs, 1),
        to = purrr::map_chr(pairs, 2)
      )
    }) |>
    dplyr::ungroup()
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
#' @param candidate_name,author_names As in [reporter_shared_awards()].
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
#' @export
second_degree_conflicts <- function(candidate_name, author_names,
                                     min_year = NULL, min_shared_pubs = 2) {
  cand_pubs <- pm_coauthors(candidate_name, min_year = min_year)
  cand_edges <- build_coauthor_edges(cand_pubs)

  cand_collab_counts <- cand_edges |>
    dplyr::filter(.data$from == candidate_name | .data$to == candidate_name) |>
    dplyr::mutate(collaborator = ifelse(.data$from == candidate_name, .data$to, .data$from)) |>
    dplyr::count(.data$collaborator, name = "n_shared_with_candidate") |>
    dplyr::filter(.data$n_shared_with_candidate >= min_shared_pubs)

  if (nrow(cand_collab_counts) == 0) {
    return(tibble::tibble(
      candidate_collaborator = character(), linked_author = character(),
      n_shared_with_candidate = integer(), evidence_pmid = character()
    ))
  }

  purrr::map_dfr(author_names, function(auth) {
    auth_pubs <- pm_coauthors(auth, min_year = min_year)
    auth_edges <- build_coauthor_edges(auth_pubs) |>
      dplyr::filter(.data$from == auth | .data$to == auth) |>
      dplyr::mutate(other = ifelse(.data$from == auth, .data$to, .data$from))

    hits <- dplyr::inner_join(
      cand_collab_counts, auth_edges,
      by = c("collaborator" = "other")
    )
    if (nrow(hits) == 0) return(NULL)

    tibble::tibble(
      candidate_collaborator = hits$collaborator,
      linked_author = auth,
      n_shared_with_candidate = hits$n_shared_with_candidate,
      evidence_pmid = hits$pmid
    )
  })
}

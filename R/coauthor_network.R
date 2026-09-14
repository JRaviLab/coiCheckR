#' Build a co-authorship edge list from a tidy PubMed table
#'
#' Converts the per-(PMID, author) rows from [pmFetchAuthors()] into
#' pairwise co-author edges (every pair of authors sharing a PMID), with
#' the query author's own name excluded as a "co-author of themself".
#'
#' @param pm_tbl Output of [pmFetchAuthors()] or [pmCoauthors()].
#' @return A tibble edge list: `from`, `to` (both `"Last FM"`), `PMID`,
#'   `year`.
#' @examples
#' pm_tbl <- tibble::tibble(
#'   PMID = c("1", "1", "2"),
#'   year = c(2020L, 2020L, 2021L),
#'   author_last = c("Smith", "Lee", "Smith"),
#'   author_fore = c("AB", "CD", "AB")
#' )
#' buildCoauthorEdges(pm_tbl)
#' @export
buildCoauthorEdges <- function(pm_tbl) {
  pm_tbl <- pm_tbl |>
    dplyr::filter(!is.na(.data$author_last)) |>
    dplyr::mutate(author_name = stringr::str_c(
      .data$author_last, .pm_normalize_forename(.data$author_fore), sep = " "
    ))

  pm_tbl |>
    dplyr::group_by(.data$PMID, .data$year) |>
    dplyr::group_modify(function(grp, ...) {
      nms <- dplyr::pull(grp, .data$author_name)
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
  purrr::map_chr(fore, function(f) {
    if (is.na(f) || !nzchar(f)) {
      return(NA_character_)
    }
    toks <- stringr::str_split(f, "[[:space:].-]+")[[1]]
    toks <- toks[nzchar(toks)]
    if (length(toks) == 0) {
      return(NA_character_)
    }
    initials <- purrr::map_chr(toks, function(t) {
      if (stringr::str_detect(t, "^[A-Z]{1,4}$")) t else stringr::str_to_upper(stringr::str_sub(t, 1, 1))
    })
    stringr::str_c(initials, collapse = "")
  }) |> unname()
}

# Reduces a "Last <forename>" name -- forename spelled out or already
# initials -- to the same canonical "Last F" form buildCoauthorEdges()
# builds internally from PubMed's own ForeName field. Without this, a
# candidate/author name search benefits from a spelled-out forename
# (PubMed's [Author] field matches it just as well, often *more*
# precisely, than bare initials -- see pmSearchAuthor()'s docs) but
# would then never match the initials-only edge labels on comparison,
# silently producing empty results despite real co-authorship existing.
.pm_canonical_name <- function(name) {
  purrr::map_chr(name, function(nm) {
    toks <- stringr::str_split(stringr::str_trim(nm), "\\s+")[[1]]
    if (length(toks) < 2) {
      return(nm)
    }
    last <- toks[1]
    fore <- stringr::str_c(toks[-1], collapse = " ")
    stringr::str_c(last, .pm_normalize_forename(fore), sep = " ")
  }) |> unname()
}

# PubMed inconsistently records a middle initial across an author's own
# papers -- the same real person can appear as "Waters C" on one record
# and "Waters CM" on another. Exact string equality on the canonical
# form would silently drop a genuine match whenever the initials happen
# not to line up exactly, so two canonical names are considered a match
# when the last name is identical and one's initials are a *prefix* of
# the other's (not just a shared first initial, which would reopen the
# common-surname false-positive problem this package is designed to
# avoid -- "Smith A" must not match both "Smith AB" and "Smith AC").
.pm_name_matches <- function(a, b) {
  if (is.na(a) || is.na(b)) {
    return(FALSE)
  }
  a_toks <- stringr::str_split(stringr::str_trim(a), "\\s+")[[1]]
  b_toks <- stringr::str_split(stringr::str_trim(b), "\\s+")[[1]]
  if (length(a_toks) == 0 || length(b_toks) == 0 || !identical(a_toks[1], b_toks[1])) {
    return(FALSE)
  }
  a_init <- if (length(a_toks) > 1) a_toks[2] else ""
  b_init <- if (length(b_toks) > 1) b_toks[2] else ""
  if (!nzchar(a_init) || !nzchar(b_init)) {
    return(nzchar(a_init) == nzchar(b_init))
  }
  stringr::str_starts(a_init, stringr::fixed(b_init)) ||
    stringr::str_starts(b_init, stringr::fixed(a_init))
}

# Vectorised: TRUE for each element of `x` that .pm_name_matches() any
# element of `targets` -- a prefix-tolerant drop-in for `x %in% targets`.
.pm_name_matches_any <- function(x, targets) {
  purrr::map_lgl(x, function(xi) {
    any(purrr::map_lgl(targets, .pm_name_matches, a = xi))
  })
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
#' @param affiliation Optional affiliation substring (or vector of
#'   substrings, OR-combined) to disambiguate the *candidate* when their
#'   surname is common (passed to [pmSearchAuthor()], same as
#'   [checkCoi()]'s `affiliation` argument). Without this, a
#'   common-surname candidate's second-degree results can include an
#'   unrelated same-named person's collaborator network.
#' @param author_affiliations The `author_names`-side counterpart to
#'   `affiliation`, for when one of the *authors* (not the candidate) has
#'   a common surname. One of: `NULL` (default, no author-side
#'   filtering); a plain character vector, applied identically to every
#'   author; or a list the same length as `author_names`, one
#'   affiliation (scalar or vector, or `NULL` for "no filter") per
#'   author.
#' @param min_year Restrict co-authorship evidence to this year or later
#'   (e.g. `Sys.Date() |> format("%Y") |> as.integer() - 4` for a 4-year
#'   window matching common COI policy).
#' @param min_shared_pubs Minimum number of shared PMIDs between the
#'   candidate's collaborator and the candidate for that collaborator to
#'   count as a "frequent" one. Default 2 (i.e. not a single one-off
#'   co-authorship).
#'
#' @return A tibble: `candidate_collaborator`, `linked_author`,
#'   `n_shared_with_candidate`, `evidence_PMID` (the PMID linking the
#'   collaborator to the author).
#' @examples
#' \donttest{
#' # Live PubMed call -- \donttest since "Smith"/"Lee" are common enough
#' # names that this can be slow (many real, unrelated hits) and isn't
#' # run by default during R CMD check or routine CRAN/Bioconductor
#' # checks. Use a real affiliation-disambiguated name for actual use.
#' tryCatch(
#'   secondDegreeConflicts("Smith AB", "Lee C", min_year = 2020),
#'   error = function(e) message("Live PubMed API unavailable: ", conditionMessage(e))
#' )
#' }
#' @export
secondDegreeConflicts <- function(candidate_name, author_names,
                                  affiliation = NULL, author_affiliations = NULL,
                                  min_year = NULL, min_shared_pubs = 2) {
  if (is.list(author_affiliations) &&
      length(author_affiliations) != length(author_names)) {
    stop("When `author_affiliations` is a list, it must have one element per `author_names`.")
  }
  author_aff_list <- if (is.null(author_affiliations)) {
    vector("list", length(author_names))
  } else if (is.list(author_affiliations)) {
    author_affiliations
  } else {
    rep(list(author_affiliations), length(author_names))
  }

  cand_pubs <- pmCoauthors(
    candidate_name, affiliation = affiliation, min_year = min_year
  )
  failed <- .sourceFailed(cand_pubs)
  cand_edges <- buildCoauthorEdges(cand_pubs)
  cand_canonical <- .pm_canonical_name(candidate_name)

  cand_collab_counts <- cand_edges |>
    dplyr::filter(
      .pm_name_matches_any(.data$from, cand_canonical) |
        .pm_name_matches_any(.data$to, cand_canonical)
    ) |>
    dplyr::mutate(collaborator = dplyr::if_else(
      .pm_name_matches_any(.data$from, cand_canonical), .data$to, .data$from
    )) |>
    dplyr::count(.data$collaborator, name = "n_shared_with_candidate") |>
    dplyr::filter(.data$n_shared_with_candidate >= min_shared_pubs)

  if (nrow(cand_collab_counts) == 0) {
    empty <- tibble::tibble(
      candidate_collaborator = character(), linked_author = character(),
      n_shared_with_candidate = integer(), evidence_PMID = character()
    )
    return(if (failed) .markSourceFailed(empty) else empty)
  }

  result <- purrr::map2_dfr(author_names, author_aff_list, function(auth, auth_aff) {
    auth_pubs <- pmCoauthors(auth, affiliation = auth_aff, min_year = min_year)
    if (.sourceFailed(auth_pubs)) failed <<- TRUE
    auth_canonical <- .pm_canonical_name(auth)
    auth_edges <- buildCoauthorEdges(auth_pubs) |>
      dplyr::filter(
        .pm_name_matches_any(.data$from, auth_canonical) |
          .pm_name_matches_any(.data$to, auth_canonical)
      ) |>
      dplyr::mutate(other = dplyr::if_else(
        .pm_name_matches_any(.data$from, auth_canonical), .data$to, .data$from
      ))

    # A plain inner_join() would require an *exact* match between the two
    # sides' collaborator names -- but each side's PubMed records can
    # independently record that same person with or without a middle
    # initial, so this uses the same prefix-tolerant matcher rather than
    # an exact join key.
    hits <- purrr::pmap_dfr(cand_collab_counts, function(collaborator, n_shared_with_candidate) {
      matched <- auth_edges |> dplyr::filter(.pm_name_matches_any(.data$other, collaborator))
      if (nrow(matched) == 0) {
        return(NULL)
      }
      tibble::tibble(
        collaborator = collaborator,
        n_shared_with_candidate = n_shared_with_candidate,
        PMID = matched$PMID
      )
    })
    if (nrow(hits) == 0) {
      return(NULL)
    }

    tibble::tibble(
      candidate_collaborator = hits$collaborator,
      linked_author = auth,
      n_shared_with_candidate = hits$n_shared_with_candidate,
      evidence_PMID = hits$PMID
    )
  })

  if (failed) result <- .markSourceFailed(result)
  result
}

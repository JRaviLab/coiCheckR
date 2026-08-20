#' Build an igraph object from one or more COI reports
#'
#' Turns `coi_report` evidence into a graph: one node per candidate and
#' per author, one edge per piece of evidence (direct co-authorship,
#' second-degree co-authorship, or shared funding), edge-typed
#' accordingly. Accepts either a single [check_coi()] result or the
#' list returned by [check_coi_batch()] (its `summary` element, if
#' present, is ignored).
#'
#' @param report A `coi_report` object, or a (possibly named) list of
#'   them.
#' @return An [igraph::graph_from_data_frame()] object. Node attribute
#'   `type` is `"candidate"` or `"author"`; edge attribute `relation`
#'   is `"direct"`, `"second_degree"`, or `"funding"`; edge attribute
#'   `evidence` holds the PMID/award number/collaborator name backing
#'   it.
#' @export
coi_to_graph <- function(report) {
  reports <- if (inherits(report, "coi_report")) list(report) else report
  reports <- reports[vapply(reports, inherits, logical(1), "coi_report")]
  if (length(reports) == 0) stop("No coi_report objects found in `report`.")

  edges <- purrr::map_dfr(reports, function(r) {
    dplyr::bind_rows(
      if (nrow(r$direct) > 0) {
        tibble::tibble(
          from = r$candidate, to = r$direct$author,
          relation = "direct", evidence = r$direct$pmid
        )
      },
      if (nrow(r$second_degree) > 0) {
        tibble::tibble(
          from = r$candidate, to = r$second_degree$linked_author,
          relation = "second_degree",
          evidence = r$second_degree$candidate_collaborator
        )
      },
      if (nrow(r$funding) > 0) {
        tibble::tibble(
          from = r$candidate, to = r$funding$queried_author,
          relation = "funding", evidence = r$funding$project_num
        )
      }
    )
  })

  if (nrow(edges) == 0) {
    stop("No conflicts found across the supplied report(s) -- nothing to graph.")
  }

  candidates <- unique(edges$from)
  authors <- unique(edges$to)
  nodes <- tibble::tibble(
    name = c(candidates, setdiff(authors, candidates)),
    type = c(rep("candidate", length(candidates)),
             rep("author", length(setdiff(authors, candidates))))
  )

  igraph::graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
}

#' Plot a conflict-of-interest network
#'
#' Thin wrapper around `igraph`'s base plotting, with defaults sized
#' for a handful of candidates vs. a manuscript's author list (this is
#' meant for a quick look while triaging a reviewer shortlist, not a
#' publication figure -- export the graph via [coi_to_graph()] and use
#' `ggraph`/`visNetwork` etc. yourself for anything more polished, to
#' keep this package's own dependency footprint to base `igraph`).
#'
#' @param report As in [coi_to_graph()].
#' @param ... Passed to `igraph`'s `plot()`.
#' @return Invisibly, the `igraph` object (also drawn as a side effect).
#' @export
plot_coi_network <- function(report, ...) {
  g <- coi_to_graph(report)

  relation_colors <- c(direct = "firebrick", second_degree = "goldenrod",
                        funding = "steelblue")
  igraph::E(g)$color <- relation_colors[igraph::E(g)$relation]

  node_colors <- c(candidate = "lightgreen", author = "lightgray")
  igraph::V(g)$color <- node_colors[igraph::V(g)$type]

  igraph::plot.igraph(
    g,
    edge.arrow.size = 0.4,
    vertex.label.cex = 0.8,
    vertex.size = 20,
    main = "Candidate-author conflict network\n(red = direct, gold = 2nd-degree, blue = funding)",
    ...
  )
  invisible(g)
}

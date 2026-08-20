.fake_report <- function(candidate, direct = NULL, second_degree = NULL, funding = NULL) {
  structure(
    list(
      candidate = candidate,
      authors = character(),
      direct = direct %||% tibble::tibble(candidate = character(), author = character(),
                                           pmid = character(), year = integer()),
      second_degree = second_degree %||% tibble::tibble(
        candidate_collaborator = character(), linked_author = character(),
        n_shared_with_candidate = integer(), evidence_pmid = character()
      ),
      funding = funding %||% tibble::tibble(
        project_num = character(), queried_author = character()
      )
    ),
    class = "coi_report"
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

test_that("summary.coi_report returns one row per non-empty evidence type", {
  r <- .fake_report(
    "Davies MR",
    direct = tibble::tibble(candidate = "Davies MR", author = "Nizet V",
                             pmid = "123", year = 2020L)
  )
  s <- summary(r)
  expect_equal(nrow(s), 1)
  expect_equal(s$relation, "direct")
  expect_equal(s$parties, "Nizet V")
})

test_that("summary.coi_report returns zero rows when no conflicts", {
  r <- .fake_report("Bessen DE")
  s <- summary(r)
  expect_equal(nrow(s), 0)
})

test_that("format_coi_summary produces one line per candidate", {
  batch <- list(
    "Davies MR" = .fake_report(
      "Davies MR",
      direct = tibble::tibble(candidate = "Davies MR", author = "Nizet V",
                               pmid = "123", year = 2020L)
    ),
    "Bessen DE" = .fake_report("Bessen DE"),
    summary = tibble::tibble(candidate = c("Davies MR", "Bessen DE"))
  )
  lines <- format_coi_summary(batch)
  expect_length(lines, 2)
  expect_true(grepl("Nizet V", lines[1]))
  expect_true(grepl("0 direct", lines[2]))
})

test_that("coi_to_graph errors clearly when there is nothing to graph", {
  r <- .fake_report("Bessen DE")
  expect_error(coi_to_graph(r), "No conflicts found")
})

test_that("coi_to_graph builds an igraph with expected node/edge counts", {
  r <- .fake_report(
    "Davies MR",
    direct = tibble::tibble(candidate = "Davies MR", author = "Nizet V",
                             pmid = "123", year = 2020L)
  )
  g <- coi_to_graph(r)
  expect_s3_class(g, "igraph")
  expect_equal(igraph::gorder(g), 2)  # Davies MR, Nizet V
  expect_equal(igraph::gsize(g), 1)
})

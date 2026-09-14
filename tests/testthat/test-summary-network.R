.fake_report <- function(candidate, direct = NULL, second_degree = NULL,
                         funding = NULL) {
  structure(
    list(
      candidate = candidate,
      authors = character(),
      direct = direct %||% tibble::tibble(
        candidate = character(), author = character(),
        PMID = character(), year = integer()
      ),
      second_degree = second_degree %||% tibble::tibble(
        candidate_collaborator = character(), linked_author = character(),
        n_shared_with_candidate = integer(), evidence_PMID = character()
      ),
      funding = funding %||% tibble::tibble(
        project_num = character(), queried_author = character()
      )
    ),
    class = "coiReport"
  )
}

test_that("summary.coiReport returns one row per non-empty evidence type", {
  r <- .fake_report(
    "Smith AB",
    direct = tibble::tibble(
      candidate = "Smith AB", author = "Lee C",
      PMID = "123", year = 2020L
    )
  )
  s <- summary(r)
  expect_equal(nrow(s), 1)
  expect_equal(s$relation, "direct")
  expect_equal(s$parties, "Lee C")
})

test_that("summary.coiReport returns zero rows when no conflicts", {
  r <- .fake_report("Park DE")
  s <- summary(r)
  expect_equal(nrow(s), 0)
})

test_that("formatCoiSummary produces one line per candidate", {
  batch <- list(
    "Smith AB" = .fake_report(
      "Smith AB",
      direct = tibble::tibble(
        candidate = "Smith AB", author = "Lee C",
        PMID = "123", year = 2020L
      )
    ),
    "Park DE" = .fake_report("Park DE"),
    summary = tibble::tibble(candidate = c("Smith AB", "Park DE"))
  )
  lines <- formatCoiSummary(batch)
  expect_length(lines, 2)
  expect_true(grepl("Lee C", lines[1]))
  expect_true(grepl("0 direct", lines[2]))
})

test_that("coiToGraph errors clearly when there is nothing to graph", {
  r <- .fake_report("Park DE")
  expect_error(coiToGraph(r), "No conflicts found")
})

test_that("coiToGraph builds an igraph with expected node/edge counts", {
  r <- .fake_report(
    "Smith AB",
    direct = tibble::tibble(
      candidate = "Smith AB", author = "Lee C",
      PMID = "123", year = 2020L
    )
  )
  g <- coiToGraph(r)
  expect_s3_class(g, "igraph")
  expect_equal(igraph::gorder(g), 2) # Smith AB, Lee C
  expect_equal(igraph::gsize(g), 1)
})

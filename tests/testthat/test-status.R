test_that(".markSourceFailed / .sourceFailed round-trip", {
  x <- tibble::tibble(a = 1)
  expect_false(.sourceFailed(x))
  x <- .markSourceFailed(x)
  expect_true(.sourceFailed(x))
})

test_that(".sourceFailed is FALSE for untagged objects, including NULL", {
  expect_false(.sourceFailed(character()))
  expect_false(.sourceFailed(tibble::tibble()))
  expect_false(.sourceFailed(NULL))
})

.empty <- function() tibble::tibble()
.nonempty <- function() tibble::tibble(x = 1)

test_that("status is potential_conflict whenever any evidence table is non-empty", {
  expect_equal(
    .classifyCoiStatus(.nonempty(), .empty(), .empty(), character()),
    "potential_conflict"
  )
  expect_equal(
    .classifyCoiStatus(.empty(), .nonempty(), .empty(), character()),
    "potential_conflict"
  )
  expect_equal(
    .classifyCoiStatus(.empty(), .empty(), .nonempty(), character()),
    "potential_conflict"
  )
})

test_that("status is insufficient_evidence when a source failed and nothing was found", {
  expect_equal(
    .classifyCoiStatus(.empty(), .empty(), .empty(), "pubmed"),
    "insufficient_evidence"
  )
})

test_that("evidence found despite a source failure is still potential_conflict, not insufficient_evidence", {
  expect_equal(
    .classifyCoiStatus(.nonempty(), .empty(), .empty(), "nih_reporter"),
    "potential_conflict"
  )
})

test_that("status is no_conflict_detected only when nothing was found and nothing failed", {
  expect_equal(
    .classifyCoiStatus(.empty(), .empty(), .empty(), character()),
    "no_conflict_detected"
  )
})

test_that("checkCoi surfaces a PubMed search failure as insufficient_evidence, not a false no_conflict_detected", {
  testthat::local_mocked_bindings(
    entrez_search = function(...) stop("simulated network failure"),
    .package = "rentrez"
  )
  report <- suppressWarnings(checkCoi(
    "Smith AB", "Lee C",
    check_second_degree = FALSE, check_funding = FALSE
  ))
  expect_equal(report$status, "insufficient_evidence")
  expect_true("pubmed" %in% report$sources_failed)
})

test_that("checkCoi still reports no_conflict_detected when PubMed succeeds with zero hits", {
  testthat::local_mocked_bindings(
    entrez_search = function(...) list(ids = character()),
    .package = "rentrez"
  )
  report <- checkCoi(
    "Smith AB", "Lee C",
    check_second_degree = FALSE, check_funding = FALSE
  )
  expect_equal(report$status, "no_conflict_detected")
  expect_length(report$sources_failed, 0)
})

test_that("pmFetchAuthors returns a properly-shaped, failure-tagged tibble when every batch errors", {
  testthat::local_mocked_bindings(
    entrez_fetch = function(...) stop("simulated network failure"),
    .package = "rentrez"
  )
  out <- suppressWarnings(pmFetchAuthors(c("1", "2")))
  expect_true(.sourceFailed(out))
  expect_equal(nrow(out), 0)
  expect_true(all(
    c("PMID", "year", "journal", "author_last", "author_fore", "affiliation") %in%
      names(out)
  ))
})

test_that("checkCoi surfaces a PubMed fetch (not just search) failure as insufficient_evidence", {
  testthat::local_mocked_bindings(
    entrez_search = function(...) list(ids = "1"),
    entrez_fetch = function(...) stop("simulated network failure"),
    .package = "rentrez"
  )
  report <- suppressWarnings(checkCoi(
    "Smith AB", "Lee C",
    check_second_degree = FALSE, check_funding = FALSE
  ))
  expect_equal(report$status, "insufficient_evidence")
  expect_true("pubmed" %in% report$sources_failed)
})

test_that("summary.coiReport surfaces a sources_failed row instead of a misleadingly empty result", {
  report <- structure(
    list(
      candidate = "Smith AB",
      authors = "Lee C",
      direct = tibble::tibble(),
      second_degree = tibble::tibble(),
      funding = tibble::tibble(),
      status = "insufficient_evidence",
      sources_failed = "nih_reporter"
    ),
    class = "coiReport"
  )
  s <- summary(report)
  expect_equal(nrow(s), 1)
  expect_equal(s$relation, "sources_failed")
  expect_equal(s$parties, "nih_reporter")
})

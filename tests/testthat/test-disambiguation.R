# Regression tests for the affiliation/org_names disambiguation features
# and the full-forename matching fix (see .pm_canonical_name).

test_that("checkCoi matches direct co-authorship when candidate_name uses a full forename", {
  testthat::local_mocked_bindings(
    pmCoauthors = function(author, affiliation = NULL, min_year = NULL, ...) {
      if (grepl("^Ravi", author)) {
        tibble::tibble(
          PMID = c("1", "1"), year = c(2024L, 2024L), journal = "J",
          author_last = c("Ravi", "Brenner"), author_fore = c("Janani", "Evan"),
          affiliation = NA_character_, query_author = author
        )
      } else {
        tibble::tibble(
          PMID = character(), year = integer(), journal = character(),
          author_last = character(), author_fore = character(),
          affiliation = character(), query_author = character()
        )
      }
    }
  )
  report <- checkCoi(
    "Ravi Janani", "Brenner E",
    check_second_degree = FALSE, check_funding = FALSE
  )
  expect_equal(nrow(report$direct), 1)
  expect_equal(report$direct$author, "Brenner E")
  expect_equal(report$status, "potential_conflict")
})

test_that("secondDegreeConflicts errors clearly on a mismatched author_affiliations list, before any network call", {
  expect_error(
    secondDegreeConflicts(
      "Smith AB", c("Lee C", "Doe D"),
      author_affiliations = list("Colorado")
    ),
    "one element per"
  )
})

test_that("secondDegreeConflicts accepts a plain author_affiliations vector applied to every author", {
  # A plain (non-list) vector should never trip the list-length-mismatch
  # check, regardless of its own length vs. author_names -- mock
  # pmCoauthors to return empty results immediately (no network) so this
  # only exercises the validation/shape logic, not a real search.
  testthat::local_mocked_bindings(
    pmCoauthors = function(author, affiliation = NULL, min_year = NULL, ...) {
      tibble::tibble(
        PMID = character(), year = integer(), journal = character(),
        author_last = character(), author_fore = character(),
        affiliation = character(), query_author = character()
      )
    }
  )
  testthat::expect_no_error(
    secondDegreeConflicts(
      "Smith AB", c("Lee C", "Doe D"),
      author_affiliations = c("Colorado", "Michigan")
    )
  )
})

test_that(".reporterRequestBody includes org_names when provided", {
  body <- .reporterRequestBody("Smith, Anne", NULL, 500, org_names = c("Colorado", "Michigan"))
  expect_equal(body$criteria$org_names, list("Colorado", "Michigan"))
})

test_that(".reporterRequestBody omits org_names when NULL", {
  body <- .reporterRequestBody("Smith, Anne", NULL, 500)
  expect_null(body$criteria$org_names)
})

test_that("reporterSharedAwards errors clearly on a mismatched author_org_names list, before any network call", {
  expect_error(
    reporterSharedAwards(
      "Smith, Anne", c("Lee, C", "Doe, D"),
      author_org_names = list("Colorado")
    ),
    "one element per"
  )
})

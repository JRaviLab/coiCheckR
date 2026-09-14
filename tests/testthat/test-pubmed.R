test_that("pmSearchAuthor builds a single-affiliation clause", {
  captured <- NULL
  testthat::local_mocked_bindings(
    entrez_search = function(db, term, retmax) {
      captured <<- as.character(term)
      list(ids = character())
    },
    .package = "rentrez"
  )
  pmSearchAuthor("Ravi J", affiliation = "Colorado")
  expect_match(captured, "AND \\(Colorado\\[Affiliation\\]\\)", fixed = FALSE)
})

test_that("pmSearchAuthor OR-combines multiple affiliations", {
  captured <- NULL
  testthat::local_mocked_bindings(
    entrez_search = function(db, term, retmax) {
      captured <<- as.character(term)
      list(ids = character())
    },
    .package = "rentrez"
  )
  pmSearchAuthor("Ravi J", affiliation = c("Colorado", "Michigan", "Rutgers"))
  expect_match(
    captured,
    "AND \\(Colorado\\[Affiliation\\] OR Michigan\\[Affiliation\\] OR Rutgers\\[Affiliation\\]\\)"
  )
})

test_that("pmSearchAuthor omits the affiliation clause entirely when NULL", {
  captured <- NULL
  testthat::local_mocked_bindings(
    entrez_search = function(db, term, retmax) {
      captured <<- as.character(term)
      list(ids = character())
    },
    .package = "rentrez"
  )
  pmSearchAuthor("Ravi J")
  expect_false(grepl("Affiliation", captured))
})

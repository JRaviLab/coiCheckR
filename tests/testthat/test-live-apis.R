# These hit real PubMed / NIH RePORTER / bioRxiv endpoints and are
# skipped by default -- CRAN/Bioconductor build checks must not depend
# on network access, and the free NCBI rate limit makes CI runs
# annoyingly slow anyway.
#
# No real names are hardcoded here. To exercise these against a known,
# real conflict, set environment variables to a candidate/author pair
# (and NIH PI names) you know to be co-authors with shared funding, and
# run locally with:
#   Sys.setenv(
#     COICHECKR_RUN_LIVE_TESTS = "true",
#     COICHECKR_TEST_CANDIDATE = "Last FM",
#     COICHECKR_TEST_AUTHOR    = "Last FM",
#     COICHECKR_TEST_AFFILIATION = "",            # optional
#     COICHECKR_TEST_PI = "Last, First",
#     COICHECKR_TEST_PI_AUTHOR = "Last, First"
#   )
#   devtools::test()
#
# With the placeholder defaults below (no env vars set), these tests
# will legitimately find nothing and fail the expect_gt() assertions --
# that's expected; they exist to be run locally against real names, not
# as part of CI or Bioconductor's build.

skip_if_not(
  identical(Sys.getenv("COICHECKR_RUN_LIVE_TESTS"), "true"),
  "Set COICHECKR_RUN_LIVE_TESTS=true to run live API tests"
)

candidate <- Sys.getenv("COICHECKR_TEST_CANDIDATE", unset = "Smith AB")
author <- Sys.getenv("COICHECKR_TEST_AUTHOR", unset = "Lee C")
affiliation <- Sys.getenv("COICHECKR_TEST_AFFILIATION", unset = "")
if (!nzchar(affiliation)) affiliation <- NULL
PI_name <- Sys.getenv("COICHECKR_TEST_PI", unset = "Smith, Anne")
PI_author <- Sys.getenv("COICHECKR_TEST_PI_AUTHOR", unset = "Lee, Charles")
this_year <- as.integer(format(Sys.Date(), "%Y"))
last_10y <- (this_year - 9):this_year

test_that("pmSearchAuthor returns PMIDs for a known author", {
  skip_if_offline()
  ids <- pmSearchAuthor(candidate, affiliation = affiliation)
  expect_gt(length(ids), 0)
})

test_that("checkCoi flags a known candidate-author conflict on all 3 axes", {
  skip_if_offline()
  report <- checkCoi(
    candidate_name = candidate,
    author_names = author,
    affiliation = affiliation,
    check_second_degree = TRUE,
    check_funding = TRUE
  )
  expect_s3_class(report, "coiReport")
  expect_gt(nrow(report$direct), 0)
  expect_gt(nrow(report$second_degree), 0)
  expect_gt(nrow(report$funding), 0)
})

test_that("reporterSearchPI returns awards for a known NIH-funded PI", {
  skip_if_offline()
  awards <- reporterSearchPI(PI_name, fiscal_years = last_10y)
  expect_gt(nrow(awards), 0)
})

test_that("reporterSharedAwards finds the known shared NIH award", {
  skip_if_offline()
  shared <- reporterSharedAwards(PI_name, PI_author, fiscal_years = last_10y)
  expect_gt(nrow(shared), 0)
})

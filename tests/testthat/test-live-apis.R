# These hit real PubMed / NIH RePORTER / bioRxiv endpoints and are
# skipped by default -- CRAN/Bioconductor build checks must not depend
# on network access, and the free NCBI rate limit makes CI runs
# annoyingly slow anyway.
#
# Default ground truth is a real, confirmed conflict: package author
# Janani Ravi ("Ravi J") and her CU Anschutz colleague Arjun Krishnan
# ("Krishnan A"), who have co-published and hold NIH funding together
# -- a screen against this pair is expected to flag on all three
# evidence types. Override via environment variables to use a
# different known pair, and run locally with:
#   Sys.setenv(
#     COICHECKR_RUN_LIVE_TESTS = "true",
#     COICHECKR_TEST_CANDIDATE = "Last FM",       # optional
#     COICHECKR_TEST_AUTHOR    = "Last FM",       # optional
#     COICHECKR_TEST_AFFILIATION = "",            # optional
#     COICHECKR_TEST_PI = "Last, First",          # optional
#     COICHECKR_TEST_PI_AUTHOR = "Last, First"    # optional
#   )
#   devtools::test()

skip_if_not(
  identical(Sys.getenv("COICHECKR_RUN_LIVE_TESTS"), "true"),
  "Set COICHECKR_RUN_LIVE_TESTS=true to run live API tests"
)

candidate <- Sys.getenv("COICHECKR_TEST_CANDIDATE", unset = "Ravi J")
author <- Sys.getenv("COICHECKR_TEST_AUTHOR", unset = "Krishnan A")
affiliation <- Sys.getenv("COICHECKR_TEST_AFFILIATION", unset = "Colorado")
if (!nzchar(affiliation)) affiliation <- NULL
pi_name <- Sys.getenv("COICHECKR_TEST_PI", unset = "Ravi, Janani")
pi_author <- Sys.getenv("COICHECKR_TEST_PI_AUTHOR", unset = "Krishnan, Arjun")
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

test_that("reporterSearchPi returns awards for a known NIH-funded PI", {
  skip_if_offline()
  awards <- reporterSearchPi(pi_name, fiscal_years = last_10y)
  expect_gt(nrow(awards), 0)
})

test_that("reporterSharedAwards finds the known shared NIH award", {
  skip_if_offline()
  shared <- reporterSharedAwards(pi_name, pi_author, fiscal_years = last_10y)
  expect_gt(nrow(shared), 0)
})

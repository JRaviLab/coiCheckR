# These hit real PubMed / NIH RePORTER / bioRxiv endpoints and are
# skipped by default -- CRAN/Bioconductor build checks must not depend
# on network access, and the free NCBI rate limit makes CI runs
# annoyingly slow anyway. Run locally with:
#   Sys.setenv(COICHECKR_RUN_LIVE_TESTS = "true"); devtools::test()

skip_if_not(
  identical(Sys.getenv("COICHECKR_RUN_LIVE_TESTS"), "true"),
  "Set COICHECKR_RUN_LIVE_TESTS=true to run live API tests"
)

test_that("pm_search_author returns PMIDs for a known author", {
  skip_if_offline()
  ids <- pm_search_author("Davies MR", affiliation = "Melbourne", max_year = 2020)
  expect_gt(length(ids), 0)
})

test_that("check_coi flags the known Davies-Nizet conflict", {
  skip_if_offline()
  report <- check_coi(
    candidate_name = "Davies MR",
    author_names = "Nizet V",
    affiliation = "Melbourne",
    check_second_degree = FALSE,
    check_funding = FALSE
  )
  expect_s3_class(report, "coi_report")
  expect_gt(nrow(report$direct), 0)
})

test_that("reporter_search_pi returns awards for a known NIH-funded PI", {
  skip_if_offline()
  awards <- reporter_search_pi("Nizet, Victor", fiscal_years = 2022:2024)
  expect_gt(nrow(awards), 0)
})

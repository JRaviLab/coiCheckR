test_that("buildCoauthorEdges pairs up co-authors on the same PMID", {
  fake <- tibble::tibble(
    PMID = c("1", "1", "1", "2", "2"),
    year = c(2020L, 2020L, 2020L, 2021L, 2021L),
    journal = "J",
    author_last = c("Smith", "Lee", "Park", "Chen", "Kim"),
    author_fore = c("AB", "CD", "E", "FG", "H")
  )

  edges <- buildCoauthorEdges(fake)

  expect_s3_class(edges, "tbl_df")
  # 3 authors on PMID 1 -> choose(3,2) = 3 edges; PMID 2 -> 1 edge
  expect_equal(nrow(edges), 4)
  expect_true(all(c("from", "to", "PMID", "year") %in% names(edges)))
})

test_that("buildCoauthorEdges handles a single-author record without error", {
  fake <- tibble::tibble(
    PMID = "1", year = 2020L, journal = "J",
    author_last = "Solo", author_fore = "A"
  )
  edges <- buildCoauthorEdges(fake)
  expect_equal(nrow(edges), 0)
})

test_that("buildCoauthorEdges drops rows with missing author_last", {
  fake <- tibble::tibble(
    PMID = c("1", "1"), year = c(2020L, 2020L), journal = "J",
    author_last = c("Smith", NA_character_),
    author_fore = c("AB", NA_character_)
  )
  edges <- buildCoauthorEdges(fake)
  expect_equal(nrow(edges), 0)
})

test_that(".pm_canonical_name reduces a full forename to the same form buildCoauthorEdges produces", {
  expect_equal(.pm_canonical_name("Ravi J"), "Ravi J")
  expect_equal(.pm_canonical_name("Ravi Janani"), "Ravi J")
  expect_equal(.pm_canonical_name("Smith AB"), "Smith AB")
  expect_equal(.pm_canonical_name("Smith Alice Beth"), "Smith AB")
})

test_that(".pm_canonical_name returns a single-token name unchanged", {
  expect_equal(.pm_canonical_name("Cher"), "Cher")
})

test_that(".pm_canonical_name is vectorised", {
  expect_equal(
    .pm_canonical_name(c("Ravi Janani", "Smith AB")),
    c("Ravi J", "Smith AB")
  )
})

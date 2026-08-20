test_that("buildCoauthorEdges pairs up co-authors on the same pmid", {
  fake <- tibble::tibble(
    pmid = c("1", "1", "1", "2", "2"),
    year = c(2020L, 2020L, 2020L, 2021L, 2021L),
    journal = "J",
    author_last = c("Smith", "Lee", "Park", "Chen", "Kim"),
    author_fore = c("AB", "CD", "E", "FG", "H")
  )

  edges <- buildCoauthorEdges(fake)

  expect_s3_class(edges, "tbl_df")
  # 3 authors on pmid 1 -> choose(3,2) = 3 edges; pmid 2 -> 1 edge
  expect_equal(nrow(edges), 4)
  expect_true(all(c("from", "to", "pmid", "year") %in% names(edges)))
})

test_that("buildCoauthorEdges handles a single-author record without error", {
  fake <- tibble::tibble(
    pmid = "1", year = 2020L, journal = "J",
    author_last = "Solo", author_fore = "A"
  )
  edges <- buildCoauthorEdges(fake)
  expect_equal(nrow(edges), 0)
})

test_that("buildCoauthorEdges drops rows with missing author_last", {
  fake <- tibble::tibble(
    pmid = c("1", "1"), year = c(2020L, 2020L), journal = "J",
    author_last = c("Smith", NA_character_),
    author_fore = c("AB", NA_character_)
  )
  edges <- buildCoauthorEdges(fake)
  expect_equal(nrow(edges), 0)
})

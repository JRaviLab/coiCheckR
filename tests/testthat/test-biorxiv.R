test_that("biorxiv_split_authors splits a semicolon-delimited string", {
  x <- "Brouwer S.; Barnett T.C.; Davies M.R.; Walker M.J."
  out <- biorxiv_split_authors(x)
  expect_length(out, 4)
  expect_equal(out[1], "Brouwer S.")
})

test_that("biorxiv_split_authors handles NA / empty gracefully", {
  expect_length(biorxiv_split_authors(NA_character_), 0)
  expect_length(biorxiv_split_authors(""), 0)
})

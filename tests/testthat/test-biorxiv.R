test_that("biorxivSplitAuthors splits a semicolon-delimited string", {
  x <- "Smith A.; Lee B.C.; Park D.E.; Chen F.G."
  out <- biorxivSplitAuthors(x)
  expect_length(out, 4)
  expect_equal(out[1], "Smith A.")
})

test_that("biorxivSplitAuthors handles NA / empty gracefully", {
  expect_length(biorxivSplitAuthors(NA_character_), 0)
  expect_length(biorxivSplitAuthors(""), 0)
})

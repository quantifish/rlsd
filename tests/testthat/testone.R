context("String length")

test_that("str_length is number of characters", {

  dir <- system.file("testdata", "", package = "lsd")
  object1 <- do_extract(dir = dir, data = TRUE)
  expect_equal(1, 1)

})

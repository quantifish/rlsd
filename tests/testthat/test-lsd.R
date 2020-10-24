context("Data input/output")

test_that("test some data", {

  dir <- system.file("testdata", "", package = "lsd")
  obj <- do_extract(dir = dir, data = TRUE)
  data <- obj@data
  p1 <- plot_mls(object = obj, do_save = FALSE)

  expect_s4_class(obj, "lsdOutput")
  expect_type(p1, "list")
  expect_true("ggplot" %in% class(p1))

  expect_equal(data$n_sex, 3)
  expect_equal(data$n_season, 2)
  expect_equal(data$tag_like_wt, 1)
  expect_that(data$mp_rule_parameters, is_a("matrix"))
  expect_that(data$size_break_l, is_a("numeric"))

})

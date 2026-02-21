library(testthat)


test_that("test perm_data_extract with simple data", {
  df <- tibble::tibble(
    y = c(1, 2, 3, 4),
    g = c("More developed", "Less developed", "More developed", "Less developed"),
    extra = 99
  )
  
  out <- perm_data_extract(df, y, g)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("outcome", "group"))
  expect_true(is.factor(out$group))
  expect_equal(out$outcome, c(1, 2, 3, 4))
  expect_equal(out$group, as.factor(c("More developed", "Less developed", "More developed", "Less developed")))
  
})


test_that("prep_perm_data on data with NA in columns", {
  df <- tibble::tibble(
    y = c(1, NA, 2, NA, 4), 
    g = c("More developed", "Less developed", NA, NA, "Less developed")
  )
  
  out <- perm_data_extract(df, y, g)
  expect_equal(nrow(out), 2)
  expect_equal(out$outcome, c(1, 4))
  expect_equal(as.character(out$group), c("More developed", "Less developed"))
  
})



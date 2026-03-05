library(testthat)
library(Sta380Project)

test_that("plot_perm_dist returns structured histogram output", {
  
  perm_res <- list(
    perm_stats = rnorm(100, mean = 0, sd = 1),
    observed  = 0.5
  )
  out <- plot_perm_dist(perm_res)
  
  expect_type(out, "list")
  expect_true("hist" %in% names(out))
  expect_true("obs" %in% names(out))
  expect_equal(sum(out$hist$counts), length(out$perm))
  expect_equal(out$obs, 0.5)
  
})



test_that("plot_perm_dist output matches observed statistic on toy data", {
  set.seed(1)
  perm_res <- list(
    perm_stats = abs(rnorm(1000)), 
    observed   = 2.5
  )
  
  expect_no_error(res <- plot_perm_dist(perm_res, bins = 40))
  
  expect_true(is.list(res))
  expect_true("obs" %in% names(res))
  expect_equal(res$obs, 2.5)
  expect_true(is.numeric(res$perm))
  expect_length(res$perm, 1000)
})


test_that("test plot_perm_dist on different bins", {
  perm_res <- list(
    perm_stats = rnorm(100, mean = 0, sd = 1),
    observed  = 0.5
  )
  res1 <- plot_perm_dist(perm_res, bins = 50)
  res2 <- plot_perm_dist(perm_res, bins = 20)
  
  expect_equal(length(res1$hist$breaks)-1, 50, tolerance = 5)
  expect_equal(length(res2$hist$breaks)-1, 20, tolerance = 5)
  expect_false(isTRUE(all.equal(res1$hist$breaks, res2$hist$breaks)))
})


test_that("plot_two_ECDFs runs without error", {
  
  outcome <- rnorm(20)
  group <- rep(c("A","B"), each = 10)
  
  expect_no_error(
    plot_two_ECDFs(outcome, group)
  )
  
})


test_that("plot_two_ECDFs errors with more than two groups", {
  
  outcome <- rnorm(15)
  group <- rep(c("A","B","C"), each = 5)
  
  expect_error(
    plot_two_ECDFs(outcome, group)
  )
  
})


test_that("plot_two_ECDFs errors with one group", {
  
  outcome <- rnorm(10)
  group <- rep("A", 10)
  
  expect_error(
    plot_two_ECDFs(outcome, group)
  )
  
})

# AI usage:
# Generative AI used in this file is:
# - naming of the test functions
# - logic of testing the histogram generated 
#   (expect_equal(sum(out$hist$counts), length(out$perm)))
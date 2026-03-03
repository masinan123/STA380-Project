library(testthat)

source("plot_perm_dist.R")
source("permutation_functions.R")
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
  
  df <- data.frame(
    outcome = c(33, 34, 32, 30, 28, 27),
    group = factor(rep(c("More developed", "Less developed"), each = 3))
  )
  
  perm_res <- perm_test_two_group_ks(df, B = 100, seed = 1, alternative = "greater")
  hist_res <- plot_perm_dist(perm_res, bins = 40)
  
  expected_obs <- perm_stat_ks(df$outcome, df$group)
  
  expect_named(hist_res, c("hist", "obs", "perm"))
  expect_equal(sum(hist_res$hist$counts), length(hist_res$perm))
  expect_equal(hist_res$obs, expected_obs)
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


# AI usage:
# Generative AI used in this file is:
# - naming of the test functions
# - logic of testing the histogram generated 
#   (expect_equal(sum(out$hist$counts), length(out$perm)))

library(testthat)
source("permutation_functions.R")

test_that("test perm_stat_ks calculates correctly", {
  
  outcome = c(1, 3, 5, 2, 4, 6)
  group = factor(c("More developed", "More developed", "More developed", 
                   "Less developed", "Less developed", "Less developed"))
  
  res = perm_stat_ks(outcome, group)
  
  expect_equal(res, 1/3)
})


test_that("perm_stat_ks returns 1 for completely separated groups", {
  outcome <- c(1, 2, 3, 10, 11, 12)
  group <- factor(rep(c("More developed", "Less developed"), each = 3))
  
  ks <- perm_stat_ks(outcome, group)
  testthat::expect_equal(ks, 1)
})


test_that("perm_stat_ks errors if group does not have exactly two groups", {
  outcome <- c(1, 2, 3)
  group <- c("A", "A", "A")
  
  expect_error(perm_stat_ks(outcome, group), "group must have exactly two levels. ")
})


test_that("test perm_stat_ks on unequal group size", {
  outcome = c(1, 3, 5, 2, 4)
  group = factor(c("More developed", "More developed", "More developed", 
                   "Less developed", "Less developed"))
  res <- perm_stat_ks(outcome, group)
  
  expect_equal(res, 1/3)
})


test_that("test perm_test_two_group_ks returns correctly", {
  d2 <- data.frame(
    outcome = c(1, 3, 5, 2, 4, 6), 
    group = factor(c("More developed", "More developed", "More developed", 
                     "Less developed", "Less developed", "Less developed"))
  )
  obs <- perm_stat_ks(d2$outcome, d2$group)
  pval <- perm_test_two_group_ks(d2, B = 1000, seed = 1)
  
  expect_equal(obs, pval$observed)
  expect_equal(pval$p_value, 1)
})

## here
test_that("perm_p_value_mc computes p-value correctly for greater/less/two.sided", {
  
  perm_stats <- c(-2, -1, 0, 1, 2)
  observed <- 1
  
  # greater
  expect_equal(
    perm_p_value_mc(observed, perm_stats, alternative = "greater"),
    (2 + 1) / (5 + 1)
  )
  
  # less
  expect_equal(
    perm_p_value_mc(observed, perm_stats, alternative = "less"),
    (4 + 1) / (5 + 1)
  )
  
  # two.sided
  expect_equal(
    perm_p_value_mc(observed, perm_stats, alternative = "two.sided"),
    (4 + 1) / (5 + 1)
  )
})


test_that("perm_p_value_mc uses abs() for two.sided (observed sign should not matter)", {
  
  perm_stats <- c(-2, -1, 0, 1, 2)
  
  p1 <- perm_p_value_mc(1, perm_stats, alternative = "two.sided")
  p2 <- perm_p_value_mc(-1, perm_stats, alternative = "two.sided")
  
  expect_equal(p1, p2)
})


test_that("perm_p_value_mc returns smallest possible Monte Carlo p-value when observed is more extreme than all perms", {
  
  perm_stats <- c(-2, -1, 0, 1, 2)
  B <- length(perm_stats)
  
  # greater
  expect_equal(
    perm_p_value_mc(10, perm_stats, alternative = "greater"),
    1 / (B + 1)
  )
  
  # less
  expect_equal(
    perm_p_value_mc(-10, perm_stats, alternative = "less"),
    1 / (B + 1)
  )
})


test_that("perm_p_value_mc errors for invalid alternative", {
  
  perm_stats <- c(-2, -1, 0, 1, 2)
  
  expect_error(
    perm_p_value_mc(1, perm_stats, alternative = "bad"),
    "alternative must be one of 'two.sided', 'greater', 'less'"
  )
})

library(testthat)

test_that("test perm_stat_ks calculates correctly", {
  
  outcome = c(1, 3, 5, 2, 4, 6)
  group = factor(c("More developed", "More developed", "More developed", 
                   "Less developed", "Less developed", "Less developed"))
  
  res = perm_stat_ks(outcome, group)
  
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


test_that("test perm_stat_ks on unequal group size", {
  outcome = c(1, 3, 5, 2, 4)
  group = factor(c("More developed", "More developed", "More developed", 
                   "Less developed", "Less developed"))
  res <- perm_stat_ks(outcome, group)
  
  expect_equal(res, 1/3)
})


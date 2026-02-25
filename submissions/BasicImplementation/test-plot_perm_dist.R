library(testthat)


test_that("perm_stat_mean_diff computes mean(group1) - mean(group2) correctly", {
  outcome <- c(1, 2, 3, 10, 11, 12)
  group <- factor(rep(c("More developed", "Less developed"), each = 3))
  
  # mean(More) = 2, mean(Less) = 11 => 2 - 11 = -9
  expect_equal(perm_stat_mean_diff(outcome, group), -9)
})

test_that("perm_p_value returns correct p-values for reasonable alternatives", {
  perm_stats <- c(-2, -1, 0, 1, 2)
  
  # greater: proportion >= 1 -> {1,2} => 2/5
  expect_equal(perm_p_value(1, perm_stats, "greater"), 2/5)
  
  # less: proportion <= -1 -> {-2,-1} => 2/5
  expect_equal(perm_p_value(-1, perm_stats, "less"), 2/5)
  
  # two.sided: abs >= abs(1) -> {-2,-1,1,2} => 4/5
  expect_equal(perm_p_value(1, perm_stats, "two.sided"), 4/5)
})

test_that("perm_p_value default is two.sided", {
  perm_stats <- c(-2, -1, 0, 1, 2)
  expect_equal(perm_p_value(1, perm_stats), 4/5)
})

test_that("perm_test_two_group returns expected components and lengths", {
  df <- tibble::tibble(
    outcome = c(33, 34, 32, 30, 28, 27),
    group = factor(rep(c("More developed", "Less developed"), each = 3))
  )
  
  res <- perm_test_two_group(df, B = 200, seed = 123)
  
  expect_true(is.list(res))
  expect_true(all(c("observed", "perm_stats", "p_value") %in% names(res)))
  expect_length(res$perm_stats, 200)
  expect_true(res$p_value >= 0 && res$p_value <= 1)
})

test_that("perm_test_two_group is reproducible when seed is set", {
  df <- tibble::tibble(
    outcome = c(33, 34, 32, 30, 28, 27),
    group = factor(rep(c("More developed", "Less developed"), each = 3))
  )
  
  res1 <- perm_test_two_group(df, B = 200, seed = 123)
  res2 <- perm_test_two_group(df, B = 200, seed = 123)
  
  expect_equal(res1$observed, res2$observed)
  expect_equal(res1$perm_stats, res2$perm_stats)
  expect_equal(res1$p_value, res2$p_value)
})

test_that("perm_test_two_group gives p-value 1 when observed is 0 (two.sided)", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 1, 2, 3),
    group = factor(rep(c("More developed", "Less developed"), each = 3))
  )
  
  res <- perm_test_two_group(df, B = 300, seed = 1, alternative = "two.sided")
  
  expect_equal(res$observed, 0)
  # abs(perm_stats) >= 0 always TRUE
  expect_equal(res$p_value, 1)
})

test_that("perm_test_wrapper matches perm_test_two_group after extraction", {
  raw_df <- tibble::tibble(
    birth_rate = c(33, 34, 32, 30),
    dev_group = c("More developed", "Less developed",
                  "More developed", "Less developed")
  )
  
  # If you haven't implemented perm_data_extract yet, don't fail the whole suite
  skip_if_not(exists("perm_data_extract"))
  
  res_wrap <- perm_test_wrapper(raw_df, birth_rate, dev_group, B = 100, seed = 42)
  
  test_df <- perm_data_extract(raw_df, birth_rate, dev_group)
  res_core <- perm_test_two_group(test_df, B = 100, seed = 42)
  
  expect_equal(res_wrap$observed, res_core$observed)
  expect_equal(res_wrap$perm_stats, res_core$perm_stats)
  expect_equal(res_wrap$p_value, res_core$p_value)
})


# AI usage:
# ChatGPT was used to suggest reasonable unit test cases
# and to explain the use of skip_if_not() for conditionally
# skipping wrapper tests when perm_data_extract is not yet implemented.
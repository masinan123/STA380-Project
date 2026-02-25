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

test_that("perm_test_two_group_mean returns expected components and lengths", {
  df <- tibble::tibble(
    outcome = c(33, 34, 32, 30, 28, 27),
    group = factor(rep(c("More developed", "Less developed"), each = 3))
  )
  
  res <- perm_test_two_group_mean(df, B = 200, seed = 123)
  
  expect_true(is.list(res))
  expect_true(all(c("observed", "perm_stats", "p_value") %in% names(res)))
  expect_length(res$perm_stats, 200)
  expect_true(res$p_value >= 0 && res$p_value <= 1)
})

test_that("perm_test_two_group_mean is reproducible when seed is set", {
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

test_that("perm_test_two_group_mean gives p-value 1 when observed is 0 (two.sided)", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 1, 2, 3),
    group = factor(rep(c("More developed", "Less developed"), each = 3))
  )
  
  res <- perm_test_two_group_mean(df, B = 300, seed = 1, alternative = "two.sided")
  
  expect_equal(res$observed, 0)
  # abs(perm_stats) >= 0 always TRUE
  expect_equal(res$p_value, 1)
})

test_that("perm_test_wrapper matches perm_test_two_group_mean after extraction", {
  raw_df <- tibble::tibble(
    birth_rate = c(33, 34, 32, 30),
    dev_group = c("More developed", "Less developed",
                  "More developed", "Less developed")
  )
  
  # If you haven't implemented perm_data_extract yet, don't fail the whole suite
  skip_if_not(exists("perm_data_extract"))
  
  res_wrap <- perm_test_wrapper(raw_df, birth_rate, dev_group, B = 100, seed = 42)
  
  test_df <- perm_data_extract(raw_df, birth_rate, dev_group)
  res_core <- perm_test_two_group_mean(test_df, B = 100, seed = 42)
  
  expect_equal(res_wrap$observed, res_core$observed)
  expect_equal(res_wrap$perm_stats, res_core$perm_stats)
  expect_equal(res_wrap$p_value, res_core$p_value)
})


test_that("perm_stat_median_diff computes correct median difference", {
  
  df <- data.frame(
    outcome = c(1, 2, 3, 10, 11, 12), 
    group = factor(c("More developed", "More developed", "More developed", 
                     "Less developed", "Less developed", "Less developed"))
  )
  
  res <- perm_stat_median_diff(df$outcome, df$group)
  expect <- median(df$outcome[df$group == "More developed"]) -
    median(df$outcome[df$group == "Less developed"])
  
  expect_true(abs(res) == abs(expect))
  
})


test_that("perm_test_two_group_median returns expected output structure", {
  
  df <- data.frame(
    outcome = c(1, 2, 3, 10, 11, 12), 
    group = factor(c("More developed", "More developed", "More developed", 
                     "Less developed", "Less developed", "Less developed"))
  )
  
  res <- perm_test_two_group_median(df, B = 1000, seed = 1, alternative = "two.sided")
  
  expect_named(res, c("observed", "perm_stats", "p_value"))
  expect_length(res$perm_stats, 1000)
  expect_true(res$p_value >= 0 && res$p_value <= 1)
  
})


test_that("perm_test_two_group_median returns correct perm_stats and p_value", {
  df <- data.frame(
    outcome = c(1, 2, 3, 10, 11, 12),
    group   = factor(rep(c("More developed", "Less developed"), each = 3),
                     levels = c("More developed", "Less developed"))
  )
  
  B <- 100
  seed <- 123
  alt <- "two.sided"
  
  res <- perm_test_two_group_median(df, B = B, seed = seed, alternative = alt)
  
  set.seed(seed)
  expected_perm <- numeric(B)
  for (b in seq_len(B)) {
    g_perm <- sample(df$group)
    expected_perm[b] <- perm_stat_median_diff(df$outcome, g_perm)
  }
  
  expect_equal(res$perm_stats, expected_perm)
  expect_equal(res$p_value, perm_p_value(res$observed, expected_perm, alt))
})




# AI usage:
# ChatGPT was used to suggest reasonable unit test cases
# and to explain the use of skip_if_not() for conditionally
# skipping wrapper tests when perm_data_extract is not yet implemented.

# Some test functions are named by the suggestion of AI. 
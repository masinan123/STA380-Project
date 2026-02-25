library(testthat)


test_that("plot_perm_dist returns structured histogram output", {
  
  perm_res <- list(
    perm = rnorm(100, mean = 0, sd = 1),
    obs  = 0.5
  )
  out <- plot_perm_dist(perm_res)
  
  expect_type(out, "list")
  expect_true("hist" %in% names(out))
  expect_true("obs" %in% names(out))
  expect_equal(sum(out$hist$counts), length(out$perm))
  expect_equal(out$obs, 0.5)
  
})



test_that("plot_perm_dist matches observed statistic from real data", {
  
  df <- read.csv("submissions/Data/wpp_country_indicators_2023.csv")
  perm_res <- perm_test_wrapper(df, 
                                outcome_col = birth_rate, 
                                group_col = dev_group, 
                                B = 100, seed = 1)
  hist_res <- plot_perm_dist(perm_res, bins = 40)
  
  test_df <- perm_data_extract(df, birth_rate, dev_group)
  expected_obs <- perm_stat_mean_diff(test_df$outcome, test_df$group)
  
  expect_type(hist_res, "list")
  expect_named(hist_res, c("hist", "obs", "perm"))
  
  expect_equal(sum(hist_res$hist$counts), length(hist_res$perm))
  expect_equal(hist_res$obs, expected_obs)
  
})




# AI usage:
# Generative AI used in this file is:
# - naming of the test functions
# - 


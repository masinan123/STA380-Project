library(dplyr)

#' Difference in Means (Test Statistic)
#'
#' Computes mean(group1) - mean(group2).
#'
#' @param outcome Numeric outcome vector.
#' @param group Group vector with two levels.
#' @return Numeric difference in means.
#'
#' @examples
#' outcome <- c(1, 2, 3, 10, 11, 12)
#' group <- rep(c("More developed", "Less developed"), each = 3)
#' perm_stat_mean_diff(outcome, group)
#'
#' @export
perm_stat_mean_diff <- function(outcome, group) {
  groups <- unique(group)
  
  mean(outcome[group == groups[1]]) - mean(outcome[group == groups[2]])
}


#' Difference in Medians (Test Statistic)
#'
#' Computes median(group1) - median(group2).
#'
#' @param outcome Numeric outcome vector.
#' @param group Group vector with two levels.
#' @return Numeric difference in medians. 
#'
#' @examples
#' outcome <- c(1, 2, 3, 10, 11, 12)
#' group <- rep(c("More developed", "Less developed"), each = 3)
#' perm_stat_median_diff(outcome, group)
#'
#' @export
perm_stat_median_diff <- function(outcome, group) {
  
  groups <- levels(as.factor(group))
  
  median(outcome[group == groups[1]]) - median(outcome[group == groups[2]])
}



#' Compute Permutation P-value
#'
#' Calculates the p-value based on the permutation distribution.
#'
#' @param observed Observed test statistic.
#' @param perm_stats Vector of permuted statistics.
#' @param alternative "two.sided", "greater", or "less".
#' @return Numeric p-value.
#'
#' @examples
#' perm_p_value(2, rnorm(1000))
#'
#' @export
perm_p_value <- function(observed, perm_stats, alternative = "two.sided") {
  
  if (alternative == "greater") {
    mean(perm_stats >= observed)
    
  } else if (alternative == "less") {
    mean(perm_stats <= observed)
    
  } else {
    mean(abs(perm_stats) >= abs(observed))
  }
}


#' Two-Group Permutation Test on Means
#'
#' Runs a permutation test using difference in means.
#'
#' @param df Data frame with columns named outcome and group.
#' @param B Number of permutations.
#' @param seed Optional random seed.
#' @param alternative Type of alternative hypothesis.
#' @return A list with observed statistic, permutation distribution and p-value.
#'
#' @examples
#' df <- tibble::tibble(
#'   outcome = c(33, 34, 32, 30, 28, 27),
#'   group = factor(rep(c("More developed", "Less developed"), each = 3))
#' )
#' perm_test_two_group(df, B = 500, seed = 1)
#'
#' @export
perm_test_two_group_mean <- function(df, B = 2000, seed = NULL, alternative = "two.sided") {
  
  if (!is.null(seed)) set.seed(seed)
  
  observed <- perm_stat_mean_diff(df$outcome, df$group)
  
  perm_stats <- numeric(B)
  
  for (b in seq_len(B)) {
    g_perm <- sample(df$group)
    perm_stats[b] <- perm_stat_mean_diff(df$outcome, g_perm)
  }
  
  p_value <- perm_p_value(observed, perm_stats, alternative)
  
  list(
    observed = observed,
    perm_stats = perm_stats,
    p_value = p_value
  )
}


#' Two-Group Permutation Test on Medians
#'
#' Runs a permutation test using difference in medians. 
#'
#' @param df Data frame with columns named outcome and group.
#' @param B Number of permutations.
#' @param seed Optional random seed.
#' @param alternative Type of alternative hypothesis.
#' @return A list with observed statistic, permutation distribution and p-value.
#'
#' @examples
#' df <- tibble::tibble(
#'   outcome = c(33, 34, 32, 30, 28, 27),
#'   group = factor(rep(c("More developed", "Less developed"), each = 3))
#' )
#' perm_test_two_group_median(df, B = 500, seed = 1)
#'
#' @export
perm_test_two_group_median <- function(df, B = 2000, seed = NULL, alternative = "two.sided") {
  
  
  if (!is.null(seed)) set.seed(seed)
  
  observed <- perm_stat_median_diff(df$outcome, df$group)
  
  perm_stats <- numeric(B)
  
  for (b in seq_len(B)) {
    g_perm <- sample(df$group)
    perm_stats[b] <- perm_stat_median_diff(df$outcome, g_perm)
  }
  
  p_value <- perm_p_value(observed, perm_stats, alternative)
  
  list(
    observed = observed,
    perm_stats = perm_stats,
    p_value = p_value
  )
}


#' Wrapper for Raw Data
#'
#' Extracts the outcome and group columns first, then runs the permutation test 
#' using either the difference in meas or the difference in medians. 
#'
#' @param df Original data frame.
#' @param outcome_col Outcome column (unquoted).
#' @param group_col Group column (unquoted).
#' @param B Number of permutations.
#' @param seed Optional seed.
#' @param alternative Type of alternative hypothesis.
#' @param statistic Character string specifying which test statistic to use.
#'        Must be one of "mean" or "median". Defaults to "mean". 
#' @return Result list from permutation test.
#'
#' @examples
#' raw_df <- tibble::tibble(
#'   birth_rate = c(33, 34, 32, 30),
#'   dev_group = c("More developed", "Less developed",
#'                 "More developed", "Less developed")
#' )
#' perm_test_wrapper(raw_df, birth_rate, dev_group, B = 200)
#'
#' @export
perm_test_wrapper <- function(df, outcome_col, group_col,
                              B = 2000, seed = NULL, alternative = "two.sided", 
                              statistic = "mean") {
  
  test_df <- perm_data_extract(df, {{ outcome_col }}, {{ group_col }})
  if (statistic == "mean") {
    perm_test_two_group_mean(test_df, B = B, seed = seed, alternative = alternative)
  }
  else {
    perm_test_two_group_median(test_df, B = B, seed = seed, alternative = alternative)
  }
}





# AI usage:
# Generative AI (ChatGPT) was used to clarify:
# - roxygen documentation formatting
# - tidy evaluation syntax using {{ }} in wrapper functions
#
# All statistical logic and implementation decisions were reviewed and
# adapted by the author.
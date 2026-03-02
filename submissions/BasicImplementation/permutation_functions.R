library(dplyr)

#  Test using KS statistic

#' Compute Kolmogorov–Smirnov (KS) Test statistic
#'
#' Computes the maximum difference between the Empirical Cumulative Distribution 
#' Functions (ECDFs) of two groups. 
#' @param outcome A numeric outcome vector.
#' @param group A group vector with two levels.
#' @return Numeric difference in means.
#'
#' @examples
#' outcome <- c(1, 2, 3, 10, 11, 12)
#' group <- rep(c("More developed", "Less developed"), each = 3)
#' perm_stat_mean_diff(outcome, group)
#'
#' @export
perm_stat_ks <- function(outcome, group) {
  groups <- as.character(group)
  groups <- unique(group)
  
  if (length(groups) != 2) {
    stop("group must have exactly two levels. ")
  }
  xA <- outcome[group == groups[1]]
  xB <- outcome[group == groups[2]]
  
  ecdf_A <- stats::ecdf(xA)
  ecdf_B <- stats::ecdf(xB)
  
  grid <- sort(unique(c(xA, xB)))
  max(abs(ecdf_A(grid) - ecdf_B(grid)))
}


#' Two-Group Permutation Test on KS statistic
#'
#' Performs a permutation test to compare the distributions of two groups
#' using the two-sample Kolmogorov-Smirnov (KS) statistic. 
#' 
#' The null hypothesis assumes that group labels are exchangeable,
#' implying identical outcome distributions between groups.
#' 
#' @param df Data frame with columns named outcome and group.
#' @param B Number of permutations.
#' @param seed Optional random seed.
#' @param alternative Type of alternative hypothesis.
#' @return A list with observed statistic, permutation distribution and p-value.
#' 
#'
#' @examples
#' df <- tibble::tibble(
#'   outcome = c(33, 34, 32, 30, 28, 27),
#'   group = factor(rep(c("More developed", "Less developed"), each = 3))
#' )
#' perm_test_two_group_mean(df, B = 500, seed = 1)
#'
#' @export
perm_test_two_group_ks <- function(df, B = 2000, seed = NULL, alternative = "two.sided") {
  
  if (!is.null(seed)) set.seed(seed)
  
  observed <- perm_stat_ks(df$outcome, df$group)
  
  perm_stats <- numeric(B)
  
  for (b in seq_len(B)) {
    g_perm <- sample(df$group)
    perm_stats[b] <- perm_stat_ks(df$outcome, g_perm)
  }
  if (alternative == "greater" || alternative == "two.sided") {
    p_value <- mean(perm_stats >= observed)
  }
  else if (alternative == "less") {
    p_value <- mean(perm_stats <= observed)
  }
  else {
    stop("alternative must be one of 'two.sided', 'greater', 'less'")
  }
  # p_value <- perm_p_value(observed, perm_stats, alternative)
  
  list(
    observed = observed,
    perm_stats = perm_stats,
    p_value = p_value
  )
}


#' Wrapper for Raw Data
#'
#' Extracts the outcome and group columns from a raw data frame first,
#' then runs the permutation test using the Kolmogorov–Smirnov statistic
#' for two development groups. 
#'
#' @param df Original data frame.
#' @param outcome_col Outcome column (unquoted).
#' @param group_col Group column (unquoted).
#' @param B Number of permutations.
#' @param seed Optional seed.
#' @param alternative Type of alternative hypothesis.
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
perm_test_wrapper_ks <- function(df, outcome_col, group_col,
                              B = 2000, seed = NULL, alternative = "two.sided") {
  
  test_df <- perm_data_extract(df, {{ outcome_col }}, {{ group_col }})
  perm_test_two_group_ks(test_df, B = B, seed = seed)
}





# AI usage:
# Generative AI (ChatGPT) was used to clarify:
# - roxygen documentation formatting
# - tidy evaluation syntax using {{ }} in wrapper functions
# - usage of "stats" package and ecdf()
#
# All statistical logic and implementation decisions were reviewed and
# adapted by the author.
library(dplyr)


#  Test using KS statistic

#' Compute Kolmogorov–Smirnov (KS) Test statistic
#'
#' Computes the maximum difference between the Empirical Cumulative Distribution 
#' Functions (ECDFs) of two groups. 
#' @param outcome A numeric outcome vector.
#' @param group A group vector with two levels.
#' @return Numeric KS statistic (maximum absolute difference between ECDFs).
#'
#' @examples
#' outcome <- c(1, 2, 3, 10, 11, 12)
#' group <- rep(c("More developed", "Less developed"), each = 3)
#' perm_stat_ks(outcome, group)
#'
#' @export
perm_stat_ks <- function(outcome, group) {
  # group should have exactly 2 levels
  g <- unique(group)
  if (length(g) != 2) stop("group must have exactly two levels. ")
  
  # The problem with the current code version is that what you have done
  # is perfectly divided these into two groups;
  # if you are trying to reshuffle for a permutation test,
  # you cannot have them neatly organized.
  x <- outcome[group == g[1]]
  y <- outcome[group == g[2]]
  
  Fx <- stats::ecdf(x)
  Gy <- stats::ecdf(y)
  z <- c(x, y)
  
  max(abs(Fx(z) - Gy(z)))
}


#' Compute Monte Carlo permutation p-value
#'
#' Computes the Monte Carlo permutation p-value using the standard
#' (+1)/(B+1) adjustment.
#'
#' @param observed Observed test statistic.
#' @param perm_stats Vector of permuted statistics.
#' @param alternative "two.sided", "greater", or "less".
#' @return Numeric p-value.
#'
#' @examples
#' perm_p_value_mc(2, rnorm(1000), alternative = "greater")
#'
#' @export
perm_p_value_mc <- function(observed, perm_stats, alternative = "two.sided") {
  
  if (alternative == "greater") {
    (sum(perm_stats >= observed) + 1) / (length(perm_stats) + 1)
    
  } else if (alternative == "less") {
    (sum(perm_stats <= observed) + 1) / (length(perm_stats) + 1)
    
  } else if (alternative == "two.sided") {
    (sum(abs(perm_stats) >= abs(observed)) + 1) / (length(perm_stats) + 1)
    
  } else {
    stop("alternative must be one of 'two.sided', 'greater', 'less'")
  }
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
#' perm_test_two_group_ks(df, B = 500, seed = 1)
#'
#' @export
perm_test_two_group_ks <- function(df, B = 2000, seed = NULL, alternative = "two.sided") {
  
  if (!is.null(seed)) set.seed(seed)
  
  # calculate the observed k-s statistic
  observed <- perm_stat_ks(df$outcome, df$group)
  
  outcome <- df$outcome
  group <- df$group
  
  g <- unique(group)
  # make sure there are two groups
  if (length(g) != 2) stop("df$group must have exactly 2 groups")
  
  perm_stats <- numeric(B)
  
  for (i in 1:B) {
    k <- sample.int(n, size = n1)  # choose indices for permuted group 1
    
    grp1 <- outcome[k]
    grp2 <- outcome[-k]
    
    Fi <- ecdf(grp1)
    Gi <- ecdf(grp2)
    
    perm_stats[i] <- max(abs(Fi(outcome) - Gi(outcome)))
  }
  
  p_value <- perm_p_value_mc(observed, perm_stats, alternative)
  
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
#' perm_test_wrapper_ks(raw_df, birth_rate, dev_group, B = 200)
#'
#' @export
perm_test_wrapper_ks <- function(df, outcome_col, group_col,
                              B = 2000, seed = NULL, alternative = "two.sided") {
  
  test_df <- perm_data_extract(df, {{ outcome_col }}, {{ group_col }})
  perm_test_two_group_ks(test_df, B = B, seed = seed, alternative = alternative)
}



#' Print a conclusion for a permutation test
#'
#' Prints an interpretation of the permutation test result based on the
#' p-value and the significance level \eqn{\alpha}. The conclusion states
#' whether the null hypothesis is rejected and provides a short
#' distributional interpretation for the Kolmogorov--Smirnov (KS) statistic.
#'
#' @param p_val A numeric value giving the p-value from the permutation test.
#' @param alpha A numeric value giving the significance level used for the test.
#'
#' @return No returned value. This function is called for its side effect of
#' printing a conclusion to the console.
#'
#' @details
#' If `p_val < alpha`, the function prints a conclusion that the null
#' hypothesis is rejected. Otherwise, it prints a conclusion that there is
#' insufficient evidence to reject the null hypothesis.
#'
#' This function is intended for reporting the result of a two-group
#' permutation test using the KS statistic in the project analysis.
#'
#' @examples
#' perm_test_conclusion(0.012, 0.05)
#' perm_test_conclusion(0.18, 0.05)
#'
#' @export
perm_test_conclusion <- function(p_val, alpha) {
  if (p_val < alpha) {
    cat("Since ",p_val,"(p-value) < ", alpha,"(alpha), we reject H0.\n")
    cat("The observed KS statistic is unlikely under random label assignment, ")
    cat("providing statistical evidence that the indicator distribution differs between development groups in 2023.\n")
  } else {
    cat("Since p \\ge", alpha, ", we fail to reject $H_0$.\n")
    cat("The observed KS statistic is consistent with the permutation distribution under exchangeability; ")
    cat("there is insufficient statistical evidence of a distributional difference between development groups.\n")
  }
}

# AI usage:
# Generative AI (ChatGPT) was used to clarify:
# - roxygen documentation formatting
# - tidy evaluation syntax using {{ }} in wrapper functions
# - usage of "stats" package and ecdf()
#
# All statistical logic and implementation decisions were reviewed and
# adapted by the author.
#' Plot the Permutation Distribution with Observed Statistic
#'
#' Draws a histogram of the permutation test statistic and shows the observed 
#' test statistic on the histogram as a vertical line. 
#'
#' @param perm_result A list containing: 
#' \describe{
#'    \item{perm_stats}{Numeric vector of permutation statistic. }
#'    \item{observed}{Observed test statistic. }
#' }
#' @param bin An Integer specifying the number of histogram bins. Defaults to 40. 
#' @return Invisibly returns a list with:
#' \describe{
#'  \item{hist}{Histogram object returned by function hist().}
#'  \item{obs}{Observed test statistic. }
#' }
#' 
#' @examples
#' perm_res <- list(perm_stats = abs(rnorm(1000)), observed = 2.5)
#' plot_perm_dist(perm_res)
#' 
#' @export
plot_perm_dist <- function(perm_result, bins = 40) {
  
  perm <- perm_result$perm_stats
  obs <- perm_result$observed
  
  max_perm <- max(perm)
  extension <- 0.2 * (max_perm - min(perm))
  x_right <- max(max_perm + extension, obs)
  
  h <- hist(perm, 
            breaks = bins, 
            col = "lightblue", 
            border = "white", 
            main = "Permutation Distribution", 
            xlab = "Permutation Test Statistic",
            xlim = c(min(perm), x_right) )
  
  abline(v = obs, col = "red", lwd = 3)
  
  perm_extreme <- perm[perm >= obs]
  if (length(perm_extreme) > 0) {
    hist(perm_extreme,
         breaks = bins,
         col = rgb(0, 1, 0, 0.4),
         add = TRUE)
  }
  # return the hist object by invisible()
  invisible(list(
    hist = h,
    obs  = obs,
    perm = perm
  ))
}


#' Plot ECDFs for Two Groups
#'
#' Draws the empirical cumulative distribution functions (ECDFs) for two groups
#' to visually compare their distributions.
#'
#' @param outcome A numeric vector containing the outcome variable.
#' @param group A vector indicating the group membership for each observation.
#' The vector must contain exactly two unique group levels.
#'
#' @return Invisibly returns a list with:
#' \describe{
#'   \item{ecdf_A}{ECDF function for the first group.}
#'   \item{ecdf_B}{ECDF function for the second group.}
#'   \item{groups}{The two group labels used in the plot.}
#' }
#'
#' @examples
#' outcome <- rnorm(20)
#' group <- rep(c("More developed", "Less developed"), each = 10)
#' plot_two_ECDFs(outcome, group)
#'
#' @export
plot_two_ECDFs <- function(outcome, group) {
  groups <- as.character(group)
  groups <- unique(groups)
  
  if (length(groups) != 2) {
    stop("group must have exactly two levels. ")
  }
  xA <- outcome[group == groups[1]]
  xB <- outcome[group == groups[2]]
  
  ecdf_A <- stats::ecdf(xA)
  ecdf_B <- stats::ecdf(xB)
  
  plot(ecdf_A, col="blue", lwd=1,
       main="ECDF Comparison between two groups",
       xlab="Outcome",
       ylab="Cumulative Probability")
  
  lines(ecdf_B, col="yellow", lwd=1)
  
  legend("bottomright",
         legend=c("More developed","Less developed"),
         col=c("blue","yellow"),
         lwd=2)
  
}


# AI usage: 
# - the usage of invisible() and rgb()

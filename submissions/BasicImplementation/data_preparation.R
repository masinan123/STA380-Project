library(dplyr)
#' Prepare Data for Permutation Test
#' 
#' Extract and cleans outcome and group variables from a dataset to use in 
#' permutation test comparing "More developed" and "Less developed" groups.
#'  
#' @details
#' Columns must be supplied without quotation marks. 
#' Only observations from the groups "More developed" and 
#' "Less developed" are retained. 
#' 
#' @param df A data frame or tibble containing the dataset. 
#' @param outcome_col A column name representing the numeric outcome variable 
#'        (e.g., birth rate, life_exp). 
#' @param group_col A column name representing the development group classification variable.
#' @return A data frame with two columns, outcome and group:
#' \describe{
#'   \item{outcome}{Numeric outcome variable.}
#'   \item{group}{Factor with levels "More developed" and "Less developed".}
#' }
#' 
#' @examples
#' df <- tibble::tibble(
#'   birth rate = c(33, 34, 32, 30),
#'   dev_group = c("More developed", "Less developed",
#'                 "More developed", "Other") )
#' test_df <- perm_data_extract(df, birth_rate, dev_group)
#' 
#' @importFrom dplyr select filter mutate
#' @export

perm_data_extract <- function(df, outcome_col, group_col) {
  df %>%
    dplyr::select(
      outcome = {{ outcome_col }}, 
      group = {{ group_col }}
    ) %>%
    dplyr::filter(!is.na(outcome), !is.na(group)) %>%
    dplyr::mutate(group = as.factor(group)) %>%
    dplyr::filter(group %in% c("More developed", "Less developed")) %>%
    droplevels()
}


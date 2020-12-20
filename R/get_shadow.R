#' Create a shadow matrix based on the missing values in a data frame.
#'
#' @description
#' The get_shadow function builds a shadow matrix based on the
#' passed in data frame.
#'
#' @param data A data frame with missing values.
#' @param vars A subset of variables to include in the shadow matrix.
#'
#' @return a [tibble][tibble::tibble-package]
#' @importFrom dplyr transmute across if_else everything
#' @export get_shadow
get_shadow <- function(data, vars = dplyr::everything()) {
  dplyr::transmute(data, dplyr::across({{ vars }},
                                       ~dplyr::if_else(is.na(.), FALSE, TRUE),
                                       .names = "{.col}_NA"))
}

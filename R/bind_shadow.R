#' Bind a shadow matrix to a data frame with NA values.
#'
#' @description
#' The bind_shadow function binds a shadow matrix to the data frame.
#' @param data A data frame with missing values.
#' @param vars A subset of variables to include in the shadow matrix.
#' @return a [tibble][tibble::tibble-package]
#' @importFrom dplyr mutate across if_else everything
#' @export bind_shadow
bind_shadow <- function(data, vars = dplyr::everything()) {
  dplyr::mutate(data, dplyr::across({{ vars }},
                      ~dplyr::if_else(is.na(.), FALSE, TRUE), .names = "{.col}_NA"))
}

#' Convert a list of data.frames to a stacked tibble
#'
#' @param mids A mice mids object.
#' @param .setid Set the name of the imputation key column. In cases where there
#' are more than 1 data.frames the key column increments by 1 for each data set.
#' Key column name defaults to ".imp".
#' @export
stack_mids <- function(mids, .setid = ".imp") {
  if (is.list(mids))
    dplyr::bind_rows(mids, .id = .setid)
}

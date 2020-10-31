#' Convert a list of data.frames to a stacked tibble
#'
#' @param mids A mice mids object.
#' @param .setid Set the name of the key column. In cases where there are
#' more than 1 data.frames the key column increments by 1 for each data set.
#' Defaults to "stack_id"
#' @export
#' @examples
#' stack <- stack_mids(mids, .setid = "stack_id")
stack_mids <- function(mids, .setid = "stack_id") {
  if (hasArg(mids) & is.list(mids))
    dplyr::bind_rows(mids, .id = .setid)
}

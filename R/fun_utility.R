#' Helper to remove columns that are empty
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
remove_na_cols <- function(data) {
  na_cols <- apply(data, function(x) all(is.na(x)), MARGIN = 2)
  data <- data[, !na_cols]
  return(data)
}

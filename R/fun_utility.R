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


#' Read the selection settings from the sys.file
#'
#' This is used to make the selection settings restorable (i.e. which
#' Operation / Place / project was selected when milQuant was closed using
#' the save & close button)
#'
#' @return a list with the settings
#' @export
read_milQuant_settings <- function() {
  result <- try(readRDS(system.file(package = "milQuant", mustWork = TRUE,
                                    "app/www/settings/db_settings.RDS")),
                silent = TRUE)
  if (inherits(result, "try-error")) {
    warning(paste("Some error in read_milQuant_settings(): ", result))
    result <- list("selected_project" = NULL,
                   "selected_places" = NULL,
                   "selected_trenches" = NULL,
                   "selected_categories" = NULL)
  }
  return(result)
}

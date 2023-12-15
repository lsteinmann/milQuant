#' Make warnings more visible
#'
#' @param msg
#'
#' @return
#' @export
#'
#' @examples
milQ_warning <- function(msg) {
  msg <- paste(crayon::cyan("milQuant: "), crayon::red(crayon::bold("!  ")), msg)
  msg
}

#' Make messages more visible
#'
#' @param msg
#'
#' @return
#' @export
#'
#' @examples
milQ_message <- function(msg) {
  msg <- paste(crayon::cyan("milQuant: "), crayon::white(crayon::bold("i  ")), msg)
  msg
}

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


#' mq_spinner
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
mq_spinner <- function(object) {
  withSpinner(object,
              image = "img/quant-spinner-smooth.gif",
              image.width = 100,
              image.height = 100)
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
milQuant_hovertemplate <- function(value = "count") {
  paste0("<b>%{fullData.name}</b><br>",
         "%{x}<br>",
         value, ": <b>%{y}</b><br>",
         "<extra></extra>")
}


#' Title
#'
#' @param type
#'
#' @return
#' @export
#'
#' @examples
ui_options_title <- function(type = "plot"){
  match.arg(type, choices = c("plot", "selection"))

  if (type == "plot") {
    title <- tagList(icon("wrench"), "Plot options")
  } else if (type == "selection") {
    title <- tagList(icon("filter"), "Selection options")
  }

  return(title)
}


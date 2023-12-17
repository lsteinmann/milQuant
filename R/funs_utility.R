#' Make warnings more visible
#'
#' @param msg Message
#'
#' @return colorful Message
#' @export
milQ_warning <- function(msg) {
  msg <- paste(crayon::cyan("milQuant: "), crayon::red(crayon::bold("!  ")), msg)
  msg
}

#' Make messages more visible
#'
#' @param msg Message
#'
#' @return colorful Message
#' @export
milQ_message <- function(msg) {
  msg <- paste(crayon::cyan("milQuant: "), crayon::white(crayon::bold("i  ")), msg)
  msg
}

#' Helper to remove columns that are empty
#'
#' @param data data.frame from which columns with all na-vals should be removed
#'
#' @return data.frame
#' @export
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
#' @param object ui objects that needs a spinner
#'
#' @return ui object witht spinner
#' @export
mq_spinner <- function(object) {
  withSpinner(object,
              image = "img/quant-spinner-smooth.gif",
              image.width = 100,
              image.height = 100)
}


#' Title
#'
#' @param value what should it say in the third row? (count / weight etc)
#'
#' @return character vector with hovertemplare for plotly
#' @export
milQuant_hovertemplate <- function(value = "count") {
  paste0("<b>%{fullData.name}</b><br>",
         "%{x}<br>",
         value, ": <b>%{y}</b><br>",
         "<extra></extra>")
}


#' Title
#'
#' @param type plot or selection
#'
#' @return tagList with icon and title
#' @export
ui_options_title <- function(type = "plot"){
  match.arg(type, choices = c("plot", "selection"))

  if (type == "plot") {
    title <- tagList(icon("wrench"), "Plot options")
  } else if (type == "selection") {
    title <- tagList(icon("filter"), "Selection options")
  }

  return(title)
}


#' Days to Milliseconds for Plotly Histogram bins
#'
#' @param x number of days
#'
#' @return x in milliseconds
#' @export
ms_days <- function(x) {
  x * 8.64e+7
}


#' Title
#'
#' @param input the ui object that the popover should be attached to
#'
#' @return same input with a tooltip/popover to inform of Load Resources button behaviour
#' @export
addLRPopover <- function(input) {

  content <- paste0(
    "The \"Load Resources\" button will query the ",
    "database for all resources of the selected categories from the ",
    "selected Operations. Depending on the number of resources, ",
    "this may take a while."
  )

  input %>%
    popify(title = "Load Resources", content = content, placement = "bottom")

}


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
#' @importFrom shinycssloaders withSpinner
#'
#' @return ui object with spinner
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

#' tagList for the Title of Selection- and Plot-Options
#'
#' @param type chr. "plot" or "selection"
#'
#' @return `tagList()` with Icon and Title
#'
#'
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
#' @param days number of days
#'
#' @return `days` in milliseconds
#' @export
ms_days <- function(days) {
  days * 8.64e+7
}


#' Add the *Load Resources*-Popover to UI-Element
#'
#' @param input The UI-object that the popover should be attached to
#'
#' @return Same input with a tooltip/popover to inform of *Load Resources*-button behaviour
#'
#' @importFrom shinyBS popify
#'
#' @export
addLRPopover <- function(input) {

  content <- paste0(
    "The \"Load Resources\" button will query the ",
    "database for all resources of the selected categories from the ",
    "selected Operations. Depending on the number of resources, ",
    "this may take a while."
  )

  popify(input,
         title = "Load Resources",
         content = content,
         placement = "bottom")
}


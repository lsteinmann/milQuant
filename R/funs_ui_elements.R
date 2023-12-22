#' tagList for Title of Plot/Selection options
#'
#' @param type plot or selection
#'
#' @return tagList with icon and title
#' @export
uiOptionsTitle <- function(type = "plot"){
  match.arg(type, choices = c("plot", "selection"))

  if (type == "plot") {
    title <- tagList(icon("wrench"), "Plot options")
  } else if (type == "selection") {
    title <- tagList(icon("filter"), "Selection options")
  }

  return(title)
}


#' Popover for 'Load Resources'-buttons
#'
#' @param input the button
#'
#' @return same input with a tooltip/popover to inform of 'Load Resources'-button behaviour
#' @export
addLRPopover <- function(input) {

  content <- paste0(
    "The \"Load Resources\" button will query the ",
    "database for all resources of the selected categories from the ",
    "selected Operations. Depending on the number of resources, ",
    "this may take a while."
  )

  input %>%
    shinyBS::popify(title = "Load Resources",
                    content = content,
                    placement = "bottom")

}


#' Add a spinning milQuant-Logo
#'
#' @param object ui objects that needs a spinner
#'
#' @return ui object with spinner
#' @export
mq_spinner <- function(object) {
  shinycssloaders::withSpinner(
    object,
    image = "img/quant-spinner-smooth.gif",
    image.width = 100,
    image.height = 100
  )
}

#' tagList of two textInputs for plot title
#'
#' inputIDs are always `ns("title")` and `ns("subtitle")`
#'
#' @param title chr. default value for title textInput
#' @param subtitle chr. default value for subtitle textInput
#'
#'
#' @return tagList with two textInput() UI elements
#' @export
plotTitleInputs <- function(id = id, title = "", subtitle = "") {
  ns <- NS(id)
  tagList(
    textInput(inputId = ns("title"),
              label = "Title",
              value = title,
              placeholder = "Enter title here"),
    textInput(inputId = ns("subtitle"),
              label = "Subtitle",
              value = subtitle,
              placeholder = "Enter subtitle here")
  )
}

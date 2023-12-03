#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
uiCategorySelector <- function(id) {

  ns <- NS(id)

  htmlOutput(ns("ui_category_selector"))
}

#' Title
#'
#' @param id
#' @param data
#' @param inputid
#'
#' @return
#' @export
#'
#' @examples
generateCategorySelector <- function(id, inputid) {
  moduleServer(
    id,
    function(input, output, session) {

      label <- "Choose one or many resource categories"

      data("milQuant_cats")

      output$ui_category_selector <- renderUI({
        pickerInput(inputId = inputid,
                    label = label,
                    choices = milQuant_cats$Finds,
                    multiple = TRUE,
                    options = list("actions-box" = TRUE,
                                   "live-search" = TRUE,
                                   "live-search-normalize" = TRUE,
                                   "live-search-placeholder" = "Search here..."))
      })
    }
  )
}

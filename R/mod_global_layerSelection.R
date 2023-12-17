#' Title
#'
#' @inheritParams db_activity_tab
#'
#' @return a htmloutput of the layerselector
#' @export
uiLayerSelector <- function(id) {

  ns <- NS(id)

  htmlOutput(ns("ui_layer_selector"))
}

#' Title
#'
#' @inheritParams db_activity_tab
#' @inheritParams generateCategorySelector
#' @param data all the data (this is excessive...)
#'
#' @return server code for layer selector
#' @export
generateLayerSelector <- function(id, data, inputid) {
  moduleServer(
    id,
    function(input, output, session) {

      # creates the selector for layers present in 'data' as a relation
      label <- "Choose one or many contexts"

      # first get the unique values that will be displayed in the selectInput
      selectable_layers <- reactive({
        selectable_layers <- data()$relation.liesWithinLayer %>%
          unique() %>%
          as.character() %>%
          sort()
      })


      # produce the selectInput object that will be displayed
      output$ui_layer_selector <- renderUI({
        pickerInput(inputId = inputid,
                    label = label,
                    choices = selectable_layers(),
                    multiple = TRUE,
                    selected = selectable_layers(),
                    options = list("actions-box" = TRUE,
                                   "live-search" = TRUE,
                                   "live-search-normalize" = TRUE,
                                   "live-search-placeholder" = "Search here..."))
      })
    }
  )
}

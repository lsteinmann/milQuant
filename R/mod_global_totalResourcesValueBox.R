#' Title
#'
#' @inheritParams totalResources_serv
#'
#' @return valueBox with number of resources that will be downloaded
#'
#' @export
totalResourcesValueBox <- function(id, width = 2) {
  ns <- NS(id)

  valueBox(
    uiOutput(ns("n")),
    "Total Resources",
    icon = icon("file"),
    color = "olive",
    width = width)
}

#' Title
#'
#' @inheritParams generateLayerSelector
#' @param sel_categories vector of selected categories
#'
#' @return number of resources used in UI
#' @export
totalResources_serv <- function(id, sel_categories) {
  moduleServer(
    id,
    function(input, output, session) {

      output$n <- renderText({
        validate(
          need(is.data.frame(react_index()), "Waiting for data...")
        )

        if (all(sel_categories == "")) {
          n <- 0
        } else {
          n <- react_index() %>%
            filter(Operation %in% db_selected_operations()) %>%
            filter(category %in% sel_categories) %>%
            nrow()
        }

        prettyNum(n, big.mark = ",")
      })
    }
  )


}

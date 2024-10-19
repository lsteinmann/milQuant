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
#' @param sel_layers vector of selected layers
#'
#' @return number of resources used in UI
#' @export
totalResources_serv <- function(id, sel_categories, sel_layers = NULL) {
  moduleServer(
    id,
    function(input, output, session) {

      output$n <- renderText({
        validate(
          need(is.data.frame(react_index()) && nrow(react_index()) != 0, "Waiting for data...")
        )

        if (all(sel_categories == "")) {
          n <- 0
        } else {
          n <- react_index() %>%
            filter(Operation %in% db_selected_operations()) %>%
            filter(category %in% sel_categories)
          if (!is.null(sel_layers)) {
            n <- n %>%
              filter(liesWithinLayer %in% sel_layers)
          }
          n <- nrow(n)
        }

        prettyNum(n, big.mark = ",")
      })
    }
  )


}

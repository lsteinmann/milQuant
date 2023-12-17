#' Global Module: totalResourcesValueBox (UI)
#'
#' @inherit mod_ui_doc
#' @param width Width of the `valueBox()`
#'
#' @return `valueBox()` with number of resources selected as gathered from Index.
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

#' Global Module: totalResourcesValueBox (Server Code)
#'
#' @inherit mod_serv_doc
#' @param sel_categories Vector of selected categories.
#'
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

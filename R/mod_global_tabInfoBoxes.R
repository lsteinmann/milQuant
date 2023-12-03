#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples

tabInfoBox_ui <- function(id, width = 10) {
  ns <- NS(id)

  infoBox(title = "Info", value = textOutput(ns("overview")),
          icon = icon("list"),
          color = "olive", width = width)
}

tabValueBox_ui <- function(id, width = 2) {
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
#' @param id
#' @param tab_data
#'
#' @return
#' @export
#'
#' @examples
tabInfoRow_server <- function(id, tab_data) {
  moduleServer(
    id,
    function(input, output, session) {

      output$n <- renderText({
        validate(
          need(is.data.frame(tab_data()), "Waiting for data...")
        )
        prettyNum(nrow(tab_data()), big.mark = ",")
      })

      output$overview <- renderText({
        validate(
          need(is.data.frame(tab_data()), "Waiting for data...")
        )
        n_layers <- length(unique(tab_data()$relation.liesWithinLayer))
        n_objects <- nrow(tab_data())
        paste("The selected trenches ", paste(db_selected_operations(), collapse = ", "),
              " (from ", paste(db_selected_places(), collapse = ", "),
              ") contain a total of ", n_objects,
              " resources from ", n_layers, " contexts.",
              sep = "")
      })
    }
  )


}

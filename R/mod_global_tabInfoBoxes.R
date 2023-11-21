#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
tabInfoRow_ui <- function(id) {

  ns <- NS(id)

  fluidRow(
    infoBox(title = "Info", value = textOutput(ns("overview")),
            icon = icon("list"),
            color = "olive", width = 10),
    valueBox(
      uiOutput(ns("n")),
      "Total Resources",
      icon = icon("file"),
      color = "olive",
      width = 2)
  )
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
        n_layers <- length(unique(tab_data()$relation.liesWithinLayer))
        n_objects <- nrow(tab_data())
        paste("The selected trenches ", paste(db_trenches(), collapse = ", "),
              " (from ", paste(db_operations(), collapse = ", "),
              ") contain a total of ", n_objects,
              " resources from ", n_layers, " contexts.",
              sep = "")
      })
    }
  )


}

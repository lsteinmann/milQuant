#' Title
#'
#' @inheritParams db_activity_tab
#'
#' @return a fluidRow with column selector, h2 and the datatable output
#' @export
plotDataTable_ui <- function(id) {

  ns <- NS(id)

  fluidRow(
    box(width = 12,
        h2("Click a bar to display table of resources"),
        htmlOutput(ns("column_selector")),
        DTOutput(ns("clickDataTable"))
    )
  )
}

#' Title
#'
#' @inheritParams db_activity_tab
#' @param resources data to be display in the table
#' @param click_data the plotly click data
#' @param x first variable to select for
#' @param customdata second variable to select for
#'
#' @return server side for the datatable
#'
#' @export
plotDataTable_server <- function(id, resources, click_data, x, customdata) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$column_selector <- renderUI({
        choices <- colnames(resources())
        choices <- choices[-which(choices == "identifier")]

        pickerInput(
          inputId = ns("selected_tbl_columns"),
          label = "Choose columns to display:",
          choices = choices,
          selected = c("shortDescription", "date", "processor"),
          multiple = TRUE,
          options = list("actions-box" = TRUE,
                         "live-search" = TRUE,
                         "live-search-normalize" = TRUE,
                         "live-search-placeholder" = "Search here...")
        )
      })

      output$clickDataTable <- renderDT({

        if (is.null(click_data())) {
          resources() %>%
            select(any_of(c("identifier", "shortDescription", "date", "processor")))
        }

        validate(
          need(is.data.frame(click_data()), "Waiting for click..."),
          need(input$selected_tbl_columns, "Can't get the selected columns from selector!")
        )

        resources() %>%
          filter(get(customdata()) %in% click_data()$customdata) %>%
          filter(get(x()) %in% click_data()$x) %>%
          select(any_of(c("identifier", input$selected_tbl_columns))) %>%
          datatable(escape = FALSE, rownames = FALSE, filter = FALSE,
                    selection = "none",
                    options = list(
                      columnDefs = list(list(className = "dt-left", targets = "_all"),
                                        list(width = "75px", targets = 0)),
                      pageLength = 25)) %>%
          formatStyle("identifier",  fontWeight = "bold")

      })


    }
  )


}

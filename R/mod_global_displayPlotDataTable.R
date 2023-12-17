#' Global Module: clickDataTable (UI)
#'
#' @inherit mod_ui_doc
#'
#' @importFrom DT DTOutput
#'
#' @return A `fluidRow()` with: h2, column selector input, and datatable output.
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

#' Global Module: clickDataTable (Server Code)
#'
#' @inherit mod_serv_doc
#' @param resources Reactive data of all *resources* to be displayed in the table.
#' @param click_data The reactive object returned by plotly click data.
#' @param x First variable to select for.
#' @param customdata Second variable to select for.
#'
#' @importFrom DT renderDT datatable formatStyle
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
          datatable(
            options = list(
              escape = FALSE, rownames = FALSE,
              filter = FALSE, selection = "none",
              columnDefs = list(
                list(className = "dt-left", targets = "_all"),
                list(width = "75px", targets = 0)))) %>%
          formatStyle("identifier",  fontWeight = "bold")
      })


    }
  )
}

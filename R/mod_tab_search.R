#' Tab-Module for Fulltext Search (UI)
#'
#' @inherit mod_ui_doc
#'
#'
#' @importFrom DT DTOutput
#'
#' @export
mod_search_ui <- function(id, tabname = NULL) {

  ns <- NS(id)

  tabItem(
    tabName = tabname,
    title = "Search",
    fluidRow(
      box(
        title = ui_options_title(type = "selection"),
        solidHeader = TRUE, collapsible = TRUE,
        width = 12,
        column(width = 4, uiCategorySelector(ns("categories"))),
        column(
          width = 2,
          actionButton(inputId = ns("loadResources"),
                       label = "Load Resources",
                       style = "margin-top:25px") %>%
            addLRPopover()
        ),
        column(width = 4, htmlOutput(ns("column_selector"))),
        column(width = 2, totalResourcesValueBox(ns("info"), width = 10))
      )
    ),
    fluidRow(
      box(width = 12,
          h2("Searchable table"),
          DTOutput(ns("search_result")) %>% mq_spinner()
      )
    )
  )

}

#' Tab-Module for Fulltext Search (Server Code)
#'
#' @inherit mod_serv_doc
#'
#' @importFrom DT renderDT datatable formatStyle
#'
#' @export
mod_search_serv <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      #generateLayerSelector("layers", resources, inputid = ns("selected_layers"))
      generateCategorySelector("categories",
                               parent = c("Find", "Other", "Feature"),
                               selected = "auto",
                               inputid = ns("selected_categories"))

      observeEvent(input$selected_categories, {
        totalResources_serv("info", sel_categories = isolate(input$selected_categories))
      })


      resources <- eventReactive(input$loadResources, {
        validate(
          need(is.data.frame(react_index()), "No Index available.")
        )

        data("milQuant_inputTypes")

        ind <- milQuant_inputTypes$inputType %in% c("text", "input")


        resources <- get_resources(resource_category = input$selected_categories,
                                   fields = milQuant_inputTypes$field_prj[ind]) %>%
          remove_na_cols() %>%
          mutate_all(~gsub("\n", "<br>", .)) %>%
          mutate_all(~gsub("\t", "&nbsp;&nbsp;&nbsp;&nbsp;", .))


        if ("content" %in% colnames(resources)) {
          resources <- resources %>%
            mutate(content_ascii = stringi::stri_trans_general(content, "Any-Latin")) %>%
            mutate(content_ascii = stringi::stri_trans_general(content_ascii, "Latin-ASCII"))
        }


        return(resources)
      })


      output$column_selector <- renderUI({
        choices <- colnames(resources())
        choices <- choices[-which(choices == "identifier")]

        if (length(input$selected_categories) > 1) {
          selected <- choices[1:4]
        } else if (input$selected_categories == "Inscription") {
          selected <- c("shortDescription", "content", "content_ascii", "translation", "description")
        } else if (input$selected_categories == "Impression") {
          selected <- c("shortDescription", "infoFromField", "findSpectrum", "specialFinds", "generalNotes")
        } else {
          selected <- choices[1:4]
        }

        pickerInput(
          inputId = ns("selected_tbl_columns"),
          label = "Choose columns to search:",
          choices = sort(choices, decreasing = FALSE),
          selected = selected,
          multiple = TRUE,
          options = list("actions-box" = TRUE,
                         "live-search" = TRUE,
                         "live-search-normalize" = TRUE,
                         "live-search-placeholder" = "Search here...")
        )
      })

      ## Workflow Tabbox
      output$search_result <- renderDT({
        resources() %>%
          select(any_of(c("identifier", input$selected_tbl_columns))) %>%
          datatable(
            escape = FALSE,
            selection = "none",
            rownames = FALSE,
            options = list(
              searchHighlight = TRUE,
              columnDefs = list(
                list(className = "dt-left", targets = "_all"),
                list(width = "75px", targets = 0)
              ),
              pageLength = 25
            )
          ) %>%
          formatStyle("identifier",  fontWeight = "bold")
      })
    }
  )
}

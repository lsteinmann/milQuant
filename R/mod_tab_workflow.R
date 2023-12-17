#' Title
#'
#' @inheritParams db_activity_tab
#'
#' @return Tab that display the workflow status selectable by layer and category
#'
#' @export
mod_worflow_ui <- function(id, tabname = NULL) {

  ns <- NS(id)

  tabItem(
    tabName = tabname,
    title = "Workflow",
    fluidRow(
      box(
        width = 12,
        title = tagList(icon("gears"), "Layer and category selection"),
        collapsible = TRUE, solidHeader = TRUE,
        column(
          width = 4,
          uiLayerSelector(ns("layers"))
        ),
        column(
          width = 4,
          uiCategorySelector(ns("categories"))
        ),
        column(
          width = 2,
          actionButton(inputId = ns("loadResources"),
                       label = "Load Resources",
                       style = "margin-top:25px") %>%
            addLRPopover()
        ),
        column(width = 2, totalResourcesValueBox(ns("info"), width = 10))
      )
    ),
    fluidRow(
      uiOutput(ns("workflow_tabs")) %>% mq_spinner()
    )
  )

}

#' Title
#'
#' @inheritParams db_activity_tab
#'
#' @return server code
#' @export
mod_worflow_serv <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      tmp <- reactive({
        react_index() %>%
          filter(Operation %in% db_selected_operations()) %>%
          rename("relation.liesWithinLayer" = liesWithinLayer)
      })

      generateLayerSelector("layers", tmp, inputid = ns("selected_layers"))
      generateCategorySelector("categories",
                               parent = "Find",
                               selected = "all",
                               inputid = ns("selected_categories"))


      observeEvent(input$selected_categories, {
        totalResources_serv("info", sel_categories = isolate(input$selected_categories))
      })

      wf_disp_cols <- reactive({
        wf_disp_cols <- c("identifier", "category", "shortDescription", "processor",
                          "date", "notes", "storagePlace", "relation.liesWithinLayer")
        wf_disp_cols
      })

      workflow_data <- eventReactive(input$loadResources, {
        validate(
          need(is.data.frame(react_index()), "No Index available.")
        )

        db_selected_categories(input$selected_categories)

        base_data <- get_resources(resource_category = input$selected_categories,
                                   liesWithinLayer = input$selected_layers) %>%
          remove_na_cols() %>%
          select(any_of(wf_disp_cols()), contains("workflow")) %>%
          mutate_at(vars(contains("workflow")), ~ ifelse(is.na(.), FALSE, TRUE))

        return(base_data)
      })



      ## Workflow Tabbox
      output$workflow_tabs <- renderUI({

        workflow_cols <- grep("workflow", colnames(workflow_data()))
        workflow_cols <- colnames(workflow_data())[workflow_cols]

        total <- nrow(workflow_data())

        do.call(
          tabBox,
          append(
            list(
              width = 12,
              title = tagList(icon("gear"), "Workflow status")),
            lapply(
              workflow_cols,
              function(wfcol) {
                n <- workflow_data() %>%
                  select(any_of(wfcol)) %>%
                  sum()

                title <- gsub("workflow.", "", wfcol, fixed = TRUE)
                perc <- round(n / total * 100, digits = 1)
                col <- ifelse(grepl("Fehlerhaft", wfcol),
                              "red", "blue")

                tabPanel(
                  title = title,
                  align = "left",
                  fluidRow(
                    infoBox(
                      width = 8, color = col,
                      title = title,
                      value = p("Applies to", strong(n),
                                "out of", strong(total),
                                "objects (",
                                perc, "%).")),
                    valueBox(
                      subtitle = "Progress",
                      value = paste0(perc, "%"),
                      icon = icon("list"), width = 4,
                      color = col)
                  ),
                  fluidRow(
                    column(
                      width = 12,
                      h3("Objects in the plot where this box has been checked: "),
                      renderDT(
                        workflow_data() %>%
                          filter(get(wfcol) == TRUE) %>%
                          select(any_of(wf_disp_cols()))
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 12,
                      h3("... and objects where it has not:"),
                      renderDT(
                        workflow_data() %>%
                          filter(get(wfcol) == FALSE) %>%
                          select(any_of(wf_disp_cols()))
                      )
                    )
                  )
                )
              })
          )
        )
      })
    }
  )
}

#' Title
#'
#' @param id
#' @param tabname
#'
#' @return
#' @export
#'
#' @examples
mod_worflow_ui <- function(id, tabname = NULL) {

  ns <- NS(id)

  tabItem(
    tabName = tabname,
    title = "Workflow",

    fluidRow(
      uiOutput(ns("workflow_tabs")) %>% mq_spinner()
    )
  )

}

#' Title
#'
#' @param id
#' @param resource_category
#'
#' @return
#' @export
#'
#' @examples
mod_worflow_serv <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      wf_disp_cols <- reactive({
        wf_disp_cols <- c("identifier", "shortDescription", "processor",
                          "date", "notes", "storagePlace")
        wf_disp_cols
      })

      workflow_data <- reactive({
        validate(
          need(is.data.frame(react_index()), "No Index available.")
        )

        data("milQuant_cats")
        base_data <- get_resources(resource_category = milQuant_cats$Find) %>%
          remove_na_cols() %>%
          select(any_of(wf_disp_cols()), contains("workflow")) %>%
          mutate_at(vars(contains("workflow")), ~ ifelse(is.na(.), FALSE, TRUE))

        return(base_data)
      })

      ## Workflow Tabbox
      output$workflow_tabs <- renderUI({

        #  workflow <- workflow_data() %>%
        #    select(identifier, contains("workflow"))

        workflow_cols <- grep("workflow", colnames(workflow_data()))
        workflow_cols <- colnames(workflow_data())[workflow_cols]

        total <- nrow(workflow_data())

        do.call(tabBox,
                append(list(width = 12,
                            title = tagList(icon("gear"), "Workflow status")),
                       lapply(workflow_cols,
                              function(wfcol) {
                                n <- workflow_data() %>%
                                  select(any_of(wfcol)) %>%
                                  sum()

                                title <- gsub("workflow.", "", wfcol, fixed = TRUE)
                                perc <- round(n / total * 100, digits = 1)
                                col <- ifelse(grepl("Fehlerhaft", wfcol),
                                              "red", "blue")

                                tabPanel(title = title,
                                         align = "left",
                                         fluidRow(
                                           infoBox(width = 8, color = col,
                                                   title = title,
                                                   value = p("Applies to", strong(n),
                                                             "out of", strong(total),
                                                             "objects (",
                                                             perc, "%).")),
                                           valueBox(subtitle = "Progress",
                                                    value = paste0(perc, "%"),
                                                    icon = icon("list"), width = 4,
                                                    color = col)
                                         ),
                                         fluidRow(
                                           column(width = 12,
                                                  h3("Objects in the plot where this box has been checked: "),
                                                  renderDT(workflow_data() %>%
                                                             filter(get(wfcol) == TRUE) %>%
                                                             select(any_of(wf_disp_cols()))
                                                  )
                                           )
                                         ),
                                         fluidRow(
                                           column(width = 12,
                                                  h3("... and objects where it has not:"),
                                                  renderDT(workflow_data() %>%
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

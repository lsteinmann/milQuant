#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
uiCategorySelector <- function(id) {

  ns <- NS(id)

  htmlOutput(ns("ui_category_selector"))
}

#' Title
#'
#' @param id
#' @param data
#' @param inputid
#'
#' @return
#' @export
#'
#' @examples
generateCategorySelector <- function(id, inputid, parent = "Find") {
  moduleServer(
    id,
    function(input, output, session) {

      label <- "Choose one or many resource categories"

      data("milQuant_cats")

      output$ui_category_selector <- renderUI({

        available_cats <- react_index() %>%
          filter(Operation %in% db_selected_operations()) %>%
          filter(category %in% milQuant_cats[[parent]]) %>%
          mutate(category = factor(category, milQuant_cats[[parent]])) %>%
          pull(category) %>%
          unique() %>%
          sort()

        pickerInput(inputId = inputid,
                    label = label,
                    choices = available_cats,
                    multiple = TRUE,
                    options = list("actions-box" = TRUE,
                                   "live-search" = TRUE,
                                   "live-search-normalize" = TRUE,
                                   "live-search-placeholder" = "Search here..."))
      })
    }
  )
}

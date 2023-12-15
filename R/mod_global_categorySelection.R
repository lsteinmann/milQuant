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
generateCategorySelector <- function(id, inputid, parent = "Find", selected = "auto") {
  moduleServer(
    id,
    function(input, output, session) {

      label <- "Choose one or many resource categories"


      output$ui_category_selector <- renderUI({
        data("milQuant_cats")

        sel_cats <- names(milQuant_cats) %in% parent
        sel_cats <- unlist(milQuant_cats[sel_cats], use.names = FALSE)

        available_cats <- react_index() %>%
          filter(Operation %in% db_selected_operations()) %>%
          filter(category %in% sel_cats) %>%
          mutate(category = factor(category, sel_cats)) %>%
          pull(category) %>%
          unique() %>%
          sort()

        if (selected == "auto") {
          selected <- db_selected_categories()
        } else if (selected == "all") {
          selected <- available_cats
        } else if (selected == "none") {
          selected <- NA
        } else {
          match.arg(selected, choices = sel_cats, several.ok = TRUE)
        }

        available_cats_list <- lapply(milQuant_cats, function (x) {
          x[x %in% available_cats]
        })

        pick_list <- sapply(available_cats_list, length) > 0
        available_cats_list <- available_cats_list[pick_list]

        available_cats_list <- lapply(available_cats_list, function(x) {
          x <- as.list(x)
          names(x) <- unlist(x)
          x
        })

        pickerInput(inputId = inputid,
                    label = label,
                    choices = available_cats_list,
                    multiple = TRUE,
                    selected = selected,
                    options = list("actions-box" = TRUE,
                                   "live-search" = TRUE,
                                   "live-search-normalize" = TRUE,
                                   "live-search-placeholder" = "Search here..."))
      })

    }
  )
}

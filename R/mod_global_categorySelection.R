#' Global Module: categorySelector (UI)
#'
#' @inherit mod_ui_doc
#'
#' @return `htmlOutput()`-Element of the category selector.
#' @export
uiCategorySelector <- function(id) {

  ns <- NS(id)

  htmlOutput(ns("ui_category_selector"))
}

#' Global Module: categorySelector (Server Code)
#'
#' @inherit mod_serv_doc
#' @param parent *parent*-category of the resources that should be selectable in the input.
#' @param selected "auto", "all", "none" or a vector of categories the should be preselected
#'
#' @export
generateCategorySelector <- function(id, inputid, parent = "Find", selected = "auto") {
  moduleServer(
    id,
    function(input, output, session) {

      label <- "Choose one or many resource categories"


      output$ui_category_selector <- renderUI({
        milQuant_cats <- milQuant_cats

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

#' Title
#'
#' @param id
#' @param tabname
#'
#' @return
#' @export
#'
#' @examples
mod_pottery_QA_ui <- function(id, tabname) {

  ns <- NS(id)

  tabItem(
    tabName = tabname,
    h1("Pottery Quantification A"),
    fluidRow(
      box(
        width = 3, height = 650,
        textInput(inputId = ns("title"), label = "Title",
                  value = "", placeholder = "Enter title here"),
        textInput(inputId = ns("subtitle"), label = "Subtitle",
                  placeholder = "Enter subtitle here"),
        uiLayerSelector(ns("layers")),
        prettyRadioButtons(inputId = ns("display_context"),
                           label = "Display Options", icon = icon("check"),
                           inline = FALSE, animation = "jelly",
                           choices = list("Do not display Context" = "none",
                                          "Context as color" = "fill",
                                          "Context on x-Axis" = "x")),
        prettyRadioButtons(inputId = ns("display_bars"),
                           label = "Position of Bars", icon = icon("check"),
                           inline = TRUE, animation = "jelly",
                           choices = list("Dodged Bars" = "dodge2",
                                          "Stacked Bars" = "stack")),
        downloadPlotButtons(ns("download"))
      ),
      box(
        width = 9, height = 650,
        plotlyOutput(ns("display_plot"), height = 620) %>% mq_spinner()
      )
    )
  )



}

#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
mod_pottery_QA_serv <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      potteryQA <- reactive({
        validate(
          need(is.data.frame(react_index()), "No Index available.")
        )

        potteryQA <- get_resources(resource_category = "Pottery_Quantification_A") %>%
          remove_na_cols()
        return(potteryQA)
      })

      generateLayerSelector("layers", potteryQA, inputid = ns("selected_layers"))


      plot_data <- reactive({
        validate(
          need(is.data.frame(potteryQA()), "Data not available.")
        )
        existing_cols <- colnames(potteryQA())
        keep <- existing_cols
        keep <- keep[grepl("count", keep)]
        #keep
        # remove "countTotal" as well
        keep <- keep[!grepl("countTotal", keep)]
        # needed for melt id
        keep <- c(keep, "relation.liesWithinLayer")

        plot_data <- potteryQA() %>%
          filter(relation.liesWithinLayer %in% input$selected_layers) %>%
          select(all_of(keep)) %>%
          melt(id = "relation.liesWithinLayer") %>%
          mutate(value = ifelse(is.na(value), 0, value)) %>%
          mutate(value = as.numeric(value)) %>%
          mutate(variable = gsub("count", "", variable)) %>%
          # so every object is a row, technically (makes ggplot easier)
          uncount(value) %>%
          # sorting factors by frequency here to make plotly tooltip nicer
          mutate(variable = fct_infreq(variable),
                 relation.liesWithinLayer = fct_infreq(relation.liesWithinLayer))
        return(plot_data)
      })

      #plot_data <- function() { return(plot_data)}

      make_plot <- reactive({


        if (input$display_context == "fill") {
          p <- ggplot(plot_data(), aes(x = variable,
                                       fill = relation.liesWithinLayer))
          legend_title <- "Context"
          x_axis_title <- "functional category"
        } else if (input$display_context == "x") {
          p <- ggplot(plot_data(), aes(x = relation.liesWithinLayer,
                                                fill = variable))
          legend_title <- "functional category"
          x_axis_title <- "Context"

        } else if (input$display_context == "none") {

          p <- ggplot(plot_data(), aes(x = variable))
          legend_title <- "none"
          x_axis_title <- "Vessel Forms"
        }

        if (input$title == "") {
          plot_title <- paste("Vessel Forms from Context: ",
                              paste(input$selected_layers, collapse = ", "),
                              sep = "")
        } else {
          plot_title <- input$title
        }

        p <- p +
          geom_bar(position = input$display_bars) +
          scale_fill_discrete(name = legend_title, guide = "legend") +
          labs(x = x_axis_title, y = "count",
               title = plot_title,
               subtitle = input$subtitle,
               caption = paste("Total Number of Fragments:", nrow(plot_data())))
        p
      })

      output$display_plot <- renderPlotly({
        convert_to_Plotly(make_plot())
      })

      makeDownloadPlotHandler("download", dlPlot = display_plot)

    }
  )

}

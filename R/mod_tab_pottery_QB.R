#' Title
#'
#' @param id
#' @param tabname
#'
#' @return
#' @export
#'
#' @examples
mod_pottery_QB_ui <- function(id, tabname) {

  ns <- NS(id)

  tabItem(
    tabName = tabname,
    h1("Pottery Quantification B"),
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
                                          "Context as Subplot" = "wrap")),
        prettyRadioButtons(inputId = ns("display_xaxis"),
                           label = "Display Options", icon = icon("check"),
                           inline = FALSE, animation = "jelly",
                           choices = list("Function on x-axis" = "function",
                                          "Period on x-axis" = "period")),
        prettyRadioButtons(inputId = ns("display_bars"),
                           label = "Position of Bars", icon = icon("check"),
                           inline = TRUE, animation = "jelly",
                           choices = list("Dodged Bars" = "dodge",
                                          "Stacked Bars" = "stack")),
        uiPeriodSelector(ns("periods")),
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
mod_pottery_QB_serv <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)


      potteryQB <- reactive({
        validate(
          need(is.data.frame(react_index()), "No Index available.")
        )

        potteryQB <-  get_resources(resource_category = "Pottery_Quantification_B") %>%
          remove_na_cols()
        return(potteryQB)
      })

      generateLayerSelector("layers", potteryQB, inputid = ns("selected_layers"))

      generatePeriodSelector("periods", inputid = ns("selected_periods"))

      plot_data <- reactive({
        validate(
          need(is.data.frame(potteryQB()), "Data not available.")
        )
        existing_cols <- colnames(potteryQB())
        keep <- existing_cols
        keep <- keep[grepl("count", keep)]
        #keep
        # remove "countTotal" as well
        keep <- keep[!grepl("countTotal", keep)]
        # needed for melt id
        keep <- c("identifier", keep, "relation.liesWithinLayer", "period", "period.start", "period.end")

        plot_data <- potteryQB() %>%
          select(all_of(keep))


        plot_data <- plot_data %>%
          filter(relation.liesWithinLayer %in% input$selected_layers) %>%
          period_filter(is_milet = is_milet,
                        selector = input$selected_periods) %>%
          melt(id = c("identifier", "relation.liesWithinLayer", "period", "period.start", "period.end")) %>%
          mutate(value = ifelse(is.na(value), 0, value)) %>%
          mutate(value = as.numeric(value)) %>%
          mutate(variable = gsub("count", "", variable)) %>%
          mutate(variable = gsub("Rim|Base|Handle|Wall", "", variable)) %>%
          uncount(value) %>%
          mutate(variable = fct_infreq(variable))
        return(plot_data)
      })

      make_plot <- reactive({
        validate(
          need(is.data.frame(plot_data()), "Data not available.")
        )

        if (input$title == "") {
          plot_title <- paste("Vessel Forms from Context: ",
                              paste(input$selected_layers, collapse = ", "),
                              sep = "")
        } else {
          plot_title <- input$title
        }


        if (input$display_xaxis == "function") {
          p <- ggplot(plot_data(), aes(x = variable,
                                                  fill = period)) +
            labs(x = "Vessel Forms", y = "count") +
            scale_fill_period(ncol = 9)
        } else if (input$display_xaxis == "period") {
          p <- ggplot(plot_data(), aes(x = period,
                                                  fill = variable)) +
            scale_fill_discrete(name = "Function", guide = "legend") +
            labs(x = "Period", y = "count")
        }


        if (input$display_context == "wrap") {
          p <- p + facet_wrap(~ relation.liesWithinLayer, ncol = 2)
        }

        p <- p +
          labs(title = plot_title,
               subtitle = input$subtitle,
               caption = paste("Total Number of Fragments:", nrow(plot_data()))) +
          geom_bar(position = input$display_bars)
        p
      })

      output$display_plot <- renderPlotly({
        convert_to_Plotly(make_plot())
      })

      makeDownloadPlotHandler("download", dlPlot = display_plot)




    }
  )

}

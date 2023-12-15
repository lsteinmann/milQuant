#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
mod_bricksQ_ui <- function(id, tabname) {

  ns <- NS(id)

  tabItem(
    tabName = tabname,

    #tabInfoRow_ui(ns("info")),

    fluidRow(
      box(
        #h3("Quantification of Bricks, Tiles and Pipes"),
        title = ui_options_title(),
        width = 3, height = 700,
        uiLayerSelector(ns("layers")),
        hr(),
        textInput(inputId = ns("title"), label = "Title",
                  value = "Quantification of Bricks, Tiles and Pipes"),
        textInput(inputId = ns("subtitle"), label = "Subtitle",
                  placeholder = "Enter subtitle here"),
        prettyRadioButtons(inputId = ns("plot_by"),
                           label = "Plot the ...",
                           choices = list("number of fragments" = "count",
                                          "weight" = "weight"),
                           selected = "count",
                           icon = icon("check"),
                           inline = TRUE, animation = "jelly"),
        prettyRadioButtons(inputId = ns("keep_context"),
                           label = "Separate by context?",
                           choices = list("yes" = TRUE,
                                          "no" = FALSE),
                           selected = FALSE,
                           icon = icon("check"),
                           inline = TRUE, animation = "jelly"),
        prettyRadioButtons(inputId = ns("bar_display"),
                           label = "Display the bars...",
                           choices = list("stacked" = "stack",
                                          "dodging" = "group"),
                           icon = icon("check"),
                           inline = TRUE, animation = "jelly"),
        downloadPlotButtons(ns("download"))
      ),
      box(
        width = 9, height = 700,
        plotlyOutput(ns("display_plot"), height = 670) %>% mq_spinner()
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
mod_bricksQ_serv <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)


      bricksQ <- reactive({
        validate(
          need(is.data.frame(react_index()), "No Index available.")
        )

        bricksQ <- get_resources(resource_category = "Brick_Quantification") %>%
          remove_na_cols() %>%
          inner_join(react_index()[,c("identifier", "Operation", "Place")],
                     by = "identifier")
        return(bricksQ)
      })

      tabInfoRow_server("info", tab_data = bricksQ)

      generateLayerSelector("layers", bricksQ, inputid = ns("selected_layers"))

      make_plot <- reactive({

        validate(
          need(is.data.frame(bricksQ()), "Waiting for data...")
        )

        existing_cols <- colnames(bricksQ())
        keep <- existing_cols
        keep <- keep[grepl(input$plot_by, keep)]
        # remove "countTotal" as well
        keep <- keep[!grepl(paste0(input$plot_by, "Total"), keep)]
        # needed for melt id
        keep <- c(keep, "relation.liesWithinLayer")

        custom_hovertemplate <- milQuant_hovertemplate(value = input$plot_by)


        plot_data <- bricksQ() %>%
          filter(relation.liesWithinLayer %in% input$selected_layers) %>%
          select(all_of(keep)) %>%
          droplevels() %>%
          rename(color = relation.liesWithinLayer) %>%
          melt(id = "color") %>%
          mutate(value = ifelse(is.na(value), 0, value)) %>%
          mutate(value = as.numeric(value)) %>%
          mutate(variable = gsub(input$plot_by, "", variable))

        if (input$plot_by == "weight") {
          plot_data$value <- plot_data$value / 1000
        }

        if (input$keep_context) {
          fig <- plot_ly(plot_data, type = "bar",
                         x = ~variable, y = ~value, color = ~color,
                         colors = viridis(length(unique(plot_data$color))),
                         hovertemplate = custom_hovertemplate)
        } else {
          fig <- plot_data %>%
            select(variable, value) %>%
            group_by(variable) %>%
            summarise(value = sum(value)) %>%
            plot_ly(type = "bar", name = "Quantification",
                    x = ~variable, y = ~value,
                    hovertemplate = custom_hovertemplate)
        }



        plot_title <- paste0('<b>', input$title, '</b><br>', input$subtitle)

        caption <- paste0("Total ", input$plot_by, ": ", sum(plot_data$value))

        x_title <- "Type of Brick / Tile / Pipe"
        y_title <- ifelse(input$plot_by == "count",
                          "number of fragments",
                          "weight in kg")

        fig <- fig %>% layout(barmode = input$bar_display,# bargap = 0.1,
                              title = list(text = plot_title),
                              xaxis = list(title = x_title,
                                           categoryorder = "total descending"),
                              yaxis = list(title = y_title))

        fig <- milquant_plotly_layout(fig, caption = caption)

        return(fig)
      })

      output$display_plot <- renderPlotly({
        make_plot()
      })

      makeDownloadPlotHandler("download", dlPlot = make_plot)

    }
  )

}

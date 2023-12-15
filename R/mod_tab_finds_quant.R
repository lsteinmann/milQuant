#' Title
#'
#' @param id
#' @param tabname
#'
#' @return
#' @export
#'
#' @examples
mod_finds_quant_ui <- function(id, tabname) {

  ns <- NS(id)

  tabItem(
    tabName = tabname,

    fluidRow(
      box(
        title = ui_options_title(type = "plot"),
        width = 3, height = 700,
        uiLayerSelector(ns("layers")),
        hr(),
        textInput(inputId = ns("title"), label = "Title",
                  placeholder = "Enter title here"),
        textInput(inputId = ns("subtitle"), label = "Subtitle",
                  placeholder = "Enter subtitle here"),
        prettyRadioButtons(inputId = ns("plot_by"),
                           label = "Plot the ...",
                           choices = list("number of fragments" = "countTotal",
                                          "weight" = "weightTotal"),
                           selected = "countTotal",
                           icon = icon("check"),
                           inline = TRUE, animation = "jelly"),
        htmlOutput(ns("var_selector")),
        prettyRadioButtons(inputId = ns("var_display"),
                           label = "Display the selected variable...",
                           choices = list("as color" = "var_is_fill",
                                          "on x-axis" = "var_is_x"),
                           selected = "var_is_fill",
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
mod_finds_quant_serv <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      quants <- reactive({
        validate(
          need(is.data.frame(react_index()), "No Index available.")
        )

        quant_cats <- c("Quantification",
                        "Pottery_Quantification_A",
                        "QuantMollusks",
                        "Brick_Quantification",
                        "PlasterQuantification")

        keep <- c("identifier", "relation.liesWithinLayer",
                  "quantificationType",
                  "countTotal", "weightTotal")

        data("milQuant_cats")

        quants <- get_resources(resource_category = milQuant_cats$Quantification) %>%
          select(all_of(keep)) %>%
          remove_na_cols() %>%
          droplevels() %>%
          inner_join(react_index()[,c("identifier", "Operation", "Place")],
                     by = "identifier")
        return(quants)
      })

      generateLayerSelector("layers", quants, inputid = ns("selected_layers"))

      output$var_selector <- renderUI({
        vars <- colnames(quants())
        no_plot <- c("identifier", "countTotal", "weightTotal")
        vars <- vars[-which(vars %in% no_plot)]

        selectInput(inputId = ns("secondary_var"),
                    label = "Choose a variable:",
                    choices = vars)

      })

      x_var <- reactiveVal(value = NULL)
      color_var <- reactiveVal(value = NULL)

      make_plot <- reactive({

        validate(
          need(is.data.frame(quants()), "Waiting for data..."),
          need(input$secondary_var, "Waiting for variable selection.")
        )

        if (input$var_display == "var_is_fill") {
          x_var("quantificationType")
          color_var(input$secondary_var)
        } else if(input$var_display == "var_is_x") {
          x_var(input$secondary_var)
          color_var("quantificationType")
        }

        plot_data <- quants() %>%
          filter(relation.liesWithinLayer %in% input$selected_layers) %>%
          mutate(x = get(x_var()),
                 color = get(color_var()),
                 value = get(input$plot_by)) %>%
          select(x, color, value) %>%
          droplevels() %>%
          mutate(value = ifelse(is.na(value), 0, value)) %>%
          mutate(value = as.numeric(value)) %>%
          group_by(x, color) %>%
          summarise(value = sum(value)) %>%
          arrange(value)

        if (input$plot_by == "weightTotal") {
          plot_data$value <- plot_data$value / 1000
        }

        fig <- plot_ly(plot_data, type = "bar",
                       x = ~x, y = ~value, color = ~color,
                       colors = viridis(length(unique(plot_data$color))),
                       hovertemplate = paste0("<b>%{fullData.name}</b><br>",
                                              "%{x}<br>",
                                              gsub("Total", "", input$plot_by),
                                              ": <b>%{y}</b><br>",
                                              "<extra></extra>"))

        plot_title <- paste0('<b>', input$title, '</b><br>', input$subtitle)

        caption <- paste0("Total ",
                          gsub("Total", "", input$plot_by),
                          ": ", round(sum(plot_data$value), digits = 3))

        x_title <- "Type of Find"
        y_title <- ifelse(input$plot_by == "countTotal",
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

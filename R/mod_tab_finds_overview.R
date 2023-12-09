#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
mod_finds_overview_ui <- function(id, tabname) {

  ns <- NS(id)

  tabItem(
    tabName = tabname,

    fluidRow(
      box(
        width = 3, height = 700,
        textInput(inputId = ns("title"), label = "Title",
                  value = "Distribution of Find-Categories"),
        textInput(inputId = ns("subtitle"), label = "Subtitle",
                  placeholder = "Enter subtitle here"),
        uiLayerSelector(ns("layers")),
        htmlOutput(ns("var_selector")),
        prettyRadioButtons(inputId = ns("var_display"),
                           label = "Display the selected variable...",
                           choices = list("as X-Axis" = "var_is_x",
                                          "as Colour" = "var_is_fill"),
                           selected = "var_is_x",
                           icon = icon("check"),
                           inline = TRUE, animation = "jelly"),
        prettyRadioButtons(inputId = ns("bar_display"),
                           label = "Display the bars...",
                           choices = list("stacked" = "stack",
                                          "dodging" = "group"),
                           icon = icon("check"),
                           inline = TRUE, animation = "jelly"),
        uiPeriodSelector(ns("periods")),
        downloadPlotButtons(ns("download"))
      ),
      box(
        width = 9, height = 700,
        plotlyOutput(ns("display_plot"), height = 670) %>% mq_spinner()
        )
    ),
    plotDataTable_ui(ns("finds_clickdata"))
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
mod_finds_overview_serv <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      finds <- reactive({

        validate(
          need(is.data.frame(react_index()), "No Index available.")
        )

        data("milQuant_cats")

        finds <- get_resources(resource_category = milQuant_cats$Find) %>%
          remove_na_cols() %>%
          mutate_if(is.logical, list(~ifelse(is.na(.), FALSE, .))) %>%
          mutate_if(is.factor, list(~fct_na_value_to_level(., "N/A"))) %>%
          inner_join(react_index()[,c("identifier", "Operation", "Place")],
                     by = "identifier")
        return(finds)
      })

      selected_finds <- reactive({
        finds() %>%
          filter(relation.liesWithinLayer %in% input$selected_layers) %>%
          period_filter(is_milet = is_milet, selector = input$selected_periods)
      })

      tabInfoRow_server("info", tab_data = finds)

      generateLayerSelector("layers", finds, inputid = ns("selected_layers"))

      output$var_selector <- renderUI({
        all_cols <- colnames(finds())
        top_vars <- c("storagePlace", "date", "Operation", "Place")
        add_vars <- all_cols[grepl("relation|workflow|period|campaign", all_cols)]
        add_vars <- sort(add_vars)
        vars <- c(top_vars, add_vars)

        to_remove <- c("isDepictedIn", "isInstanceOf", "isSameAs",
                       vars[!vars %in% all_cols])


        vars <- vars[!grepl(paste(to_remove,collapse = "|"),
                            vars)]

        selectInput(inputId = ns("secondary_var"),
                    label = "Choose a variable:",
                    choices = vars)

      })

      # might be weird to get this inputid
      generatePeriodSelector("periods", inputid = ns("selected_periods"))

      x_var <- reactiveVal(value = NULL)
      color_var <- reactiveVal(value = NULL)

      plot_data <- reactive({

        validate(
          need(is.data.frame(selected_finds()), "Waiting for data..."),
          need(input$secondary_var, "Waiting for variable selection.")
        )

        if (input$var_display == "var_is_fill") {
          x_var("category")
          color_var(input$secondary_var)
        } else if(input$var_display == "var_is_x") {
          x_var(input$secondary_var)
          color_var("category")
        }

        plot_data <- selected_finds() %>%
          mutate(x = get(x_var()), color = get(color_var())) %>%
          select(x, color) %>%
          droplevels() %>%
          count(x, color) %>%
          group_by(x) %>%
          arrange(n)

        return(plot_data)
      })

      make_plot <- reactive({

        validate(
          need(is.data.frame(plot_data()), "Waiting for data...")
        )

        fig <- plot_ly(plot_data(), x = ~x, y = ~n,
                       color = ~color,
                       customdata = ~color,
                       type = "bar",
                       source = "allfinds_plot",
                       colors = viridis(length(unique(plot_data()$color))),
                       hovertemplate = milQuant_count_hovertemplate())

        legend_title <- ifelse(input$var_display == "var_is_fill",
                               input$secondary_var,
                               "Resource-Category")

        x_title <- ifelse(input$var_display == "var_is_x",
                          input$secondary_var,
                          "Resource-Category")

        plot_title <- paste0('<b>', input$title, '</b><br>', input$subtitle)

        caption <- paste0("Total: ", sum(plot_data()$n))

        fig <- fig %>% layout(barmode = input$bar_display,
                              title = list(text = plot_title),
                              xaxis = list(title = x_title,
                                           categoryorder = "total descending"),
                              yaxis = list(title = "count"),
                              legend = list(title = list(text = legend_title)))

        fig <- milquant_plotly_layout(fig, caption = caption)

        return(fig)
      })

      output$display_plot <- renderPlotly({
        make_plot()
      })

      click_data <- reactive({
        event_data("plotly_click", source = "allfinds_plot")
      })

      plotDataTable_server("finds_clickdata",
                           resources = selected_finds,
                           click_data = click_data,
                           x = x_var,
                           customdata = color_var)

      makeDownloadPlotHandler("download", dlPlot = make_plot)


    }
  )

}

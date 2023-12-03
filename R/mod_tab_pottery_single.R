#' Title
#'
#' @param id
#' @param tabname
#'
#' @return
#' @export
#'
#' @examples
mod_pottery_single_ui <- function(id, tabname) {

  ns <- NS(id)

  tabItem(
    tabName = tabname,
    fluidRow(
      box(
        width = 3, height = 700,
        textInput(inputId = ns("title"), label = "Title",
                  placeholder = "Enter title here"),
        textInput(inputId = ns("subtitle"), label = "Subtitle",
                  placeholder = "Enter subtitle here"),
        uiLayerSelector(ns("layers")),
        htmlOutput(ns("x_selector")),
        htmlOutput(ns("fill_selector")),
        prettyRadioButtons(inputId = ns("bar_display"),
                           label = "Choose how to display the bars",
                           icon = icon("check"),
                           inline = TRUE, animation = "jelly",
                           choices = list("stacked" = "stack",
                                          "dodging" = "group")),
        uiPeriodSelector(ns("periods")),
        downloadPlotButtons(ns("download"))
      ),
      box(
        width = 9, height = 700,
        plotlyOutput(ns("display_plot"), height = 620) %>% mq_spinner()
      )
    ),
    plotDataTable_ui(ns("resources_clickdata"))
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
mod_pottery_single_serv <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      generateCategorySelector("categories",
                               parent = "Find",
                               inputid = ns("selected_categories"))

      resources <- reactive({
        validate(
          need(is.data.frame(react_index()), "No Trenches and/or Places selected.")
        )

        resources <- get_resources(resource_category = "Pottery") %>%
          remove_na_cols() %>%
          mutate_if(is.logical, list(~ifelse(is.na(.), FALSE, .))) %>%
          mutate_if(is.factor, list(~fct_na_value_to_level(., "N/A"))) %>%
          inner_join(react_index()[,c("identifier", "Operation", "Place")],
                     by = "identifier")

        return(resources)
      })

      selected_resources <- reactive({
        validate(
          need(is.data.frame(resources()), "No Resources loaded.")
        )

        resources() %>%
          filter(relation.liesWithinLayer %in% input$selected_layers) %>%
          period_filter(is_milet = is_milet,
                        selector = input$selected_periods)
      })



      var_choices <- reactive({
        validate(
          need(is.data.frame(resources()), "No Resources loaded.")
        )
        var_choices <- colnames(resources())
        var_choices <- var_choices[!var_choices %in% drop_for_plot_vars]
        var_choices <- var_choices[!grepl("dimension", var_choices)]
        var_choices <- var_choices[!grepl("coin", var_choices)]
        var_choices <- var_choices[!grepl("amount.*", var_choices)]
      })

      output$x_selector <- renderUI({
        validate(
          need(var_choices(), "No Variables selected.")
        )
        selectInput(inputId = ns("x_var"), label = "Choose a variable for the x-axis:",
                    choices = var_choices(), selected = "potteryGroup")
      })

      output$fill_selector <- renderUI({
        validate(
          need(var_choices(), "No Variables selected.")
        )
        selectInput(inputId = ns("fill_var"), label = "Choose a variable for the color:",
                    choices = var_choices(), selected = "functionalCategory")
      })

      generateLayerSelector("layers", resources, inputid = ns("selected_layers"))
      generatePeriodSelector("periods", inputid = ns("selected_periods"))

      plot_data <- reactive({
        validate(
          need(is.data.frame(selected_resources()), "Waiting for data..."),
          need(is.character(input$x_var), "No variables selected.")
        )

        plot_data <- selected_resources() %>%
          mutate(x = get(input$x_var),
                 color = get(input$fill_var)) %>%
          select(x, color) %>%
          droplevels() %>%
          count(x, color) %>%
          group_by(x) %>%
          arrange(n)


        return(plot_data)
      })

      make_plot <- reactive({

        validate(
          need(is.data.frame(plot_data()), "I am not getting the data!")
        )

        fig <- plot_ly(plot_data(), x = ~x, y = ~n,
                       color = ~color, customdata = ~color,
                       type = "bar", source = ns("plot"),
                       colors = viridis(length(unique(plot_data()$color))),
                       hovertemplate = paste0("<b>%{fullData.name}</b><br>",
                                              "%{x}<br>",
                                              "count: <b>%{y}</b><br>",
                                              "<extra></extra>"))

        fig <- fig %>% event_register('plotly_click')

        legend_title <- input$fill_var

        x_title <- input$x_var

        plot_title <- paste0('<b>', input$title, '</b><br>', input$subtitle)

        caption <- paste0("Total: ", sum(plot_data()$n))

        fig <- fig %>% layout(barmode = input$bar_display,
                              title = list(text = plot_title),
                              xaxis = list(title = x_title,
                                           categoryorder = "total descending"),
                              yaxis = list(title = "count"),
                              legend = list(title = list(text = legend_title)))

        milquant_plotly_layout(fig, caption = caption)
      })

      output$display_plot <- renderPlotly({
        make_plot()
      })

      click_data <- reactive({
        event_data("plotly_click", source = ns("plot"))
      })

      plotDataTable_server("resources_clickdata",
                           resources = selected_resources,
                           click_data = click_data,
                           x = reactive({input$x_var}),
                           customdata = reactive({input$fill_var}))


      makeDownloadPlotHandler("download", dlPlot = make_plot)



    }
  )

}

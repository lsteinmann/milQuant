#' Title
#'
#' @param id
#' @param tabname
#'
#' @return
#' @export
#'
#' @examples
mod_aoristic_finds_ui <- function(id, tabname) {

  ns <- NS(id)

  tabItem(
    tabName = tabname,
    fluidRow(
      box(solidHeader = TRUE, collapsible = TRUE,
          width = 12,
          title = "Selection Options for Bar Charts",
          column(width = 4, uiCategorySelector(ns("categories"))),
          column(width = 2, actionButton(inputId = ns("loadResources"),
                                         label = "Load Resources",
                                         style = "margin-top:25px")),
          column(width = 4, p('The "Load Resources" button will query the
          database for all resources of the selected categories from the
          selected Operations. Depending on the number of resources,
          this may take a while.')),
          column(width = 2, tabValueBox_ui(ns("info"), width = 10))
      )
    ),
    fluidRow(
      box(
        width = 3, height = 650,
        textInput(inputId = ns("title"), label = "Title",
                  placeholder = "Enter title here"),
        textInput(inputId = ns("subtitle"), label = "Subtitle",
                  placeholder = "Enter subtitle here"),
        uiLayerSelector(ns("layers")),
        switchInput(inputId = ns("derive_dating"),
                    label = "Derive Dating from Periods",
                    labelWidth = "150px",
                    size = "mini",
                    onStatus = "success",
                    offStatus = "danger")
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
mod_aoristic_finds_serv <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      generateCategorySelector("categories",
                               parent = "Find",
                               inputid = ns("selected_categories"))

      resources <- eventReactive(input$loadResources, {
        validate(
          need(is.data.frame(react_index()), "No Trenches and/or Places selected."),
          need(input$selected_categories, "No Categories selected.")
        )
        db_selected_categories(input$selected_categories)

        resources <- get_resources(resource_category = input$selected_categories,
                                   fields = c("period", "dating")) %>%
          remove_na_cols() %>%
          mutate_if(is.logical, list(~ifelse(is.na(.), FALSE, .))) %>%
          mutate_if(is.factor, list(~fct_na_value_to_level(., "N/A"))) %>%
          inner_join(react_index()[,c("identifier", "Operation", "Place")],
                     by = "identifier")

        return(resources)
      })

      tabInfoRow_server("info", tab_data = resources)
      generateLayerSelector("layers", resources, inputid = ns("selected_layers"))

      plot_data <- reactive({
        validate(
          need(is.data.frame(resources()), "Waiting for data...")
        )

        if (input$derive_dating) {
          plot_data <- resources() %>%
            derive_dating_from_periods()
        } else {
          plot_data <- resources()
        }

        plot_data <- plot_data %>%
          filter(relation.liesWithinLayer %in% input$selected_layers) %>%
          select(identifier, Operation, dating.min, dating.max) %>%
          datsteps(stepsize = 1, calc = "prob", cumulative = TRUE) %>%
          scaleweight(var = "all", val = "probability")


        return(plot_data)
      })

      make_plot <- reactive({

        validate(
          need(is.data.frame(plot_data()), "I am not getting the data!")
        )

        dens <- density(plot_data()$DAT_step,
                        weights = plot_data()$probability)

        miny <- 0
        maxy <- max(dens$y)

        fig <- plot_ly() %>%
          add_histogram(data = plot_data(),
                        x = ~DAT_step,
                        color = ~variable,
                        xbins = list(size = 1)) %>%
          add_lines(data = dens,x = dens$x, y = dens$y,
                    yaxis = "y2",
                    name = "Probability Density",
                    line = list(width = 3)) %>%
          layout(yaxis2 = list(overlaying = "y",
                               side = "right",
                               range = c(miny, maxy),
                               showgrid = F,
                               zeroline = F),
                 barmode = "stack")

        plot_title <- paste0('<b>', input$title, '</b><br>', input$subtitle)

        caption <- paste0("Number of objects: ",
                          length(unique(plot_data()$ID)))

        fig <- fig %>% layout(title = list(text = plot_title),
                              xaxis = list(title = "years BCE / CE"),
                              yaxis = list(title = "maximum number of objects per year"))


        milquant_plotly_layout(fig, caption = caption)
      })

      output$display_plot <- renderPlotly({
        make_plot()
      })
    }
  )

}

#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
mod_loomweights_hist_ui <- function(id, tabname) {

  ns <- NS(id)

  tabItem(
    tabName = tabname,

    #tabInfoRow_ui(ns("info")),

    fluidRow(
      box(
        title = ui_options_title(type = "plot"),
        width = 3, height = 850,
        uiLayerSelector(ns("layers")),
        hr(class = "layer-hr"),
        textInput(inputId = ns("title"), label = "Title",
                  value = "Weight-Histogram for Loomweights"),
        textInput(inputId = ns("subtitle"), label = "Subtitle",
                  placeholder = "Enter subtitle here"),
        sliderInput(inputId = ns("n_bins"), label = "Number of bins (0 = auto):",
                    min = 0,
                    max = 100,
                    value = 0),
        #prettyRadioButtons(inputId = ns("lw_histnorm"), label = "Histnorm",
        #                   inline = TRUE, choices = c("", "percent", "probability", "density", "probability density")),
        htmlOutput(ns("color_var_selector")),
        prettyRadioButtons(inputId = ns("condition_filter"),
                           label = "Filter for condition:",
                           icon = icon("check"),
                           inline = TRUE, animation = "jelly",
                           choices = list("complete" = "intakt",
                                          "75% to complete" = "75-100",
                                          "display all objects" = "all"),
                           selected = "75-100"),
        htmlOutput(ns("weight_slider")),
        #uiPeriodSelector(ns("periods")),
        downloadPlotButtons(ns("download"))
      ),
      box(
        width = 9, height = 850,
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
mod_loomweights_hist_serv <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      loomweights <- reactive({

        validate(
          need(is.data.frame(react_index()), "No Index available.")
        )

        loomweights <- get_resources(resource_category = "Loomweight") %>%
          remove_na_cols() %>%
          mutate_if(is.logical, list(~ifelse(is.na(.), FALSE, .))) %>%
          mutate_if(is.factor, list(~fct_na_value_to_level(., "N/A"))) %>%
          inner_join(react_index()[,c("identifier", "Operation", "Place")],
                     by = "identifier")
        return(loomweights)
      })

      generateLayerSelector("layers", loomweights, inputid = ns("selected_layers"))
      #generatePeriodSelector("periods", inputid = ns("selected_periods"))

      color_var_choices <- list("Operation" = "relation.isRecordedIn",
                        "Condition" = "condition",
                        "Percentage preserved" = "conditionAmount",
                        "Height" = "dimensionHeight_cm_1",
                        "Find Localization" = "localization",
                        "Category" = "loomweightCategory",
                        "Decoration" = "loomweightDetail")

      output$color_var_selector <- renderUI({
        selectInput(inputId = ns("color_var"), label = "Choose a variable for the color:",
                    choices = color_var_choices, selected = "loomweightCategory")
      })

      output$weight_slider <- renderUI({
        max_weight <- round(max(na.omit(loomweights()$weightTotal))+5,-1)
        # Slider Input to choose max and min weight
        sliderInput(ns("weight_slider_selection"), label = "Weight Range",
                    min = 0, max = max_weight, value = c(0, max_weight))
      })

      x_var <- reactiveVal(value = NULL)
      color_var <- reactiveVal(value = "loomweightCategory")

      plot_data <- reactive({

        validate(
          need(is.data.frame(loomweights()), "Waiting for data.")
        )

        color_var(input$color_var)

        if (input$condition_filter == "all") {
          condition_filter <- unique(loomweights()$conditionAmount)
        } else if (input$condition_filter == "75-100") {
          condition_filter <- c("intakt", "Fragmentarisch_75-100")
        } else {
          condition_filter <- input$condition_filter
        }

        plot_data <- loomweights() %>%
          # filter by periods from the slider if config is milet
          #period_filter(is_milet = is_milet, selector = input$selected_periods) %>%
          filter(relation.liesWithinLayer %in% input$selected_layers) %>%
          filter(conditionAmount %in% condition_filter) %>%
          mutate(color = get(color_var())) %>%
          select(identifier, color, weightTotal) %>%
          filter(weightTotal >= input$weight_slider_selection[1]) %>%
          filter(weightTotal <= input$weight_slider_selection[2])

        return(plot_data)
      })

      make_plot <- reactive({

        validate(
          need(is.data.frame(plot_data()), "Waiting for plot data...")
        )

        fig <- plot_ly(plot_data(), type = "histogram",
                       x = ~weightTotal, color = ~color,
                       nbinsx = input$n_bins,
                       #histnorm = input$lw_histnorm,
                       source = "loomweights_histogram",
                       colors = viridis(length(unique(plot_data()$color))),
                       hovertemplate = milQuant_hovertemplate())

        legend_title <- names(color_var_choices[which(color_var_choices == color_var())])

        x_title <- "Weight"

        plot_title <- paste0('<b>', input$title, '</b><br>', input$subtitle)

        caption <- paste0("Total: ", nrow(plot_data()))

        fig <- fig %>% layout(barmode = "stack",# bargap = 0.1,
                              title = list(text = plot_title),
                              xaxis = list(title = x_title),
                              yaxis = list(title = "count"),
                              legend = list(title = list(text = legend_title)))

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

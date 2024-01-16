#' Title
#'
#' If I make it only slightly customizable, this could actually be a better
#' generalized version for all quant tabs
#'
#' @inheritParams db_activity_tab
#'
#' @return A tab which displays the Pottery Quantifications
#'
#' @export
mod_pottery_QA_ui <- function(id, tabname) {

  ns <- NS(id)

  tabItem(
    tabName = tabname,

    fluidRow(
      box(
        title = uiOptionsTitle(type = "plot"),
        width = 3, height = 650,
        uiLayerSelector(ns("layers")),
        hr(class = "layer-hr"),
        plotTitleInputs(id = id),
        prettyRadioButtons(inputId = ns("plot_by"),
                           label = "Plot the ...",
                           choices = list("number of fragments" = "count",
                                          "weight" = "weight"),
                           selected = "count",
                           icon = icon("check"),
                           inline = TRUE, animation = "jelly"),
        selectInput(inputId = ns("sec_var"),
                    label = "Choose a variable:",
                    choices = list(Context = "relation.liesWithinLayer",
                                   Place = "Place",
                                   Operation = "Operation"),
                    selected = "Operation"),
        prettyRadioButtons(inputId = ns("display_variable"),
                           label = "Display the selected variable...", icon = icon("check"),
                           inline = TRUE, animation = "jelly",
                           choices = list("omit" = "none",
                                          "as color" = "fill",
                                          "on x-axis" = "x"),
                           selected = "none"),
        prettyRadioButtons(inputId = ns("bar_display"),
                           label = "Display the bars...",
                           icon = icon("check"),
                           inline = TRUE, animation = "jelly",
                           choices = list("stacked" = "stack",
                                          "dodging" = "group"),
                           selected = "stack"),
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
#' @inheritParams db_activity_tab
#'
#' @return server code
#' @export
mod_pottery_QA_serv <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      potteryQA <- reactive({
        validate(
          need(login_connection(), "Not connected."),
          need(db_selected_operations(), "No Operations selected."),
          need(is.data.frame(react_index()) && nrow(react_index()) != 0, "No Index available.")
        )

        potteryQA <- get_resources(resource_category = "Pottery_Quantification_A") %>%
          inner_join(react_index()[,c("identifier", "Operation", "Place")],
                     by = "identifier") %>%
          remove_na_cols()
        return(potteryQA)
      })

      generateLayerSelector("layers", potteryQA, inputid = ns("selected_layers"))


      plot_data <- reactive({
        validate(
          need(is.data.frame(potteryQA()), "Data not available.")
        )

        plot_data <- potteryQA() %>%
          filter(relation.liesWithinLayer %in% input$selected_layers) %>%
          mutate(relation.liesWithinLayer = droplevels(relation.liesWithinLayer))



        # get columnnames associated with the values to plot
        existing_cols <- colnames(plot_data)
        valuecols <- existing_cols[grepl(input$plot_by, existing_cols)]
        valuecols <- valuecols[!grepl(paste0(input$plot_by, "Total"),
                                      valuecols)]
        # and additionally the ones which should be selected for plot_data
        keep <- valuecols
        # artificial secondary variable so that code below works when display is none
        if (input$display_variable != "none") {
          tmp_sec_var <- input$sec_var
        } else {
          tmp_sec_var <- "none"
          plot_data$none <- "Pottery"
        }
        keep <- c(keep, tmp_sec_var)

        plot_data <- plot_data %>%
          select(all_of(keep)) %>%
          mutate_at(valuecols, as.numeric) %>%
          mutate_at(valuecols, ~replace_na(., 0)) %>%
          rename(variable = tmp_sec_var) %>%
          pivot_longer(cols = !variable, names_to = "funCat") %>%
          mutate(funCat = gsub("count|weight", "", funCat)) %>%
          mutate(funCat = gsub("Rim|Base|Handle|Wall", "", funCat)) %>%
          mutate(funCat = as.factor(funCat)) %>%
          group_by(across(c(-value))) %>%
          summarise(value = sum(value))

        if (input$plot_by == "weight") {
          plot_data$value <- plot_data$value / 1000
        }

        return(plot_data)
      })

      #plot_data <- function() { return(plot_data)}

      make_plot <- reactive({

        custom_hovertemplate <- milQuant_hovertemplate(value = input$plot_by)

        if (input$display_variable == "fill") {
          legend_title <- input$sec_var
          x_title <- "functional category"

          fig <- plot_ly(plot_data(),
                         x = ~funCat, color = ~variable,  y = ~value,
                         type = "bar",
                         hovertemplate = custom_hovertemplate)

        } else if (input$display_variable == "x") {
          legend_title <- "functional category"
          x_title <- input$sec_var

          fig <- plot_ly(plot_data(),
                         x = ~variable, color = ~funCat,  y = ~value,
                         type = "bar",
                         hovertemplate = custom_hovertemplate)

        } else if (input$display_variable == "none") {
          legend_title <- "none"
          x_title <- "functional category"

          fig <- plot_ly(plot_data(),
                         x = ~funCat, y = ~value, name = "Pottery",
                         type = "bar",
                         hovertemplate = custom_hovertemplate)
        }

        plot_title <- paste0('<b>', input$title, '</b><br>', input$subtitle)

        caption <- paste0("Total: ", sum(plot_data()$value))

        y_title <- ifelse(input$plot_by == "count",
                          "number of fragments",
                          "weight in kg")

        fig <- fig %>% layout(barmode = input$bar_display,
                              title = list(text = plot_title),
                              yaxis = list(title = y_title),
                              xaxis = list(title = x_title))

        fig <- milquant_plotly_layout(fig, caption = caption)

        return(fig)
      })

      output$display_plot <- renderPlotly({
        make_plot()
      })

      makeDownloadPlotHandler("download", dlPlot = display_plot)

    }
  )

}

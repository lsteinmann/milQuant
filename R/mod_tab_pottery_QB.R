#' Title
#'
#'  # TODO might want to make it possible to do this with absolute
#'  values replaced from the periods
#'
#' @inheritParams db_activity_tab
#'
#' @return Tab with evaluation of the pottery quant B forms by period
#'
#' @export
mod_pottery_QB_ui <- function(id, tabname) {

  ns <- NS(id)

  tabItem(
    tabName = tabname,

    fluidRow(
      box(
        title = uiOptionsTitle(type = "plot"),
        width = 3, height = 750,
        uiLayerSelector(ns("layers")),
        hr(class = "layer-hr"),
        plotTitleInputs(id = id),
        div(
          style = "display:inline-block",
          switchInput(inputId = ns("split_multiple"),
                    label = "Split counts for multiple periods",
                    labelWidth = "170px",
                    size = "mini", inline = TRUE,
                    onStatus = "success",
                    offStatus = "danger"),
          span(icon("info-circle"), id = "split_multiple_i", style = "color: #3b515b"),
          bsPopover(id = "split_multiple_i", trigger = "hover",
                    title = "Split Counts for Multiple Periods",
                    content = paste0("When splitting by periods, each object ",
                    "will be duplicated for each period it would fit into and ",
                    "counts or weights will be devided equally among the ",
                    "resulting periods."),
                    placement = "right",
                    options = list(container = "body"))
        ),
        prettyRadioButtons(inputId = ns("display_context"),
                           label = "Display Layer/Context as subplot (maximum 5)", icon = icon("check"),
                           inline = TRUE, animation = "jelly",
                           choices = list("yes" = TRUE,
                                          "no" = FALSE),
                           selected = FALSE),
        prettyRadioButtons(inputId = ns("display_xaxis"),
                           label = "Display x-axis as...", icon = icon("check"),
                           inline = TRUE, animation = "jelly",
                           choices = list("Period" = "period",
                                          "functional category" = "function"),
                           selected = "period"),
        prettyRadioButtons(inputId = ns("bar_display"),
                           label = "Display the bars...",
                           icon = icon("check"),
                           inline = TRUE, animation = "jelly",
                           choices = list("stacked" = "stack",
                                          "dodging" = "group")),
        uiPeriodSelector(ns("periods")),
        downloadPlotButtons(ns("download"))
      ),
      box(
        width = 9, height = 750,
        plotlyOutput(ns("display_plot"), height = 720) %>% mq_spinner()
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
mod_pottery_QB_serv <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)


      potteryQB <- reactive({
        validate(
          need(login_connection(), "Not connected."),
          need(db_selected_operations(), "No Operations selected."),
          need(is.data.frame(react_index()) && nrow(react_index()) != 0, "No Index available.")
        )

        potteryQB <-  get_resources(resource_category = "Pottery_Quantification_B") %>%
          remove_na_cols()
        return(potteryQB)
      })

      generateLayerSelector("layers", potteryQB, inputid = ns("selected_layers"))

      generatePeriodSelector("periods", inputid = ns("selected_periods"))

      debounced_selected_layers <- reactive({ input$selected_layers }) %>%
        debounce(2000)

      plot_data <- reactive({
        validate(
          need(is.data.frame(potteryQB()), "Data not available.")
        )
        existing_cols <- colnames(potteryQB())
        countcols <- existing_cols
        countcols <- countcols[grepl("count", countcols)]
        #keep
        # remove "countTotal" as well
        countcols <- countcols[!grepl("countTotal", countcols)]
        # needed for melt id
        keep <- c("identifier", countcols, "relation.liesWithinLayer", "period", "period.start", "period.end")

        plot_data <- potteryQB() %>%
          select(all_of(keep)) %>%
          filter(relation.liesWithinLayer %in% debounced_selected_layers()) %>%
          period_filter(is_milet = is_milet,
                        selector = input$selected_periods) %>%
          mutate(relation.liesWithinLayer = droplevels(relation.liesWithinLayer)) %>%
          mutate_at(countcols, as.numeric) %>%
          mutate_at(countcols, ~replace_na(., 0)) %>%
          multiply_resources_by_period_range(split = input$split_multiple)

        remove_cols <- c("period.start", "period.end", "identifier")

        if (input$display_context) {
          melt_by <- c("relation.liesWithinLayer", "period")
        } else {
          melt_by <- "period"
        }

        plot_data <- plot_data %>%
          select(!all_of(remove_cols)) %>%
          add_all_periods_to_all_contexts()

        if (input$display_context == FALSE) {
          plot_data <- plot_data %>%
            select(!relation.liesWithinLayer)
        }

        plot_data <- plot_data %>%
          pivot_longer(cols = !all_of(melt_by), names_to = "funCat") %>%
          mutate(funCat = gsub("count", "", funCat)) %>%
          mutate(funCat = gsub("Rim|Base|Handle|Wall", "", funCat)) %>%
          mutate(funCat = as.factor(funCat)) %>%
          group_by(across(c(-value))) %>%
          summarise(value = sum(value))

        if (input$display_xaxis == "function") {
          plot_data <- plot_data %>%
            rename(x_axis = funCat,
                   color = period)
        } else if (input$display_xaxis == "period") {
          plot_data <- plot_data %>%
            rename(x_axis = period,
                   color = funCat)
        }


        return(plot_data)
      })

      make_plot <- reactive({
        validate(
          need(is.data.frame(plot_data()), "Data not available.")
        )

        plot_title <- paste0('<b>', input$title, '</b><br>', input$subtitle)

        data("milQuant_periods")

        if (input$display_xaxis == "function") {
          legend_title <- "Period"
          x_title <- "functional category"
          custom_colors <- unlist(milQuant_periods$colors)
          custom_hovertemplate <- milQuant_hovertemplate()

        } else if (input$display_xaxis == "period") {
          legend_title <- "functional category"
          x_title <- "Period"
          custom_colors <- viridis(length(unique(plot_data()$color)))
          custom_hovertemplate <- milQuant_hovertemplate()
        }

        if (input$display_context) {
          max_y <- max(plot_data()$value, na.rm = TRUE)
          max_y <- max_y * 1.25
          sp_plot_data <- split(plot_data(), plot_data()$relation.liesWithinLayer)
          if (length(sp_plot_data) > 5) {
            sp_plot_data <- sp_plot_data[1:5]
          }
          figs <- lapply(sp_plot_data, function(pd) {
            subfig <- plot_ly(pd,
                    x = ~x_axis, y = ~value, color = ~color,
                    type = "bar",
                    colors = custom_colors,
                    hovertemplate = custom_hovertemplate,
                    legendgroup = ~color) %>%
              layout(annotations = list(showarrow = F, y = max_y, x = 0,
                                        text = paste0("<b>", unique(pd$relation.liesWithinLayer), "</b>")),
                     yaxis = list(range = c(0, max_y),
                                  title = "count"))
            if (identical(pd, sp_plot_data[[1]])) {
              subfig <- style(subfig, showlegend = TRUE)
            } else {
              subfig <- style(subfig, showlegend = FALSE)
            }
            return(subfig)
          })

          fig <- subplot(figs, nrows = length(figs), shareX = TRUE, shareY = TRUE)
        } else {
          fig <- plot_ly(plot_data(),
                         x = ~x_axis, y = ~value, color = ~color,
                         type = "bar",
                         colors = custom_colors,
                         hovertemplate = custom_hovertemplate)
        }



        if (input$display_xaxis == "function") {
          fig <- fig %>%
            layout(xaxis = list(title = x_title,
                                categoryorder = "total descending"),
                   legend = list(title = list(text = legend_title),
                                 categoryorder = levels(milQuant_periods$order)))
        } else if (input$display_xaxis == "period") {
          fig <- fig %>%
            layout(xaxis = list(title = x_title,
                                categoryorder = levels(milQuant_periods$order)),
                   legend = list(title = list(text = legend_title),
                                 categoryorder = "total descending"))
        }


        caption <- paste0("Total: ", sum(plot_data()$value))

        fig <- fig %>% layout(barmode = input$bar_display,
                              title = list(text = plot_title),
                              yaxis = list(title = "count"))

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

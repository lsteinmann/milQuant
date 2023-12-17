#' Tab-Module for Aoristic Plots (UI)
#'
#' @inherit mod_ui_doc
#'
#' @importFrom shinyWidgets actionBttn switchInput prettyCheckboxGroup
#' @importFrom shinyBS bsPopover
#'
#' @export
mod_aoristic_finds_ui <- function(id, tabname) {

  ns <- NS(id)

  tabItem(
    tabName = tabname,
    fluidRow(
      box(
        title = ui_options_title(type = "selection"),
        solidHeader = TRUE, collapsible = TRUE,
        width = 12,
        column(width = 4, uiCategorySelector(ns("categories"))),
        column(width = 2,
               actionButton(inputId = ns("loadResources"),
                            label = "Load Resources",
                            style = "margin-top:25px") %>%
                 addLRPopover()),
        column(width = 4),
        column(width = 2, totalResourcesValueBox(ns("info"), width = 10))
      )
    ),
    fluidRow(
      box(
        title = ui_options_title(type = "plot"),
        width = 3, height = 700,
        uiLayerSelector(ns("layers")),
        hr(class = "layer-hr"),
        textInput(inputId = ns("title"), label = "Title",
                  placeholder = "Enter title here"),
        textInput(inputId = ns("subtitle"), label = "Subtitle",
                  placeholder = "Enter subtitle here"),
        div(
          style = "display:inline-block",
          switchInput(inputId = ns("derive_dating"),
                      label = "Derive Dating from Periods",
                      labelWidth = "150px",
                      size = "mini", inline = TRUE,
                      onStatus = "success",
                      offStatus = "danger"),
          span(icon("info-circle"), id = "derive_dating_i", style = "color: #3b515b"),
          bsPopover(id = "derive_dating_i", trigger = "hover",
                    title = "Derive Dating from Periods",
                    content = paste0("When activating this option, objects ",
                                     "that are dated to a period without any ",
                                     "absolute numerical dating will receive ",
                                     "the absolute numerical dating ",
                                     "associated with the periods they are ",
                                     "dated to (see Periodization in the ",
                                     "Documentation Manual)."),
                    placement = "right",
                    options = list(container = "body"))
        ),
        selectInput(inputId = ns("fill_var"),
                    label = "Choose a variable for the color: ",
                    choices = list(Category = "category",
                                   Context = "relation.liesWithinLayer",
                                   Trench = "Operation",
                                   Place = "Place"),
                    selected = "category"),
        prettyCheckboxGroup(inputId = ns("plot_color"),
                           label = "Separate graph by variable for...",
                           choices = list("dating probability density" = "dens",
                                          "histogram of maximum number of objects" = "hist"),
                           selected = "hist", status = "danger",
                           fill = TRUE, bigger = TRUE,
                           inline = FALSE, animation = "jelly"),
        p("For information on this method see: "),
        actionBttn(
          inputId = "doi",
          onclick = "window.open('https://doi.org/10.1017/aap.2021.8', '_blank')",
          label = "10.1017/aap.2021.8",
          style = "bordered",
          color = "primary",
          icon = icon("newspaper")
        )
      ),
      box(
        width = 9, height = 700,
        plotlyOutput(ns("display_plot"), height = 620) %>% mq_spinner()
      )
    )
  )

}

#' Tab-Module for Aoristic Plots (Server Code)
#'
#' @inherit mod_serv_doc
#'
#' @importFrom datplot datsteps scaleweight
#' @importFrom stats density
#'
#' @export
mod_aoristic_finds_serv <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      generateCategorySelector("categories",
                               parent = "Find",
                               inputid = ns("selected_categories"))

      observeEvent(input$selected_categories, {
        totalResources_serv("info", sel_categories = isolate(input$selected_categories))
      })

      resources <- eventReactive(input$loadResources, {
        validate(
          need(is.data.frame(react_index()), "No Trenches and/or Places selected."),
          need(input$selected_categories, "No Categories selected.")
        )
        db_selected_categories(input$selected_categories)

        resources <- get_resources(resource_category = input$selected_categories,
                                   fields = c("period", "dating")) %>%
          filter(!period == "unbestimmt") %>%
          filter(!is.na(period) | !is.na(dating.complete)) %>%
          inner_join(react_index()[,c("identifier", "Operation", "Place")],
                     by = "identifier")

        return(resources)
      })

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
          select(identifier, input$fill_var, dating.min, dating.max) %>%
          datsteps(stepsize = 1, calc = "prob", cumulative = TRUE)


        return(plot_data)
      })

      make_plot <- reactive({

        validate(
          need(is.data.frame(plot_data()), "I am not getting the data!")
        )

        # calculate density depending on ...
        if ("dens" %in% input$plot_color) {
          split_data <- scaleweight(plot_data(), var = 2, val = "probability")
          split_data <- split(split_data, f = plot_data()$variable)
          split_data <- split_data[lapply(split_data, nrow) > 1]
          dens <- lapply(split_data, function(x) {
            density(x$DAT_step,
                    weights = x$probability)
          })

          dens <- data.frame(
            x = unlist(lapply(dens, "[[", "x")),
            y = unlist(lapply(dens, "[[", "y")),
            variable = rep(names(dens), each = length(dens[[1]]$x))
          )
        } else {
          dens <- plot_data() %>%
            scaleweight(var = "all", val = "probability")
          dens <- density(dens$DAT_step,
                    weights = dens$probability)
        }

        maxy <- max(dens$y) * 1.25
        miny <- 0

        fig <- plot_ly()

        # make the histogram
        if ("hist" %in% input$plot_color) {
          fig <- fig %>%
            add_histogram(data = plot_data(),
                          x = ~DAT_step,
                          color = ~variable,
                          xbins = list(size = 1),
                          alpha = 0.6)
        } else {
          fig <- fig %>%
            add_histogram(data = plot_data(),
                          x = ~DAT_step,
                          xbins = list(size = 1),
                          alpha = 0.6)
        }


        # make the density plot
        if ("dens" %in% input$plot_color) {
          fig <- fig %>%
            add_lines(data = dens,
                      x = dens$x,
                      y = dens$y,
                      color = dens$variable,
                      yaxis = "y2",
                      name = dens$variable,
                      line = list(width = 3))
        } else {
          fig <- fig %>%
            add_lines(data = dens,
                      x = dens$x,
                      y = dens$y,
                      yaxis = "y2",
                      name = "Probability density",
                      line = list(width = 3))
        }


        fig <- fig %>%
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
                              yaxis = list(title = "maximum number of objects per year"),
                              yaxis2 = list(title = "density"))


        milquant_plotly_layout(fig, caption = caption)
      })

      output$display_plot <- renderPlotly({
        make_plot()
      })
    }
  )

}

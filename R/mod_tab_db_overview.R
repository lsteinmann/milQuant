#' Title
#'
#' @param id
#' @param tabname
#'
#' @return
#' @export
#'
#' @examples
db_overview_tab <- function(id, tabname) {

  ns <- NS(id)

  tabItem(
    tabName = tabname,
    title = "Overview",

    fluidRow(
      infoBox(
        title = "Info",
        value = "This is an overview of all resources in the selected database, excluding images and types.
        All other tabs display resources from the selected Trenches.",
        icon = icon("vial"),
        color = "light-blue",
        width = 10),
      valueBox(
        uiOutput(ns("overview_n")),
        "Total Resources",
        icon = icon("file"),
        color = "light-blue",
        width = 2)),
    fluidRow(
      box(title = "Overview", status = "primary",
          solidHeader = TRUE, collapsible = FALSE,
          width = 12, height = 750,
          fluidRow(width = 12, height = 670,
                   column(width = 12,
                          plotlyOutput(ns("display_plot"), height = 570) %>%
                            mq_spinner())),

          fluidRow(width = 12, height = 100,
                   title = "Plot Controls",
                   column(width = 1),
                   column(width = 3,
                          prettyRadioButtons(inputId = ns("barmode"),
                                             label = "bar display",
                                             icon = icon("check"),
                                             inline = TRUE, animation = "jelly",
                                             choices = list("stacked" = "stack",
                                                            "dodging" = "group"))),
                   column(width = 3,
                          prettyRadioButtons(inputId = ns("display_x"),
                                             label = "x-axis",
                                             icon = icon("check"),
                                             inline = TRUE, animation = "jelly",
                                             choices = list("Place" = "Place",
                                                            "Operation" = "Operation",
                                                            "Category" = "category"),
                                             selected = "Place")),
                   column(width = 3,
                          prettyRadioButtons(inputId = ns("display_color"),
                                             label = "bar color",
                                             icon = icon("check"),
                                             inline = TRUE, animation = "jelly",
                                             choices = list("Place" = "Place",
                                                            "Operation" = "Operation",
                                                            "Category" = "category"),
                                             selected = "category")),
                   column(width = 2))
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
db_overview_server <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)


      output$overview_n <- renderText({
        validate(
          need(react_index(), "No project selected.")
        )
        prettyNum(nrow(react_index()), big.mark = ",")
      })

      output$display_plot <- renderPlotly({
        validate(
          need(react_index(), "No project selected.")
        )

        # tmp_index <- react_index()
        # if (!is.null(input$selected_operations)) {
        #   tmp_index <- react_index() %>%
        #     filter(Place %in% input$selected_operations)
        # } else {
        #   tmp_index <- react_index()
        # }

        do_not_display <- c("Place", "Project",
                            "Type", "TypeCatalog",
                            "Image", "Photo", "Drawing")

        x_var <- input$display_x
        color_var <- input$display_color

        plot_data <- react_index() %>%
          select(category, Place, Operation) %>%
          filter(!category %in% do_not_display) %>%
          mutate(x = get(x_var), color = get(color_var)) %>%
          droplevels() %>%
          select(x, color) %>%
          count(x, color) %>%
          group_by(x) %>%
          arrange(n)

        plot_title <- paste0("resources in project ", input$selected_project)

        x_label <- ifelse(x_var == "category", "Resource-Category", x_var)
        color_label <- ifelse(color_var == "category", "Resource-Category", color_var)

        fig <- plot_ly(plot_data, x = ~x, y = ~n, color = ~color, text = ~color,
                       type = "bar", textposition = "none", source = "overview_plot",
                       colors = viridis(length(unique(plot_data$color))),
                       hovertemplate = paste0("<b>%{fullData.name}</b><br>",
                                              "%{x}<br>",
                                              "count: <b>%{y}</b><br>",
                                              "<extra></extra>"))
        fig <- fig %>% layout(barmode = input$barmode,
                              title = plot_title,
                              xaxis = list(title = x_label, categoryorder = "total descending"),
                              yaxis = list(title = "count"),
                              legend = list(title=list(text = color_label)))

        milquant_plotly_layout(fig, caption = FALSE)
      })



    }
  )

}

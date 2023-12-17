#' Title
#'
#' @inheritParams db_activity_tab
#'
#' @return tabPanel with overview of the project db
#' @export
db_overview_tab <- function(id, tabname) {

  ns <- NS(id)

  tabPanel(
    #tabName = tabname,
    title = tagList(icon("graduation-cap"), "Project Overview"),
    fluidRow(
      box(
        width = 9, collapsible = TRUE,
        style = "min-height:100px;",
        title = "Plot options", solidHeader = TRUE,
        column(
          width = 4,
          prettyRadioButtons(
            inputId = ns("barmode"),
            label = "Choose how to display the bars",
            icon = icon("check"),
            inline = TRUE, animation = "jelly",
            choices = list("stacked" = "stack",
                           "dodging" = "group"))
        ),
        column(
          width = 4,
          prettyRadioButtons(
            inputId = ns("display_x"),
            label = "Choose what to display on the x-axis",
            icon = icon("check"),
            inline = TRUE, animation = "jelly",
            choices = list("Place" = "Place",
                           "Operation" = "Operation",
                           "Category" = "category"),
            selected = "Place")
        ),
        column(
          width = 4,
          prettyRadioButtons(
            inputId = ns("display_color"),
            label = "Choose what to display as color",
            icon = icon("check"),
            inline = TRUE, animation = "jelly",
            choices = list("Place" = "Place",
                           "Operation" = "Operation",
                           "Category" = "category"),
            selected = "category")
        )
      ),
      box(
        title = "Info",
        width = 3, background = "light-blue",
        style = "min-height:100px;",
        solidHeader = TRUE, collapsible = TRUE,
        p(textOutput(ns("overview_info")))
      )
    ),
    fluidRow(
      box(title = "Overview", status = "primary",
          solidHeader = TRUE, collapsible = FALSE,
          width = 12, height = 700,
          plotlyOutput(ns("display_plot"), height = 630) %>%
            mq_spinner()
      )
    )
  )


}

#' Title
#'
#' @inheritParams db_overview_tab
#'
#' @return server code for db_overview_tab
#' @export
db_overview_server <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)


      output$overview_info <- renderText({
        validate(
          need(react_index(), "No project selected.")
        )
        n <- prettyNum(nrow(react_index()), big.mark = ",")
        text <- paste("This plot displays all", n, "resources in the",
        db_selected_project(),
        "project database, excluding images and types.",
        "All other tabs display resources from the selected Operations.")
        return(text)
      })

      output$display_plot <- renderPlotly({
        validate(
          need(react_index(), "No project selected.")
        )

        # tmp_index <- react_index()
        # if (!is.null(input$selected_places)) {
        #   tmp_index <- react_index() %>%
        #     filter(Place %in% input$selected_places)
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
                       hovertemplate = milQuant_hovertemplate())
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

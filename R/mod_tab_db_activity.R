db_activity_tab <- function(id, tabname) {

  ns <- NS(id)

  tabItem(
    tabName = tabname,
    title = "Project database activity",

    fluidRow(
      box(
        width = 9, collapsible = TRUE,
        title = "Plot options", solidHeader = TRUE,
        column(
          width = 4,
          airDatepickerInput(
            inputId = ns("daterange"),
            label = "Select range of dates:",
            range = TRUE,
            value = c(as.Date("2021-01-01"), Sys.Date())
          )
        ),
        column(
          width = 4,
          uiOutput(ns("user_selection"))
        ),
        column(
          width = 4,
          prettyRadioButtons(
            inputId = ns("action_display"),
            label = "Choose the type of action to display",
            icon = icon("check"),
            inline = TRUE, animation = "jelly",
            selected = "both",
            choices = list("resource created" = "created",
                           "resource modified" = "modified",
                           "both" = "both"))
        )
      ),
      box(
        title = "Last changed",
        width = 3, background = "light-blue",
        solidHeader = TRUE, collapsible = TRUE,
        "The last 10 changes were made to: ",
        textOutput(ns("last_ten_changes")))
    ),
    fluidRow(
      box(
        title = "Recent database activity", status = "primary",
        solidHeader = TRUE, collapsible = FALSE,
        width = 12, height = 750,
        plotlyOutput(ns("display_plot"), height = 570) %>%
          mq_spinner()
      )
    )
  )
}

db_activity_server <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      latest_changed_resources <- reactive({
        validate(
          need(login_connection(), "Not connected."),
          need(react_index(), "No project selected.")
        )
        idf_last_changed(login_connection(), index = react_index(), n = 10)
      })

      output$last_ten_changes <- renderText(
        paste(latest_changed_resources(), collapse = ", ")
      )

      plot_data <- reactive({
        plot_data <- idf_get_changes(connection = login_connection(),
                                     ids = unique(react_index()$identifier)) %>%
          mutate(date = as.Date(date)) %>%
          left_join(react_index(), by = "identifier")
        return(plot_data)
      })

      output$user_selection <- renderUI({
        pickerInput(inputId = ns("user_display"),
                    label = "Select users to display",
                    choices = sort(unique(plot_data()$user), decreasing = FALSE),
                    selected = unique(plot_data()$user),
                    multiple = TRUE,
                    options = list("actions-box" = TRUE,
                                   "live-search" = TRUE,
                                   "live-search-normalize" = TRUE,
                                   "live-search-placeholder" = "Search here..."))
      })

      output$display_plot <- renderPlotly({
        validate(
          need(react_index(), "No project selected.")
        )

        if (input$action_display == "both") {
          action_display_clean <- c("created", "modified")
        } else {
          action_display_clean <- input$action_display
        }

        fig <- plot_data() %>%
          filter(date >= input$daterange[1] & date <= input$daterange[2]) %>%
          filter(action %in% action_display_clean) %>%
          filter(user %in% input$user_display) %>%
          mutate(Place = factor(Place,
                                levels = sort(unique(Place),
                                              decreasing = TRUE))) %>%
          plot_ly(x = ~date, color = ~Place,
                       hovertemplate = paste0("<b>%{fullData.name}</b><br>",
                                              "%{x}<br>",
                                              "count: <b>%{y}</b><br>",
                                              "<extra></extra>"),
                  type = "histogram")

        plot_title <- paste0("Activity in project ", input$selected_project)

        x_label <- "date of action"
        color_label <- "Place"
        y_label <- paste("number of instances resources were",
                         paste(action_display_clean, collapse = " and "))

        fig <- fig %>%
          layout(barmode = "stack",
                 title = plot_title,
                 xaxis = list(title = x_label),
                 yaxis = list(title = y_label),
                 legend = list(title = list(text = color_label)))

        fig <- milquant_plotly_layout(fig)

        return(fig)
      })

    }
  )

}

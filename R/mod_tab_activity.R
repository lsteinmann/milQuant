#' Title
#'
#' @param id Session id
#' @param tabname Name of the tab used to link it
#'
#' @return A tab
#'
#' @export
db_activity_tab <- function(id, tabname) {

  ns <- NS(id)

  tabItem(
    tabName = tabname,
    title = "Project database activity",
    fluidRow(
      box(
        width = 9, collapsible = TRUE,
        style = "min-height:100px;",
        title = uiOptionsTitle(type = "plot"), solidHeader = TRUE,
        column(
          width = 4,
          dateRangeInput(
            inputId = ns("daterange"),
            weekstart = 1,
            language = "en",
            label = "Select range of dates:",
            start = Sys.Date() - 7,
            end = Sys.Date(),
            min = "2021-01-01",
            max = Sys.Date()
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
        style = "min-height:100px;",
        "The last 10 changes were made to: ",
        textOutput(ns("last_ten_changes")))
    ),
    fluidRow(
      box(
        title = "Recent database activity", status = "primary",
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
#' @inheritParams db_activity_tab
#'
#' @return server code
#' @export
db_activity_server <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      latest_changed_resources <- reactive({
        validate(
          need(login_connection(), "Not connected."),
          need(react_index(), "No project loaded.")
        )
        idf_last_changed(login_connection(), index = react_index(), n = 10)
      })

      output$last_ten_changes <- renderText(
        paste(latest_changed_resources(), collapse = ", ")
      )

      plot_data <- reactive({
        uuids <- react_index() %>%
          filter(isRecordedIn %in% db_selected_operations()) %>%
          pull(identifier) %>%
          unique()

        plot_data <- idf_get_changes(connection = login_connection(),
                                     ids = uuids) %>%
          mutate(date = as.Date(date)) %>%
          left_join(react_index(), by = "identifier")
        return(plot_data)
      })

      output$user_selection <- renderUI({
        validate(
          need(nrow(plot_data()) > 0, "Wating for data...")
        )

        users <- plot_data() %>%
          filter(date >= input$daterange[1] & date <= input$daterange[2]) %>%
          pull(user) %>%
          unique()

        pickerInput(inputId = ns("user_display"),
                    label = "Select users to display",
                    choices = sort(users, decreasing = FALSE),
                    selected = users,
                    multiple = TRUE,
                    options = list("actions-box" = TRUE,
                                   "live-search" = TRUE,
                                   "live-search-normalize" = TRUE,
                                   "live-search-placeholder" = "Search here..."))
      })

      date_binsize <- reactive({
        #input$daterange <- c(Sys.Date() - 28, Sys.Date())
        len_time <- length(seq(from = input$daterange[1],
                            to = input$daterange[2],
                            by = 1))

        if (len_time / 7 < 8) {
          # 1 days for under eight weeks
          n <- ms_days(1)
        } else if (len_time < (365 / 2)) {
          # 1 week for under half a year
          n <- ms_days(7)
        } else if (len_time < (365 * 2)) {
          # 1 month for under two years
          n <- "M1"
        } else {
          # let plotly choose otherwise
          n <- 0
        }

        return(n)
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
                  source = "activity_plot",
                  hovertemplate = milQuant_hovertemplate(),
                  type = "histogram",
                  xbins = list(size = date_binsize()))

        plot_title <- paste0("Activity in project ", input$selected_project)

        x_label <- "date of action"
        color_label <- "Place"
        y_label <- paste("number of instances resources were",
                         paste(action_display_clean, collapse = " and "))

        fig <- fig %>%
          layout(barmode = "stack",
                 title = plot_title,
                 xaxis = list(title = x_label,
                              range = c(input$daterange[1], input$daterange[2]),
                              dtick = date_binsize(),
                              tick0 = "2000-01-01"),
                 yaxis = list(title = y_label),
                 legend = list(title = list(text = color_label)))

        fig <- milquant_plotly_layout(fig)

        return(fig)
      })

    }
  )

}

#' UI for the milQuant app
#'
#' @param app_version
app_ui <- function(app_version = packageVersion("milQuant")) {
  #  header

  header <- dashboardHeader(
    title = "milQuant",
    tags$li(class = "dropdown",
            actionButton(label = "Save and Quit",
                         icon = icon("power-off"),
                         inputId = "close_app")))

  #  sidebar
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "tab_home", icon = icon("right-to-bracket")),
      shinyjs::hidden(tags$div(id = "tab_connect.success-div",
                               class = "success-text",
                               textOutput("load.success_msg",
                                          container = tags$p))),
      actionButton("refreshIndex", "Refresh Index", icon = icon("refresh")),
      menuItem("Project overview", tabName = "db_overview_tab", icon = icon("graduation-cap")),
      uiOutput("place_selector"),
      uiOutput("operation_selector"),
      menuItem("Activity", tabName = "db_activity_tab", icon = icon("people-arrows")),
      menuItem("Bar Charts for Find Groups", tabName = "tab_barchart_finds", icon = icon("chart-bar")),
      menuItem("Issues / Contact", icon = icon("file-contract"),
               href = "https://github.com/lsteinmann/milQuant")
    )
  )

  #  body
  body <- dashboardBody(
    # Include shinyjs and custom extensions
    shinyjs::useShinyjs(),

    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/main.css")
    ),

    tags$script(src = "js/toggleBtnsOnBusy.js"),

    tabItems(
      make_home_tab("tab_home"),
      db_overview_tab("db_overview", tabname = "db_overview_tab"),
      db_activity_tab("db_activity", tabname = "db_activity_tab"),
      mod_barchart_finds_ui("barchart_finds", tabname = "tab_barchart_finds")

      # others, grouped
    )
  )


  # Combine elements to create UI
  ui <- dashboardPage(header, sidebar, body)

  # Return the ui
  ui
}

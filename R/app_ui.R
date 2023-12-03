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
      menuItem("Aoristic Density Plots", tabName = "tab_aoristic_finds", icon = icon("chart-area")),
      menuItem("Pottery", tabName = "tab_pottery_all", icon = icon("trophy"),
               menuSubItem("Pottery (single)", tabName = "tab_pottery_single",
                           icon = icon("martini-glass-empty")),
               menuSubItem("Pottery Quantification A", tabName = "tab_potteryQA",
                           icon = icon("champagne-glasses")),
               menuSubItem("Pottery Quantification B", tabName = "tab_potteryQB",
                           icon = icon("champagne-glasses"))
      ),
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
      mod_barchart_finds_ui("barchart_finds", tabname = "tab_barchart_finds"),
      mod_aoristic_finds_ui("aoristic_finds", tabname = "tab_aoristic_finds"),
      mod_pottery_single_ui("pottery_single", tabname = "tab_pottery_single"),
      mod_pottery_QA_ui("pottery_QA", tabname = "tab_potteryQA"),
      mod_pottery_QB_ui("pottery_QB", tabname = "tab_potteryQB")
    )
  )


  # Combine elements to create UI
  ui <- dashboardPage(header, sidebar, body)

  # Return the ui
  ui
}

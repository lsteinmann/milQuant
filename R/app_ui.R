#' UI for the milQuant app
#'
#' @param app_version Version of the Package to be display in UI
#'
#' @return the UI
#'
#' @export
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
      tags$div(id = "tab_connect.success-div",
               class = "success-text", style = "display:inline-block",
               htmlOutput("load.success_msg")),
      actionButton("refreshIndex", "Refresh Index", icon = icon("refresh")),
      menuItem("Activity", tabName = "db_activity_tab", icon = icon("people-arrows")),
      menuItem("Fulltext Search", tabName = "tab_search", icon = icon("magnifying-glass")),
      menuItem("Workflow", tabName = "tab_workflow", icon = icon("gears")),
      menuItem("Finds", tabName = "tab_all_finds", icon = icon("magnifying-glass-chart"),
               menuSubItem("Inventoried Finds", tabName = "tab_finds_overview", icon = icon("ranking-star")),
               menuSubItem("Bar Charts for Find Groups", tabName = "tab_barchart_finds", icon = icon("chart-column")),
               menuSubItem("Aoristic Density Plots", tabName = "tab_aoristic_finds", icon = icon("chart-area"))),
      menuItem("Quantifications", tabName = "quants", icon = icon("chart-simple"),
               menuSubItem("Quantification (General)", tabName = "tab_finds_quant", icon = icon("chart-simple")),
               menuSubItem("Bricks/Tiles/Pipes", tabName = "tab_bricksQ", icon = icon("shapes")),
               menuSubItem("Mollusks", tabName = "tab_molluskQ", icon = icon("shrimp")),
               menuSubItem("Plaster", tabName = "tab_plasterQ", icon = icon("brush"))
      ),
      menuItem("Pottery", tabName = "tab_pottery_all", icon = icon("trophy"),
               menuSubItem("Pottery (single)", tabName = "tab_pottery_single",
                           icon = icon("martini-glass-empty")),
               menuSubItem("Pottery Quantification A", tabName = "tab_potteryQA",
                           icon = icon("champagne-glasses")),
               menuSubItem("Pottery Quantification B", tabName = "tab_potteryQB",
                           icon = icon("champagne-glasses"))
      ),
      menuItem("Loomweights", tabName = "tab_loomweights_hist", icon = icon("scale-unbalanced")),
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

      db_activity_tab("db_activity", tabname = "db_activity_tab"),
      mod_search_ui("search", tabname = "tab_search"),
      mod_worflow_ui("workflow", tabname = "tab_workflow"),

      # generic and overview of all finds
      mod_finds_overview_ui("finds_overview", tabname = "tab_finds_overview"),
      mod_finds_quant_ui("finds_quant", tabname = "tab_finds_quant"),
      mod_barchart_finds_ui("barchart_finds", tabname = "tab_barchart_finds"),
      mod_aoristic_finds_ui("aoristic_finds", tabname = "tab_aoristic_finds"),

      # pottery
      mod_pottery_single_ui("pottery_single", tabname = "tab_pottery_single"),
      mod_pottery_QA_ui("pottery_QA", tabname = "tab_potteryQA"),
      mod_pottery_QB_ui("pottery_QB", tabname = "tab_potteryQB"),

      # loomweights
      mod_loomweights_hist_ui("loomweights_hist", tabname = "tab_loomweights_hist"),

      # quants
      mod_quants_ui("bricksQ", tabname = "tab_bricksQ"),
      mod_quants_ui("molluskQ", tabname = "tab_molluskQ"),
      mod_quants_ui("plasterQ", tabname = "tab_plasterQ")
    )
  )


  # Combine elements to create UI
  ui <- dashboardPage(header, sidebar, body)

  # Return the ui
  ui
}

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
      menuItem("Home", tabName = "home_tab", icon = icon("right-to-bracket")),
      shinyjs::hidden(tags$div(id = "tab_connect.success-div",
                               class = "success-text",
                               textOutput("load.success_msg",
                                          container = tags$p))),
      actionButton("refreshIndex", "Refresh Index", icon = icon("refresh")),
      menuItem("Project overview", tabName = "db_overview_tab", icon = icon("graduation-cap")),
      menuItem("Activity", tabName = "db_activity_tab", icon = icon("people-arrows")),
      uiOutput("selected_operations"),
      uiOutput("selected_trenches"),

      menuItem("Workflow", tabName = "workflow_tab", icon = icon("gear")),

      menuItem("Find Overview", tabName = "finds_overview", icon = icon("chart-bar"),
               menuSubItem("Inventoried Finds", tabName = "all_finds", icon = icon("chart-bar")),
               menuSubItem("Quantifications", tabName = "all_finds_quant_tab", icon = icon("chart-simple"))),
      menuItem("Pottery", tabName = "pottery_all", icon = icon("trophy"),
               menuSubItem("Pottery (single)", tabName = "pottery_tab",
                           icon = icon("martini-glass-empty")),
               menuSubItem("Pottery Quantification A", tabName = "potteryQA_tab",
                           icon = icon("champagne-glasses")),
               menuSubItem("Pottery Quantification B", tabName = "potteryQB_tab",
                           icon = icon("champagne-glasses"))
      ),
      menuItem("Bricks and Tiles", tabName = "bricks_all", icon = icon("square"),
               menuSubItem("Bricks and Tiles", tabName = "bricks_tab",
                           icon = icon("house")),
               menuSubItem("Brick/Tile/Pipe Quantification", tabName = "bricksQ_tab",
                           icon = icon("shapes"))
      ),
      menuItem("Coins", tabName = "coins_tab", icon = icon("circle-dollar-to-slot"),
               menuSubItem("Coins", tabName = "coins_tab", icon = icon("chart-column")),
               menuSubItem("Coins (Aoristic)", tabName = "coins_aor_tab", icon = icon("chart-area"))),
      menuItem("Loomweights", tabName = "loomweights_hist", icon = icon("weight-hanging"),
               menuSubItem("Loomweights", tabName = "loomweights_bar_tab", icon = icon("weight-hanging")),
               menuSubItem("Loomweights by Weight", tabName = "loomweights_hist_tab", icon = icon("scale-unbalanced"))),
      menuItem("Other Finds", tabName = "otherFinds", icon = icon("network-wired"),
               menuItem("Bone", tabName = "bone_tab", icon = icon("bone")),
               menuItem("Glass", tabName = "glass_tab", icon = icon("whiskey-glass")),
               menuItem("Lamps", tabName = "lamps_tab", icon = icon("fire-flame-curved")),
               menuItem("Metal", tabName = "metal_tab", icon = icon("utensils")),
               menuItem("Plaster", tabName = "plaster_tab", icon = icon("paintbrush")),
               menuItem("Sculpture", tabName = "sculpture_tab", icon = icon("person-skating")),
               menuItem("Stone", tabName = "stone_tab", icon = icon("volcano")),
               menuItem("Terracotta", tabName = "terracotta_tab", icon = icon("horse-head"))),
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
      make_home_tab(app_version = app_version, tabname = "home_tab"),
      db_overview_tab("db_overview", tabname = "db_overview_tab"),
      db_activity_tab("db_activity", tabname = "db_activity_tab"),

      worflow_tab("workflow", tabname = "workflow_tab"),

      # overview of all finds
      all_finds_tab("all_finds"),
      basic_quant_tab("all_finds_quant", tabname = "all_finds_quant_tab"),

      # pottery
      barplot_tab("pottery", tabname = "pottery_tab"),
      potteryQA_tab("pottery_QA", tabname = "potteryQA_tab"),
      potteryQB_tab("pottery_QB", tabname = "potteryQB_tab"),

      # bricks and tiles
      barplot_tab("bricks", tabname = "bricks_tab"),
      bricksQ_tab("bricksQ_bar"),

      # coins
      barplot_tab("coins", tabname = "coins_tab"),
      aoristic_tab("coins_aor", tabname = "coins_aor_tab"),

      # loomweights
      loomweights_tab("lw_hist"),
      barplot_tab("lw_bar", tabname = "loomweights_bar_tab"),

      # others, grouped
      barplot_tab("bone", tabname = "bone_tab"),
      barplot_tab("glass", tabname = "glass_tab"),
      barplot_tab("lamps", tabname = "lamps_tab"),
      barplot_tab("metal", tabname = "metal_tab"),
      barplot_tab("plaster", tabname = "plaster_tab"),
      barplot_tab("sculpture", tabname = "sculpture_tab"),
      barplot_tab("stone", tabname = "stone_tab"),
      barplot_tab("terracotta", tabname = "terracotta_tab")
    )
  )


  # Combine elements to create UI
  ui <- dashboardPage(header, sidebar, body)

  # Return the ui
  ui
}

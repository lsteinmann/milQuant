#' Make the UI for the landing page ("home")
#'
#' @param tabname reference of the tab for the sidebar
#'
#' @return a tabitem()
#' @export
make_home_tab <- function(tabname = "tab_home") {
  tabItem(
    tabName = tabname,
    title = "Home",
    fluidRow(
      box(width = 10,
          column(width = 3,
                 img(src = "img/milQuant-logo.png",
                     height = 69, align = "center")
          ),
          column(width = 7,
                 shinyjs::hidden(
                   tags$div(id = "welcome_div",
                            class = "login-text",
                            textOutput("welcome_text",
                                       container = tags$p)))
          )
      ),
      infoBox(width = 2, title = "Version",
              subtitle = "date: 24.11.2023",
              icon = icon("code-branch"),
              value = getNamespaceVersion("milQuant"),
              color = "black",
              href = "https://github.com/lsteinmann/milQuant")
    ),
    fluidRow(
      tabBox(
        width = 12,
        tabPanel(
          title = tagList(icon("wrench"), "Database and Project Settings"),
          fluidRow(
            column(
              width = 4,
              h4("Database and Project selection options"),
              column(
                width = 8,
                uiOutput("project_selector"),
                uiOutput("place_selector"),
                uiOutput("operation_selector")
              ),
              column(
                width = 4,
                actionButton(inputId = "loadDatabase",
                             label = "Load Database",
                             style = "margin-top:25px"))
            ),
            column(
              width = 8,
              box(
                title = tagList(icon("circle-info"), "Info on Selection Options"),
                width = 12,
                solidHeader = TRUE,
                status = "info",
                column(
                  width = 12,
                  tags$b("Large projects may take a while to load. Be prepared to wait after clicking 'Load Database'."),
                  br(), br()
                ),
                column(
                  width = 6,
                  includeHTML("www/html/home_infotext_1.html")
                ),
                column(
                  width = 6,
                  includeHTML("www/html/home_infotext_2.html")
                )
              )
            )
          ),
          fluidRow(
            box(
              title = "Before you get started...",
              width = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              status = "warning",
              includeHTML("www/html/home_beforeyoustart_text.html")
            )
          )
        ),
        db_overview_tab("db_overview", tabname = "db_overview_tab"),
        tabPanel(
          title = tagList(icon("newspaper"), "News"),
          fluidRow(
            box(width = 12, includeHTML("www/html/NEWS.html"))
          )
        )
      )
    )
  )
}

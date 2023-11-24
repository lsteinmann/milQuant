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
      box(
        title = "Before you get started...", width = 12, solidHeader = TRUE,
        status = "warning", collapsible = TRUE,
        column(6, p("In the side bar to the left, you see a dropdown. You need to select
          a Place or Operation to start with from that dropdown. This means
          you need to tell milQuant what excavation area you are interested in,
          for example in \"Insula UV/8-9\" on Humeitepe. This reflects the
          \"Place\" and \"Survey\"-resources from Field Desktop."),
               p("After you leave this selection, a second dropdown will appear. This
          time, you should select the Trench(es) you want to work with.")),
        column(6,
               p("Though sometimes cumbersome, this selection process helps speed
          up the app a great deal. When you are all done, you can start
          browsing the different tabs."),
               p("Please note that if you add new resources in Field Desktop while
          working with milQuant, you need to press the 'Refresh Index' button
          and wait in order to be able to see them."))

      )
    ),
    fluidRow(
      box(width = 4, height = "200px", title = "Select a project to work with",
          div(class = "welcome-row-div",
              column(8, uiOutput("project_selector")),
              column(4, actionButton(inputId = "loadDatabase",
                           label = "Load Database",
                           style = "margin-top:25px")))),
      box(width = 6, status = "warning",
          p("Large projects may take a while to load. Be prepared to wait
            after clicking 'Load Database'."))
    )
  )
}

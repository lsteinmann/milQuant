make_home_tab <- function(app_version = app_version, tabname = "home_tab") {
  tabItem(
    tabName = tabname,
    title = "Home", # name of the tab
    # hide the welcome message at the first place
    fluidRow(
      box(width = 10,
          div(img(src = "img/milQuant-logo.png",
                  height = 100, align = "center"))),
      infoBox(width = 2, title = "Version", subtitle = "date: 13.11.2023",
              icon = icon("code-branch"), value = paste0("v", app_version),
              color = "black",
              href = "https://github.com/lsteinmann/milQuant")
    ),
    fluidRow(
      box(width = 12,
          div(
            shinyjs::hidden(tags$div(id = "tab_connect.welcome_div",
                                     class = "login-text",
                                     textOutput("tab_connect.welcome_text",
                                                container = tags$h1))))),
    ),
    fluidRow(
      box(width = 6, height = "200px", title = "Select a project to work with",
          div(class = "welcome-row-div",
              uiOutput("selected_project"),
              actionButton(inputId = "loadDatabase",
                           label = "Load Database"))),
      box(p("With this App, you can view and download various plots of data from
      the Milesian Field Desktop-Database. Field Desktop has to be running on
      your computer or in a local network. Choose a project
      from your Field Desktop Client from the selection to the left.")),
      box(title = "Please note", status = "warning",
          p("Large projects may take a while to load. Be prepared to wait
            after clicking 'Load Database'."))
    ),
    fluidRow(
      box(width = 12, title = "Before you get started...",
          p("In the side bar to the left, you see a dropdown. You need to select
          a Place or Operation to start with from that dropdown. This means
          you need to tell milQuant what excavation area you are interested in,
          for example in \"Insula UV/8-9\" on Humeitepe. This reflects the
          \"Place\" and \"Survey\"-resources from Field Desktop."),
          p("After you leave this selection, a second dropdown will appear. This
          time, you should select the Trench(es) you want to work with."),
          p("Though sometimes cumbersome, this selection process helps speed
          up the app a great deal. When you are all done, you can start
          browsing the different tabs."),
          p("Please note that if you add new resources in Field Desktop while
          working with milQuant, you need to press the 'Refresh Index' button
          and wait in order to be able to see them.")
      )
    )
  )
}

#' Server function for the milQuant app
#'
#' @param input
#' @param output
#' @param session
app_server <- function(input, output, session) {


  # % -------------------------------------------------------------Connection

  #session <- getDefaultReactiveDomain()

  # setup the two reactiveValues that will be used nearly everywhere
  login_connection <<- reactiveVal()
  projects <- reactiveVal()

  db_settings <<- reactiveValues(selected_project = NA,
                                 selected_places = NA,
                                 selected_operations = NA,
                                 selected_categories = NA)


  # define the ui of the login modal
  login_modal <- modalDialog(
    title = "Enter connection details to continue",
    footer = actionButton("tab_connect.connect","Connect"),
    textInput("tab_connect.host","Host (Field Desktop)", value = "127.0.0.1"),
    textInput("tab_connect.user","Username", value = "username"),
    passwordInput("tab_connect.pwd","Password", value = "hallo"),
    tags$div(class = "warn-text",textOutput("tab_connect.error_msg"))
  )

  # show login dialog box when initiated
  showModal(login_modal, session = session)

  # server code to handle the connection to field in the modal
  username <- source(system.file(
    package = "milQuant", mustWork = TRUE,
    "app/www/settings/shared_settings.R"))$value$username
  updateTextInput(session, "tab_connect.user", value = username)

  password <- source(system.file(
    package = "milQuant", mustWork = TRUE,
    "app/www/settings/shared_settings.R"))$value$synchpw
  updateTextInput(session, "tab_connect.pwd", value = password)

  observeEvent(input$tab_connect.connect, {

    # manually validate connection
    message("Checking the connection you provided.")
    test_connection <- suppressMessages(
      connect_idaifield(serverip = input$tab_connect.host,
                        user = input$tab_connect.user,
                        pwd = input$tab_connect.pwd)
      )

    ping <- tryCatch({
      idf_ping(test_connection)
    }, warning = function(w) {
      conditionMessage(w)
    }, error = function(e) {
      conditionMessage(e)
    })

    if (ping == TRUE) {
      # succesfully connected:
      login_connection(test_connection)
      removeModal() # remove login dialog
      output$welcome_text <- renderText(
        glue("Welcome to milQuant - Quantitative Analysis
              with Data from Field, {input$tab_connect.user}!")
        )
      shinyjs::show("welcome_div") # show welcome message
      message("Success! Can connect to Field.")
    } else if (ping == FALSE) {
      message("Failed! Can't connect to Field:")
      # display the message
      output$tab_connect.error_msg <- renderText(ping)
      shinyjs::show("tab_connect.error_msg")
    } else {
      message("Something unexpected happened: ")
      # I have no idea what happened if this happens, but
      # it can't be right
      output$tab_connect.error_msg <- renderText(ping)
      shinyjs::show("tab_connect.error_msg")
    }
  })

  observeEvent(input$tab_connect.connect, {
    validate(
      need(inherits(login_connection(),
                    "idf_connection_settings"),
           "No Connection set.")
    )
    message("Getting list of projects.")
    # Produces the List of projects in the database
    projects(idf_projects(login_connection()))
    message("Done.\nWaiting for you to load project.")
  })

  # server code to handle basic settings, i.e. project, trench/operation
  # seen on home_tab and sidebar
  observeEvent(input$tab_connect.connect, {
    validate(
      need(projects(), "Project list not available.")
    )

    db_settings$selected_project <- startup_settings$selected_project
    db_settings$selected_places <- startup_settings$selected_places
    db_settings$selected_operations <- startup_settings$selected_operations
    db_settings$selected_categories <- startup_settings$selected_categories

    output$project_selector <- renderUI({
      selectizeInput(inputId = "selected_project",
                     label = "Choose a Project to work with",
                     choices = projects(), multiple = FALSE,
                     selected = startup_settings$selected_project,
                     options = list(
                       placeholder = "Please select an option below")
      )
    })
  })


  # % -------------------------------------------------------------Index

  react_index <<- reactiveVal(value = NULL)

  observeEvent(input$loadDatabase, {
    busy_dialog <- make_busy_dialog(project = isolate(input$selected_project))

    shinyjs::hide("load.success_msg")
    showModal(busy_dialog)

    message("Trying to connect to the project:")
    try_project <- try_project_connection(
      connection = login_connection(),
      project = input$selected_project
    )

    if (try_project) {
      # manually assign the selected project to the connection object and
      # update the loging connection in the settings-reactiveValues
      new_login_connection <- login_connection()
      new_login_connection$project <- input$selected_project
      login_connection(new_login_connection)
      message("Success! Getting the Index:")

      # THEN get the index.
      react_index(get_index(connection = login_connection()))
      message("Done.")

      # make and show the success msg in the UI
      msg <- paste("Using project:", isolate(input$selected_project))
      output$load.success_msg <- renderText({msg})
      shinyjs::show("load.success_msg")

      output$current_project <- renderText({input$selected_project})
      db_settings$selected_project <- input$selected_project

      is_milet <<- input$selected_project %in% c("milet", "milet-test")
      if (is_milet) {
        reorder_periods <<- TRUE
      } else {
        reorder_periods <<- FALSE
      }

      removeModal()
    } else {
      # make and show the success msg in the UI if something went wrong
      msg <- paste("An error has occured: ", try_project)
      output$load.error_msg <- renderText(msg)
      shinyjs::show("load.error_msg")
    }
  })

  observeEvent(input$close_busy_dialog,{
    removeModal()
  })

  observeEvent(input$refreshIndex, {
    message("Fetching the Index again...")
    react_index(get_index(connection = login_connection()))
    message("Done.")
  })

  #
  # % -------------------------------------------------------------DB Selection

  observeEvent(input$selected_operations, {
    db_settings$selected_places <- input$selected_places
    db_settings$selected_operations <- input$selected_operations
  })


  # Produces the List of Places to select from the reactive Index
  # may not update when index is refreshed
  available_places <- reactive({
    validate(
      need(react_index(), "No project selected.")
    )
    get_list_of_places_with_finds(index = react_index())
  })

  available_operations <- reactive({
    validate(
      need(available_places(), "No Place selected.")
    )
    get_list_of_operations_in_places(
      index = react_index(),
      selected_places = input$selected_places
    )
  })

  output$place_selector <- renderUI({
    validate(
      need(react_index(), "No project selected."),
      need(available_places(), "Waiting for Places...")
    )

    pickerInput(inputId = "selected_places",
                label = "Choose one or more Places to work with",
                choices = available_places(),
                selected = startup_settings$selected_places,
                multiple = TRUE,
                options = list("actions-box" = TRUE,
                               "live-search" = TRUE,
                               "live-search-normalize" = TRUE,
                               "live-search-placeholder" = "Search here..."))
  })

  output$operation_selector <- renderUI({
    validate(
      need(react_index(), "No project selected."),
      need(input$selected_places, "No Places selected.")
    )
    pickerInput(inputId = "selected_operations",
                label = "Choose one or more Operations (Trenches, Surveys) to work with",
                choices = available_operations(),
                selected = startup_settings$selected_operations,
                multiple = TRUE,
                options = list("actions-box" = TRUE,
                               "live-search" = TRUE,
                               "live-search-normalize" = TRUE,
                               "live-search-placeholder" = "Search here..."))
  })


  # % ----------------------------------------------------------------On Exit

  observeEvent(input$close_app,{
    tmp <- list(selected_project = isolate(db_settings$selected_project),
                selected_places = isolate(db_settings$selected_places),
                selected_operations = isolate(db_settings$selected_operations),
                selected_categories = isolate(db_settings$selected_categories))
    try(saveRDS(tmp, system.file(package = "milQuant", mustWork = TRUE,
                             "app/www/settings/db_settings.RDS")))
    print("Shiny: EXIT")
    stopApp()
  })

  # run this is file does not exist
  #tmp <- list(selected_project = "milet-test",
  #            selected_places = "",
  #            selected_operations = "")
  #saveRDS(tmp, "inst/app/www/settings/db_settings.RDS")


  # % ----------------------------------------------------------------Modules




  # server code only for overview pages
  db_overview_server("db_overview")
  db_activity_server("db_activity")


  # Generic Plots
  mod_barchart_finds_serv("barchart_finds", resource_category = "Pottery")
  #mod_aoristic_finds_serv("aoristic_finds", resource_category = "Pottery")

  # all other (automated) finds




  # server code for future sculpture tab
  # test commit
  #source('source/server/sculpture_serv.R', local = TRUE)

  # close the R session when Chrome closes
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
    })
  }

}

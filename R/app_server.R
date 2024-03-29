#' Server function for the milQuant app
#'
#' @param input input
#' @param output output
#' @param session session
#'
#' @return the server function
app_server <- function(input, output, session) {

  # % -------------------------------------------------------------Connection

  #session <- getDefaultReactiveDomain()
  shinyjs::hide("load.success_msg")

  # setup the two reactiveValues that will be used nearly everywhere
  login_connection <<- reactiveVal()
  projects <- reactiveVal()

  db_selected_project <<- reactiveVal(NA)
  db_selected_places <<- reactive({ input$selected_places }) %>%
    debounce(500)
  db_selected_operations <<- reactive({ input$selected_operations }) %>%
    debounce(2000)
  db_selected_categories <<- reactiveVal(startup_settings$selected_categories)


  # define the ui of the login modal
  login_modal <- create_login_modal()

  # show login dialog box when initiated
  showModal(login_modal, session = session)

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
        paste("Welcome, ", input$tab_connect.user, "!",
              sep = "")
        )

      if (isolate(input$tab_connect.savesettings) == TRUE) {
        new_settings <- paste(
          'list("fieldhost" = "', input$tab_connect.host, '", ',
          '"username" = "', input$tab_connect.user, '", ',
          '"synchpw" = "', input$tab_connect.pwd, '")',
          sep = "")
        writeLines(
          new_settings,
          system.file(package = "milQuant", mustWork = TRUE,
                      "app/www/settings/shared_settings.R")
        )
      }


      shinyjs::show("welcome_div") # show welcome message
      message(milQ_message("Success! Can connect to Field."))
    } else if (ping == FALSE) {
      message(milQ_warning("Failed! Can't connect to Field:"))
      # display the message
      output$tab_connect.error_msg <- renderText(ping)
      shinyjs::show("tab_connect.error_msg")
    } else {
      warning(milQ_warning("Something unexpected happened: "))
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

    output$project_selector <- renderUI({
      selectizeInput(inputId = "selected_project",
                     label = "Database-Projects", # "Choose a Project to work with",
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
      message(milQ_message("Success! Getting the Index:"))

      # THEN get the index.
      react_index(get_index(connection = login_connection()))
      message("Done.")

      # show project info and success msg in sidebar
      db_selected_project(isolate(input$selected_project))
      shinyjs::show("load.success_msg")

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
    message(milQ_message("Fetching the Index again..."))
    react_index(get_index(connection = login_connection()))
    message("Done.")
  })

  # make and show the success msg in the UI

  output$load.success_msg <- renderUI({

    on_project <- paste(db_selected_project())

    info_msg <- paste0(
      "<b>Places</b> (", length(input$selected_places), "): ",
      paste(input$selected_places, collapse = ", "),
      "<br>",
      "<b>Operations</b> (", length(input$selected_operations), "): ",
      paste(input$selected_operations, collapse = ", ")
    )

    tags$span(
      popify(span(icon("info-circle", style = "padding-left: 5px; padding-right: 10px"),
                  on_project,
                  id = "project_i", style = "color: #b7b8bc"),
             title = paste0("On project: <b>", on_project, "</b>"),
             content = info_msg, placement = "right", trigger = "hover",
             options = list(container = "body"))
    )
  })



  #
  # % -------------------------------------------------------------DB Selection

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
                label = "Places", # "Choose one or more Places to work with",
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

    if (length(available_operations()) == 1) {
      selected <- available_operations()
    } else {
      selected <- startup_settings$selected_operations
    }

    pickerInput(inputId = "selected_operations",
                label = "Operations (Trenches, Surveys)", # "Choose one or more Operations (Trenches, Surveys) to work with",
                choices = available_operations(),
                selected = selected,
                multiple = TRUE,
                options = list("actions-box" = TRUE,
                               "live-search" = TRUE,
                               "live-search-normalize" = TRUE,
                               "live-search-placeholder" = "Search here..."))
  })


  # % ----------------------------------------------------------------On Exit

  observeEvent(input$close_app,{
    tmp <- list(selected_project = isolate(db_selected_project()),
                selected_places = isolate(db_selected_places()),
                selected_operations = isolate(db_selected_operations()),
                selected_categories = isolate(db_selected_categories()))
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
  mod_search_serv("search")
  mod_worflow_serv("workflow")


  # Generic Plots for Finf Overview
  mod_finds_overview_serv("finds_overview")
  mod_barchart_finds_serv("barchart_finds")
  mod_aoristic_finds_serv("aoristic_finds")

  mod_pottery_single_serv("pottery_single")
  mod_pottery_QA_serv("pottery_QA")
  mod_pottery_QB_serv("pottery_QB")

  mod_loomweights_hist_serv("loomweights_hist")

  mod_finds_quant_serv("finds_quant")
  mod_quants_serv("bricksQ", resource_category = "Brick_Quantification")
  mod_quants_serv("molluskQ", resource_category = "QuantMollusks")
  mod_quants_serv("plasterQ", resource_category = "PlasterQuantification")




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

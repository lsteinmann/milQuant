#' Server function for the milQuant app
#'
#' @param input
#' @param output
#' @param session
app_server <- function(input, output, session) {

  #session <- getDefaultReactiveDomain()

  # define the ui of the login dialog box
  login_dialog <- modalDialog(
    title = "Enter connection details to continue",
    footer = actionButton("tab_connect.connect","Connect"),
    textInput("tab_connect.host","Host (Field Desktop)", value = "127.0.0.1"),
    textInput("tab_connect.user","Username", value = "username"),
    passwordInput("tab_connect.pwd","Password", value = "hallo"),
    tags$div(class = "warn-text",textOutput("tab_connect.error_msg"))
  )


  busy_dialog <- modalDialog(
    title = "Loading Project, please wait...",
    div(class = "warn-text", textOutput("load.error_msg") %>%
          withSpinner(image = "/img/quant-spinner-smooth.gif",
                      image.width = 120,
                      image.height = 120,
                      proxy.height = "100px",
                      color.background = "#ffffff")),
    footer = shinyjs::hidden(actionButton(inputId = "close_busy_dialog", "Close"))
  )

  # show login dialog box when initiated
  showModal(login_dialog, session = session)
  # server code to handle the connection to field in the modal
  default.username <- source(system.file(package = "milQuant", mustWork = TRUE,
                                         "app/www/settings/settings.R"))$value$username
  updateTextInput(session, "tab_connect.user", value = default.username)

  default.password <- source(system.file(package = "milQuant", mustWork = TRUE,
                                         "app/www/settings/settings.R"))$value$synchpw
  updateTextInput(session, "tab_connect.pwd", value = default.password)

  login_connection <<- reactiveVal(NA)

  observeEvent(input$tab_connect.connect, {
    host <- input$tab_connect.host
    user <- input$tab_connect.user
    pwd <- input$tab_connect.pwd

    # manually validate connection
    message("Checking the connection you provided.")
    test_connection <- connect_idaifield(serverip = host,
                                         user = user,
                                         pwd = pwd)

    ping <- tryCatch({
      idf_ping(test_connection)
    }, warning = function(w) {
      conditionMessage(w)
    }, error = function(e) {
      conditionMessage(e)
    })

    if (ping == TRUE) {
      # succesfully connect:
      login_connection(test_connection)
      removeModal() # remove login dialog
      output$tab_connect.welcome_text <- renderText(glue("Welcome to milQuant - Quantitative Analysis
                                                       with Data from Field, {user}!"))
      shinyjs::show("tab_connect.welcome_div") # show welcome message
      message("Success! Can connect to Field.")
    } else if (ping == FALSE) {
      message("Failed! Can't connect to Field and don't know why.")
      # I have no idea what happened if this happens, but
      # it can't be right
      output$tab_connect.error_msg <- renderText(paste("Unforeseen Error:", ping))
      shinyjs::show("tab_connect.error_msg")
    } else {
      message("Failed! Can't connect to Field:")
      # display the message
      output$tab_connect.error_msg <- renderText(ping)
      shinyjs::show("tab_connect.error_msg")
    }
  })

  observeEvent(input$tab_connect.connect, {
    validate(
      need(inherits(login_connection(), "idf_connection_settings"), "No Connection set.")
    )
    message("Getting list of projects.")
    # Produces the List of projects in the database
    projects <<- idf_projects(login_connection())
    message("Done.\nWaiting for you to load project.")
  })

  # server code to handle basic settings, i.e. project, trench/operation
  # seen on home_tab and sidebar
  serv_settings <- function() {
    observeEvent(input$tab_connect.connect, {
      validate(
        need(exists("projects"), "Project list not available.")
      )
      output$selected_project <- renderUI({
        selectizeInput(inputId = "selected_project",
                       label = "Choose a Project to work with",
                       choices = projects, multiple = FALSE,
                       selected = selection_settings$selected_project,
                       options = list(
                         placeholder = "Please select an option below")
        )
      })
    })


    react_index <<- reactiveVal(value = NULL)

    observeEvent(input$loadDatabase, {

      # error message is only generated once and after this process here has been
      # finished for the first time, the dialog doesnt show the spinner anymore
      # i don't know how to fix that
      # ...

      shinyjs::hide("load.success_msg")
      showModal(busy_dialog)


      message("Trying to connect to the project:")
      try_project <- tryCatch({
        client <- idaifieldR:::proj_idf_client(login_connection(), include = "query",
                                               project = input$selected_project)
        query <- paste0(
          '{
      "selector": { "resource.id": "project" },
      "fields": [ "resource.id", "resource.identifier" ]
       }')
        response <- idaifieldR:::response_to_list(client$post(body = query))
        input$selected_project %in% unlist(response)
      }, warning = function(w) {
        conditionMessage(w)
      }, error = function(e) {
        conditionMessage(e)
      })

      if (try_project) {
        new_login_connection <- login_connection()
        new_login_connection$project <- input$selected_project
        login_connection(new_login_connection)
        message("Success! Getting the Index:")
        newIndex <- get_index(source = login_connection())
        react_index(newIndex)
        message("Done.")
        rm(newIndex)
        output$load.success_msg <- renderText(paste("Using project:",
                                                    isolate(input$selected_project)))
        shinyjs::show("load.success_msg")
        output$current_project <- renderText({input$selected_project})
        removeModal()
      } else {
        output$load.error_msg <- renderText(paste("An error has occured: ",
                                                  try_project))
        shinyjs::show("load.error_msg")
      }
    })

    observeEvent(input$close_busy_dialog,{
      removeModal()
    })

    observeEvent(input$selected_project, {
      hide('load.success_msg')
      is_milet <<- input$selected_project %in% c("milet", "milet-test")
      if (is_milet) {
        reorder_periods <<- TRUE
      } else {
        reorder_periods <<- FALSE
      }
    })


    observeEvent(input$refreshIndex, {
      message("Fetching the Index again...")
      newIndex <- get_index(source = login_connection())
      react_index(newIndex)
      message("Done.")
      rm(newIndex)
    })


    # Produces the List of Places to select from the reactive Index
    # may not update when index is refreshed
    operations <- reactive({
      tmp_operations <- react_index() %>%
        filter(category %in% c(find_categories, quant_categories)) %>%
        pull(Place) %>%
        unique()
      tmp_operations <- tmp_operations[!is.na(tmp_operations)]
      #  ind <- tmp_operations %in% c("Bauwerkskatalog", "Inschriften",
      #                               "Milet_Stadt", "Steindepot",
      #                               "HU_Streufunde", "Kalabaktepe",
      #                               "Südstadt", "Typenkatalog")
      #  tmp_operations <- tmp_operations[!ind]
      operations <- sort(tmp_operations)
      rm(tmp_operations)
      return(operations)
    })


    db_operations <<- reactive({input$selected_operations}) %>% debounce(2000)

    trenches <- reactive({
      validate(
        need(db_operations(), "No operation selected.")
      )
      if (db_operations()[1] == "select everything") {
        tmp_trenches <- react_index() %>%
          pull(isRecordedIn) %>%
          unique()
      } else {
        tmp_trenches <- react_index() %>%
          filter(Place %in% db_operations()) %>%
          pull(isRecordedIn) %>%
          unique()
      }

      tmp_trenches <- tmp_trenches[!is.na(tmp_trenches)]
      trenches <- sort(tmp_trenches)
      rm(tmp_trenches)
      return(trenches)
    })

    db_trenches <<- reactive({input$selected_trenches}) %>% debounce(2000)


    #Place Selector -- Return the requested dataset as text
    # apparently i do not use this anywhere??
    #output$selected_place <- renderText({
    #  paste(input$selected_operations)
    #})
    output$selected_operations <- renderUI({
      validate(
        need(react_index(), "No project selected.")
      )
      choices <- operations()
      if (length(choices) == 0) {
        choices <- c("select everything")
      }
      pickerInput(inputId = "selected_operations",
                  label = "Choose one or more Places / Operations to work with",
                  choices = choices,
                  selected = selection_settings$selected_operations,
                  multiple = TRUE,
                  options = list("actions-box" = TRUE,
                                 "live-search" = TRUE,
                                 "live-search-normalize" = TRUE,
                                 "live-search-placeholder" = "Search here..."))
    })

    output$selected_trenches <- renderUI({
      validate(
        need(react_index(), "No project selected.")
      )
      pickerInput(inputId = "selected_trenches",
                  label = "Choose one or more Trenches to work with",
                  choices = trenches(),
                  selected = selection_settings$selected_trenches,
                  multiple = TRUE,
                  options = list("actions-box" = TRUE,
                                 "live-search" = TRUE,
                                 "live-search-normalize" = TRUE,
                                 "live-search-placeholder" = "Search here..."))
    })

    # TODO: try to get a periods list
    # this is not going to work
    #react_periods <- reactive({
    #  validate(
    #    need(react_index(), "No project selected.")
    #  )
    #  config <- get_configuration(login_connection, projectname = input$selected_project)
    #  return(react_periods)
    #})


    observeEvent(input$close_app,{
      selection_settings <- list("selected_project" = input$selected_project,
                                 "selected_operations" = input$selected_operations,
                                 "selected_trenches" = input$selected_trenches)
      saveRDS(selection_settings, "defaults/selection_settings.RDS")
      print("Shiny: EXIT")
      stopApp()
    })
  }




  # server code only for overview pages
  db_overview_server("db_overview")

  worflow_tab_server("workflow")


  # general finds plot
  all_finds_server("all_finds")
  basic_quant_server("all_finds_quant")

  # server code for pottery form (single)
  barplot_server("pottery", resource_category = "Pottery")
  potteryQA_server("pottery_QA")
  potteryQB_server("pottery_QB")

  # server code for bricks
  barplot_server("bricks", resource_category = "Brick")
  bricksQ_server("bricksQ_bar")

  # server code for coins
  barplot_server("coins", resource_category = "Coin")
  aoristic_server("coins_aor", resource_category = "Coin")

  # loomweights
  loomweights_server("lw_hist")
  barplot_server("lw_bar", resource_category = "Loomweight")

  # all other (automated) finds
  barplot_server("bone", resource_category = "Bone")
  barplot_server("glass", resource_category = "Glass")
  barplot_server("lamps", resource_category = "Lamp")
  barplot_server("metal", resource_category = "Metal")
  barplot_server("plaster", resource_category = "PlasterFragment")
  barplot_server("sculpture", resource_category = "Sculpture")
  barplot_server("stone", resource_category = "Stone")
  barplot_server("terracotta", resource_category = "Terracotta")




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

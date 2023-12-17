#' Produce the UI of the Login modal
#'
#' The modal pops up on Startup and prompts
#' IP-Adress and Password (as well as username,
#' but that is currently optional.)
#'
#' @return UI-Element modalDialog()
#' @export
make_login_dialog <- function() {
  modalDialog(
    title = "Enter connection details to continue",
    footer = actionButton("tab_connect.connect","Connect"),
    textInput("tab_connect.host","Host (Field Desktop)", value = "127.0.0.1"),
    textInput("tab_connect.user","Username", value = "username"),
    passwordInput("tab_connect.pwd","Password", value = "hallo"),
    tags$div(class = "warn-text",textOutput("tab_connect.error_msg"))
  )
}

#' Produce the modal which is used to indicate that the Index is loaded
#'
#' The modal pops up on Startup and prompts
#' IP-Adress and Password (as well as username,
#' but that is currently optional.)
#'
#' @param project name of the project
#'
#' @importFrom shinycssloaders withSpinner
#'
#' @return UI-Element modalDialog()
#' @export
make_busy_dialog <- function(project = "") {
  modalDialog(
    title = paste0("Loading index of project '",
                   project,
                   "', please wait..."),
    div(class = "warn-text", textOutput("load.error_msg") %>%
          withSpinner(image = "/img/quant-spinner-smooth.gif",
                      image.width = 120,
                      image.height = 120,
                      proxy.height = "100px",
                      color.background = "#ffffff")),
    footer = ""
  )
}

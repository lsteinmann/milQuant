#' Title
#'
#'  #TODO do you really need to do it like this...
#'
#' @return writes values to global variables needed for the app
#'
#' @importFrom grDevices colorRampPalette
#'
#' @export
app_onstart <- function() {
  # Set variables which are available at the beginning of all sessions

  startup_settings <<- read_milQuant_settings()

  message(milQ_message("Loaded all data, milQuant will start now."))
}

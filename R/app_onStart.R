#' Title
#'
#'  #TODO do you really need to do it like this...
#'
#' @return writes values to global variables needed for the app
#'
#' @export
app_onstart <- function() {
  # Set variables which are available at the beginning of all sessions

  ## Design

  uhhcol <- c("#e2001a", "#3b515b")
  uhhcol <<- colorRampPalette(uhhcol)

  uhhcol_two <- c("#3b515b", "#b6bfc3", "#e2001a", "#f07f8c", "#80b8dd", "#0271bb")
  uhhcol_two <<- colorRampPalette(uhhcol_two)


  data("milQuant_periods")
  periods <<- milQuant_periods$order

  period_colors <<- unname(unlist(milQuant_periods$colors))

  startup_settings <<- read_milQuant_settings()



  message(milQ_message("Loaded all data, milQuant will start now."))
}

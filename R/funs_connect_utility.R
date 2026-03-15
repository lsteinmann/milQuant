#' Try to get some data from the selected project
#'
#'
#' @param connection an object as returned by `idaifieldR::connect_idaifield()`
#' @param project the project name
#'
#' @return TRUE if succesfull, message (character) if not
#' @export
try_project_connection <- function(connection, project) {
  result <- tryCatch({
    connection$project <- project
    idf_ping(connection)
  }, warning = function(w) {
    conditionMessage(w)
    FALSE
  }, error = function(e) {
    conditionMessage(e)
    FALSE
  })

  return(result)
}

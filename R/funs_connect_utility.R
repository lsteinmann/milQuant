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
    client <- idaifieldR:::proj_idf_client(connection,
                                           include = "query",
                                           project = project)
    query <- paste0(
      '{
      "selector": { "resource.id": "project" },
      "fields": [ "resource.id", "resource.identifier" ]
       }')
    response <- idaifieldR:::response_to_list(client$post(body = query))
    project %in% unlist(response)
  }, warning = function(w) {
    conditionMessage(w)
  }, error = function(e) {
    conditionMessage(e)
  })

  return(result)
}

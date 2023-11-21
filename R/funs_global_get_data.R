#' get_index
#'
#' @param source
#'
#' @return
#' @export get_index
#'
#' @examples
get_index <- function(source = login_connection()) {
  options(digits = 20)

  uidlist <- get_field_index(source,
                          verbose = TRUE,
                          gather_trenches = TRUE) %>%
    mutate(Operation = ifelse(is.na(isRecordedIn),
                              liesWithin,
                              isRecordedIn)) %>%
    mutate(Operation = ifelse(category %in% c("Type","TypeCatalog"),
                              "Typenkatalog",
                              Operation)) %>%
    mutate(Place = ifelse(category %in% c("Type","TypeCatalog"),
                          "Typenkatalog",
                          Place)) %>%
    mutate(Operation = ifelse(is.na(Operation),
                              "none",
                              Operation))
  return(uidlist)
}


#' idf_uid_query
#'
#' @param login_connection
#' @param uids
#'
#' @return
#' @export
#'
#' @examples
idf_uid_query <- function(login_connection, uids) {
  message("Started the query...")
  query <- paste('{ "selector": { "_id": { "$in": [',
                 paste0('"', uids, '"', collapse = ", "),
                 '] } }}',
                 sep = "")
  proj_client <- idaifieldR:::proj_idf_client(login_connection,
                                              include = "query")
  message("Loading...")

  response <- proj_client$post(body = query)
  response <- idaifieldR:::response_to_list(response)
  message("Done.\nProcessing...")

  result <- lapply(response$docs,
                   function(x) list("id" = x$resource$id, "doc" = x))


  result <- idaifieldR:::name_docs_list(result)
  result <- idaifieldR:::type_to_category(result)

  new_names <- lapply(result, function(x) x$identifier)
  new_names <- unlist(new_names)
  names(result) <- new_names

  message("Done.")

  attr(result, "connection") <- login_connection
  attr(result, "projectname") <- login_connection$project
  message("Getting config at this point:")
  attr(result, "config") <- get_configuration(login_connection)

  result <- structure(result, class = "idaifield_docs")
  return(result)
}


#' get_resources
#'
#' @param resource_category
#'
#' @return
#' @export
#'
#' @examples
get_resources <- function(resource_category = find_categories) {
  message("Invalidating and querying DB now:")
  uids <- react_index() %>%
    filter(isRecordedIn %in% db_trenches()) %>%
    filter(category %in% resource_category) %>%
    pull(UID)

  message(paste0("Getting ", length(uids), " resources of category: ",
                 paste(resource_category, collapse = ", "), "..."))

  selected <- idf_uid_query(login_connection(), uids)
  message("Processing data (simplify_idaifield(), prep_for_shiny()).\nMay point out possible problems:")
  selected <- selected %>%
    simplify_idaifield(uidlist = react_index(),
                       keep_geometry = FALSE, replace_uids = TRUE) %>%
    prep_for_shiny(reorder_periods = reorder_periods)

  message("Done.")
  selected
}


#' Download the index
#'
#' Get the index from a project db using a connection object with
#' `idaifieldR::get_field_index()` and prepares it for use in milQuant.
#'
#' @param connection an object as returned by `idaifieldR::connect_idaifield()`
#'
#' @return a data.frame with the index
#'
#' @export
get_index <- function(connection = "") {
  options(digits = 20)

  index <- get_field_index(connection,
                           verbose = TRUE,
                           gather_trenches = TRUE) %>%
    mutate(Operation = ifelse(is.na(isRecordedIn),
                              liesWithin,
                              isRecordedIn)) %>%
    mutate(Operation = ifelse(category %in% c("Type", "TypeCatalog"),
                              "Typenkatalog",
                              Operation)) %>%
    mutate(Place = ifelse(category %in% c("Type", "TypeCatalog"),
                          "Typenkatalog",
                          Place)) %>%
    mutate(Operation = ifelse(is.na(Operation),
                              "none",
                              Operation))
  return(index)
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


#' Compiles a vector of Places which contain finds
#'
#' Uses the index to build said vector. This way, Places which do not
#' contain any finds are not listed in the UI-element. milQuant obviously
#' needs find- and quantification-resources to work.
#'
#' @param index
#'
#' @return a vector of Place identifiers
#' @export
get_list_of_places_with_finds <- function(index) {
  data("milQuant_cats")
  tmp_places <- index %>%
    filter(category %in% c(milQuant_cats$Finds, milQuant_cats$Quantifications)) %>%
    pull(Place) %>%
    unique()

  tmp_places <- tmp_places[!is.na(tmp_places)]
  places <- sort(tmp_places)
  return(tmp_places)
}

#' Compiles a vector of Operations in the selected Place
#'
#' Uses the index to build said vector. Filters for Operations that are
#' children of the selected places.

#' @param index
#' @param selected_operations
#'
#' @return a vector of operation identifiers
#' @export
get_list_of_operations_in_places <- function(index, selected_places) {
  if ("select everything" %in% selected_places) {
    tmp_operation <- index %>%
      pull(isRecordedIn) %>%
      unique()
  } else {
    tmp_operation <- index %>%
      filter(Place %in% selected_places) %>%
      pull(isRecordedIn) %>%
      unique()
  }

  tmp_operation <- tmp_operation[!is.na(tmp_operation)]
  operation <- sort(tmp_operation)
  return(tmp_operation)
}


#' Alternative to gather trenches for milQuant
#'
#'
# connection <- connect_idaifield(project = "milet", pwd = "hallo")
# uidlist <- get_field_index(connection)

alt_gather_trenches <- function(uidlist) {
  gather_mat <- as.data.frame(matrix(ncol = 1, nrow = nrow(uidlist)))
  colnames(gather_mat) <- c("identifier")

  places <- uidlist$identifier[which(uidlist$category == "Place")]

  gather_mat$identifier <- uidlist$identifier
  gather_mat$category <- uidlist$category

  parents <- ifelse(is.na(uidlist$isRecordedIn),
                    uidlist$liesWithin,
                    uidlist$isRecordedIn)
  gather_mat$Operation <- parents
  parents <- unique(parents)
  par_indices <- match(parents, uidlist$identifier)
  parents <- data.frame("parent" = parents,
                        "parent_category" = uidlist$category[par_indices])

  parents$container <- ifelse(is.na(uidlist$isRecordedIn[par_indices]),
                              uidlist$liesWithin[par_indices],
                              uidlist$isRecordedIn[par_indices])

  # all parents that are an operation on project level should be their own "place"
  operation_categories <- getOption("idaifield_categories")$operations
  ind <- is.na(parents$container) & parents$parent_category %in% operation_categories
  parents$container <- ifelse(ind, parents$parent, parents$container)

  catalog_categories <- getOption("idaifield_categories")$catalogues
  ind <- parents$parent_category %in% catalog_categories
  parents$container <- ifelse(ind, "TypeCatalog", parents$container)

  gather_mat$Place <- parents$container[match(gather_mat$Operation, parents$parent)]

  img <- c("Image", "Drawing", "Photo")
  gather_mat$Place <- ifelse(gather_mat$category %in% img, "Images", gather_mat$Place)
  gather_mat$Operation <- ifelse(gather_mat$category %in% img,
                                 gather_mat$category,
                                 gather_mat$Operation)

  gather_mat$Place <- ifelse(is.na(gather_mat$Place),
                             parents$container[match(gather_mat$identifier, parents$parent)],
                             gather_mat$Place)

  gather_mat$category <- NULL

  return(gather_mat)
}


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
get_index <- function(connection) {
  options(digits = 20)

  index <- get_field_index(connection,
                           verbose = TRUE,
                           find_layers = TRUE,
                           gather_trenches = FALSE)
  index <- index %>%
    left_join(alt_gather_trenches(index))


  # everything will need an individual fix for surveys or the plots will
  # never work!
  # which(index$category == "Survey")
  # TODO: get all items that are recorded in a survey and make SurveyArea the
  # value in "isRecordedIn" while SurveyUnit is the value in "liesWithin"
  # .... probably.

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
  message("Invalidating and querying DB now:")
# conn <- connect_idaifield(project = "milet", pwd = "hallo")
# index <- get_field_index(conn)
# index <- index %>% left_join(alt_gather_trenches(index))
# react_index <- function() { index }
# db_selected_operations <- function() { unique(index$isRecordedIn) }
# db_selected_operations()
# login_connection <- function () { conn }
# fields <- c("period", "dating")
# uids <- unique(index$UID)
get_resources <- function(resource_category = "Pottery", fields = "all") {
  uids <- react_index() %>%
    filter(isRecordedIn %in% db_selected_operations()) %>%
    filter(category %in% resource_category) %>%
    pull(UID)

  message(paste0("Getting ", length(uids), " resources of category: ",
                 paste(resource_category, collapse = ", "), "..."))

  q_selector <- paste0('"selector": { "resource.id": { "$in": [',
                       paste0('"', uids, '"', collapse = ", "),
                       '] } }')
  if (length(fields) == 1 && fields == "all") {
    f_selector <- ""
  } else {
    fields <- c("identifier", "category", "type",
                "relations.liesWithin", "relations.isRecordedIn",
                fields)
    fields <- paste0("resource.", fields)
    f_selector <- paste0(', "fields": [ ',
                         paste0('"', fields, '"', collapse = ", "),
                         ' ]')
  }

  query <- paste0('{ ', q_selector, f_selector, ' }')

  selected <- idf_json_query(login_connection(), query = query)

  message("Processing data (simplify_idaifield(), prep_for_shiny()).\nMay point out possible problems:")
  result <- selected %>%
    simplify_idaifield(uidlist = react_index(),
                       keep_geometry = FALSE,
                       find_layers = TRUE,
                       replace_uids = TRUE) %>%
    prep_for_shiny(reorder_periods = reorder_periods)

  message("Done.")
  return(result)
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
    filter(category %in% c(milQuant_cats$Find, milQuant_cats$Quantification)) %>%
    pull(Place) %>%
    unique()

  tmp_places <- tmp_places[!is.na(tmp_places)]
  places <- sort(tmp_places)

  return(places)
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
  operations <- sort(tmp_operation)

  return(operations)
}

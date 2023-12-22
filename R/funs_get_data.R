#' Prep for shiny
#'
#' @param data data (list) from get_idaifield_docs
#' @param reorder_periods should periods be an ordered factor? (only if is_milet)
#'
#' @importFrom utils type.convert
#'
#' @return a data.frame
#' @export
prep_for_shiny <- function(data, reorder_periods = reorder_periods) {
  data <- data %>%
    idaifield_as_matrix() %>%
    as.data.frame()

  if (is.null(data$relation.liesWithin)) {
    data$relation.liesWithin <- NA
  }
  if (is.null(data$relation.liesWithinLayer)) {
    data$relation.liesWithinLayer <- "NA"
  }

  data <- data %>%
    mutate(relation.liesWithinLayer = na_if(relation.liesWithinLayer, "NA")) %>%
    mutate(relation.liesWithinLayer = ifelse(is.na(relation.liesWithinLayer),
                                             relation.liesWithin,
                                             relation.liesWithinLayer)) %>%
    mutate(relation.liesWithinLayer = ifelse(is.na(relation.liesWithinLayer),
                                             relation.isRecordedIn,
                                             relation.liesWithinLayer)) %>%
    remove_na_cols() %>%
    type.convert(as.is = FALSE)

  tryCatch({
    data <- data %>%
      mutate(date = as.Date(as.character(date), format = "%d.%m.%Y"))
  }, error = function(e) message(paste("Caught problem in prep_for_shiny(): ", e)))

  tryCatch({
    data <- data %>%
      mutate(beginningDate = as.Date(as.character(beginningDate), format = "%d.%m.%Y"))
  }, error = function(e) message(paste("Caught problem in prep_for_shiny(): ", e)))

  tryCatch({
    data <- data %>%
      mutate(endDate = as.Date(as.character(endDate), format = "%d.%m.%Y"))
  }, error = function(e) message(paste("Caught problem in prep_for_shiny(): ", e)))


  if(reorder_periods) {
    if (length(data$period) == 0) {
      data$period <- NA
    }
    if (length(data$period.start) == 0) {
      data$period.start <- NA
    }
    if (length(data$period.end) == 0) {
      data$period.end <- NA
    }

    milQuant_periods <- milQuant_periods

    data <- data %>%
      mutate_at(c("period", "period.end", "period.start"), as.character) %>%
      # fix value for periods that have been assigned multiple periods
      # TODO i need to think of something better here, it is horrible
      mutate(period = ifelse(grepl(pattern = ";", period), "multiple", period)) %>%
      # assign "unbestimmt" instead of NA to make period picker work
      # TODO i need to think of something better here as well
      mutate(period = ifelse(is.na(period), "unbestimmt", period)) %>%
      mutate(period.end = ifelse(is.na(period.end), "unbestimmt", period.end)) %>%
      mutate(period.start = ifelse(is.na(period.start), "unbestimmt", period.start)) %>%
      mutate(period = factor(period,
                             levels = levels(milQuant_periods$order),
                             ordered = TRUE),
             period.end = factor(period.end,
                                 levels = levels(milQuant_periods$order),
                                 ordered = TRUE),
             period.start = factor(period.start,
                                   levels = levels(milQuant_periods$order),
                                   ordered = TRUE))
  }


  return(data)
}




#' Alternative to gather trenches for milQuant
#'
#' @param uidlist index of the project db
#'
#' @return the index with Places
#'
#' @export
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
#' @param login_connection connect_idaifield()-object
#' @param uids vector of UUIDs that should be downloaded
#'
#' @return a docs list with the resources of said UUIDs
#' @export
idf_uid_query <- function(login_connection, uids) {
  message(milQ_message("Started the query..."))
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



#' Title
#'
#' @param resource_category see milQuant_cats
#' @param fields fields to get from the db
#' @param liesWithinLayer if a vector of layers, will be filtered by finds from those layers according to index
#' @param prep_for_shiny should prep_for_shiny() be run?
#'
#' @return depending on prep_for_shiny, either a list or a data.frame
#' @export
get_resources <- function(resource_category = "Pottery",
                          fields = "all",
                          liesWithinLayer = NULL,
                          prep_for_shiny = TRUE) {
  message(milQ_message("Invalidating and querying DB now:"))

  if (!is.null(liesWithinLayer)) {
    uids <- react_index() %>%
      filter(isRecordedIn %in% db_selected_operations()) %>%
      filter(liesWithinLayer %in% liesWithinLayer) %>%
      filter(category %in% resource_category) %>%
      pull(UID)
  } else {
    uids <- react_index() %>%
      filter(isRecordedIn %in% db_selected_operations()) %>%
      filter(category %in% resource_category) %>%
      pull(UID)
  }



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
                       spread_fields = prep_for_shiny,
                       find_layers = TRUE,
                       replace_uids = TRUE)

  if (prep_for_shiny == TRUE) {
    result <- result %>%
    prep_for_shiny(reorder_periods = reorder_periods)
  }

  message(milQ_message("Done."))
  return(result)
}


#' Compiles a vector of Places which contain finds
#'
#' Uses the index to build said vector. This way, Places which do not
#' contain any finds are not listed in the UI-element. milQuant obviously
#' needs find- and quantification-resources to work.
#'
#' @param index index of the db
#'
#' @return a vector of Place identifiers
#' @export
get_list_of_places_with_finds <- function(index) {
  milQuant_cats <- milQuant_cats
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

#' @param index the index of the db
#' @param selected_places the places selected in the input
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


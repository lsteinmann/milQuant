#' Prep for shiny
#'
#' @param data data (list) from get_idaifield_docs
#' @param reorder_periods should periods be an ordered factor? (only if is_milet)
#'
#' @return a data.frame
#' @export
prep_for_shiny <- function(data, reorder_periods = reorder_periods) {
  data <- data %>%
    idaifield_as_matrix() %>%
    as.data.frame() %>%
    left_join(select(react_index(),
                     c("identifier", "liesWithinLayer", "Operation", "Place")),
              by = join_by(identifier)) %>%
    rename(relation.liesWithinLayer = liesWithinLayer)

  # I am removing this to see if it had any sense or not!
  #if (is.null(data$relation.liesWithin)) {
  #  data$relation.liesWithin <- NA
  #}
  #if (is.null(data$relation.liesWithinLayer)) {
  #  # was: data$relation.liesWithinLayer <- "NA" # why was I doing this?
  #  data$relation.liesWithinLayer <- NA
  #}
  #
  #data <- data %>%
  #  # Why am I doing this?
  #  mutate(relation.liesWithinLayer = na_if(relation.liesWithinLayer, "NA")) %>%
  #  mutate(relation.liesWithinLayer = ifelse(is.na(relation.liesWithinLayer),
  #                                           relation.liesWithin,
  #                                           relation.liesWithinLayer)) %>%
  #  mutate(relation.liesWithinLayer = ifelse(is.na(relation.liesWithinLayer),
  #                                           relation.isRecordedIn,
  #                                           relation.liesWithinLayer)) %>%
  data <- data %>%
    remove_na_cols() %>%
    type.convert(as.is = FALSE)

  tryCatch({
    data <- data %>%
      mutate(date.start = as.Date(as.character(date.start), format = "%d.%m.%Y")) %>%
      mutate(date.end = as.Date(as.character(date.end), format = "%d.%m.%Y"))
  }, error = function(e) message(paste("Caught date problem in prep_for_shiny(): ", e)))


  if(reorder_periods) {
    # if the column does not exist, it will be NULL of length 0; we need the
    # columns to exist for most plots, so we just pretend here:
    if (length(data$period.start) == 0) {
      data$period.start <- NA
    }
    if (length(data$period.end) == 0) {
      data$period.end <- NA
    }
    data <- data %>%
      mutate_at(c("period.end", "period.start"), as.character) %>%
      # add "multiple" as the value when start and end are not the same
      mutate(period = ifelse(period.start == period.end, period.end, "multiple")) %>%
      # the period picket will not work with NA, we have to use "unbestimmt"
      mutate(period = ifelse(is.na(period), "unbestimmt", period)) %>%
      mutate(period.end = ifelse(is.na(period.end), "unbestimmt", period.end)) %>%
      mutate(period.start = ifelse(is.na(period.start), "unbestimmt", period.start)) %>%
      # finally, we also need to make all of them an ordered factor
      mutate(period = factor(period,
                             levels = levels(periods),
                             ordered = TRUE),
             period.end = factor(period.end,
                                 levels = levels(periods),
                                 ordered = TRUE),
             period.start = factor(period.start,
                                   levels = levels(periods),
                                   ordered = TRUE))
  }


  return(data)
}




#' Alternative to gather trenches for milQuant
#'
#' @param uidlist index of the project db
#'
#' @return the index with Places
#' @export
#'
#' @examples
#' connection <- connect_idaifield(project = "milet", pwd = "hallo")
#' uidlist <- get_field_index(connection)
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
  index <- get_field_index(connection,
                           verbose = TRUE,
                           find_layers = TRUE,
                           gather_trenches = FALSE)
  index <- index %>%
    left_join(alt_gather_trenches(index), by = join_by(identifier))


  # everything will need an individual fix for surveys or the plots will
  # never work!
  # which(index$category == "Survey")
  # TODO: get all items that are recorded in a survey and make SurveyArea the
  # value in "isRecordedIn" while SurveyUnit is the value in "liesWithin"
  # .... probably.

  return(index)
}

#' Get Resources
#'
#' @param resource_category see milQuant_cats
#' @param fields fields to get from the db
#' @param selected_layers if a vector of layers, will be filtered by finds from those layers according to index
#' @param prep_for_shiny should prep_for_shiny() be run?
#'
#' @return depending on prep_for_shiny, either a list or a data.frame
#' @export
#'
#' @examples
#' conn <- connect_idaifield(project = "milet", pwd = "hallo")
#' index <- get_field_index(conn)
#' index <- index %>% left_join(alt_gather_trenches(index))
#' react_index <- function() { index }
#' db_selected_operations <- function() { unique(index$isRecordedIn) }
#' db_selected_operations()
#' login_connection <- function () { conn }
#' fields <- c("period", "dating")
#' uids <- unique(index$UID)
get_resources <- function(resource_category = "Pottery",
                          fields = "all",
                          selected_layers = NULL,
                          prep_for_shiny = TRUE) {
  message(milQ_message("Invalidating and querying DB now:"))

  if (!is.null(selected_layers)) {
    uids <- react_index() %>%
      filter(isRecordedIn %in% db_selected_operations()) %>%
      filter(liesWithinLayer %in% selected_layers) %>%
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
    if ("date" %in% fields) {
      fields <- c(fields, "beginningDate", "endDate")
    }
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

  message("Processing data (simplify_idaifield()) ...")
  result <- selected %>%
    simplify_idaifield(index = react_index(),
                       config = react_config(),
                       keep_geometry = FALSE,
                       find_layers = FALSE,
                       silent = TRUE)

  if (prep_for_shiny == TRUE) {
    message("Processing data (prep_for_shiny()) ...")
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


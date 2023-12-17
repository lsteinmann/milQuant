
#' apply the period filter if the config is milet
#'
#'
#'
#' @param find_df df of find resources to select for periods
#' @param is_milet on project milet?
#' @param selector the list from the associated input
#'
#' @return the df, selected
#'
#' @export
period_filter <- function(find_df, is_milet = FALSE, selector = NULL) {
  if (is_milet) {
    find_df <- find_df %>%
      filter(period.start >= selector[1] & period.end <= selector[2])  %>%
      filter(period.end <= selector[2])
  }
  return(find_df)
}


#' derive_dating_from_periods
#'
#' #TODO use this from chrongler
#'
#' @param data data.frame with the resources to be transformed
#'
#' @return data.frame with absolute values for dating where they were empty previously
#' @export
derive_dating_from_periods <- function(data) {
  data("milQuant_periods")

  needed_cols <- c("period.start", "period.end",
                   "dating.min", "dating.max",
                   "dating.source")

  if (!all(needed_cols %in% colnames(data))) {
    warning(milQ_warning("Cannot derive dating from periods because columns are missing!"))
    return(data)
  } else if (is_milet == FALSE) {
    warning(milQ_warning("Deriving dating from periods only works with the milet-config!"))
    return(data)
  }

  new_dating <- apply(data, MARGIN = 1, FUN = function(x) {
    x <- as.list(x)
    new <- list()
    min <- milQuant_periods$dating[[x$period.start]]$dating.min
    min <- as.numeric(min)
    new$new.dating.min <- ifelse(is.numeric(min), min, NA)
    max <- milQuant_periods$dating[[x$period.end]]$dating.max
    max <- as.numeric(max)
    new$new.dating.max <- ifelse(is.numeric(max), max, NA)
    new
  })
  new_dating <- do.call(rbind.data.frame, new_dating)

  min_na <- which(is.na(data$dating.min) & !is.na(data$dating.max) & !is.na(new_dating$new.dating.min))
  max_na <- which(!is.na(data$dating.min) & is.na(data$dating.max) & !is.na(new_dating$new.dating.max))

  complete_na <- is.na(data$dating.min) & is.na(data$dating.max)

  replace <- which(complete_na & !is.na(new_dating$new.dating.min))
  data[c(replace, min_na), "dating.min"] <- new_dating[c(replace, min_na), c("new.dating.min")]
  data[c(replace, max_na), "dating.max"] <- new_dating[c(replace, max_na), c("new.dating.max")]
  data$dating.source <- as.character(data$dating.source)
  data[replace, "dating.source"] <- "Derived from period"
  data[c(min_na, max_na), "dating.source"] <- "Partially derived from period"


  return(data)
}

#' multiply_resources_by_period_range
#'
#' #TODO use this from chrongler
#'
#' @param data data.frame with the resources to be transformed
#' @param split TRUE/FALSE. A way out to not multiply while still calling the function
#'
#' @return data.frame with duplicated rows and counts devided by nr of duplicates
#'
#' @export
multiply_resources_by_period_range <- function(data, split = TRUE) {

  if (split == FALSE) {
    # a way out to switch of this function when the switch input is disable
    return(data)
  }

  data("milQuant_periods")

  needed_cols <- c("period", "period.start", "period.end")

  if (!all(needed_cols %in% colnames(data))) {
    warning(milQ_warning("Cannot derive dating from periods because columns are missing!"))
    return(data)
  } else if (is_milet == FALSE) {
    warning(milQ_warning("Deriving dating from periods only works with the milet-config!"))
    return(data)
  }
  new_data <- split(data, data$period)

  if ("multiple" %in% names(new_data)) {

    if (nrow(new_data$multiple) == 0) {
      return(data)
    }

    multiple <- split(new_data$multiple, seq_len(nrow(new_data$multiple)))
    names(multiple) <- new_data$multiple$identifier

    multiple <- lapply(multiple, function (x) {
      #x$period.start <- ordered(x$period.start, levels = milQuant_periods$order)
      #x$period.end <- ordered(x$period.end, levels = milQuant_periods$order)
      sequence <- seq(from = as.numeric(x$period.start), to = as.numeric(x$period.end))
      new_periods <- milQuant_periods$order[sequence]
      repl <- rep(list(x), length(new_periods))
      repl <- do.call(bind_rows, repl)
      repl$period <- new_periods
      repl[, grepl("count", colnames(repl))] <- repl[, grepl("count", colnames(repl))] / length(new_periods)
      repl
    })
    multiple <- do.call(bind_rows, multiple)

    new_data$multiple <- multiple

    new_data <- do.call(bind_rows, new_data)

    return(new_data)
  } else {
    message("No 'multiple' available in period-column.")
    return(data)
  }
}

#' add_all_periods_to_all_contexts
#'
#' @param data data.frame with the resources to be transformed
#'
#' @return data.frame where each context has an empty row for earch period (to make it possible to use the same legend in plotly)
#' @export
add_all_periods_to_all_contexts <- function(data) {
  needed_cols <- c("relation.liesWithinLayer", "period")

  if (!all(needed_cols %in% colnames(data))) {
    warning(milQ_warning("Cannot add periods because columns are missing!"))
    return(data)
  } else if (is_milet == FALSE) {
    warning(milQ_warning("Adding periods only works with the milet-config!"))
    return(data)
  }
  sp_data <- split(data, data$relation.liesWithinLayer)

  data("milQuant_periods")

  sp_data <- lapply(sp_data, function (x) {
    ind <- which(!milQuant_periods$order %in% unique(x$period))
    if (length(ind) > 0) {
      to_add <- matrix(ncol = ncol(x), nrow = length(ind))
      to_add[,] <- 0
      colnames(to_add) <- colnames(x)
      to_add <- as.data.frame(to_add)
      to_add$relation.liesWithinLayer <- unique(x$relation.liesWithinLayer)
      to_add$period <- milQuant_periods$order[ind]
      new <- bind_rows(x, to_add)
      return(new)
    } else {
      return(x)
    }
  })
  result <- do.call(bind_rows, sp_data)
  return(result)
}



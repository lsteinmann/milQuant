#' Make warnings more visible
#'
#' @param msg
#'
#' @return
#' @export
#'
#' @examples
milQ_warning <- function(msg) {
  msg <- paste(crayon::cyan("milQuant: "), crayon::red(crayon::bold("!  ")), msg)
  msg
}

#' Make messages more visible
#'
#' @param msg
#'
#' @return
#' @export
#'
#' @examples
milQ_message <- function(msg) {
  msg <- paste(crayon::cyan("milQuant: "), crayon::white(crayon::bold("i  ")), msg)
  msg
}

#' Helper to remove columns that are empty
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
remove_na_cols <- function(data) {
  na_cols <- apply(data, function(x) all(is.na(x)), MARGIN = 2)
  data <- data[, !na_cols]
  return(data)
}


#' Read the selection settings from the sys.file
#'
#' This is used to make the selection settings restorable (i.e. which
#' Operation / Place / project was selected when milQuant was closed using
#' the save & close button)
#'
#' @return a list with the settings
#' @export
read_milQuant_settings <- function() {
  result <- try(readRDS(system.file(package = "milQuant", mustWork = TRUE,
                                    "app/www/settings/db_settings.RDS")),
                silent = TRUE)
  if (inherits(result, "try-error")) {
    warning(paste("Some error in read_milQuant_settings(): ", result))
    result <- list("selected_project" = NULL,
                   "selected_places" = NULL,
                   "selected_trenches" = NULL,
                   "selected_categories" = NULL)
  }
  return(result)
}

#' derive_dating_from_periods
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
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
#' @param data
#'
#' @return
#' @export
#'
#' @examples
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
#' @param data
#'
#' @return
#' @export
#'
#' @examples
add_all_periods_to_all_contexts <- function(data) {
  needed_cols <- c("relation.liesWithinLayer", "period")

  if (!all(needed_cols %in% colnames(data))) {
    warning(milQ_warning("Cannot derive dating from periods because columns are missing!"))
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




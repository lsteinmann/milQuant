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


# data <- result

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

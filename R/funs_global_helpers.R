#' Prep for shiny
#'
#' @param data
#' @param reorder_periods
#'
#' @return
#' @export
#'
#' @examples
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




#' Returns the UIDs of entries belonging to the variable in question
#'
#' @param filter_operation
#' @param index
#'
#' @return
#' @export
#'
#' @examples
uid_by_operation <- function(filter_operation = "all",
                             index = react_index()) {

  if (filter_operation == "none") {
    parent_category <- "unknown"
  } else {
    parent_category <- index$category[index$identifier == filter_operation]
  }
  if (filter_operation == "all") {
    uid_filter <- index$UID
  } else if (parent_type == "Place") {
    uid_filter <- index %>%
      filter(Place == filter_operation) %>%
      pull(UID)
  } else if (filter_operation == "none") {
    uid_filter <- index %>%
      filter(Operation == filter_operation) %>%
      pull(UID)
  } else {
    uid_filter <- index %>%
      filter(isRecordedIn == filter_operation) %>%
      pull(UID)
  }
  return(uid_filter)
}


#' apply the period filter if the config is milet
#'
#' @param find_df
#' @param is_milet
#' @param selector
#'
#' @return
#' @export
#'
#' @examples
period_filter <- function(find_df, is_milet = FALSE, selector = NULL) {
  if (is_milet) {
    find_df <- find_df %>%
      filter(period.start >= selector[1] & period.end <= selector[2])  %>%
      filter(period.end <= selector[2])
  }
  return(find_df)
}

#' mq_spinner
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
mq_spinner <- function(object) {
  withSpinner(object,
              image = "img/quant-spinner-smooth.gif",
              image.width = 100,
              image.height = 100)
}


#' milquant_plotly_layout
#'
#' @param plotly_fig
#' @param caption
#'
#' @return
#' @export
#'
#' @examples
milquant_plotly_layout <- function(plotly_fig, caption = FALSE) {
  plotly_fig <- plotly_fig %>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("select2d", "lasso2d")) %>%
    layout(yaxis = list(tickmode = "auto", showline = FALSE, gridwidth = 3,
                        gridcolor = "grey20"),
           xaxis = list(gridcolor = "grey60"),
           title = list(xanchor = "left", x = 0,
                        pad = list(b = 70),
                        yref = "container", automargin = TRUE),
           showlegend = TRUE)

  if (is.character(caption)) {
    plotly_fig <- plotly_fig %>%
      layout(annotations = list(text = caption,
                                xref = "paper", yref = "paper",
                                xanchor = "right", yanchor = "top",
                                x = 1, y = 1,
                                showarrow = FALSE,
                                bgcolor = "white",
                                font = list(size = 10)))
  }

  return(plotly_fig)
}


#' convert_to_Plotly
#'
#' @param ggplot_p
#' @param source
#' @param tooltip
#'
#' @return
#' @export
#'
#' @examples
convert_to_Plotly <- function(ggplot_p,
                               source = "ggplot_source",
                               tooltip = c("y", "x", "fill")) {
  # add theme
  ggplot_p <- ggplot_p + Plot_Base_Theme

  plot_ly <- ggplotly(ggplot_p, source = source, tooltip = tooltip) %>%
    layout(title = list(text = paste0(ggplot_p$labels$title, "<br>",
                                      "<sup>", ggplot_p$labels$subtitle, "</sup>")))
  plot_ly <- milquant_plotly_layout(plot_ly, caption = ggplot_p$labels$caption)
  return(plot_ly)
}




#' milquant_plotly_layout
#'
#' general layout parameters for all plotly plots
#'
#' @param plotly_fig a plotly plot
#' @param caption the caption that should be attached
#'
#' @return a plotly plot
#' @export
milquant_plotly_layout <- function(plotly_fig, caption = FALSE) {
  plotly_fig <- plotly_fig %>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("select2d", "lasso2d")) %>%
    layout(yaxis = list(tickmode = "auto", showline = FALSE, gridwidth = 3,
                        gridcolor = "grey20"),
           xaxis = list(gridcolor = "grey60"),
           title = list(xanchor = "left", x = 0,
                        pad = list(b = 70),
                        yref = "container", automargin = TRUE))

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


#' Title
#'
#' get the plottable variables from the config to preselect for fields that may otherwise be impossible to plot
#'
#' @param resource_category category of resources to get vars for
#' @param colnames colnames in the data
#' @param type which sort of vars for plotting should be selected
#'
#' @return a vector of possible variables
#' @export
get_plot_vars <- function(resource_category, colnames, type = "categorical") {

  match.arg(type, choices = c("categorical", "numeric", "textual"), several.ok = TRUE)

  switch (type,
          categorical = posInputTypes <- c("dropdown", "radio", "boolean"),
          numeric = posInputTypes <- c("unsignedInt", "radio", "unsignedFloat", "date", "float"),
          textual = posInputTypes <- c("input", "multiInput", "simpleInput", "literature")
  )

  switch (type,
          categorical = default <- c("category", "Operation", "Place", "storagePlace", "relation.liesWithinLayer", "period.start", "period.end", "period"),
          numeric = default <- c("dating.min", "dating.max"),
          textual = default <- c("processor")
  )

  data("milQuant_inputTypes")
  data("milQuant_cats")

  parent <- lapply(milQuant_cats, function(x) {
    any(resource_category %in% x)
  })
  parent <- names(parent[unlist(parent)])

  useable <- milQuant_inputTypes %>%
    filter(category %in% c(parent, resource_category)) %>%
    filter(inputType %in% posInputTypes) %>%
    pull(field)


  useable <- c(default, useable)

  result <- colnames[colnames %in% useable]
  result <- sort(result)

  return(result)
}

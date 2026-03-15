#' milquant_plotly
#'
#' general layout parameters for all plotly plots
#'
#' @param plotly_fig a plotly plot
#'
#' @return a plotly plot
#' @export
milquant_plotly <- function(plotly_fig) {
  plotly_fig %>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("select2d", "lasso2d"))
}
#' add caption to plotly layout in upper right corner
#'
#'
#' @param caption chr (content of caption)
#'
#' @return a plotly plot
#' @export
add_caption <- function(plotly_fig, caption = "") {
  plotly_fig %>%
    layout(annotations =
             list(text = caption,
                  xref = "paper", yref = "paper",
                  xanchor = "right", yanchor = "top",
                  x = 1, y = 1,
                  showarrow = FALSE,
                  bgcolor = "white",
                  font = list(size = 10)
              )
           )
}
#' add title to plotly layout in upper left corner
#'
#'
#' @param title chr (content of title)
#'
#' @return a plotly plot
#' @export
add_title <- function(plotly_fig, title = "") {
  plotly_fig %>%
    layout(
      title = list(
        text = title,
        xref = "paper", yref = "container",
        xanchor = "left", yanchor = "top",
        x = 0, y = 0.98,
        pad = list(t = 10, b = 10, l = 0, r = 0)),
      margin = list(t = 50)
    )
}

#' use bottom legend
#'
#' @param legend_title chr (content of title)
#'
#' @return a plotly plot
#' @export
add_legend <- function(plotly_fig, title = "", position = "bottom", mb = 100, y = -0.2) {
  if (position == "bottom") {
    plotly_fig %>%
      layout(legend = list(title = list(text = title),
                           orientation = "h",   # show entries horizontally
                           xanchor = "center",  # use center of legend as anchor
                           x = 0.5, y = y),
             margin = list(b = mb))
  } else if (position == "right") {
    plotly_fig %>%
      layout(legend = list(title = list(text = title)),
             margin = list(r = 50)
             )
  }
}

#' Title
#'
#' get the plottable variables from the config to preselect for fields that may otherwise be impossible to plot
#'
#' @param resource_categories category of resources to get vars for
#' @param colnames colnames in the data
#' @param type which sort of vars for plotting should be selected
#'
#' @return a vector of possible variables
#' @export
get_plot_vars <- function(resource_categories, colnames, type = "categorical") {

  match.arg(type, choices = c("categorical", "continuous", "textual"), several.ok = TRUE)

  switch (type,
          categorical = posInputTypes <- c("dropdown", "radio", "boolean"),
          continuous = posInputTypes <- c("unsignedInt", "unsignedFloat", "date", "float"),
          textual = posInputTypes <- c("input", "multiInput", "simpleInput", "literature")
  )

  switch (type,
          categorical = default <- c("category", "Operation", "Place", "storagePlace", "relation.liesWithinLayer", "period.start", "period.end", "period"),
          continuous = default <- c("dating.min", "dating.max"),
          textual = default <- c("processor")
  )
  useable <- react_inputtypes() %>%
    filter(category %in% resource_categories) %>%
    filter(inputType %in% posInputTypes) %>%
    pull(fieldname) %>%
    unique()

  useable <- c(default, useable)

  result <- colnames[colnames %in% useable]
  return(sort(result))
}

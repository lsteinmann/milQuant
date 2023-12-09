
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


#' Title
#' #TODO delete
#'
#' @param ncol
#'
#' @return
#' @export
#'
#' @examples
scale_fill_period <<- function(ncol = 9) {
  scale_fill_manual(values = period_colors,
                    breaks = periods,
                    limits = periods,
                    guide = guide_legend(name = "Period",
                                         ncol = ncol,
                                         byrow = TRUE))
}


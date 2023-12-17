#' Global Module: downloadPlotButtons (UI)
#'
#' @inherit mod_ui_doc
#'
#' @return A `div()` with a `downloadButton()`
#' @export
downloadPlotButtons <- function(id) {

  ns <- NS(id)

  div(
    #downloadButton(ns("dPNG"), "Download plot (PNG)"),
    #downloadButton(ns("dPDF"), "Download plot (PDF)"),
    downloadButton(ns("dHTML"), "Download plot (HTML)")
  )
}

#' Global Module: downloadPlotButtons (Server Code)
#'
#' @inherit mod_serv_doc
#' @param dlPlot The plot to be downloaded.
#'
#' @importFrom htmlwidgets saveWidget
#'
#' @export
makeDownloadPlotHandler <- function(id, dlPlot) {

  moduleServer(
    id,
    function(input, output, session) {

      #output$dPNG <- downloadHandler(
      #  filename = paste(format(Sys.Date(), "%Y%m%d"),
      #                   "_milQuant_plot.png", sep = ""),
      #  content <- function(file) {
      #    ggsave(file, plot = dlPlot() + Plot_Base_Theme + Plot_Base_Guide,
      #           device = "png",
      #           width = 25, height = 15, units = "cm")
      #  }
      #)

      #output$dPDF <- downloadHandler(
      #  filename = paste(format(Sys.Date(), "%Y%m%d"),
      #                   "_milQuant_plot.pdf", sep = ""),
      #  content <- function(file) {
      #    ggsave(file, plot = dlPlot() + Plot_Base_Theme + Plot_Base_Guide,
      #           device = "pdf",
      #           width = 25, height = 15, units = "cm")
      #  }
      #)

      output$dHTML <- downloadHandler(
        filename = paste(format(Sys.Date(), "%Y%m%d"),
                         "_milQuant_plot.html", sep = ""),
        content <- function(file) {
          saveWidget(as_widget(dlPlot()), file)
        }
      )

    }
  )
}

#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
downloadPlotButtons <- function(id) {

  ns <- NS(id)

  div(
    #downloadButton(ns("dPNG"), "Download plot (PNG)"),
    #downloadButton(ns("dPDF"), "Download plot (PDF)"),
    downloadButton(ns("dHTML"), "Download plot (HTML)")
  )
}

#' Title
#'
#' @param id
#' @param dlPlot
#'
#' @return
#' @export
#'
#' @examples
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
          htmlwidgets::saveWidget(as_widget(dlPlot()), file)
        }
      )

    }
  )
}

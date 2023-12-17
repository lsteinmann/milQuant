#' @keywords internal
#'
#' @importFrom shinydashboard box
#' @importFrom dplyr filter
#' @import plotly
#' @import shiny
#' @import shinydashboard
#' @import idaifieldR
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom utils data
#'
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' The UI of a Module
#'
#' @param id The namespace-id for this Module.
#' @param tabname Name of the tab used to link this module.
#'
#' @return UI for the Module
#'
#' @keywords internal
mod_ui_doc <- function(id, tabname) {}

#' The Server-Code of a Module
#'
#' @inheritParams mod_ui_doc
#' @param resource_category The category of *resource*s to be used in this module.
#' @param inputid The `inputId` that should be used for the UI-Element.
#' Specified here to avoid `ns()`-problems and keep track of the value easier.
#'
#' @return Server Code for the Module
#'
#' @keywords internal
mod_serv_doc <- function(id, resource_category) {}

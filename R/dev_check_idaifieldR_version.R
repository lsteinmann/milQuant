#' For checking while in development
#'
#' @return TRUE if there is no problem, FALSE if there is one
#' @keywords internal
#'
#' @examples
devel_idf_version <- function () {
  idfdevel <- grepl("9000", getNamespaceVersion("idaifieldR"))
  check <- check_idaifieldr_version(getNamespaceVersion("idaifieldR"))
  if (idfdevel) {
    msg <- paste(crayon::bold("You are working with a development-version of idaifieldR!"),
                 "Be sure to publish it before you publish milQuant.")
    msg <- msg %>%
      crayon::white() %>%
      crayon::bgRed()
    warning(msg)
    return(FALSE)
  } else if (check) {
    msg <- paste0(crayon::bold("You are working with idaifieldR v",
                               getNamespaceVersion("idaifieldR"), ". ",
                               "That is not the current release!"))
    msg <- msg %>%
      crayon::black() %>%
      crayon::bgCyan()
    warning(msg)
    return(FALSE)
  } else {
    return(TRUE)
  }
}


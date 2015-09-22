#' Launch VisualToolkit in the default browser
#'
#' @details See \url{http://www.google.com} for documentation and tutorials
#'
#' @examples
#' if (interactive()) {
#'   visualToolkit()
#' }
#' @export
visualToolkit <- function() {

  appDir <- system.file("visualToolkit", package = "visualToolkit")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")

}


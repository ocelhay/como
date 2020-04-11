################################################################################
#
#' Run the shiny app
#' 
#' @return Open browser
#' 
#' @examples
#' if(interactive()) comomodel()
#' 
#' @export
#'
#
################################################################################

comomodel <- function() {
  appDir <- system.file("comoapp", package = "como")
  
  # Print information on the session
  R.Version()$version.string
  print(paste0("App version: ", packageVersion("como")))

  # Open app  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
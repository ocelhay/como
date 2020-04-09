#' Run the shiny app
#'
#' @return
#' Open browser
#' @export
#'
#' @import bsplus deSolve highcharter lubridate pushbar RColorBrewer readxl 
#' @import reshape2 scales shiny shinyBS shinycssloaders shinyhelper shinythemes 
#' @import shinyWidgets tidyverse timevis

comomodel <- function() {
  appDir <- system.file("comoapp", package = "como")
  
  # Print information on the session
  R.Version()$version.string
  print(paste0("App version: ", packageVersion("como")))
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
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
  shiny::runApp(appDir, display.mode = "normal")
}
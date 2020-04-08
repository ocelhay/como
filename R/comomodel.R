#' Run the CoMo Model App
#'
#' @return
#' Open browser
#' @export
#'
#' @import bsplus deSolve highcharter lubridate pushbar RColorBrewer readxl 
#' @import reshape2 scales shiny shinyBS shinycssloaders shinyhelper shinythemes 
#' @import shinyWidgets tidyverse timevis

comomodel <- function() {
  appDir <- system.file("comoapp", package = "comomodel")
  shiny::runApp(appDir, display.mode = "normal")
}
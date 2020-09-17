#' Run the standalone version
#'
#' @return
#' Open browser
#' @export
#'
#' @import bsplus deSolve gridExtra highcharter knitr lubridate pushbar RColorBrewer readxl 
#' @import reshape2 scales shiny shinycssloaders shinyhelper shinyjs shinythemes 
#' @import shinyWidgets tidyverse

run_app_standalone <- function(options = list()) {
  app_dir <- system.file("comoapp", package = "como")
  shinyAppDir(app_dir, options = options)
}
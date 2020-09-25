#' Run the standalone version
#' 
#' @param options Named options that should be passed to the runApp call.
#' @return
#' Open browser
#' @export
#'
#' @import bsplus deSolve gridExtra highcharter knitr lubridate pushbar RColorBrewer readxl 
#' @import reshape2 rmarkdown scales shiny shinycssloaders shinyhelper shinyjs shinythemes 
#' @import shinyWidgets tidyverse

run_app_standalone <- function(options = list()) {
  app_dir <- system.file("comoapp", package = "como")
  shinyAppDir(app_dir, options = options)
}
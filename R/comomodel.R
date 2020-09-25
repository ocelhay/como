#' Run the shiny app
#'
#' @return
#' Open browser
#' @export
#'
#' @import bsplus deSolve gridExtra highcharter knitr lubridate pushbar RColorBrewer readxl 
#' @import reshape2 rmarkdown scales shiny shinycssloaders shinyhelper shinyjs shinythemes 
#' @import shinyWidgets tidyverse

comomodel <- function() {
  appDir <- system.file("comoapp", package = "como")
  
  # Print information on the session
  R.Version()$version.string
  print(paste0("App version: ", utils::packageVersion("como")))
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
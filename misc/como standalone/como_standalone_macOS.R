# Perform (once per machine) a manual installation of nodejs (https://nodejs.org/en/).

# Install latest version of shinybox
if(FALSE){
  rm(list = ls())
  remove.packages("shinybox")
  detach("package:shinybox", unload = TRUE)
  remotes::install_github("ocelhay/shinybox")
}

library(shinybox)

# Check that the package is working.
# remotes::install_github("ocelhay/como", ref = "v18")
# como::run_app_standalone()

time <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
(build_path <- paste0("/Users/olivier/Documents/Projets/CoMo/Standalone_", time))
dir.create(build_path)


shinybox(
  app_name = "CoMo",
  author = "CoMo Consortium",
  description = "Model from the Covid-19 International Modelling Consortium",
  semantic_version = "v18.0.1", # important de garder le format vx.y.z - to add as a validation?
  cran_like_url = "https://cran.microsoft.com/snapshot/2021-01-10",
  mac_file = "/Users/olivier/Documents/Projets/Standalone R Shiny/R/macOS/2020-10-13/R-4.0-branch.tar.gz",
  mac_r_url = "https://mac.r-project.org/high-sierra/R-4.0-branch/x86_64/R-4.0-branch.tar.gz", # only used if mac_file is NULL
  git_host = "github",
  git_repo = "ocelhay/como@v18",
  function_name = "run_app_standalone", 
  local_package_path = NULL,
  package_install_opts = list(type = "binary"),
  build_path = build_path,
  rtools_path_win = NULL,
  nodejs_path = "/usr/local/bin/",
  run_build = TRUE)

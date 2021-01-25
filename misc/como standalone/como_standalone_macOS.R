# Perform (once per machine) a manual installation of nodejs (https://nodejs.org/en/).

# Install latest version of shinybox
if(FALSE){
  rm(list = ls())
  remove.packages("shinybox")
  detach("package:shinybox", unload = TRUE)
  remotes::install_github("ocelhay/shinybox", auth_token = "")
}

library(shinybox)

# Check that the package is working.
# remotes::install_github("ocelhay/como", ref = "dev")
# como::run_app_standalone()

# Build a directory on Dektop
time <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
(build_path <- paste0("/Users/olivier/Documents/Projets/CoMo/Standalone_", time))
dir.create(build_path)



nodejs_path <- "/usr/local/bin/"
nodejs_version <- system("node -v", intern = TRUE)

shinybox(
  app_name = "CoMo",
  author = "CoMo Consortium",
  description = "Model from the Covid-19 International Modelling Consortium",
  semantic_version = "v17.0.2", # important de garder le format vx.y.z - to add as a validation?
  mran_date = "2020-12-01",
  cran_like_url = NULL,
  mac_url = "https://mac.r-project.org/high-sierra/R-4.0-branch/x86_64/R-4.0-branch.tar.gz",
  git_host = "github",
  git_repo = "ocelhay/como@master",
  function_name = "run_app_standalone", 
  local_package_path = NULL,
  package_install_opts = list(type = "binary"),
  build_path = build_path,
  rtools_path_win = NULL,
  nodejs_path = file.path(system.file(package = "shinybox"), "nodejs"),
  nodejs_version = nodejs_version,
  permission = TRUE,
  run_build = TRUE)


# run_build_release(nodejs_path = nodejs_path,
#                   app_path = paste0(build_path, "/CoMo"),
#                   nodejs_version = nodejs_version)

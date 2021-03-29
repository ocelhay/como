# Requirements:
# (1) Check that shinybox is working by running (create_standalone_shinyboxtestapp.R)
# (2) Check that the package is working:
# remotes::install_github("ocelhay/como", ref = "master")
# como::run_app_standalone()
# (3) needs Rtools for Windows for compilation of package 'comoOdeCpp'
# make sure that Rtools in the PATH
# https://stackoverflow.com/questions/47539125/how-to-add-rtools-bin-to-the-system-path-in-r

# Make sure to use the correct version of shinybox
rm(list = ls())
cat("\014")
remove.packages("shinybox")
detach("package:shinybox", unload = TRUE)
remotes::install_github("ocelhay/shinybox")
library(shinybox)

# Build a directory on the Desktop
time <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
build_path <- paste0("C:/Users/olivi/Desktop/", time)
dir.create(build_path)


shinybox(
  app_name = "CoMo",
  author = "CoMo Consortium",
  description = "Model from the Covid-19 International Modelling Consortium",
  semantic_version = "v18.1.0",
  cran_like_url = "https://cran.microsoft.com/snapshot/2021-01-10",
  mac_file = NULL,
  mac_r_url = NULL,
  git_host = "github",
  git_repo = "ocelhay/como@v18",
  function_name = "run_app_standalone", 
  local_package_path = NULL,
  package_install_opts = list(type = "binary"),
  build_path = build_path,
  rtools_path_win = "C:\\rtools40\\usr\\bin",
  nodejs_path = "C:/Program Files/nodejs/",
  run_build = TRUE)

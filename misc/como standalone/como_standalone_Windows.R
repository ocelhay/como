# Perform (once per machine) a manual installation of nodejs (https://nodejs.org/en/).

# Install latest version of shinybox.
# remotes::install_github("ocelhay/shinybox", auth_token = "")
library(shinybox)

# Check that the package is working.
# needs Rtools for Windows for compilation of package 'comoOdeCpp'
# library(devtools)
# Sys.setenv(PATH = paste("C:/Rtools/bin", Sys.getenv("PATH"), sep=";"))

# remotes::install_github("ocelhay/comoOdeCpp", subdir = "comoOdeCpp")
# remotes::install_github("ocelhay/como", ref = "dev")
# como::run_app_standalone()

# needs Rtools for Windows for compilation of package 'comoOdeCpp'
# make sure that Rtools in the PATH
# https://stackoverflow.com/questions/47539125/how-to-add-rtools-bin-to-the-system-path-in-r

time <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
build_path <- paste0("C:/Users/olivi/Desktop/", time)
dir.create(build_path)
app_version <- "16.2.4"  # must be x.y.z with three levels!
nodejs_path <- "C:/Program Files/nodejs/"
nodejs_version <- "v14.7.0"

# Remove any folder 'app_name' on build_path.

electrify(
  app_name = "CoMo",
  short_description = "CoMo Consortium | COVID-19 App",
  semantic_version = app_version, 
  build_path = build_path,
  mran_date = "2020-09-30",
  function_name = "run_app_standalone",
  git_host = "github",
  git_repo = "ocelhay/como@dev",
  local_package_path = NULL,
  package_install_opts = list(type = "binary"),
  rtools_path_win = "C:\\rtools40\\usr\\bin",
  run_build = TRUE,
  nodejs_version = nodejs_version,  # should not matter since run_build = FALSE
  permission = TRUE)


 ## Build the release.
# run_build_release(
#   nodejs_path = nodejs_path,
#   app_path = file.path(build_path, app_name),
#   nodejs_version = nodejs_version)
 
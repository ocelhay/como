# Perform (once per machine) a manual installation of nodejs (https://nodejs.org/en/).

# Install latest version of shinybox
# remove.packages("shinybox")
# detach("package:shinybox", unload = TRUE)
# remotes::install_github("ocelhay/shinybox", auth_token = "")

rm(list = ls())
.rs.restartR()
library(shinybox)

# Check that the package is working.
# remotes::install_github("ocelhay/como", ref = "dev")
# como::run_app_standalone()

app_name <- "CoMo"
semantic_version <- "16.2.3"  # must be x.y.z with three levels!

# Build the path
time <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
(build_path <- paste0("/Users/olivier/Desktop/", time))
dir.create(build_path)

nodejs_path <- "/usr/local/bin/"
nodejs_version <- system("node -v", intern = TRUE)


electrify(
  app_name = app_name,
  short_description = "CoMo Consortium | COVID-19 App",
  semantic_version = semantic_version, 
  build_path = build_path,
  mran_date = "2020-09-30",
  # cran_like_url = "https://cran.r-project.org/",
  function_name = "run_app_standalone",
  git_host = "github",
  git_repo = "ocelhay/como@dev",
  local_package_path = NULL,
  package_install_opts = list(type = "binary"),
  rtools_path_win = NULL,
  run_build = TRUE,
  nodejs_version = nodejs_version,
  mac_url = "https://mac.r-project.org/high-sierra/R-4.0-branch/x86_64/R-4.0-branch.tar.gz",
  permission = TRUE)


run_build_release(nodejs_path = nodejs_path,
                  app_path = paste0(build_path, "/CoMo"),
                  nodejs_version = nodejs_version)

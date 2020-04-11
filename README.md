# CoMo COVID-19 App

<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Travis build status](https://travis-ci.com/ernestguevarra/como.svg?branch=master)](https://travis-ci.com/ernestguevarra/como)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/ernestguevarra/como?branch=master&svg=true)](https://ci.appveyor.com/project/ernestguevarra/como)
<!-- badges: end -->

The interface was developed after consultation with several members of the CoMo Consortium. It possesses all the features and functionalities necessary to successfully perform a model fit to the available data and investigate the potential impact of several non-pharmaceutical interventions.

Whilst every effort has been taken during the development of this tool/model for it to be as accurate and reliable as possible it is important that the user understands that the outputs are a prediction based on the assumptions chosen through the input parameter values. In view of the current uncertainty on the COVID-19 mechanisms of action, the output of the model should be used to explore multiple scenarios and in combination with a larger evidence base during decision-making.

The appropriate use of this tool/model and its output can contribute to effective policymaking, but misuse or misinterpretation of the output can mislead decision-making. Any decisions taken whist using these tools are the responsibility of the user and no liability whatsoever will be taken by the developers/authors of the tool.

The App is available at [https://comomodel.net](https://comomodel.net) but can also be run locally. Follow the instructions below to install, update or launch the offline App.

## Install Offline App

- Download and install R (any version above 3.6.1) from [https://cran.r-project.org](https://cran.r-project.org)

- Open R and run in the console:

```r
# Install CoMo App for offline use
if(!require(remotes)) install.packages("remotes")
remotes::install_github("ocelhay/como", dependencies = TRUE)
```

- If prompted `--- Please select a CRAN mirror for use in this session ---`, select `Cloud`.
- If prompted `Do you want to install from sources the packages which need compilation? (Yes/no/cancel)`, return `no`. 

**Troubleshooting:**

1. if the internet connection is not steady, re-run the commands as often as required for the download to proceed.
2. restart R and update all packages `update.packages()`


## Update Offline App

To update to the latest version, run: `remotes::install_github("ocelhay/como", upgrade = "never")`


## Launch Offline App

To launch the CoMo App, open R and run in the console:

```
library(como)
comomodel()
```

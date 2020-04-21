<!-- badges: start -->
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![](https://img.shields.io/badge/devel%20version-12.02-blue.svg)](https://github.com/ocelhay/como)
[![Build Status](https://travis-ci.org/ocelhay/como.svg?branch=master)](https://travis-ci.org/ocelhay/como)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/ocelhay/como?branch=master&svg=true)](https://ci.appveyor.com/project/ocelhay/como)
<!-- badges: end -->

# CoMo COVID-19 App

## Important Disclaimer

Every effort has been taken during the development of this tool/model for it to be as accurate and reliable as possible. However, it is important that you understand that the output is a prediction based on the assumptions made when selecting the input parameter values. In view of the current uncertainty around COVID-19 mechanisms’ of action, you should use the model output to explore multiple scenarios. Also, you should use the model output in conjunction with a larger evidence base when making any decisions.

The appropriate use of this tool/model and its output can contribute to effective policymaking. However, any misuse or misinterpretation of the output can lead to poor decision-making. You are responsible for any decisions taken based on the use of this tool/model. No liability whatsoever is accepted by the developers/authors of the tool/model.


## About

The interface was developed after consultation with several members of the CoMo Consortium. It possesses all the features and functionalities necessary to successfully perform a model fit to the available data and investigate the potential impact of several non-pharmaceutical interventions.

The [App is online](https://comomodel.net) but can also be run locally. Follow the instructions below to install, update or launch the offline App.


## Installation

### Install Offline App

- Download and install R (any version above 3.6.1) — https://cran.r-project.org.
- Open R and run in the console:

```
if (!require('pacman')) install.packages('pacman', quiet = TRUE)

pacman::p_load(bsplus, DT, deSolve, highcharter, lubridate, pushbar, RColorBrewer, readxl, remotes,
               reshape2, scales, shiny, shinyBS, shinycssloaders, shinyhelper, shinythemes, 
               shinyWidgets, tidyverse)
```

- If prompted `--- Please select a CRAN mirror for use in this session ---`, select 'Cloud'.
- If prompted `Do you want to install from sources the packages which need compilation? (Yes/no/cancel)`, return `no`. 
- Close and reopen R.
- Run in the console: `remotes::install_github("ocelhay/como", upgrade = "never")`


### Update Offline App

To update to the latest version, run: `remotes::install_github("ocelhay/como", upgrade = "never")`


### Launch Offline App

To launch the CoMo App, open R and run in the console: `como::comomodel()`


### Troubleshooting

1. If the internet connection is not steady, re-run the commands as often as required for the download to proceed.
2. Restart R and update all packages `update.packages()`

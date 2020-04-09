[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# CoMo COVID-19 App

## Online Version

https://comomodel.net

## Offline Version

### Installation Instructions

- Download and install R (any version above 3.6.1) — https://cran.r-project.org.
- Open R and run in the console:

```
# Install CoMo App for offline use
if (!require('pacman')) install.packages('pacman', quiet = TRUE)

pacman::p_load(bsplus, deSolve, highcharter, lubridate, pushbar, RColorBrewer, readxl, remotes,
               reshape2, scales, shiny, shinyBS, shinycssloaders, shinyhelper, shinythemes, 
               shinyWidgets, tidyverse, timevis)
```

If prompted `--- Please select a CRAN mirror for use in this session ---`, select 'Cloud'.
If prompted `Do you want to install from sources the packages which need compilation? (Yes/no/cancel)`, return `no`. 

- Close and reopen R.
- Run in the console:

```
remotes::install_github("ocelhay/como", upgrade = "never")
```


Troubleshooting:

- If the internet connection is not steady, re-run the commands as often as required for the download to proceed.




### Update Instructions

You can check the version by running in the console `packageVersion("como")`. To update to the latest version, run in the console:

```
remotes::install_github("ocelhay/como", upgrade = "never")
```


### Run Offline Version

To launch the CoMo App, run in the console:

```
library(como)
comomodel()
```

Troubleshooting:

- Restart R
- Update all packages `update.packages()`

## Important Disclaimer

Whilst every effort has been taken during the development of this tool/model for it to be as accurate and reliable as possible it is important that the user understands that the outputs are a prediction based on the assumptions chosen through the input parameter values. In view of the current uncertainty on the COVID-19 mechanisms of action, the output of the model should be used to explore multiple scenarios and in combination with a larger evidence base during decision-making.

The appropriate use of this tool/model and its output can contribute to effective policymaking, but misuse or misinterpretation of the output can mislead decision-making. Any decisions taken whist using these tools are the responsibility of the user and no liability whatsoever will be taken by the developers/authors of the tool.

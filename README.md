<!-- badges: start -->
[![License: CC BY-NC 4.0](https://img.shields.io/badge/License-CC%20BY--NC%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc/4.0/)
[![](https://img.shields.io/badge/devel%20version-12.14-blue.svg)](https://github.com/ocelhay/como)
[![Build Status](https://travis-ci.org/ocelhay/como.svg?branch=master)](https://travis-ci.org/ocelhay/como)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/ocelhay/como?branch=master&svg=true)](https://ci.appveyor.com/project/ocelhay/como)
<!-- badges: end -->

# CoMo COVID-19 App

## Authors

- <a href="https://orcid.org/0000-0002-2971-9110" width="16" height="16" target="_blank">Olivier Celhay<img src="https://orcid.org/sites/default/files/images/orcid_16x16(1).gif" border="0"></a>, ORCID
- <a href="https://orcid.org/0000-0002-6507-6597" width="16" height="16" target="_blank">Ricardo Aguas<img src="https://orcid.org/sites/default/files/images/orcid_16x16(1).gif" border="0"></a>, ORCID
- <a href="https://orcid.org/0000-0001-9733-8304" width="16" height="16" target="_blank">Sai Thein Than Tun<img src="https://orcid.org/sites/default/files/images/orcid_16x16(1).gif" border="0"></a>, ORCID
- <a href="https://orcid.org/0000-0002-6523-185X" width="16" height="16" target="_blank">Lisa J. White<img src="https://orcid.org/sites/default/files/images/orcid_16x16(1).gif" border="0"></a>, ORCID
- CoMo Consortium

## Important Disclaimer

Whilst every effort has been taken during the development of this tool/model for it to be as accurate and reliable as possible it is important that the user understands that the outputs are a prediction based on the assumptions chosen through the input parameter values. In view of the current uncertainty on the COVID-19 mechanisms of action, the output of the model should be used to explore multiple scenarios and in combination with a larger evidence base during decision-making.

The appropriate use of this tool/model and its output can contribute to effective policymaking, but misuse or misinterpretation of the output can mislead decision-making. Any decisions taken whist using these tools are the responsibility of the user and no liability whatsoever will be taken by the developers/authors of the tool.

## License

This is a human-readable summary of (and not a substitute for) the [license](https://github.com/ocelhay/como/blob/master/LICENSE.txt).

You are free to:

- Share — copy and redistribute the material in any medium or format
- Adapt — remix, transform, and build upon the material

Under the following terms:

- Attribution — You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
- NonCommercial — You may not use the material for commercial purposes.
- ShareAlike — If you remix, transform, or build upon the material, you must distribute your contributions under the same license as the original.

## About

The interface was developed after consultation with several members of the CoMo Consortium. It possesses all the features and functionalities necessary to successfully perform a model fit to the available data and investigate the potential impact of several non-pharmaceutical interventions.

The [App is online](https://comomodel.net) but can also be run locally. Follow the instructions below to install, update or launch the offline App.


### Install Offline App

- [Download and install R](https://cran.r-project.org) (any version above 3.6.1)
- Open R and run in the console: `remotes::install_github("ocelhay/como", upgrade = "never")`
- Close and reopen R.

To launch the CoMo App, open R and run in the console: `como::comomodel()`

To update to the latest version, run: `remotes::install_github("ocelhay/como", upgrade = "never")`


### Troubleshooting

1. During the installation, if prompted `--- Please select a CRAN mirror for use in this session ---`, select 'Cloud'.
2. During the installation, if prompted `Do you want to install from sources the packages which need compilation? (Yes/no/cancel)`, return `no`.
3. If the internet connection is not steady, re-run the commands as often as required for the download to proceed.
4. Restart R and update all packages `update.packages()`


<!-- badges: start -->
[![License: CC BY-NC 4.0](https://img.shields.io/badge/License-CC%20BY--NC%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc/4.0/)
[![](https://img.shields.io/badge/devel%20version-16.2.3-blue.svg)](https://github.com/ocelhay/como)
[![Build Status](https://travis-ci.org/ocelhay/como.svg?branch=master)](https://travis-ci.org/ocelhay/como)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/ocelhay/como?branch=master&svg=true)](https://ci.appveyor.com/project/ocelhay/como)
[![DOI](https://zenodo.org/badge/251726959.svg)](https://zenodo.org/badge/latestdoi/251726959)
<!-- badges: end -->

# COVID-19 App | CoMo Consortium

## Authors

- <a href="https://orcid.org/0000-0002-2971-9110" width="16" height="16" target="_blank">Olivier Celhay<img src="https://orcid.org/sites/default/files/images/orcid_16x16(1).gif" border="0"></a>, ORCID
- <a href="https://orcid.org/0000-0002-6507-6597" width="16" height="16" target="_blank">Ricardo Aguas<img src="https://orcid.org/sites/default/files/images/orcid_16x16(1).gif" border="0"></a>, ORCID
- <a href="https://orcid.org/0000-0002-7405-7507" width="16" height="16" target="_blank">Bo Gao<img src="https://orcid.org/sites/default/files/images/orcid_16x16(1).gif" border="0"></a>, ORCID
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


## Online App

The App can be used from https://comomodel.net. 

## Offline App

The App can also be run locally. Follow the instructions below to install, update or launch the offline App.

### Installation

- [Download and install R](https://cran.r-project.org) (any version above 3.6.1)
- Install a C/C++ compiler:

(A) on Windows

- check your version of by typing `R.Version()$version.string` in your console
- if your R version is `3.6.x`, install [this version of Rtools.](https://cran.r-project.org/bin/windows/Rtools/Rtools35.exe).
- if your R version is `4.0` and up, install [this version of Rtools.](https://cran.r-project.org/bin/windows/Rtools/rtools40-x86_64.exe)
- Run the installer, you can accept the defaults throughout.
- Restart your machine

(B) on macOS

- check if your R version is `4.0` and up. Otherwise update R.
- Install [XCode 11 from the Mac App Store.](https://developer.apple.com/xcode/resources/).
- check your macOS version (From the Apple menu  in the corner of your screen, choose About This Mac. You'll see the macOS name, such as macOS Mojave, followed by its version number.)
- Go to https://github.com/fxcoudert/gfortran-for-macOS/releases, identify your macOS version, click on the corresponding **> Assets** in the page and download the .dmg (e.g. gfortran-8.2-Mojave.dmg)
- Run the installer and restart your machine

Once the C/C++ compiler has been installed:

- Open R and run in the console: `install.packages("remotes")`
- Run: `remotes::install_github("ocelhay/como", upgrade = "never")`
- Close and reopen R.


### Launch

To launch the CoMo App, open R and run in the console: `como::comomodel()`.

### Update

To update to the latest version, run: `remotes::install_github("ocelhay/como", upgrade = "never")`


### Troubleshooting

1. During the installation, if prompted `--- Please select a CRAN mirror for use in this session ---`, select 'Cloud'.
2. During the installation, if prompted `Do you want to install from sources the packages which need compilation? (Yes/no/cancel)`, return `no`.
3. If the internet connection is not steady, re-run the commands as often as required for the download to proceed.
4. Restart R and update all packages `update.packages()`

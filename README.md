<!-- badges: start -->
[![License: CC BY-NC 4.0](https://img.shields.io/badge/License-CC%20BY--NC%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc/4.0/)
[![](https://img.shields.io/badge/devel%20version-17.0.2.svg)](https://github.com/ocelhay/como)
[![Build Status](https://travis-ci.org/ocelhay/como.svg?branch=master)](https://travis-ci.org/ocelhay/como)
[![DOI](https://zenodo.org/badge/251726959.svg)](https://zenodo.org/badge/latestdoi/251726959)
<!-- badges: end -->


[The COVID-19 International Modelling Consortium (CoMo Consortium)](https://como.bmj.com) was created by researchers at the University of Oxford together with academic colleagues at Cornell University and is partnering with infectious disease modellers and other public health experts from more than 40 countries across Africa, Asia, and South and North America.

A model framework was developed by researchers from the University of Oxford and Cornell. The model continues to evolve based on ideas from all members of the Consortium and implemented by the CoMo technical team. The CoMo Consortium has developed an age-structured, compartmental SEIRS (susceptible-exposed-infectious-recovered-susceptible) model to estimate the trajectory of COVID-19 based on different scenarios, and assess the potential impact of the various behavioural change strategies as well as treatment and vaccines, when they become available. 
The model is continually being updated and refined based on feedback from members of the CoMo Consortium and the developing analytical needs. 

**The CoMo App offers an interface to this model.**

- The latest stable release from the app can be accessed online at https://comomodel.net
- Standalone versions of the app for Windows and macOS are available: [standalone stable releases](https://github.com/ocelhay/como/releases/latest).

(Experimental.) The latest development version is available [as a standalone app](https://github.com/ocelhay/como/releases) or [an online app](https://livedataoxford.shinyapps.io/comoappdev/).

(Advanced.) The app can also run as an R package:

- requires Rtools on Windows OS, XCode 11 and gfortran on macOS.
- install the package: `remotes::install_github("ocelhay/como", upgrade = "never")`
- launch the app: `como::comomodel()`



### Authors

- Olivier Celhay, [ORCID Profile](https://orcid.org/0000-0002-2971-9110)
- Ricardo Aguas, [ORCID Profile](https://orcid.org/0000-0002-6507-6597)
- Bo Gao, [ORCID Profile](https://orcid.org/0000-0002-7405-7507)
- Sai Thein Than Tun, [ORCID Profile](https://orcid.org/0000-0001-9733-8304)
- Lisa J. White, [ORCID Profile](https://orcid.org/0000-0002-6523-185X)
- [CoMo Consortium](https://como.bmj.com)

### Disclaimer

Whilst every effort has been taken during the development of this tool/model for it to be as accurate and reliable as possible it is important that the user understands that the outputs are a prediction based on the assumptions chosen through the input parameter values. In view of the current uncertainty on the COVID-19 mechanisms of action, the output of the model should be used to explore multiple scenarios and in combination with a larger evidence base during decision-making.

The appropriate use of this tool/model and its output can contribute to effective policymaking, but misuse or misinterpretation of the output can mislead decision-making. Any decisions taken whist using these tools are the responsibility of the user and no liability whatsoever will be taken by the developers/authors of the tool.

### License

This project is released under the [Attribution-NonCommercial 4.0 International License](https://github.com/ocelhay/como/blob/master/LICENSE.txt).






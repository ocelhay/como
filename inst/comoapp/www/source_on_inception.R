library(bsplus)
library(comoOdeCpp)
library(deSolve)
library(DT)
library(highcharter)
library(lubridate)
library(pushbar)
library(readxl)
library(reshape2)
library(scales)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinyhelper)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

# load data ----
load(file = "./www/data/cases.Rda")
load(file = "./www/data/contacts.Rda")
load(file = "./www/data/demog.Rda")
load(file = "./www/data/mort_sever_default.Rda")

# choices for dropdowns ----
countries_cases <- sort(unique(cases$country))
countries_contact <- names(contact_home)
countries_demographic <- sort(unique(population$country))
all_interventions <- c("_",
                       "Self-isolation if Symptomatic",
                       "Screening (when S.I.)",
                       "Household Isolation (when S.I.)",
                       "Social Distancing",
                       "Handwashing",
                       "Working at Home",
                       "School Closures",
                       "Shielding the Elderly",
                       "International Travel Ban",
                       "Vaccination")

# Default values for interventions
nb_interventions_max <- 30
new_intervention_value <- all_interventions[1]
new_daterange_value <- c(as.Date("2020-01-01"), as.Date("2020-12-31"))
new_coverage_value <- 0


# options ----
hc_export_items <- c("downloadPNG", "downloadCSV", "downloadXLS")

# For model (can be defined with the data but without any input) ----

A <- length(age_categories)

# Define the indices for each variable ----
Sindex <- 1:A
Eindex <- (A + 1):(2 * A)
Iindex <- (2 * A + 1):(3 * A)
Rindex <- (3 * A + 1):(4 * A)
Xindex <- (4 * A + 1):(5 * A)
Hindex <- (5 * A + 1):(6 * A)
HCindex <- (6 * A + 1):(7 * A)
Cindex <- (7 * A + 1):(8 * A)
CMindex <- (8 * A + 1):(9 * A)
Vindex <- (9 * A + 1):(10 * A)
QSindex <- (10 * A + 1):(11 * A)
QEindex <- (11 * A + 1):(12 * A)
QIindex <- (12 * A + 1):(13 * A)
QRindex <- (13 * A + 1):(14 * A)
CLindex <- (14 * A + 1):(15 * A)
QCindex <- (15 * A + 1):(16 * A)
ICUindex <- (16 * A + 1):(17 * A)
ICUCindex <- (17 * A + 1):(18 * A)
ICUCVindex <- (18 * A + 1):(19 * A)
Ventindex <- (19 * A + 1):(20 * A)
VentCindex <- (20 * A + 1):(21 * A)
CMCindex <- (21 * A + 1):(22 * A)

# Define index case ----
ageindcase <- 20
aci <- floor((ageindcase / 5) + 1) # age class of index case

# Set model initial conditions ----
initI <- rep(0, A)  # Infected and symptomatic
initE <- rep(0, A)  # Incubating
initE[aci] <- 1     # Place random index case in E compartment
initR <- rep(0, A)  # Immune
initX <- rep(0, A)  # Isolated
initV <- rep(0, A)  # Vaccinated
initQS <- rep(0, A) # quarantined S
initQE <- rep(0, A) # quarantined E
initQI <- rep(0, A) # quarantined I
initQR <- rep(0, A) # quarantined R
initH <- rep(0, A)  # hospitalised
initHC <- rep(0, A) # hospital critical
initC <- rep(0, A)  # Cumulative cases (true)
initCM <- rep(0, A) # Cumulative deaths (true)
initCL <- rep(0, A) # symptomatic cases
initQC <- rep(0, A) # quarantined C
initICU <- rep(0, A)   # icu
initICUC <- rep(0, A)  # icu critical
initICUCV <- rep(0, A)  # icu critical
initVent <- rep(0, A)  # icu vent
initVentC <- rep(0, A) # icu vent crit
initCMC <- rep(0, A)   # Cumulative deaths (true)

source("./www/fun_validation_interventions.R")
source("./www/fun_inputs.R")


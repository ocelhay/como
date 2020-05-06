library(bsplus)
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
library(shinythemes)
library(shinyWidgets)
library(tidyverse)


load(file = "./www/data/cases.Rda")
load(file = "./www/data/contacts.Rda")
load(file = "./www/data/demog.Rda")
load(file = "./www/data/mort_sever_default.Rda")

# choices of countries for dropdown
countries_cases <- sort(unique(cases$country))
countries_contact <- names(contact_home)
countries_demographic <- sort(unique(population$country))

hc_export_items <- c("downloadPNG", "downloadCSV", "downloadXLS")

all_interventions <- c("Lockdown, Low",
                       "Lockdown, Mid",
                       "Lockdown, High",
                       "Self-isolation if Symptomatic",
                       "Screening (when Self-isolation)",
                       "Social Distancing",
                       "Handwashing",
                       "Working at Home",
                       "School Closures",
                       "Shielding the Elderly",
                       "Travel Ban",
                       "Voluntary home quarantine",
                       "Vaccination")

# V13
new_intervention_value <- all_interventions[4]
new_daterange_value <- c(as.Date("2020-01-01"), as.Date("2020-12-31"))
new_coverage_value <- 0

nb_interventions_max <- 30
source("./www/fun_validation_interventions.R")

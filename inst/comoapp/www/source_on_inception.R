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


load(file = "./www/data/cases.Rda")
load(file = "./www/data/contacts.Rda")
load(file = "./www/data/demog.Rda")
A <- length(age_categories)

load(file = "./www/data/mort_sever_default.Rda")

# choices of countries for dropdown
countries_cases <- sort(unique(cases$country))
countries_contact <- names(contact_home)
countries_demographic <- sort(unique(population$country))

hc_export_items <- c("downloadPNG", "downloadCSV", "downloadXLS")

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

source("./www/fun_validation_interventions.R")
source("./www/fun_inputs.R")


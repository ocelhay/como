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

# options for exporting highchart
hc_export_items <- c("downloadPNG", "downloadCSV", "downloadXLS")
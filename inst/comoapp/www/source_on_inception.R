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

# choices of countries for cases
countries_cases <- c("-- Own Value ---", cases %>% pull(country) %>% unique() %>% sort())

# per year ageing matrix
A <- length(age_categories)
dd <- seq(1:A)/seq(1:A)
ageing <- t(diff(diag(dd), lag = 1)/(5*365.25))
ageing <- cbind(ageing, 0 * seq(1:A)) # no ageing from last compartment

# choices of countries for social contact
countries_contact <- names(contact_home)
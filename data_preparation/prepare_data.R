# From data on raw_data folder, generate data_CoMo.RData to be loaded in the App
# data_CoMo.RData should be added/updated in the www/data folder of the App
# about_data.md in the www/markdown folder should be updated to reflect whenever cases data is updated

# Prolegomenon ----
setwd("/Users/olivier/Documents/CoMo/como/data_preparation/raw_data/")
library(readxl)
library(tidyverse)


# Cases Data ----
file <- "COVID-19-geographic-disbtribution-worldwide-2020-05-12.xlsx"

cases <- read_excel(file) %>%
  transmute(date = as.Date(dateRep), cases = cases, deaths = deaths, country = countriesAndTerritories) %>%
  group_by(country) %>%
  arrange(date) %>%
  mutate(cumulative_death = cumsum(deaths)) %>%
  ungroup()


# Mortality/Severity
mort_sever_default <- read.csv("COVID_severe_mortality_age.csv", header = TRUE)

mort_sever_default <- mort_sever_default %>%
  mutate(ihr = ihr / 100) %>% # starting unit should be % - scaling to a value between 0 and 1
  mutate(ifr = ifr/max(ifr))  # starting unit should be % - scaling to a value between 0 and 1


# Social Contacts ----

# home ---
file_1 <- "contact_matrices_152_countries/MUestimates_home_1.xlsx"
file_2 <- "contact_matrices_152_countries/MUestimates_home_2.xlsx"
contact_1 <- lapply(excel_sheets(file_1), read_excel, path = file_1)
vec <- excel_sheets(file_1)
names(contact_1) <- vec

contact_2 <- lapply(excel_sheets(file_2), read_excel, path = file_2, col_names = paste0("X", 1:16))
vec <- excel_sheets(file_2)
names(contact_2) <- vec

contact_home <- c(contact_1, contact_2)

# contact_home[["Albania"]]

# school ---
file_1 <- "contact_matrices_152_countries/MUestimates_school_1.xlsx"
file_2 <- "contact_matrices_152_countries/MUestimates_school_2.xlsx"
contact_1 <- lapply(excel_sheets(file_1), read_excel, path = file_1)
vec <- excel_sheets(file_1)
names(contact_1) <- vec

contact_2 <- lapply(excel_sheets(file_2), read_excel, path = file_2, col_names = paste0("X", 1:16))
vec <- excel_sheets(file_2)
names(contact_2) <- vec

contact_school <- c(contact_1, contact_2)


# work ----
file_1 <- "contact_matrices_152_countries/MUestimates_work_1.xlsx"
file_2 <- "contact_matrices_152_countries/MUestimates_work_2.xlsx"
contact_1 <- lapply(excel_sheets(file_1), read_excel, path = file_1)
vec <- excel_sheets(file_1)
names(contact_1) <- vec

contact_2 <- lapply(excel_sheets(file_2), read_excel, path = file_2, col_names = paste0("X", 1:16))
vec <- excel_sheets(file_2)
names(contact_2) <- vec

contact_work <- c(contact_1, contact_2)


# other ----
file_1 <- "contact_matrices_152_countries/MUestimates_other_locations_1.xlsx"
file_2 <- "contact_matrices_152_countries/MUestimates_other_locations_2.xlsx"
contact_1 <- lapply(excel_sheets(file_1), read_excel, path = file_1)
vec <- excel_sheets(file_1)
names(contact_1) <- vec

contact_2 <- lapply(excel_sheets(file_2), read_excel, path = file_2, col_names = paste0("X", 1:16))
vec <- excel_sheets(file_2)
names(contact_2) <- vec

contact_other <- c(contact_1, contact_2)

# Population ----
age_categories <- c("0-4 y.o.", "5-9 y.o.", "10-14 y.o.", "15-19 y.o.", "20-24 y.o.", "25-29 y.o.", "30-34 y.o.", 
                    "35-39 y.o.", "40-44 y.o.", "45-49 y.o.", "50-54 y.o.", "55-59 y.o.", "60-64 y.o.", "65-69 y.o.", 
                    "70-74 y.o.", "75-79 y.o.", "80-84 y.o.", "85-89 y.o.", "90-94 y.o.", "95-99 y.o.", "100+ y.o.")

population <- read_excel("population_UN/WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.xlsx", 
                         sheet = 1, skip = 16) %>%
  filter(Type == "Country/Area", `Reference date (as of 1 July)` == 2020) %>%
  rename(country = `Region, subregion, country or area *`) %>%
  select(country, `0-4`:`100+`) %>%
  pivot_longer(-country, names_to = "age_category", values_to = "pop") %>%
  mutate(pop = round(1000*as.numeric(pop)), age_category = paste0(age_category, " y.o.")) %>%
  mutate(age_category = factor(age_category, levels = age_categories))


population_birth <- read_excel("population_UN/WPP2019_FERT_F06_BIRTHS_BY_AGE_OF_MOTHER.xlsx", 
                               sheet = 1, skip = 16) %>%
  filter(Type == "Country/Area", Period == "2015-2020") %>%
  rename(country = `Region, subregion, country or area *`) %>%
  select(country, `15-19`:`45-49`) %>%
  pivot_longer(-country, names_to = "age_category", values_to = "birth") %>%
  mutate(birth = as.numeric(birth), age_category = paste0(age_category, " y.o.")) %>%
  mutate(age_category = factor(age_category, levels = age_categories)) %>%
  complete(country, age_category, fill = list(birth = 0))


population_death <- read_excel("population_UN/WPP2019_MORT_F04_1_DEATHS_BY_AGE_BOTH_SEXES.xlsx", 
                               sheet = 1, skip = 16) %>%
  filter(Type == "Country/Area", Period == "2015-2020") %>%
  rename(country = `Region, subregion, country or area *`) %>%
  select(country, `0-4`:`95+`) %>%
  mutate(`95-99` = `95+`, `100+` = `95+`) %>%
  select(-`95+`) %>%
  pivot_longer(-country, names_to = "age_category", values_to = "death") %>%
  mutate(death = as.numeric(death), age_category = paste0(age_category, " y.o.")) %>%
  mutate(age_category = factor(age_category, levels = age_categories)) %>%
  complete(country, age_category, fill = list(death = 0))

population <- left_join(population, population_birth, by = c("country", "age_category"))
population <- left_join(population, population_death, by = c("country", "age_category"))

population <- population %>%
  mutate(birth = (1000 * birth) / (5 * pop * 365.25),  # convert from 1000s per 5 year period to per person per day
         death = (1000 * death) / (5 * pop * 365.25))  # convert from 1000s per 5 year period to per person per day



# Export data ----
setwd("/Users/olivier/Documents/CoMo/como/data_preparation/")
save(cases, file = "cases.Rda")
save(mort_sever_default, file = "mort_sever_default.Rda")
save(contact_home, contact_work, contact_school, contact_other, file = "contacts.Rda")
save(age_categories, population, file = "demog.Rda")

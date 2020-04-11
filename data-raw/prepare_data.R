# Prolegomenon ----
#setwd("/Users/olivier/Documents/CoMo/como/data")

# The line above will always fail on anybody else's computer as this is hardcoded 
# to a specific person's settings - this is not good practice. Best practice is 
# to not use setwd() for creating R packages or for code that is going to be
# shared with someone else.

# Load libraries
library(readxl)
library(tidyverse)

# Cases Data ----
file <- "data-raw/COVID-19-geographic-disbtribution-worldwide-2020-04-10.xlsx"

cases <- read_excel(file) %>%
  transmute(date = as.Date(dateRep), cases = cases, deaths = deaths, 
            country = countriesAndTerritories) %>%
  group_by(country) %>%
  arrange(date) %>%
  mutate(cumulative_death = cumsum(deaths)) %>%
  ungroup()

## Save cases as an .rda file in data
usethis::use_data(cases, overwrite = TRUE, compress = "xz")


# Mortality/Severity
mort_sever_default <- read.csv("data-raw/COVID_severe_mortality_age.csv", header = TRUE) %>%
  mutate(ihr = 4*ihr/100) %>%
  mutate(ifr = ifr/max(ifr))

## Save mort_sever_default as an .rda file in data
usethis::use_data(mort_sever_default, overwrite = TRUE, compress = "xz")

# Social Contacts ----

# home ---
file_1 <- "data-raw/contact_matrices_152_countries/MUestimates_home_1.xlsx"
file_2 <- "data-raw/contact_matrices_152_countries/MUestimates_home_2.xlsx"
contact_1 <- lapply(excel_sheets(file_1), read_excel, path = file_1)
vec <- excel_sheets(file_1)
names(contact_1) <- vec

contact_2 <- lapply(excel_sheets(file_2), read_excel, path = file_2, col_names = paste0("X", 1:16))
vec <- excel_sheets(file_2)
names(contact_2) <- vec

contact_home <- c(contact_1, contact_2)

## Save contact_home as an .rda file in data
usethis::use_data(contact_home, overwrite = TRUE, compress = "xz")

# contact_home[["Albania"]]

# school ---
file_1 <- "data-raw/contact_matrices_152_countries/MUestimates_school_1.xlsx"
file_2 <- "data-raw/contact_matrices_152_countries/MUestimates_school_2.xlsx"
contact_1 <- lapply(excel_sheets(file_1), read_excel, path = file_1)
vec <- excel_sheets(file_1)
names(contact_1) <- vec

contact_2 <- lapply(excel_sheets(file_2), read_excel, path = file_2, col_names = paste0("X", 1:16))
vec <- excel_sheets(file_2)
names(contact_2) <- vec

contact_school <- c(contact_1, contact_2)

## Save contact_school as .rda file in data
usethis::use_data(contact_school, overwrite = TRUE, compress = "xz")

# work ----
file_1 <- "data-raw/contact_matrices_152_countries/MUestimates_work_1.xlsx"
file_2 <- "data-raw/contact_matrices_152_countries/MUestimates_work_2.xlsx"
contact_1 <- lapply(excel_sheets(file_1), read_excel, path = file_1)
vec <- excel_sheets(file_1)
names(contact_1) <- vec

contact_2 <- lapply(excel_sheets(file_2), read_excel, path = file_2, col_names = paste0("X", 1:16))
vec <- excel_sheets(file_2)
names(contact_2) <- vec

contact_work <- c(contact_1, contact_2)

## Save contact_work as .rda file in data
usethis::use_data(contact_work, overwrite = TRUE, compress = "xz")


# other ----
file_1 <- "data-raw/contact_matrices_152_countries/MUestimates_other_locations_1.xlsx"
file_2 <- "data-raw/contact_matrices_152_countries/MUestimates_other_locations_2.xlsx"
contact_1 <- lapply(excel_sheets(file_1), read_excel, path = file_1)
vec <- excel_sheets(file_1)
names(contact_1) <- vec

contact_2 <- lapply(excel_sheets(file_2), read_excel, path = file_2, col_names = paste0("X", 1:16))
vec <- excel_sheets(file_2)
names(contact_2) <- vec

contact_other <- c(contact_1, contact_2)

## Save contact_other as an .rda file in data
usethis::use_data(contact_other, overwrite = TRUE, compress = "xz")

# Population ----
age_categories <- c("0-4 y.o.", "5-9 y.o.", "10-14 y.o.", "15-19 y.o.", "20-24 y.o.", "25-29 y.o.", "30-34 y.o.", 
                    "35-39 y.o.", "40-44 y.o.", "45-49 y.o.", "50-54 y.o.", "55-59 y.o.", "60-64 y.o.", "65-69 y.o.", 
                    "70-74 y.o.", "75-79 y.o.", "80-84 y.o.", "85-89 y.o.", "90-94 y.o.", "95-99 y.o.", "100+ y.o.")

countries_demog <- c("Afghanistan", "Albania", "Algeria", "Angola", "Antigua and Barbuda", 
                     "Argentina", "Armenia", "Aruba", "Australia", "Austria", "Azerbaijan", 
                     "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", 
                     "Belize", "Benin", "Bhutan", "Bolivia (Plurinational State of)", 
                     "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei Darussalam", 
                     "Bulgaria", "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", 
                     "Cameroon", "Canada", "Central African Republic", "Chad", "Channel Islands", 
                     "Chile", "China", "China, Hong Kong SAR", "China, Macao SAR", 
                     "China, Taiwan Province of China", "Colombia", "Comoros", "Congo", 
                     "Costa Rica", "Côte d'Ivoire", "Croatia", "Cuba", "Curaçao", 
                     "Cyprus", "Czechia", "Dem. People's Republic of Korea", "Democratic Republic of the Congo", 
                     "Denmark", "Djibouti", "Dominican Republic", "Ecuador", "Egypt", 
                     "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini", 
                     "Ethiopia", "Fiji", "Finland", "France", "French Guiana", "French Polynesia", 
                     "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada", 
                     "Guadeloupe", "Guam", "Guatemala", "Guinea", "Guinea-Bissau", 
                     "Guyana", "Haiti", "Honduras", "Hungary", "Iceland", "India", 
                     "Indonesia", "Iran (Islamic Republic of)", "Iraq", "Ireland", 
                     "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", 
                     "Kenya", "Kiribati", "Kuwait", "Kyrgyzstan", "Lao People's Democratic Republic", 
                     "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Lithuania", 
                     "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Maldives", 
                     "Mali", "Malta", "Martinique", "Mauritania", "Mauritius", "Mayotte", 
                     "Mexico", "Micronesia (Fed. States of)", "Mongolia", "Montenegro", 
                     "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "Netherlands", 
                     "New Caledonia", "New Zealand", "Nicaragua", "Niger", "Nigeria", 
                     "North Macedonia", "Norway", "Oman", "Pakistan", "Panama", "Papua New Guinea", 
                     "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Puerto Rico", 
                     "Qatar", "Republic of Korea", "Republic of Moldova", "Réunion", 
                     "Romania", "Russian Federation", "Rwanda", "Saint Lucia", "Saint Vincent and the Grenadines", 
                     "Samoa", "Sao Tome and Principe", "Saudi Arabia", "Senegal", 
                     "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", 
                     "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Sudan", 
                     "Spain", "Sri Lanka", "State of Palestine", "Sudan", "Suriname", 
                     "Sweden", "Switzerland", "Syrian Arab Republic", "Tajikistan", 
                     "Thailand", "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", 
                     "Tunisia", "Turkey", "Turkmenistan", "Uganda", "Ukraine", "United Arab Emirates", 
                     "United Kingdom", "United Republic of Tanzania", "United States of America", 
                     "United States Virgin Islands", "Uruguay", "Uzbekistan", "Vanuatu", 
                     "Venezuela (Bolivarian Republic of)", "Viet Nam", "Western Sahara", 
                     "Yemen", "Zambia", "Zimbabwe")

population <- read_excel("data-raw/population_data/WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.xlsx", 
                         sheet = 1, skip = 16) %>%
  filter(Type == "Country/Area", `Reference date (as of 1 July)` == 2020) %>%
  rename(country = `Region, subregion, country or area *`) %>%
  select(country, `0-4`:`100+`) %>%
  pivot_longer(-country, names_to = "age_category", values_to = "pop") %>%
  mutate(pop = round(1000*as.numeric(pop)), age_category = paste0(age_category, " y.o.")) %>%
  mutate(age_category = factor(age_category, levels = age_categories))


population_birth <- read_excel("data-raw/population_data/WPP2019_FERT_F06_BIRTHS_BY_AGE_OF_MOTHER.xlsx", 
                               sheet = 1, skip = 16) %>%
  filter(Type == "Country/Area", Period == "2015-2020") %>%
  rename(country = `Region, subregion, country or area *`) %>%
  select(country, `15-19`:`45-49`) %>%
  pivot_longer(-country, names_to = "age_category", values_to = "birth") %>%
  mutate(birth = as.numeric(birth), age_category = paste0(age_category, " y.o.")) %>%
  mutate(age_category = factor(age_category, levels = age_categories)) %>%
  complete(country, age_category, fill = list(birth = 0))


population_death <- read_excel("data-raw/population_data/WPP2019_MORT_F04_1_DEATHS_BY_AGE_BOTH_SEXES.xlsx", 
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

## Save population as an .rda file in data
usethis::use_data(population, overwrite = TRUE, compress = "xz")

# Export data ----
#save(cases, mort_sever_default,
#     contact_home, contact_work, contact_school, contact_other,
#     age_categories, countries_demog, population,
#     file = "../inst/comoapp/www/data/data_CoMo.RData")

# Above lines saving data as an .RData file not needed anymore. The approach
# used above for data can be tricky when packaging an app as the data is not 
# portable. The new approach used here where the datasets are included into the
# R package means that the datasets are installed in a way that is consistent
# with the R architecture and is highly portable. When package is installed,
# the datasets are installed lazily and are loaded into the RAM when needed.

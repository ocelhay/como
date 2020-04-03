# Cases data ----
load(file = "./www/data/cases.RData")
countries_cases <- c("-- Own Value ---", cases %>% pull(country) %>% unique() %>% sort())

# Demographic Data ----
load(file = "./www/data/population.RData")

# per year ageing matrix
A <- length(age_categories)
dd <- seq(1:A)/seq(1:A)
ageing <- t(diff(diag(dd), lag = 1)/(5*365.25))
ageing <- cbind(ageing, 0 * seq(1:A)) # no ageing from last compartment

# Contact Data ----
load(file = "./www/data/contact_data.RData")
countries_contact <- names(contact_home)

# Age dependent hosp and mort ----
mort_sever_default <- read.csv("./www/data/COVID_severe_mortality_age.csv", header = TRUE) %>%
  mutate(ihr = 4*ihr/100) %>%
  mutate(ifr = ifr/max(ifr))
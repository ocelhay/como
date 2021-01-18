library(readxl)
library(stringr)
library(tidyverse)

# Create dataframe of interventions ----
file_path <- "/Users/olivier/Documents/Projets/CoMo/como/Template_CoMoCOVID-19App_v17.xlsx"

interventions_excel <- read_excel(file_path, sheet = "Interventions") %>%
  transmute(
    intervention = Intervention,
    date_start = as.Date(`Date Start`),
    date_end = as.Date(`Date End`),
    value = Value,
    uniy = Unit,
    age_groups = `Age Groups`,
    apply_to = `Apply to`
    ) %>%
  filter(! is.na(intervention))

# Test content of interventions_excel ----
# Test if listed interventions are valid
valid_interventions_v17 <- c("Dexamethasone", "Handwashing", "International Travel Ban",
                                "Mask Wearing", "Mass Testing", "School Closures", "Self-isolation if Symptomatic",
                                "(*Self-isolation) Household Isolation", "(*Self-isolation) Screening", "Shielding the Elderly",
                                "Social Distancing", "Vaccination", "Working at Home")

if(all(interventions_excel$intervention %in% valid_interventions_v17)) message("Okay, all interventions are valid.")
if(!all(interventions_excel$intervention %in% valid_interventions_v17)) stop("Stop, some interventions are not valid.")


# Test that age_groups is provided when and only when the intervention is in interventions_age_groups
interventions_age_groups <- c("Mass Testing", "School Closures", "Vaccination")
test_interventions_age_groups <- all(interventions_excel %>% filter(!is.na(age_groups)) %>% pull(intervention) %in% interventions_age_groups)

if(test_interventions_age_groups) message("Okay, age groups are provided only to Mass Testing, School Closures, or Vaccination.")
if(!test_interventions_age_groups) stop("Stop, age groups are provided to some interventions when it should not.")

# Parse age groups ----
# complte the age_groups column
interventions_excel$age_groups[is.na(interventions_excel$age_groups)] <- "1-21"

parse_age_group <- function(vec) {
  # vec <- "1-3; 8; 19-21"
  # vec <- "1; 8; 21   "
  vec <- str_replace_all(vec, "-", ":")
  vec <- str_replace_all(vec, ";", ",")
  vec <- paste0("c(", vec, ")")
  vec2 <- eval(parse(text = vec))
  
  output <- rep(FALSE, 21)
  output[vec2] <- TRUE
  return(output)
}

map(interventions_excel$age_groups, parse_age_group)
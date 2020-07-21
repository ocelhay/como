# Definitions of several variables ----
popstruc <- population_rv$data %>% 
  select(age_category, pop) %>% 
  rename(agefloor = age_category) %>% 
  as.data.frame()

popbirth <- population_rv$data %>% 
  select(age_category, birth) %>% 
  as.data.frame() # unit should be per person per day

mort <- population_rv$data %>% 
  pull(death) # unit should be per person per day

ihr <- mort_sever_rv$data %>% 
  select(age_category, ihr) %>% 
  as.data.frame()

ifr <- mort_sever_rv$data %>% 
  select(age_category, ifr) %>% 
  as.data.frame()

# Complete contact Matrices ----
c_home <- contact_home[[input$country_contact]] %>% as.matrix()
c_school <- contact_school[[input$country_contact]] %>% as.matrix()
c_work <- contact_work[[input$country_contact]] %>% as.matrix()
c_other <- contact_other[[input$country_contact]] %>% as.matrix()

nce <- A - length(c_home[1, ])

contact_home <- matrix(0, nrow = A, ncol = A)
contact_school <- matrix(0, nrow = A, ncol = A)
contact_work <- matrix(0, nrow = A, ncol = A)
contact_other <- matrix(0, nrow = A, ncol = A)

for (i in 1:(A - nce)) {
  for (j in 1:(A - nce)) {
    contact_home[i, j] <- c_home[i, j]
    contact_school[i, j] <- c_school[i, j]
    contact_work[i, j] <- c_work[i, j]
    contact_other[i, j] <- c_other[i, j]
  }
}

for (i in (A + 1 - nce):A) {
  for (j in 1:(A - nce)) {
    contact_home[i, j] <- c_home[(A - nce), j]
    contact_school[i, j] <- c_school[(A - nce), j]
    contact_work[i, j] <- c_work[(A - nce), j]
    contact_other[i, j] <- c_other[(A - nce), j]
  }
}
for (i in 1:(A - nce)) {
  for (j in (A + 1 - nce):A) {
    contact_home[i, j] <- c_home[i, (A - nce)]
    contact_school[i, j] <- c_school[i, (A - nce)]
    contact_work[i, j] <- c_work[i, (A - nce)]
    contact_other[i, j] <- c_other[i, (A - nce)]
  }
}
for (i in (A + 1 - nce):A) {
  for (j in (A + 1 - nce):A) {
    contact_home[i, j] <- c_home[(A - nce), (A - nce)]
    contact_school[i, j] <- c_school[(A - nce), (A - nce)]
    contact_work[i, j] <- c_work[(A - nce), (A - nce)]
    contact_other[i, j] <- c_other[(A - nce), (A - nce)]
  }
}

# Define time variables ----
startdate <- input$date_range[1]
stopdate <- input$date_range[2]
times <- seq(0, as.numeric(stopdate - startdate))


# Define parameters vector ----
parameters <- reactiveValuesToList(input)[
  c("p", "rho", "omega", "gamma", "nui", "report", "reportc", "reporth", 
    "beds_available", "icu_beds_available", "ventilators_available", 
    "pdeath_h", "pdeath_hc", "pdeath_icu", "pdeath_icuc", 
    "pdeath_vent", "pdeath_ventc", "ihr_scaling", "nus", 
    "nu_icu", "nu_vent", "rhos", "amp", 
    "pclin", "prob_icu", "prob_vent", "selfis_eff", "dist_eff", "hand_eff", 
    "work_eff", "w2h", "school_eff", "s2h", "cocoon_eff", "age_cocoon", 
    "vaccine_eff", "vac_campaign", "mean_imports", "screen_test_sens", 
    "screen_overdispersion", "quarantine_days", "quarantine_effort", 
    "quarantine_eff_home", "quarantine_eff_other", "household_size", 
    "noise", "iterations", "confidence", 
    # below are additions in v14.14
    "age_vaccine_min", "mass_test_sens", "isolation_days", "age_testing_min", "age_testing_max")] %>% 
  unlist()

parameters <- c(
  parameters, 
  give = 95, nusc = input$nus, nu_icuc = input$nu_icu, nu_ventc = input$nu_vent, phi = which(month.name == input$phi))


# Transform/scale parameters ----
parameters["rho"] <- parameters["rho"] / 100
parameters["omega"] <- (1 / (parameters["omega"] * 365))
parameters["gamma"] <- 1 / parameters["gamma"]
parameters["nui"] <- 1 / parameters["nui"]
parameters["report"] <- parameters["report"] / 100
parameters["reportc"] <- parameters["reportc"] / 100
parameters["reporth"] <- parameters["reporth"] / 100
parameters["nus"] <- 1 / parameters["nus"]
parameters["rhos"] <- parameters["rhos"] / 100
parameters["amp"] <- parameters["amp"] / 100
parameters["selfis_eff"] <- parameters["selfis_eff"] / 100
parameters["dist_eff"] <- parameters["dist_eff"] / 100
parameters["hand_eff"] <- parameters["hand_eff"] / 100
parameters["work_eff"] <- parameters["work_eff"] / 100
parameters["w2h"] <- parameters["w2h"] / 100
parameters["school_eff"] <- parameters["school_eff"] / 100
parameters["s2h"] <- parameters["s2h"] / 100
parameters["cocoon_eff"] <- parameters["cocoon_eff"] / 100
parameters["age_cocoon"] <- floor((parameters["age_cocoon"] / 5) + 1)
parameters["vaccine_eff"] <- parameters["vaccine_eff"] / 100
parameters["screen_test_sens"] <- parameters["screen_test_sens"] / 100
parameters["quarantine_days"] <- parameters["quarantine_days"]
parameters["quarantine_effort"] <- 1 / parameters["quarantine_effort"]
parameters["quarantine_eff_home"] <- parameters["quarantine_eff_home"] / -100
parameters["quarantine_eff_other"] <- parameters["quarantine_eff_other"] / 100
parameters["give"] <- parameters["give"] / 100
parameters["pdeath_h"] <- parameters["pdeath_h"] / 100
parameters["pdeath_hc"] <- parameters["pdeath_hc"] / 100
parameters["pdeath_icu"] <- parameters["pdeath_icu"] / 100
parameters["pdeath_icuc"] <- parameters["pdeath_icuc"] / 100
parameters["pdeath_vent"] <- parameters["pdeath_vent"] / 100
parameters["pdeath_ventc"] <- parameters["pdeath_ventc"] / 100
parameters["nusc"] <- 1 / parameters["nusc"]
parameters["nu_icu"] <- 1 / parameters["nu_icu"]
parameters["nu_icuc"] <- 1 / parameters["nu_icuc"]
parameters["nu_vent"] <- 1 / parameters["nu_vent"]
parameters["nu_ventc"] <- 1 / parameters["nu_ventc"]
parameters["pclin"] <- parameters["pclin"] / 100
parameters["prob_icu"] <- parameters["prob_icu"] / 100
parameters["prob_vent"] <- parameters["prob_vent"] / 100
parameters["confidence"] <- parameters["confidence"] / 100
parameters["mass_test_sens"] <- parameters["mass_test_sens"] / 100

# TODO: move this line to a better location
ihr[,2] <- parameters["ihr_scaling"]*ihr[,2]


# Define dataframe of interventions ----
inp <- bind_rows(interventions$baseline_mat %>% mutate(`Apply to` = "Baseline (Calibration)"),
                 interventions$future_mat %>% mutate(`Apply to` = "Hypothetical Scenario")) %>%
  rename(Intervention = intervention, `Date Start` = date_start, `Date End` = date_end, `Value` = value)


# initial conditions for the main solution vector ----
initS <- popstruc[, 2] - initE - initI - initR - initX - initZ - initV - initH - initHC -
  initQS - initQE - initQI - initQR - initCL - initQC - initICU - initICUC -
  initICUCV - initVent - initVentC  # Susceptible (non-immune)

Y <- c(initS, initE, initI, initR, initX, initH, initHC, initC, initCM, initV, initQS, initQE, initQI, 
       initQR, initCL, initQC, initICU, initICUC, initICUCV, initVent, initVentC, initCMC, initZ)
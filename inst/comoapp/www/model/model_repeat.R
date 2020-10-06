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
    # additions in v14.14:
    "age_vaccine_min", "mass_test_sens", "isolation_days", "age_testing_min", "age_testing_max",
    # additions in v15.1:
    "pdeath_ho", "pdeath_hco", "pdeath_icuo", "pdeath_icuco",
    "propo2", "dexo2", "dexo2c", "dexv", "dexvc", "vent_dex",
    "mask_eff",
    # additions in v16.2:
    "prob_icu_v", "prob_icu_vr", "prob_icu_r", "prob_v_v", "prob_v_vr", "prob_v_r",
    "pclin_v", "pclin_vr", "pclin_r", "sigmaEV", "sigmaEVR", "sigmaER", "sigmaR", "vac_dur",
    "vac_dur_r", "report_natdeathI", "report_natdeathCL", "report_v",
    "report_cv", "report_vr", "report_cvr", "report_r", "report_cr", "reporth_ICU",
    "report_death_HC", "pdeath_vent_hc", "pdeath_icu_hc", "pdeath_icu_hco",
    "reporth_g", "seroneg",
    "vaccine_eff_r", "age_vaccine_max"
    )] %>% 
  unlist()


parameters <- c(
  parameters, 
  give = 95, 
  nusc = input$nus, nu_icuc = input$nu_icu, nu_ventc = input$nu_vent, 
  phi = which(month.name == input$phi))


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
# additions in v15.1:
parameters["pdeath_ho"] <- parameters["pdeath_ho"] / 100
parameters["pdeath_hco"] <- parameters["pdeath_hco"] / 100
parameters["pdeath_icuo"] <- parameters["pdeath_icuo"] / 100
parameters["propo2"] <- parameters["propo2"] / 100
parameters["dexo2"] <- parameters["dexo2"] / 100
parameters["dexo2c"] <- parameters["dexo2c"] / 100
parameters["dexv"] <- parameters["dexv"] / 100
parameters["dexvc"] <- parameters["dexvc"] / 100
parameters["vent_dex"] <- parameters["vent_dex"] / 100
parameters["mask_eff"] <- parameters["mask_eff"] / 100
# additions in v16.2:
parameters["report_v"]<-parameters["report_v"]/100
parameters["report_cv"]<-parameters["report_cv"]/100
parameters["report_vr"]<-parameters["report_vr"]/100
parameters["report_cvr"]<-parameters["report_cvr"]/100
parameters["report_r"]<-parameters["report_r"]/100
parameters["report_cr"]<-parameters["report_cr"]/100
parameters["vaccine_eff_r"]<-parameters["vaccine_eff_r"]/100
parameters["prob_icu_v"]<-parameters["prob_icu_v"]/100
parameters["prob_icu_vr"]<-parameters["prob_icu_vr"]/100
parameters["prob_icu_r"]<-parameters["prob_icu_r"]/100
parameters["prob_v_v"]<-parameters["prob_v_v"]/100
parameters["prob_v_r"]<-parameters["prob_v_r"]/100
parameters["prob_v_vr"]<-parameters["prob_v_vr"]/100
parameters["pclin_v"]<-parameters["pclin_v"]/100
parameters["pclin_vr"]<-parameters["pclin_vr"]/100
parameters["pclin_r"]<-parameters["pclin_r"]/100
parameters["sigmaEV"]<-parameters["sigmaEV"]/100
parameters["sigmaER"]<-parameters["sigmaER"]/100
parameters["sigmaEVR"]<-parameters["sigmaEVR"]/100
parameters["sigmaR"]<-parameters["sigmaR"]/100
parameters["vac_dur"]<-1/parameters["vac_dur"]/100
parameters["vac_dur_r"]<-1/parameters["vac_dur_r"]/100
parameters["report_natdeathI"]<-parameters["report_natdeathI"]/100
parameters["report_natdeathCL"]<-parameters["report_natdeathCL"]/100
parameters["report_death_HC"]<-parameters["report_death_HC"]/100
parameters["reporth_ICU"]<-parameters["reporth_ICU"]/100
parameters["pdeath_vent_hc"]<-parameters["pdeath_vent_hc"]/100
parameters["pdeath_icu_hc"]<-parameters["pdeath_icu_hc"]/100
parameters["pdeath_icu_hco"]<-parameters["pdeath_icu_hco"]/100
parameters["reporth_g"]<-parameters["reporth_g"]/100
parameters["seroneg"]<-(1/parameters["seroneg"])

ihr[,2] <- parameters["ihr_scaling"] * ihr[,2]

# Define dataframe of interventions ----
inp <- bind_rows(interventions$baseline_mat %>% mutate(`Apply to` = "Baseline (Calibration)"),
                 interventions$future_mat %>% mutate(`Apply to` = "Hypothetical Scenario")) %>%
  rename(Intervention = intervention, `Date Start` = date_start, `Date End` = date_end, `Value` = value)


# initial conditions for the main solution vector ----
initI <- rep(0, A)  # Infected and symptomatic
initE <- rep(0, A)  # Incubating
initE[aci] <- 1     # Place random index case in E compartment
initR <- (input$pre / 100) * popstruc[,2]  # Immune
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
initICUCV <- rep(0, A) # icu critical
initVent <- rep(0, A)  # icu vent
initVentC <- rep(0, A) # icu vent crit
initCMC <- rep(0, A)   # Cumulative deaths - overload (true)
initZ <- rep(0, A)     # testing - quarantined (true)
initEV <- rep(0, A)    # vaccinated exposed
initER <- rep(0, A)    # recovered exposed
initEVR <- rep(0, A)   # recovered and vaccinated exposed
initVR <- rep(0, A)    # recovered and vaccinated
initQV <- rep(0, A)    # quarantined and vaccinated
initQEV <- rep(0, A)   # quarantined, exposed and vaccinated
initQEVR <- rep(0, A)  # quarantined, exposed, recovered and vaccinated
initQER <- rep(0, A)   # quarantined, exposed and recovered
initQVR <- rep(0, A)   # quarantined, recovered and vaccinated
initHCICU <- rep(0, A) # icu not seeking
initHCV <- rep(0, A)   # ventilator not seeking
initAb <- rep(0, A)   # ventilator not seeking

initS <- popstruc[, 2] -initE-initI-initCL-initR-initX-initZ-initV-initH-initHC-initICU-initICUC-initICUCV-initVent-initVentC-initQS-initQE-initQI-initQR-initQC-initEV-initER-initEVR-initVR-initQV-initQEV-initQEVR-initQER-initQVR-initHCICU-initHCV # Susceptible (non-immune)

Y<-c(initS,initE,initI,initR,initX,initH,initHC,initC,initCM,initV, initQS, initQE, initQI, initQR, initCL, initQC, initICU, initICUC, initICUCV, initVent, initVentC, initCMC,initZ, initEV, initER, initEVR, initVR, initQV,initQEV,initQEVR,initQER,initQVR,initHCICU,initHCV,initAb) # initial conditions for the main solution vector
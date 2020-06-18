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
parameters <- reactiveValuesToList(input)[c("p", "rho", "omega", "gamma", "nui", "report", "reportc", "reporth", 
                                              "beds_available", "icu_beds_available", "ventilators_available", 
                                              "pdeath_h", "pdeath_hc", "pdeath_icu", "pdeath_icuc", 
                                              "pdeath_vent", "pdeath_ventc", "ihr_scaling", "nus", 
                                              "nu_icu", "nu_vent", "rhos", "amp", 
                                              "pclin", "prob_icu", "prob_vent", "selfis_eff", "dist_eff", "hand_eff", 
                                              "work_eff", "w2h", "school_eff", "s2h", "cocoon_eff", "age_cocoon", 
                                              "vaccine_eff", "vac_campaign", "mean_imports", "screen_test_sens", 
                                              "screen_overdispersion", "quarantine_days", "quarantine_effort", 
                                              "quarantine_eff_home", "quarantine_eff_other", "household_size", 
                                              "noise", "iterations", "confidence")] %>% unlist()

parameters <- c(parameters, give = 95, nusc = input$nus, nu_icuc = input$nu_icu, nu_ventc = input$nu_vent,
                phi = which(month.name == input$phi))


# Transform parameters ----
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

# TODO: move this line to a better location
ihr[,2] <- parameters["ihr_scaling"]*ihr[,2]


# Define dataframe of interventions ----
inp <- bind_rows(interventions$baseline_mat %>% mutate(`Apply to` = "Baseline (Calibration)"),
                 interventions$future_mat %>% mutate(`Apply to` = "Hypothetical Scenario")) %>%
  rename(Intervention = intervention, `Date Start` = date_start, `Date End` = date_end, `Value` = value)


# initial conditions for the main solution vector ----
initS <- popstruc[, 2] - initE - initI - initR - initX - initV - initH - initHC -
  initQS - initQE - initQI - initQR - initCL - initQC - initICU - initICUC -
  initICUCV - initVent - initVentC  # Susceptible (non-immune)

Y <- c(initS, initE, initI, initR, initX, initH, initHC, initC, initCM, initV, initQS, initQE, initQI, 
     initQR, initCL, initQC, initICU, initICUC, initICUCV, initVent, initVentC, initCMC)

# Function to process ode outcome ----
process_ode_outcome <- function(out, parameters, nature){
  out <- out[[nature]]
  
  # define spline functions ----
  f <- c(1, (1 + parameters["give"]) / 2, (1 - parameters["give"]) / 2, 0)
  KH <- parameters["beds_available"]
  x.H <- c(0, (1 + parameters["give"]) * KH / 2, (3 - parameters["give"]) * KH / 2, 2 * KH)
  fH <- splinefun(x.H, f, method = "hyman")
  
  KICU <- parameters["icu_beds_available"] + parameters["ventilators_available"]
  x.ICU <- c(0, (1 + parameters["give"]) * KICU / 2, (3 - parameters["give"]) * KICU / 2, 2 * KICU)
  fICU <- splinefun(x.ICU, f, method = "hyman")
  
  # define critH, crit ----
  critH <- NULL
  crit <- NULL
  
  for (i in 1:length(times)) {
    critH[i] <- min(1 - fH((sum(out[i, (Hindex + 1)])) + sum(out[i, (ICUCindex + 1)]) + sum(out[i, (ICUCVindex + 1)])), 1)
    crit[i] <- min(1 - fICU((sum(out[i, (ICUindex + 1)])) + (sum(out[i, (Ventindex + 1)])) + (sum(out[i, (VentCindex + 1)]))))
  }
  
  # total population
  pop1<-out[,(Sindex+1)]+out[,(Eindex+1)]+out[,(Iindex+1)]+out[,(CLindex+1)]+out[,(Rindex+1)]+out[,(Xindex+1)]+out[,(Vindex+1)]+
    out[,(QSindex+1)]+out[,(QEindex+1)]+out[,(QIindex+1)]+out[,(QCindex+1)]+out[,(QRindex+1)]+
    out[,(Hindex+1)]+out[,(HCindex+1)]+out[,(ICUindex+1)]+out[,(ICUCindex+1)]+out[,(ICUCVindex+1)]+out[,(Ventindex+1)]+out[,(VentCindex+1)]
  tpop1<-rowSums(pop1)
  # time<-as.Date(out[,1]+startdate)
  # daily incidence
  inc1 <- parameters["report"]*parameters["gamma"]*(1-parameters["pclin"])*out[,(Eindex+1)]%*%(1-ihr[,2])+
    parameters["reportc"]*parameters["gamma"]*parameters["pclin"]*out[,(Eindex+1)]%*%(1-ihr[,2])+
    parameters["report"]*parameters["gamma"]*(1-parameters["pclin"])*out[,(QEindex+1)]%*%(1-ihr[,2])+
    parameters["reportc"]*parameters["gamma"]*parameters["pclin"]*out[,(QEindex+1)]%*%(1-ihr[,2])
  
  inc1h<- parameters["gamma"]*out[,(Eindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters["prob_icu"])*parameters["reporth"]+
    parameters["gamma"]*out[,(Eindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters["prob_icu"])*(1-parameters["reporth"])+
    parameters["gamma"]*out[,(QEindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters["prob_icu"])+
    parameters["gamma"]*out[,(Eindex+1)]%*%ihr[,2]*critH*parameters["reporth"]*(1-parameters["prob_icu"])+
    parameters["gamma"]*out[,(QEindex+1)]%*%ihr[,2]*critH*parameters["reporth"]*(1-parameters["prob_icu"])+
    parameters["gamma"]*out[,(Eindex+1)]%*%ihr[,2]*parameters["prob_icu"]+
    parameters["gamma"]*out[,(QEindex+1)]%*%ihr[,2]*parameters["prob_icu"]
  
  dailyinc1<-rowSums(inc1)+rowSums(inc1h)      # daily incidence
  cuminc1<-colSums(inc1)+colSums(inc1h)        # cumulative incidence
  previcureq1<-rowSums(out[,(Hindex+1)])+ rowSums(out[,(ICUCindex+1)])+rowSums(out[,(ICUCVindex+1)]) # surge beds occupancy
  previcureq21<-rowSums(out[,(ICUindex+1)])+rowSums(out[,(VentCindex+1)])   # icu beds occupancy
  previcureq31<-rowSums(out[,(Ventindex+1)])   # ventilator occupancy
  cmortality1<-rowSums(out[,(CMindex+1)])      # cumulative mortality
  overloadH1<-rowSums(out[,(HCindex+1)])       # requirement for beds
  overloadICU1<-rowSums(out[,(ICUCindex+1)])   # requirement for icu beds
  overloadICUV1<-rowSums(out[,(ICUCVindex+1)]) # requirement for ventilators
  overloadVent1<-rowSums(out[,(VentCindex+1)]) # requirement for ventilators
  ccases1<-rowSums(out[,(Cindex+1)])           # cumulative cases
  reqsurge1<-rowSums(out[,(Hindex+1)])+overloadH1
  reqicu1<-rowSums(out[,(ICUindex+1)])+overloadICU1
  reqvent1<-rowSums(out[,(Ventindex+1)])+overloadICUV1+overloadVent1
  
  inc_overloadH1<-((parameters["gamma"]*(1-parameters["prob_icu"])*out[,(Eindex+1)]))
  inc_overloadICU1<-((parameters["gamma"]*parameters["prob_icu"]*(1-parameters["prob_vent"])*out[,(Eindex+1)]))
  for (i in 1:length(times)) {
    inc_overloadH1[i,]<-inc_overloadH1[i,]*critH[i]*ihr[,2]
    inc_overloadICU1[i,]<-inc_overloadICU1[i,]*crit[i]*ihr[,2]
  }
  inc_overloadH1<-cumsum(rowSums(inc_overloadH1))
  inc_overloadICU1<-cumsum(rowSums(inc_overloadICU1))
  
  cinc_mort_H1 <- cumsum(rowSums(parameters["nus"]*parameters["pdeath_h"]*(out[,(Hindex+1)]%*%ifr[,2])))
  cinc_mort_HC1 <- cumsum(rowSums(parameters["nusc"]*parameters["pdeath_hc"]*(out[,(HCindex+1)]%*%ifr[,2])))
  cinc_mort_ICU1 <- cumsum(rowSums(parameters["nu_icu"]*parameters["pdeath_icu"]*out[,(ICUindex+1)]%*%ifr[,2]))
  cinc_mort_ICUC1 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["pdeath_icuc"]*out[,(ICUCindex+1)]%*%ifr[,2]))
  cinc_mort_ICUCV1 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*out[,(ICUCVindex+1)]%*%ifr[,2]))
  cinc_mort_Vent1 <- cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*out[,(Ventindex+1)]%*%ifr[,2]))
  cinc_mort_VentC1 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*out[,(VentCindex+1)]%*%ifr[,2]))
  base_mort_H1 <- cumsum(rowSums(out[,(Hindex+1)]%*%mort))
  base_mort_HC1 <- cumsum(rowSums(out[,(HCindex+1)]%*%mort))
  base_mort_ICU1 <- cumsum(rowSums(out[,(ICUindex+1)]%*%mort))
  base_mort_ICUC1 <- cumsum(rowSums(out[,(ICUCindex+1)]%*%mort))
  base_mort_ICUCV1 <- cumsum(rowSums(out[,(ICUCVindex+1)]%*%mort))
  base_mort_Vent1 <- cumsum(rowSums(out[,(Ventindex+1)]%*%mort))
  base_mort_VentC1 <- cumsum(rowSums(out[,(VentCindex+1)]%*%mort))
  base_mort_S1 <- cumsum(rowSums(out[,(Sindex+1)]%*%mort))
  base_mort_E1 <- cumsum(rowSums(out[,(Eindex+1)]%*%mort))
  base_mort_I1 <- cumsum(rowSums(out[,(Iindex+1)]%*%mort))
  base_mort_CL1 <- cumsum(rowSums(out[,(CLindex+1)]%*%mort))
  base_mort_X1 <- cumsum(rowSums(out[,(Xindex+1)]%*%mort))
  base_mort_QS1 <- cumsum(rowSums(out[,(QSindex+1)]%*%mort))
  base_mort_QE1 <- cumsum(rowSums(out[,(QEindex+1)]%*%mort))
  base_mort_QI1 <- cumsum(rowSums(out[,(QIindex+1)]%*%mort))
  base_mort_QC1 <- cumsum(rowSums(out[,(QCindex+1)]%*%mort))
  base_mort_QR1 <- cumsum(rowSums(out[,(QRindex+1)]%*%mort))
  base_mort_R1 <- cumsum(rowSums(out[,(Rindex+1)]%*%mort))
  
  Rt <- NULL
  for (i in (ceiling(1/parameters["nui"])+1):length(times)){
    Rt[i]<-cumsum(sum(parameters["gamma"]*out[i,(Eindex+1)]))/cumsum(sum(parameters["gamma"]*out[(i-1/parameters["nui"]),(Eindex+1)]))
    if(Rt[i] >= 7) {Rt[i] <- NA}
  }
  
  # Export in a cohesive format ----
  results <- list()
  results$time <- startdate + times  # dates
  results$Rt <- Rt
  results$cum_mortality <- round(cmortality1)  # cumulative mortality
  results$pct_total_pop_infected <- round(100 * tail(cumsum(rowSums(parameters["gamma"]*out[,(Eindex+1)])),1)/last(tpop1), 1)  # proportion of the  population that has been infected at the end of the simulation
  results$doubling_time <- round(log(2)*7 / (log(dailyinc1[2+7] / dailyinc1[2])), 2)  # (Baseline only) to double the number of infections at inception
  results$required_beds <- round(previcureq1)  # required beds
  results$saturation <- parameters["beds_available"]  # saturation
  results$daily_incidence <- round(dailyinc1)  # daily incidence (Reported)
  results$daily_total_cases <- round(rowSums(parameters["gamma"]*out[,(Eindex+1)]+parameters["gamma"]*out[,(QEindex+1)])) # daily incidence (Reported + Unreported)  # daily incidence (Reported + Unreported)
  results$hospital_surge_beds <- round(previcureq1)
  results$icu_beds <- round(previcureq21)
  results$ventilators <- round(previcureq31)
  results$normal_bed_requirement <- round(reqsurge1) #real required beds. previcureq1 above is the occupancy
  results$icu_bed_requirement <- round(reqicu1)
  results$icu_ventilator_requirement <- round(reqvent1)
  
  results$death_natural_non_exposed <- round(base_mort_S1)
  results$death_natural_exposed <- round(base_mort_E1 + base_mort_I1 + base_mort_CL1 + base_mort_X1 + base_mort_QS1 + 
                                           base_mort_QE1 + base_mort_QI1 + base_mort_QC1 + base_mort_QR1 + base_mort_R1+
                                           base_mort_H1+base_mort_HC1+base_mort_ICU1+base_mort_ICUC1+base_mort_ICUCV1+
                                           base_mort_Vent1+base_mort_VentC1)
  results$death_treated_hospital <- round(cinc_mort_H1)
  results$death_treated_icu <- round(cinc_mort_ICU1)
  results$death_treated_ventilator <- round(cinc_mort_Vent1)
  results$death_untreated_hospital <- round(cinc_mort_HC1)
  results$death_untreated_icu <- round(cinc_mort_ICUC1)
  results$death_untreated_ventilator <- round(cinc_mort_VentC1)+round(cinc_mort_ICUCV1)
  results$attributable_deaths <- results$death_treated_hospital + results$death_treated_icu + results$death_treated_ventilator +
    results$death_untreated_hospital + results$death_untreated_icu + results$death_untreated_ventilator
  results$attributable_deaths_end <- last(results$attributable_deaths)
  results$total_deaths <- results$attributable_deaths + results$death_natural_non_exposed + results$death_natural_exposed
  results$total_deaths_end <- last(results$total_deaths)
  results$total_reported_deaths_end <- last(results$cum_mortality)
  results$base_mort_H <- base_mort_H1
  results$base_mort_HC <- base_mort_HC1
  results$base_mort_ICU <- base_mort_ICU1
  results$base_mort_ICUC <- base_mort_ICUC1
  results$base_mort_ICUCV <- base_mort_ICUCV1
  results$base_mort_Vent <- base_mort_Vent1
  results$base_mort_VentC <- base_mort_VentC1
  results$base_mort_S <- base_mort_S1
  results$base_mort_E <- base_mort_E1
  results$base_mort_I <- base_mort_I1
  results$base_mort_CL <- base_mort_CL1
  results$base_mort_X <- base_mort_X1
  results$base_mort_QS <- base_mort_QS1
  results$base_mort_QE <- base_mort_QE1
  results$base_mort_QI <- base_mort_QI1
  results$base_mort_QC <- base_mort_QC1
  results$base_mort_QR <- base_mort_QR1
  results$base_mort_R <- base_mort_R1
  
  ## AGE DEPENDENT MORTALITY
  cinc_mort_H1 <- parameters["nus"]*parameters["pdeath_h"]*(out[,(Hindex+1)])
  cinc_mort_HC1 <- parameters["nusc"]*parameters["pdeath_hc"]*(out[,(HCindex+1)])
  cinc_mort_ICU1 <- parameters["nu_icu"]*parameters["pdeath_icu"]*out[,(ICUindex+1)]
  cinc_mort_ICUC1 <- parameters["nu_icuc"]*parameters["pdeath_icuc"]*out[,(ICUCindex+1)] 
  cinc_mort_ICUCV1 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*out[,(ICUCVindex+1)]
  cinc_mort_Vent1 <- parameters["nu_vent"]*parameters["pdeath_vent"]*out[,(Ventindex+1)] 
  cinc_mort_VentC1 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*out[,(VentCindex+1)] 
  totage1<-as.data.frame(cinc_mort_H1+cinc_mort_HC1+cinc_mort_ICU1+cinc_mort_ICUC1+cinc_mort_ICUCV1+cinc_mort_Vent1+cinc_mort_VentC1)
  basemort_H1<-(out[,(Hindex+1)])
  basemort_HC1<-(out[,(HCindex+1)])
  basemort_ICU1<-(out[,(ICUindex+1)])
  basemort_ICUC1<-(out[,(ICUCindex+1)])
  basemort_ICUCV1<-(out[,(ICUCVindex+1)])
  basemort_Vent1<-(out[,(Ventindex+1)])
  basemort_VentC1<-(out[,(VentCindex+1)])
  totbase1<-as.data.frame(basemort_H1+basemort_HC1+basemort_ICU1+basemort_ICUC1+basemort_ICUCV1+basemort_Vent1+basemort_VentC1)
  tc<-c()
  
  for (i in 1:dim(cinc_mort_H1)[1]) {
    for (j in 1:dim(cinc_mort_H1)[2]) {
      tc<-rbind(tc,c(i, j, totage1[i,j]*ifr[j,2]+totbase1[i,j]*mort[j])) 
    }
  }
  tc<-as.data.frame(tc)
  colnames(tc)<-c("Day","Age","value")
  
  results$tc <- tc %>%
    mutate(Date = startdate + Day,
           age_cat = case_when(
             Age >=  1 & Age <= 6   ~ "≤ 30 y.o.",
             Age >  6 & Age <= 8    ~ "30-40 y.o.",
             Age >  8 & Age <= 10    ~ "40-50 y.o.",
             Age >  10 & Age <= 12    ~ "50-60 y.o.",
             Age >  12 & Age <= 14    ~ "60-70 y.o.",
             Age >=  15  ~ "≥ 70 y.o.")) %>%
    mutate(age_cat = factor(age_cat, levels = rev(c("≤ 30 y.o.", "30-40 y.o.",
                                                    "40-50 y.o.", "50-60 y.o.", "60-70 y.o.", "≥ 70 y.o."))))
  
  mortality_lag <- data.frame(Age = popstruc$agefloor)
  if(nrow(out) >= 30)  mortality_lag <- bind_cols(mortality_lag, 
                                                  data.frame(day30 = out[30,CMindex+1]/out[30,Cindex+1]) %>%
                                                    mutate(day30 = ifelse(is.infinite(day30), 0, day30)) %>%
                                                    rename(`Day 30` = day30))
  if(nrow(out) >= 60)  mortality_lag <- bind_cols(mortality_lag, 
                                                  data.frame(day60 = out[60,CMindex+1]/out[60,Cindex+1]) %>%
                                                    mutate(day60 = ifelse(is.infinite(day60), 0, day60)) %>%
                                                    rename(`Day 60` = day60))
  if(nrow(out) >= 90)  mortality_lag <- bind_cols(mortality_lag, 
                                                  data.frame(day90 = out[90,CMindex+1]/out[90,Cindex+1]) %>%
                                                    mutate(day90 = ifelse(is.infinite(day90), 0, day90)) %>%
                                                    rename(`Day 90` = day90))
  if(nrow(out) >= 120)  mortality_lag <- bind_cols(mortality_lag, 
                                                   data.frame(day120 = out[120,CMindex+1]/out[120,Cindex+1]) %>%
                                                     mutate(day120 = ifelse(is.infinite(day120), 0, day120)) %>%
                                                     rename(`Day 120` = day120))
  
  results$mortality_lag <- mortality_lag

  return(results)
}
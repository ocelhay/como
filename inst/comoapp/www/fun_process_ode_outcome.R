process_ode_outcome <- function(out, parameters, startdate, times, ihr, ifr, mort, popstruc){
  
  out_min<-out$min
  out_max<-out$max
  out_med<-out$mean
  
  critH<-c()
  crit<-c()
  critV<-c()
  
  f <- c(1,(1+parameters["give"])/2,(1-parameters["give"])/2,0)
  KH<-parameters["beds_available"]
  KICU<- parameters["icu_beds_available"]+parameters["ventilators_available"]
  Kvent<- parameters["ventilators_available"]
  x.H <- c(0,(1+parameters["give"])*KH/2,(3-parameters["give"])*KH/2,2*KH)
  x.ICU <- c(0,(1+parameters["give"])*KICU/2,(3-parameters["give"])*KICU/2,2*KICU)
  x.Vent <- c(0,(1+parameters["give"])*Kvent/2,(3-parameters["give"])*Kvent/2,2*Kvent)
  fH <- splinefun(x.H, f, method = "hyman")
  fICU <- splinefun(x.ICU, f, method = "hyman")
  fVent<- splinefun(x.Vent, f, method = "hyman")
  
  for (i in 1:length(times)){
    critH[i]<-min(1-fH((sum(out_med[i,(Hindex+1)]))+sum(out_med[i,(ICUCindex+1)])+sum(out_med[i,(ICUCVindex+1)])),1)
    crit[i]<-min(1-fICU((sum(out_med[i,(ICUindex+1)]))+(sum(out_med[i,(Ventindex+1)]))+(sum(out_med[i,(VentCindex+1)]))))
    critV[i]<-min(1-fVent((sum(out_med[i,(Ventindex+1)]))),1)
  }
  
  # total population
  pop1<-out_med[,(Sindex+1)]+out_med[,(Eindex+1)]+out_med[,(Iindex+1)]+out_med[,(CLindex+1)]+out_med[,(Rindex+1)]+out_med[,(Xindex+1)]+out_med[,(Vindex+1)]+
    out_med[,(QSindex+1)]+out_med[,(QEindex+1)]+out_med[,(QIindex+1)]+out_med[,(QCindex+1)]+out_med[,(QRindex+1)]+
    out_med[,(Hindex+1)]+out_med[,(HCindex+1)]+out_med[,(ICUindex+1)]+out_med[,(ICUCindex+1)]+out_med[,(ICUCVindex+1)]+out_med[,(Ventindex+1)]+out_med[,(VentCindex+1)]
  tpop1<-rowSums(pop1)
  time<-as.Date(out_med[,1]+startdate)
  
  # added rowSums to the next two lines TODO Check if okay
  dailyinc1<-rowSums(out$mean_cases)      # daily incidence
  cuminc1<-rowSums(out$mean_cum_cases)       # cumulative incidence
  previcureq1<-rowSums(out_med[,(Hindex+1)])+ rowSums(out_med[,(ICUCindex+1)])+rowSums(out_med[,(ICUCVindex+1)]) # surge beds occupancy
  previcureq21<-rowSums(out_med[,(ICUindex+1)])+rowSums(out_med[,(VentCindex+1)])   # icu beds occupancy
  previcureq31<-rowSums(out_med[,(Ventindex+1)])   # ventilator occupancy
  cmortality1<-rowSums(out_med[,(CMindex+1)])      # cumulative mortality
  overloadH1<-rowSums(out_med[,(HCindex+1)])       # requirement for beds
  overloadICU1<-rowSums(out_med[,(ICUCindex+1)])   # requirement for icu beds
  overloadICUV1<-rowSums(out_med[,(ICUCVindex+1)]) # requirement for ventilators
  overloadVent1<-rowSums(out_med[,(VentCindex+1)]) # requirement for ventilators
  ccases1<-rowSums(out_med[,(Cindex+1)])           # cumulative cases
  reqsurge1<-rowSums(out_med[,(Hindex+1)])+overloadH1
  reqicu1<-rowSums(out_med[,(ICUindex+1)])+overloadICU1
  reqvent1<-rowSums(out_med[,(Ventindex+1)])+overloadICUV1+overloadVent1
  
  
  ##########################    CALCULATE MORTALITY 
  pdeath_hc<-parameters["pdeath_hc"]
  prob_icu<-parameters["prob_icu"]
  prob_vent<-parameters["prob_vent"]
  pdeath_icuc<-parameters["pdeath_icuc"]
  pdeath_ventc<-parameters["pdeath_ventc"]
  
  cinc_mort_H1 <- cumsum(rowSums(parameters["nus"]*parameters["pdeath_h"]*(out_med[,(Hindex+1)]%*%ifr[,2])))
  cinc_mort_HC1 <- cumsum(rowSums(parameters["nusc"]*parameters["pdeath_hc"]*(out_med[,(HCindex+1)]%*%ifr[,2])))
  cinc_mort_ICU1 <- cumsum(rowSums(parameters["nu_icu"]*parameters["pdeath_icu"]*out_med[,(ICUindex+1)]%*%ifr[,2]))
  cinc_mort_ICUC1 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["pdeath_icuc"]*out_med[,(ICUCindex+1)]%*%ifr[,2]))
  cinc_mort_ICUCV1 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*out_med[,(ICUCVindex+1)]%*%ifr[,2]))
  cinc_mort_Vent1 <- cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*out_med[,(Ventindex+1)]%*%ifr[,2]))
  cinc_mort_VentC1 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*out_med[,(VentCindex+1)]%*%ifr[,2]))
  base_mort_H1 <- cumsum(rowSums(out_med[,(Hindex+1)]%*%mort))
  base_mort_HC1 <- cumsum(rowSums(out_med[,(HCindex+1)]%*%mort))
  base_mort_ICU1 <- cumsum(rowSums(out_med[,(ICUindex+1)]%*%mort))
  base_mort_ICUC1 <- cumsum(rowSums(out_med[,(ICUCindex+1)]%*%mort))
  base_mort_ICUCV1 <- cumsum(rowSums(out_med[,(ICUCVindex+1)]%*%mort))
  base_mort_Vent1 <- cumsum(rowSums(out_med[,(Ventindex+1)]%*%mort))
  base_mort_VentC1 <- cumsum(rowSums(out_med[,(VentCindex+1)]%*%mort))
  base_mort_S1 <- cumsum(rowSums(out_med[,(Sindex+1)]%*%mort))
  base_mort_E1 <- cumsum(rowSums(out_med[,(Eindex+1)]%*%mort))
  base_mort_I1 <- cumsum(rowSums(out_med[,(Iindex+1)]%*%mort))
  base_mort_CL1 <- cumsum(rowSums(out_med[,(CLindex+1)]%*%mort))
  base_mort_X1 <- cumsum(rowSums(out_med[,(Xindex+1)]%*%mort))
  base_mort_QS1 <- cumsum(rowSums(out_med[,(QSindex+1)]%*%mort))
  base_mort_QE1 <- cumsum(rowSums(out_med[,(QEindex+1)]%*%mort))
  base_mort_QI1 <- cumsum(rowSums(out_med[,(QIindex+1)]%*%mort))
  base_mort_QC1 <- cumsum(rowSums(out_med[,(QCindex+1)]%*%mort))
  base_mort_QR1 <- cumsum(rowSums(out_med[,(QRindex+1)]%*%mort))
  base_mort_R1 <- cumsum(rowSums(out_med[,(Rindex+1)]%*%mort))
  
  
  # Export in a cohesive format ----
  results <- list()
  results$time <- startdate + times  # dates
  results$Rt <- out$mean_Rt
  results$cum_mortality <- round(cmortality1)  # cumulative mortality
  results$pct_total_pop_infected <- out$mean_infections
  results$doubling_time <- round(log(2)*7 / (log(dailyinc1[2+7] / dailyinc1[2])), 2)  # (Baseline only) to double the number of infections at inception
  results$required_beds <- round(previcureq1)  # required beds
  results$saturation <- parameters["beds_available"]  # saturation
  results$daily_incidence <- round(dailyinc1)  # daily incidence (Reported)
  results$daily_total_cases <- round(out$mean_daily_infection) # daily incidence (Reported + Unreported)  # daily incidence (Reported + Unreported)
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
  cinc_mort_H1 <- parameters["nus"]*parameters["pdeath_h"]*(out_med[,(Hindex+1)])
  cinc_mort_HC1 <- parameters["nusc"]*parameters["pdeath_hc"]*(out_med[,(HCindex+1)])
  cinc_mort_ICU1 <- parameters["nu_icu"]*parameters["pdeath_icu"]*out_med[,(ICUindex+1)]
  cinc_mort_ICUC1 <- parameters["nu_icuc"]*parameters["pdeath_icuc"]*out_med[,(ICUCindex+1)] 
  cinc_mort_ICUCV1 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*out_med[,(ICUCVindex+1)]
  cinc_mort_Vent1 <- parameters["nu_vent"]*parameters["pdeath_vent"]*out_med[,(Ventindex+1)] 
  cinc_mort_VentC1 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*out_med[,(VentCindex+1)] 
  totage1<-as.data.frame(cinc_mort_H1+cinc_mort_HC1+cinc_mort_ICU1+cinc_mort_ICUC1+cinc_mort_ICUCV1+cinc_mort_Vent1+cinc_mort_VentC1)
  basemort_H1<-(out_med[,(Hindex+1)])
  basemort_HC1<-(out_med[,(HCindex+1)])
  basemort_ICU1<-(out_med[,(ICUindex+1)])
  basemort_ICUC1<-(out_med[,(ICUCindex+1)])
  basemort_ICUCV1<-(out_med[,(ICUCVindex+1)])
  basemort_Vent1<-(out_med[,(Ventindex+1)])
  basemort_VentC1<-(out_med[,(VentCindex+1)])
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
  if(nrow(out_med) >= 30)  mortality_lag <- bind_cols(mortality_lag, 
                                                      data.frame(day30 = out_med[30,CMindex+1]/out_med[30,Cindex+1]) %>%
                                                        mutate(day30 = ifelse(is.infinite(day30), 0, day30)) %>%
                                                        rename(`Day 30` = day30))
  if(nrow(out_med) >= 60)  mortality_lag <- bind_cols(mortality_lag, 
                                                      data.frame(day60 = out_med[60,CMindex+1]/out_med[60,Cindex+1]) %>%
                                                        mutate(day60 = ifelse(is.infinite(day60), 0, day60)) %>%
                                                        rename(`Day 60` = day60))
  if(nrow(out_med) >= 90)  mortality_lag <- bind_cols(mortality_lag, 
                                                      data.frame(day90 = out_med[90,CMindex+1]/out_med[90,Cindex+1]) %>%
                                                        mutate(day90 = ifelse(is.infinite(day90), 0, day90)) %>%
                                                        rename(`Day 90` = day90))
  if(nrow(out_med) >= 120)  mortality_lag <- bind_cols(mortality_lag, 
                                                       data.frame(day120 = out_med[120,CMindex+1]/out_med[120,Cindex+1]) %>%
                                                         mutate(day120 = ifelse(is.infinite(day120), 0, day120)) %>%
                                                         rename(`Day 120` = day120))
  
  results$mortality_lag <- mortality_lag
  
  previcureq1_max<-rowSums(out_max[,(Hindex+1)])+ rowSums(out_max[,(ICUCindex+1)])+rowSums(out_max[,(ICUCVindex+1)]) # surge beds occupancy
  previcureq21_max<-rowSums(out_max[,(ICUindex+1)])+rowSums(out_max[,(VentCindex+1)])   # icu beds occupancy
  previcureq31_max<-rowSums(out_max[,(Ventindex+1)])   # ventilator occupancy
  cmortality1_max<-rowSums(out_max[,(CMindex+1)])      # cumulative mortality
  overloadH1_max<-rowSums(out_max[,(HCindex+1)])       # requirement for beds
  overloadICU1_max<-rowSums(out_max[,(ICUCindex+1)])   # requirement for icu beds
  overloadICUV1_max<-rowSums(out_max[,(ICUCVindex+1)]) # requirement for ventilators
  overloadVent1_max<-rowSums(out_max[,(VentCindex+1)]) # requirement for ventilators
  ccases1_max<-rowSums(out_max[,(Cindex+1)])           # cumulative cases
  reqsurge1_max<-rowSums(out_max[,(Hindex+1)])+overloadH1  # surge beds total requirements
  reqicu1_max<-rowSums(out_max[,(ICUindex+1)])+overloadICU1 # ICU beds total requirements
  reqvent1_max<-rowSums(out_max[,(Ventindex+1)])+overloadICUV1+overloadVent1 # ventilator beds total requirements
  
  previcureq1_min<-rowSums(out_min[,(Hindex+1)])+ rowSums(out_min[,(ICUCindex+1)])+rowSums(out_min[,(ICUCVindex+1)]) # surge beds occupancy
  previcureq21_min<-rowSums(out_min[,(ICUindex+1)])+rowSums(out_min[,(VentCindex+1)])   # icu beds occupancy
  previcureq31_min<-rowSums(out_min[,(Ventindex+1)])   # ventilator occupancy
  cmortality1_min<-rowSums(out_min[,(CMindex+1)])      # cumulative mortality
  overloadH1_min<-rowSums(out_min[,(HCindex+1)])       # requirement for beds
  overloadICU1_min<-rowSums(out_min[,(ICUCindex+1)])   # requirement for icu beds
  overloadICUV1_min<-rowSums(out_min[,(ICUCVindex+1)]) # requirement for ventilators
  overloadVent1_min<-rowSums(out_min[,(VentCindex+1)]) # requirement for ventilators
  ccases1_min<-rowSums(out_min[,(Cindex+1)])           # cumulative cases
  reqsurge1_min<-rowSums(out_min[,(Hindex+1)])+overloadH1  # surge beds total requirements
  reqicu1_min<-rowSums(out_min[,(ICUindex+1)])+overloadICU1 # ICU beds total requirements
  reqvent1_min<-rowSums(out_min[,(Ventindex+1)])+overloadICUV1+overloadVent1 # ventilator beds total requirements
  
  results$Rt_max <- out$max_Rt
  results$Rt_min <- out$min_Rt
  
  results$daily_incidence_max <- out$max_cases
  results$daily_incidence_min <- out$min_cases  
  
  results$daily_total_cases_max <- out$max_daily_infection
  results$daily_total_cases_min <- out$min_daily_infection
  
  results$total_reported_deaths_end_min <- last(cmortality1_min)
  results$total_reported_deaths_end_max <- last(cmortality1_max)
  
  results$pct_total_pop_infected_min <- out$min_infections  # proportion of the  population that has been infected at the end of the simulation
  results$pct_total_pop_infected_max <- out$max_infections  # proportion of the  population that has been infected at the end of the simulation
  
  return(results)
}
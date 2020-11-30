process_ode_outcome <- function(out, iterations,intv_vector){
  # out_min<-out$min
  # out_max<-out$max
  # out_mean<-out$mean
  # 
  # critH<-c()
  # crit<-c()
  # critV<-c()
  # 
  # for (i in 1:length(times)){
  #   critH[i]<-min(1-fH((sum(out_mean[i,(Hindex+1)]))+sum(out_mean[i,(ICUCindex+1)])+sum(out_mean[i,(ICUCVindex+1)])),1)
  #   crit[i]<-min(1-fICU((sum(out_mean[i,(ICUindex+1)]))+(sum(out_mean[i,(Ventindex+1)]))+(sum(out_mean[i,(VentCindex+1)]))))
  #   critV[i]<-min(1-fVent((sum(out_mean[i,(Ventindex+1)]))),1)
  # }
  
  # total population
  pop1<-out_mean[,(Sindex+1)]+out_mean[,(Eindex+1)]+out_mean[,(Iindex+1)]+out_mean[,(CLindex+1)]+out_mean[,(Rindex+1)]+
    out_mean[,(Xindex+1)]+out_mean[,(Vindex+1)]+out_mean[,(Zindex+1)]+out_mean[,(EVindex+1)]+out_mean[,(ERindex+1)]+out_mean[,(EVRindex+1)]+
    out_mean[,(QSindex+1)]+out_mean[,(QEindex+1)]+out_mean[,(QIindex+1)]+out_mean[,(QCindex+1)]+out_mean[,(QRindex+1)]+
    out_mean[,(QVindex+1)]+out_mean[,(QEVindex+1)]+out_mean[,(QERindex+1)]+out_mean[,(QVRindex+1)]+out_mean[,(QEVRindex+1)]+
    out_mean[,(Hindex+1)]+out_mean[,(HCindex+1)]+out_mean[,(ICUindex+1)]+out_mean[,(ICUCindex+1)]+out_mean[,(ICUCVindex+1)]+
    out_mean[,(Ventindex+1)]+out_mean[,(VentCindex+1)]+out_mean[,(HCICUindex+1)]+out_mean[,(HCVindex+1)]
  tpop1<-rowSums(pop1)
  
  
  ##########################    AB prevalence
  # ab_age<-out_mean[,(Abindex+1)]
  # ab_all_ages<-rowSums(out_mean[,(Abindex+1)])
  
  ##########################    CALCULATE MORTALITY 
  # dexo2_hist <- rep(0,length(times))
  # dexo2c_hist <- rep(0,length(times))
  # dexv_hist <- rep(0,length(times))
  # dexvc_hist <- rep(0,length(times))
  # for (tt in times) {
  #   if(tt < max(times)){
  #     if(intv_vector$dex[tt*20+1]) {
  #       dexo2_hist[tt+1] <- parameters["dexo2"]
  #       dexo2c_hist[tt+1] <- parameters["dexo2c"]
  #       dexv_hist[tt+1] <- parameters["dexv"]
  #       dexvc_hist[tt+1] <- parameters["dexvc"]
  #     } else {
  #       dexo2_hist[tt+1] <- 1
  #       dexo2c_hist[tt+1] <- 1
  #       dexv_hist[tt+1] <- 1
  #       dexvc_hist[tt+1] <- 1
  #     }
  #   } else {
  #     dexo2_hist[tt+1] <- dexo2_hist[tt]
  #     dexo2c_hist[tt+1] <- dexo2c_hist[tt]
  #     dexv_hist[tt+1] <- dexv_hist[tt]
  #     dexvc_hist[tt+1] <- dexvc_hist[tt]
  #   }
  # }
  
  # cinc_mort_1 <- cumsum(rowSums(parameters["nus"]*parameters["propo2"]*parameters["pdeath_ho"]*dexo2_hist*(out_mean[,(Hindex+1)]%*%ifr[,2])))
  # cinc_mort_2 <- cumsum(rowSums(parameters["nus"]*(1-parameters["propo2"])*parameters["pdeath_h"]*(out_mean[,(Hindex+1)]%*%ifr[,2])))
  # 
  # cinc_mort_3 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_hco"]*(out_mean[,(HCindex+1)]%*%ifr[,2])))
  # cinc_mort_4 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_hc"]*(out_mean[,(HCindex+1)]%*%ifr[,2])))
  # 
  # cinc_mort_5 <- cumsum(rowSums(parameters["nu_icu"]*parameters["propo2"]*parameters["pdeath_icuo"]*dexo2_hist*(out_mean[,(ICUindex+1)]%*%ifr[,2])))
  # cinc_mort_6 <- cumsum(rowSums(parameters["nu_icu"]*(1-parameters["propo2"])*parameters["pdeath_icu"]*(out_mean[,(ICUindex+1)]%*%ifr[,2])))
  # cinc_mort_7 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["propo2"]*parameters["pdeath_icuco"]*dexo2c_hist*(out_mean[,(ICUCindex+1)]%*%ifr[,2])))
  # cinc_mort_8 <- cumsum(rowSums(parameters["nu_icuc"]*(1-parameters["propo2"])*parameters["pdeath_icuc"]*(out_mean[,(ICUCindex+1)]%*%ifr[,2])))
  # 
  # cinc_mort_9 <- cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*dexv_hist*(out_mean[,(Ventindex+1)]%*%ifr[,2])))
  # cinc_mort_10 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_mean[,(VentCindex+1)]%*%ifr[,2])))
  # cinc_mort_11 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_mean[,(ICUCVindex+1)]%*%ifr[,2])))
  # 
  # cinc_mort_12 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_icu_hco"]*(out_mean[,(HCICUindex+1)]%*%ifr[,2])))
  # cinc_mort_13 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_icu_hc"]*(out_mean[,(HCICUindex+1)]%*%ifr[,2])))
  # cinc_mort_14 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["report_death_HC"]*parameters["pdeath_vent_hc"]*(out_mean[,(HCVindex+1)]%*%ifr[,2])))
  # 
  # cinc_mort_121 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["propo2"]*parameters["pdeath_icu_hco"]*(out_mean[,(HCICUindex+1)]%*%ifr[,2])))
  # cinc_mort_131 <- cumsum(rowSums(parameters["nu_icuc"]*(1-parameters["propo2"])*parameters["pdeath_icu_hc"]*(out_mean[,(HCICUindex+1)]%*%ifr[,2])))
  # cinc_mort_141 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_vent_hc"]*(out_mean[,(HCVindex+1)]%*%ifr[,2])))
  # 
  # 
  # cinc_mort_H1 <- cinc_mort_1 + cinc_mort_2
  # cinc_mort_HC1 <- cinc_mort_3 + cinc_mort_4 + cinc_mort_12 + cinc_mort_13 + cinc_mort_14
  # cinc_mort_ICU1 <- cinc_mort_5 + cinc_mort_6
  # cinc_mort_ICUC1 <- cinc_mort_7 + cinc_mort_8
  # cinc_mort_Vent1 <- cinc_mort_9
  # cinc_mort_VentC1 <- cinc_mort_10
  # cinc_mort_ICUCV1 <- cinc_mort_11
  # 
  # # all deaths due to covid19 disease - reported + unreported
  # cinc_mort_all<-cinc_mort_1+cinc_mort_2+cinc_mort_3+cinc_mort_4+cinc_mort_5+cinc_mort_6+
  #   cinc_mort_7+cinc_mort_8+cinc_mort_9+cinc_mort_10+cinc_mort_11+cinc_mort_121+cinc_mort_131+cinc_mort_141
  
  # base_mort_H1 <- cumsum(rowSums(out_mean[,(Hindex+1)]%*%mort))
  # base_mort_HC1 <- cumsum(rowSums(parameters["report_death_HC"]*out_mean[,(HCindex+1)]%*%mort))
  # base_mort_ICU1 <- cumsum(rowSums(out_mean[,(ICUindex+1)]%*%mort))
  # base_mort_ICUC1 <- cumsum(rowSums(out_mean[,(ICUCindex+1)]%*%mort))
  # base_mort_ICUCV1 <- cumsum(rowSums(out_mean[,(ICUCVindex+1)]%*%mort))
  # base_mort_Vent1 <- cumsum(rowSums(out_mean[,(Ventindex+1)]%*%mort))
  # base_mort_VentC1 <- cumsum(rowSums(out_mean[,(VentCindex+1)]%*%mort))
  # base_mort_Z1 <- cumsum(rowSums(out_mean[,(Zindex+1)]%*%mort))
  # base_mort_HCICU1 <- cumsum(rowSums(parameters["report_death_HC"]*out_mean[,(HCICUindex+1)]%*%mort))
  # base_mort_HCV1 <- cumsum(rowSums(parameters["report_death_HC"]*out_mean[,(HCVindex+1)]%*%mort))
  # 
  # base_mort_V1 <- cumsum(rowSums(out_mean[,(Vindex+1)]%*%mort))
  # base_mort_S1 <- cumsum(rowSums(out_mean[,(Sindex+1)]%*%mort))
  # base_mort_QS1 <- cumsum(rowSums(out_mean[,(QSindex+1)]%*%mort))
  # base_mort_QR1 <- cumsum(rowSums(out_mean[,(QRindex+1)]%*%mort))
  # base_mort_R1 <- cumsum(rowSums(out_mean[,(Rindex+1)]%*%mort))
  # base_mort_QVR1 <- cumsum(rowSums(out_mean[,(QVRindex+1)]%*%mort))
  # base_mort_VR1 <- cumsum(rowSums(out_mean[,(VRindex+1)]%*%mort))
  # base_mort_QV1 <- cumsum(rowSums(out_mean[,(QVindex+1)]%*%mort))
  # 
  # base_mort_E1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(Eindex+1)]%*%mort))
  # base_mort_I1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(Iindex+1)]%*%mort))
  # base_mort_CL1 <- cumsum(rowSums(parameters["report_natdeathCL"]*out_mean[,(CLindex+1)]%*%mort))
  # base_mort_X1 <- cumsum(rowSums(parameters["report_natdeathCL"]*out_mean[,(Xindex+1)]%*%mort))
  # base_mort_QE1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(QEindex+1)]%*%mort))
  # base_mort_QI1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(QIindex+1)]%*%mort))
  # base_mort_QC1 <- cumsum(rowSums(parameters["report_natdeathCL"]*out_mean[,(QCindex+1)]%*%mort))
  # base_mort_ER1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(ERindex+1)]%*%mort))
  # base_mort_EV1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(EVindex+1)]%*%mort))
  # base_mort_EVR1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(EVRindex+1)]%*%mort))
  # base_mort_QEV1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(QEVindex+1)]%*%mort))
  # base_mort_QER1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(QERindex+1)]%*%mort))
  # base_mort_QEVR1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(QEVRindex+1)]%*%mort))
  # 
  # 
  # base_mort_HC11 <- cumsum(rowSums(out_mean[,(HCindex+1)]%*%mort))
  # base_mort_HCICU11 <- cumsum(rowSums(out_mean[,(HCICUindex+1)]%*%mort))
  # base_mort_HCV11 <- cumsum(rowSums(out_mean[,(HCVindex+1)]%*%mort))
  # base_mort_E11 <- cumsum(rowSums(out_mean[,(Eindex+1)]%*%mort))
  # base_mort_I11 <- cumsum(rowSums(out_mean[,(Iindex+1)]%*%mort))
  # base_mort_CL11 <- cumsum(rowSums(out_mean[,(CLindex+1)]%*%mort))
  # base_mort_X11 <- cumsum(rowSums(out_mean[,(Xindex+1)]%*%mort))
  # base_mort_QE11 <- cumsum(rowSums(out_mean[,(QEindex+1)]%*%mort))
  # base_mort_QI11 <- cumsum(rowSums(out_mean[,(QIindex+1)]%*%mort))
  # base_mort_QC11 <- cumsum(rowSums(out_mean[,(QCindex+1)]%*%mort))
  # base_mort_ER11 <- cumsum(rowSums(out_mean[,(ERindex+1)]%*%mort))
  # base_mort_EV11 <- cumsum(rowSums(out_mean[,(EVindex+1)]%*%mort))
  # base_mort_EVR11 <- cumsum(rowSums(out_mean[,(EVRindex+1)]%*%mort))
  # base_mort_QEV11 <- cumsum(rowSums(out_mean[,(QEVindex+1)]%*%mort))
  # base_mort_QER11 <- cumsum(rowSums(out_mean[,(QERindex+1)]%*%mort))
  # base_mort_QEVR11 <- cumsum(rowSums(out_mean[,(QEVRindex+1)]%*%mort))
  # 
  # # all deaths of infected with sars-cov-2 virus - reported + unreported
  # nat_deaths_inf <- round(base_mort_E11 + base_mort_I11 + base_mort_CL11 + base_mort_X11 +
  #                           base_mort_ER11 + base_mort_EV11+  base_mort_EVR11+
  #                           base_mort_QE11 + base_mort_QI11 + base_mort_QC11 +
  #                           base_mort_QEV11 + base_mort_QER11 + base_mort_QEVR11 + base_mort_Z1+
  #                           base_mort_H1+base_mort_HC11+base_mort_ICU1+base_mort_ICUC1+base_mort_ICUCV1+
  #                           base_mort_Vent1+base_mort_VentC1+base_mort_HCICU11+base_mort_HCV11)
  
  # Export in a cohesive format ----
  results <- list()
  results$time <- startdate + times  # dates
  results$N <- tpop1
  
  ## Ab
  results$ab_all_ages<-ab_all_ages
  results$ab<-ab_age
  
  # Rt/ FOI
  dailyinc1<-out$mean_cases         # daily incidence
  results$Rt <- out$mean_Rt
  results$pct_total_pop_infected <- out$mean_infections
  results$doubling_time <- round(log(2)*7 / (log(dailyinc1[2+7] / dailyinc1[2])), 2)  # (Baseline only) to double the number of infections at inception
  results$daily_incidence <- round(dailyinc1)  # daily incidence (Reported)
  results$daily_total_cases <- round(out$mean_daily_infection) # daily incidence (Reported + Unreported)  # daily incidence (Reported + Unreported)
  
  # Hospital requirements
  previcureq1<-rowSums(out_mean[,(Hindex+1)])+ rowSums(out_mean[,(ICUCindex+1)])+rowSums(out_mean[,(ICUCVindex+1)]) # surge beds occupancy
  previcureq21<-rowSums(out_mean[,(ICUindex+1)])+rowSums(out_mean[,(VentCindex+1)])   # icu beds occupancy
  previcureq31<-rowSums(out_mean[,(Ventindex+1)])   # ventilator occupancy
  overloadH1<-rowSums(out_mean[,(HCindex+1)])       # requirement for beds
  overloadICU1<-rowSums(out_mean[,(ICUCindex+1)])+rowSums(out_mean[,(HCICUindex+1)])   # requirement for icu beds
  overloadICUV1<-rowSums(out_mean[,(ICUCVindex+1)]) # requirement for ventilators
  overloadVent1<-rowSums(out_mean[,(VentCindex+1)])+rowSums(out_mean[,(HCVindex+1)]) # requirement for ventilators
  
  results$required_beds <- round(previcureq1)  # required beds
  results$saturation <- parameters["beds_available"]  # saturation
  results$hospital_surge_beds <- round(previcureq1)
  results$icu_beds <- round(previcureq21)
  results$ventilators <- round(previcureq31)
  results$normal_bed_requirement <- round(rowSums(out_mean[,(Hindex+1)])+overloadH1)   #real required beds. previcureq1 above is the occupancy
  results$icu_bed_requirement <- round(rowSums(out_mean[,(ICUindex+1)])+overloadICU1)
  results$icu_ventilator_requirement <- round(rowSums(out_mean[,(Ventindex+1)])+overloadICUV1+overloadVent1)
  
  ### MORTALITY
  results$cum_mortality <- round(rowSums(out_mean[,(CMindex+1)]))       # cumulative mortality
  results$deaths_from_covid<-last(cinc_mort_all)
  results$deaths_with_covid<-last(nat_deaths_inf)
  results$death_natural_non_exposed <- round(base_mort_S1+base_mort_V1+base_mort_QS1)
  results$death_natural_exposed <- round(base_mort_E1 + base_mort_I1 + base_mort_CL1 + base_mort_X1 + 
                                           base_mort_R1+ base_mort_ER1 + base_mort_EV1+  base_mort_EVR1+   
                                           base_mort_QE1 + base_mort_QI1 + base_mort_QC1 + base_mort_QR1 + 
                                           base_mort_QEV1 + base_mort_QER1 + base_mort_QEVR1 + base_mort_QVR1 +
                                           base_mort_H1+base_mort_HC1+base_mort_ICU1+base_mort_ICUC1+base_mort_ICUCV1+
                                           base_mort_Vent1+base_mort_VentC1+base_mort_HCICU1+base_mort_HCV1)
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
  
  
  # ## AGE DEPENDENT MORTALITY
  cinc_mort_H1 <- parameters["nus"]*parameters["propo2"]*parameters["pdeath_ho"]*dexo2_hist*(out_mean[,(Hindex+1)]%*%ifr[,2])+
    parameters["nus"]*(1-parameters["propo2"])*parameters["pdeath_h"]*(out_mean[,(Hindex+1)]%*%ifr[,2])
  cinc_mort_HC1 <- parameters["nusc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_hco"]*(out_mean[,(HCindex+1)]%*%ifr[,2])+
    parameters["nusc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_hc"]*(out_mean[,(HCindex+1)]%*%ifr[,2])
  cinc_mort_ICU1 <- parameters["nu_icu"]*parameters["propo2"]*parameters["pdeath_icuo"]*dexo2_hist*(out_mean[,(ICUindex+1)]%*%ifr[,2])+
    parameters["nu_icu"]*(1-parameters["propo2"])*parameters["pdeath_icu"]*(out_mean[,(ICUindex+1)]%*%ifr[,2])
  cinc_mort_ICUC1 <- parameters["nu_icuc"]*parameters["propo2"]*parameters["pdeath_icuco"]*dexo2c_hist*(out_mean[,(ICUCindex+1)]%*%ifr[,2])+
    parameters["nu_icuc"]*(1-parameters["propo2"])*parameters["pdeath_icuc"]*(out_mean[,(ICUCindex+1)]%*%ifr[,2])
  cinc_mort_Vent1  <- parameters["nu_vent"]*parameters["pdeath_vent"]*dexv_hist*(out_mean[,(Ventindex+1)]%*%ifr[,2])
  cinc_mort_VentC1 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_mean[,(VentCindex+1)]%*%ifr[,2])
  cinc_mort_ICUCV1 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_mean[,(ICUCVindex+1)]%*%ifr[,2])
  cinc_mort_HCICU1 <- parameters["nu_icuc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_icu_hco"]*(out_mean[,(HCICUindex+1)]%*%ifr[,2])+
    parameters["nu_icuc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_icu_hc"]*(out_mean[,(HCICUindex+1)]%*%ifr[,2])
  cinc_mort_HCV1 <- parameters["nu_ventc"]*parameters["report_death_HC"]*parameters["pdeath_vent_hc"]*(out_mean[,(HCVindex+1)]%*%ifr[,2])
  
  totage1<-as.data.frame(cinc_mort_H1+cinc_mort_HC1+cinc_mort_ICU1+cinc_mort_ICUC1+
                           cinc_mort_Vent1+cinc_mort_VentC1+cinc_mort_ICUCV1+cinc_mort_HCICU1+cinc_mort_HCV1)
  
  
  basemort_H1<-(out_mean[,(Hindex+1)])
  basemort_HC1<-(out_mean[,(HCindex+1)])
  basemort_ICU1<-(out_mean[,(ICUindex+1)])
  basemort_ICUC1<-(out_mean[,(ICUCindex+1)])
  basemort_ICUCV1<-(out_mean[,(ICUCVindex+1)])
  basemort_Vent1<-(out_mean[,(Ventindex+1)])
  basemort_VentC1<-(out_mean[,(VentCindex+1)])
  basemort_HCICU1<-(out_mean[,(HCICUindex+1)])
  basemort_HCV1<-(out_mean[,(HCVindex+1)])
  totbase1<-as.data.frame(basemort_H1+basemort_HC1+basemort_ICU1+basemort_ICUC1+basemort_ICUCV1+
                            basemort_Vent1+basemort_VentC1+basemort_HCICU1+basemort_HCV1)
  
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
             Age >=  1 & Age <= 6   ~ "<= 30 y.o.",
             Age >  6 & Age <= 8    ~ "30-40 y.o.",
             Age >  8 & Age <= 10    ~ "40-50 y.o.",
             Age >  10 & Age <= 12    ~ "50-60 y.o.",
             Age >  12 & Age <= 14    ~ "60-70 y.o.",
             Age >=  15  ~ ">= 70 y.o.")) %>%
    mutate(age_cat = factor(age_cat, levels = rev(c("<= 30 y.o.", "30-40 y.o.",
                                                    "40-50 y.o.", "50-60 y.o.", "60-70 y.o.", ">= 70 y.o."))))
  
  
  mortality_lag <- data.frame(Age = popstruc$agefloor)
  if(nrow(out_mean) >= 30)  mortality_lag <- bind_cols(mortality_lag,
                                                       data.frame(day30 = out_mean[30,CMindex+1]/out_mean[30,Cindex+1]) %>%
                                                         mutate(day30 = ifelse(is.infinite(day30), 0, day30)))
  if(nrow(out_mean) >= 60)  mortality_lag <- bind_cols(mortality_lag,
                                                       data.frame(day60 = out_mean[60,CMindex+1]/out_mean[60,Cindex+1]) %>%
                                                         mutate(day60 = ifelse(is.infinite(day60), 0, day60)))
  if(nrow(out_mean) >= 90)  mortality_lag <- bind_cols(mortality_lag,
                                                       data.frame(day90 = out_mean[90,CMindex+1]/out_mean[90,Cindex+1]) %>%
                                                         mutate(day90 = ifelse(is.infinite(day90), 0, day90))) 
  if(nrow(out_mean) >= 120)  mortality_lag <- bind_cols(mortality_lag,
                                                        data.frame(day120 = out_mean[120,CMindex+1]/out_mean[120,Cindex+1]) %>%
                                                          mutate(day120 = ifelse(is.infinite(day120), 0, day120)))
  
  results$mortality_lag <- mortality_lag
  
  
  if(iterations>1){
    
    cinc_mort_1 <- cumsum(rowSums(parameters["nus"]*parameters["propo2"]*parameters["pdeath_ho"]*dexo2_hist*(out_min[,(Hindex+1)]%*%ifr[,2])))
    cinc_mort_2 <- cumsum(rowSums(parameters["nus"]*(1-parameters["propo2"])*parameters["pdeath_h"]*(out_min[,(Hindex+1)]%*%ifr[,2])))
    cinc_mort_3 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_hco"]*(out_min[,(HCindex+1)]%*%ifr[,2])))
    cinc_mort_4 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_hc"]*(out_min[,(HCindex+1)]%*%ifr[,2])))
    cinc_mort_5 <- cumsum(rowSums(parameters["nu_icu"]*parameters["propo2"]*parameters["pdeath_icuo"]*dexo2_hist*(out_min[,(ICUindex+1)]%*%ifr[,2])))
    cinc_mort_6 <- cumsum(rowSums(parameters["nu_icu"]*(1-parameters["propo2"])*parameters["pdeath_icu"]*(out_min[,(ICUindex+1)]%*%ifr[,2])))
    cinc_mort_7 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["propo2"]*parameters["pdeath_icuco"]*dexo2c_hist*(out_min[,(ICUCindex+1)]%*%ifr[,2])))
    cinc_mort_8 <- cumsum(rowSums(parameters["nu_icuc"]*(1-parameters["propo2"])*parameters["pdeath_icuc"]*(out_min[,(ICUCindex+1)]%*%ifr[,2])))
    cinc_mort_9 <- cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*dexv_hist*(out_min[,(Ventindex+1)]%*%ifr[,2])))
    cinc_mort_10 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_min[,(VentCindex+1)]%*%ifr[,2])))
    cinc_mort_11 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_min[,(ICUCVindex+1)]%*%ifr[,2])))
    cinc_mort_121 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["propo2"]*parameters["pdeath_icu_hco"]*(out_min[,(HCICUindex+1)]%*%ifr[,2])))
    cinc_mort_131 <- cumsum(rowSums(parameters["nu_icuc"]*(1-parameters["propo2"])*parameters["pdeath_icu_hc"]*(out_min[,(HCICUindex+1)]%*%ifr[,2])))
    cinc_mort_141 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_vent_hc"]*(out_min[,(HCVindex+1)]%*%ifr[,2])))
    
    cinc_mort_all_min<-cinc_mort_1+cinc_mort_2+cinc_mort_3+cinc_mort_4+cinc_mort_5+cinc_mort_6+
      cinc_mort_7+cinc_mort_8+cinc_mort_9+cinc_mort_10+cinc_mort_11+cinc_mort_121+cinc_mort_131+cinc_mort_141
    
    cinc_mort_1 <- cumsum(rowSums(parameters["nus"]*parameters["propo2"]*parameters["pdeath_ho"]*dexo2_hist*(out_max[,(Hindex+1)]%*%ifr[,2])))
    cinc_mort_2 <- cumsum(rowSums(parameters["nus"]*(1-parameters["propo2"])*parameters["pdeath_h"]*(out_max[,(Hindex+1)]%*%ifr[,2])))
    cinc_mort_3 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_hco"]*(out_max[,(HCindex+1)]%*%ifr[,2])))
    cinc_mort_4 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_hc"]*(out_max[,(HCindex+1)]%*%ifr[,2])))
    cinc_mort_5 <- cumsum(rowSums(parameters["nu_icu"]*parameters["propo2"]*parameters["pdeath_icuo"]*dexo2_hist*(out_max[,(ICUindex+1)]%*%ifr[,2])))
    cinc_mort_6 <- cumsum(rowSums(parameters["nu_icu"]*(1-parameters["propo2"])*parameters["pdeath_icu"]*(out_max[,(ICUindex+1)]%*%ifr[,2])))
    cinc_mort_7 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["propo2"]*parameters["pdeath_icuco"]*dexo2c_hist*(out_max[,(ICUCindex+1)]%*%ifr[,2])))
    cinc_mort_8 <- cumsum(rowSums(parameters["nu_icuc"]*(1-parameters["propo2"])*parameters["pdeath_icuc"]*(out_max[,(ICUCindex+1)]%*%ifr[,2])))
    cinc_mort_9 <- cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*dexv_hist*(out_max[,(Ventindex+1)]%*%ifr[,2])))
    cinc_mort_10 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_max[,(VentCindex+1)]%*%ifr[,2])))
    cinc_mort_11 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_max[,(ICUCVindex+1)]%*%ifr[,2])))
    cinc_mort_121 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["propo2"]*parameters["pdeath_icu_hco"]*(out_max[,(HCICUindex+1)]%*%ifr[,2])))
    cinc_mort_131 <- cumsum(rowSums(parameters["nu_icuc"]*(1-parameters["propo2"])*parameters["pdeath_icu_hc"]*(out_max[,(HCICUindex+1)]%*%ifr[,2])))
    cinc_mort_141 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_vent_hc"]*(out_max[,(HCVindex+1)]%*%ifr[,2])))
    
    cinc_mort_all_max<-cinc_mort_1+cinc_mort_2+cinc_mort_3+cinc_mort_4+cinc_mort_5+cinc_mort_6+
      cinc_mort_7+cinc_mort_8+cinc_mort_9+cinc_mort_10+cinc_mort_11+cinc_mort_121+cinc_mort_131+cinc_mort_141
    
    base_mort_H1 <- cumsum(rowSums(out_min[,(Hindex+1)]%*%mort))
    base_mort_ICU1 <- cumsum(rowSums(out_min[,(ICUindex+1)]%*%mort))
    base_mort_ICUC1 <- cumsum(rowSums(out_min[,(ICUCindex+1)]%*%mort))
    base_mort_ICUCV1 <- cumsum(rowSums(out_min[,(ICUCVindex+1)]%*%mort))
    base_mort_Vent1 <- cumsum(rowSums(out_min[,(Ventindex+1)]%*%mort))
    base_mort_VentC1 <- cumsum(rowSums(out_min[,(VentCindex+1)]%*%mort))
    base_mort_Z1 <- cumsum(rowSums(out_min[,(Zindex+1)]%*%mort))
    base_mort_HC11 <- cumsum(rowSums(out_min[,(HCindex+1)]%*%mort))
    base_mort_HCICU11 <- cumsum(rowSums(out_min[,(HCICUindex+1)]%*%mort))
    base_mort_HCV11 <- cumsum(rowSums(out_min[,(HCVindex+1)]%*%mort))
    base_mort_E11 <- cumsum(rowSums(out_min[,(Eindex+1)]%*%mort))
    base_mort_I11 <- cumsum(rowSums(out_min[,(Iindex+1)]%*%mort))
    base_mort_CL11 <- cumsum(rowSums(out_min[,(CLindex+1)]%*%mort))
    base_mort_X11 <- cumsum(rowSums(out_min[,(Xindex+1)]%*%mort))
    base_mort_QE11 <- cumsum(rowSums(out_min[,(QEindex+1)]%*%mort))
    base_mort_QI11 <- cumsum(rowSums(out_min[,(QIindex+1)]%*%mort))
    base_mort_QC11 <- cumsum(rowSums(out_min[,(QCindex+1)]%*%mort))
    base_mort_ER11 <- cumsum(rowSums(out_min[,(ERindex+1)]%*%mort))
    base_mort_EV11 <- cumsum(rowSums(out_min[,(EVindex+1)]%*%mort))
    base_mort_EVR11 <- cumsum(rowSums(out_min[,(EVRindex+1)]%*%mort))
    base_mort_QEV11 <- cumsum(rowSums(out_min[,(QEVindex+1)]%*%mort))
    base_mort_QER11 <- cumsum(rowSums(out_min[,(QERindex+1)]%*%mort))
    base_mort_QEVR11 <- cumsum(rowSums(out_min[,(QEVRindex+1)]%*%mort))
    
    nat_deaths_inf_min <- last(round(base_mort_E11 + base_mort_I11 + base_mort_CL11 + base_mort_X11 + 
                                       base_mort_ER11 + base_mort_EV11+  base_mort_EVR11+   
                                       base_mort_QE11 + base_mort_QI11 + base_mort_QC11 +  
                                       base_mort_QEV11 + base_mort_QER11 + base_mort_QEVR11 + base_mort_Z1+
                                       base_mort_H1+base_mort_HC11+base_mort_ICU1+base_mort_ICUC1+base_mort_ICUCV1+
                                       base_mort_Vent1+base_mort_VentC1+base_mort_HCICU11+base_mort_HCV11))
    
    base_mort_H1 <- cumsum(rowSums(out_max[,(Hindex+1)]%*%mort))
    base_mort_ICU1 <- cumsum(rowSums(out_max[,(ICUindex+1)]%*%mort))
    base_mort_ICUC1 <- cumsum(rowSums(out_max[,(ICUCindex+1)]%*%mort))
    base_mort_ICUCV1 <- cumsum(rowSums(out_max[,(ICUCVindex+1)]%*%mort))
    base_mort_Vent1 <- cumsum(rowSums(out_max[,(Ventindex+1)]%*%mort))
    base_mort_VentC1 <- cumsum(rowSums(out_max[,(VentCindex+1)]%*%mort))
    base_mort_Z1 <- cumsum(rowSums(out_max[,(Zindex+1)]%*%mort))
    base_mort_HC11 <- cumsum(rowSums(out_max[,(HCindex+1)]%*%mort))
    base_mort_HCICU11 <- cumsum(rowSums(out_max[,(HCICUindex+1)]%*%mort))
    base_mort_HCV11 <- cumsum(rowSums(out_max[,(HCVindex+1)]%*%mort))
    base_mort_E11 <- cumsum(rowSums(out_max[,(Eindex+1)]%*%mort))
    base_mort_I11 <- cumsum(rowSums(out_max[,(Iindex+1)]%*%mort))
    base_mort_CL11 <- cumsum(rowSums(out_max[,(CLindex+1)]%*%mort))
    base_mort_X11 <- cumsum(rowSums(out_max[,(Xindex+1)]%*%mort))
    base_mort_QE11 <- cumsum(rowSums(out_max[,(QEindex+1)]%*%mort))
    base_mort_QI11 <- cumsum(rowSums(out_max[,(QIindex+1)]%*%mort))
    base_mort_QC11 <- cumsum(rowSums(out_max[,(QCindex+1)]%*%mort))
    base_mort_ER11 <- cumsum(rowSums(out_max[,(ERindex+1)]%*%mort))
    base_mort_EV11 <- cumsum(rowSums(out_max[,(EVindex+1)]%*%mort))
    base_mort_EVR11 <- cumsum(rowSums(out_max[,(EVRindex+1)]%*%mort))
    base_mort_QEV11 <- cumsum(rowSums(out_max[,(QEVindex+1)]%*%mort))
    base_mort_QER11 <- cumsum(rowSums(out_max[,(QERindex+1)]%*%mort))
    base_mort_QEVR11 <- cumsum(rowSums(out_max[,(QEVRindex+1)]%*%mort))
    
    nat_deaths_inf_max<- last(round(base_mort_E11 + base_mort_I11 + base_mort_CL11 + base_mort_X11 + 
                                      base_mort_ER11 + base_mort_EV11+  base_mort_EVR11+   
                                      base_mort_QE11 + base_mort_QI11 + base_mort_QC11 +  
                                      base_mort_QEV11 + base_mort_QER11 + base_mort_QEVR11 + base_mort_Z1+
                                      base_mort_H1+base_mort_HC11+base_mort_ICU1+base_mort_ICUC1+base_mort_ICUCV1+
                                      base_mort_Vent1+base_mort_VentC1+base_mort_HCICU11+base_mort_HCV11))
    
    
    previcureq1_max<-rowSums(out_max[,(Hindex+1)])+ rowSums(out_max[,(ICUCindex+1)])+rowSums(out_max[,(ICUCVindex+1)]) # surge beds occupancy
    previcureq21_max<-rowSums(out_max[,(ICUindex+1)])+rowSums(out_max[,(VentCindex+1)])   # icu beds occupancy
    previcureq31_max<-rowSums(out_max[,(Ventindex+1)])   # ventilator occupancy
    cmortality1_max<-rowSums(out_max[,(CMindex+1)])      # cumulative mortality
    overloadH1_max<-rowSums(out_max[,(HCindex+1)])       # requirement for beds
    overloadICU1_max<-rowSums(out_max[,(ICUCindex+1)])+ rowSums(out_max[,(HCICUindex+1)])  # requirement for icu beds
    overloadICUV1_max<-rowSums(out_max[,(ICUCVindex+1)])+ rowSums(out_max[,(HCVindex+1)])  # requirement for ventilators
    overloadVent1_max<-rowSums(out_max[,(VentCindex+1)]) # requirement for ventilators
    ccases1_max<-rowSums(out_max[,(Cindex+1)])           # cumulative cases
    reqsurge1_max<-rowSums(out_max[,(Hindex+1)])+overloadH1  # surge beds total requirements
    reqicu1_max<-rowSums(out_max[,(ICUindex+1)])+overloadICU1 # ICU beds total requirements
    reqvent1_max<-rowSums(out_max[,(Ventindex+1)])+overloadICUV1+overloadVent1 # ventilator beds total requirements
    
    previcureq1_min<-rowSums(out_min[,(Hindex+1)])+rowSums(out_min[,(ICUCindex+1)])+rowSums(out_min[,(ICUCVindex+1)]) # surge beds occupancy
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
  }
  return(results)
}
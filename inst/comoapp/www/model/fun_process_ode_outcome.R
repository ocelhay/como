process_ode_outcome <- function(out, parameters, startdate, times, ihr, ifr, mort, popstruc, vectors){
  # Define object to return ----
  results <- list(
    time = startdate + times
  )
  
  # Results derived from out$min, out$max or out$mean ----
  compute_results <- function(out_mat) {
    # Occupancies
    surge_bed_occupancy <- rowSums(out_mat[, c(Hindex, ICUCindex, ICUCVindex) + 1])
    icu_bed_occupancy <- rowSums(out_mat[, c(ICUindex, VentCindex) + 1])
    ventilator_occupancy <- rowSums(out_mat[, Ventindex + 1])
    
    # Requirements ----
    overloadH1<-rowSums(out_mat[,(HCindex+1)])       # requirement for beds
    overloadICU1<-rowSums(out_mat[,(ICUCindex+1)])   # requirement for icu beds
    overloadICUV1<-rowSums(out_mat[,(ICUCVindex+1)]) # requirement for ventilators
    overloadVent1<-rowSums(out_mat[,(VentCindex+1)]) # requirement for ventilators
    ccases1<-rowSums(out_mat[,(Cindex+1)])           # cumulative cases
    reqsurge1<-rowSums(out_mat[,(Hindex+1)])+overloadH1
    reqicu1<-rowSums(out_mat[,(ICUindex+1)])+overloadICU1
    reqvent1<-rowSums(out_mat[,(Ventindex+1)])+overloadICUV1+overloadVent1
    
    # Age-dependent mortality ----
    totage_H1 <- parameters["nus"]*parameters["pdeath_h"]*(out_mat[,(Hindex+1)])
    totage_HC1 <- parameters["nusc"]*parameters["pdeath_hc"]*(out_mat[,(HCindex+1)])
    totage_ICU1 <- parameters["nu_icu"]*parameters["pdeath_icu"]*out_mat[,(ICUindex+1)]
    totage_ICUC1 <- parameters["nu_icuc"]*parameters["pdeath_icuc"]*out_mat[,(ICUCindex+1)] 
    totage_ICUCV1 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*out_mat[,(ICUCVindex+1)]
    totage_Vent1 <- parameters["nu_vent"]*parameters["pdeath_vent"]*out_mat[,(Ventindex+1)] 
    totage_VentC1 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*out_mat[,(VentCindex+1)] 
    totage <- as.data.frame(totage_H1+totage_HC1+totage_ICU1+totage_ICUC1+totage_ICUCV1+totage_Vent1+totage_VentC1)
    
    totage_base_H1 <- out_mat[, (Hindex+1)]
    totage_base_HC1 <- out_mat[, (HCindex+1)]
    totage_base_ICU1 <- out_mat[, (ICUindex+1)]
    totage_base_ICUC1 <- out_mat[, (ICUCindex+1)]
    totage_base_ICUCV1 <- out_mat[, (ICUCVindex+1)]
    totage_base_Vent1 <- out_mat[, (Ventindex+1)]
    totage_base_VentC1 <- out_mat[, (VentCindex+1)]
    totbase <- as.data.frame(totage_base_H1+totage_base_HC1+totage_base_ICU1+totage_base_ICUC1+totage_base_ICUCV1+totage_base_Vent1+totage_base_VentC1)
    
    tc <- NULL
    for (i in 1:dim(totage_H1)[1]) {
      for (j in 1:dim(totage_H1)[2]) {
        tc <- rbind(tc,c(i, j, totage[i,j]*ifr[j,2]+totbase[i,j]*mort[j])) 
      }
    }
    tc<-as.data.frame(tc)
    colnames(tc)<-c("Day","Age","value")
    
    tc <- tc %>%
      mutate(Date = startdate + Day,
             age_cat = case_when(
               Age >= 1 & Age <= 6   ~ "less than 30 y.o.",
               Age > 6 & Age <= 8    ~ "30-40 y.o.",
               Age > 8 & Age <= 10    ~ "40-50 y.o.",
               Age > 10 & Age <= 12    ~ "50-60 y.o.",
               Age > 12 & Age <= 14    ~ "60-70 y.o.",
               Age >= 15  ~ "above 70 y.o.")) %>%
      mutate(age_cat = factor(age_cat, levels = rev(c("less than 30 y.o.", "30-40 y.o.", "40-50 y.o.", "50-60 y.o.", "60-70 y.o.", "above 70 y.o."))))
    
    # Mortality lag ----
    mortality_lag <- data.frame(Age = popstruc$agefloor)
    if(nrow(out_mat) >= 30)  mortality_lag <- bind_cols(mortality_lag, 
                                                        data.frame(day30 = out_mat[30,CMindex+1]/out_mat[30,Cindex+1]) %>%
                                                          mutate(day30 = ifelse(is.infinite(day30), 0, day30)) %>%
                                                          rename(`Day 30` = day30))
    if(nrow(out_mat) >= 60)  mortality_lag <- bind_cols(mortality_lag, 
                                                        data.frame(day60 = out_mat[60,CMindex+1]/out_mat[60,Cindex+1]) %>%
                                                          mutate(day60 = ifelse(is.infinite(day60), 0, day60)) %>%
                                                          rename(`Day 60` = day60))
    if(nrow(out_mat) >= 90)  mortality_lag <- bind_cols(mortality_lag, 
                                                        data.frame(day90 = out_mat[90,CMindex+1]/out_mat[90,Cindex+1]) %>%
                                                          mutate(day90 = ifelse(is.infinite(day90), 0, day90)) %>%
                                                          rename(`Day 90` = day90))
    if(nrow(out_mat) >= 120)  mortality_lag <- bind_cols(mortality_lag, 
                                                         data.frame(day120 = out_mat[120,CMindex+1]/out_mat[120,Cindex+1]) %>%
                                                           mutate(day120 = ifelse(is.infinite(day120), 0, day120)) %>%
                                                           rename(`Day 120` = day120))
    
    
    # Calculate mortality ----
    dexo2_hist <- rep(1,length(times))
    dexo2c_hist <- rep(1,length(times))
    dexv_hist <- rep(1,length(times))
    dexvc_hist <- rep(1,length(times))
    for (tt in times) {
      if(tt < max(times)){
        if(vectors$dex[tt*20+1]) {
          dexo2_hist[tt+1] <- parameters["dexo2"]
          dexo2c_hist[tt+1] <- parameters["dexo2c"]
          dexv_hist[tt+1] <- parameters["dexv"]
          dexvc_hist[tt+1] <- parameters["dexvc"]
        } else {
          dexo2_hist[tt+1] <- 1
          dexo2c_hist[tt+1] <- 1
          dexv_hist[tt+1] <- 1
          dexvc_hist[tt+1] <- 1
        }
      } else {
        dexo2_hist[tt+1] <- dexo2_hist[tt]
        dexo2c_hist[tt+1] <- dexo2c_hist[tt]
        dexv_hist[tt+1] <- dexv_hist[tt]
        dexvc_hist[tt+1] <- dexvc_hist[tt]
      }
    }
    cinc_mort_1 <- cumsum(rowSums(parameters["nus"]*parameters["propo2"]*parameters["pdeath_ho"]*dexo2_hist*(out_mat[,(Hindex+1)]%*%ifr[,2])))
    cinc_mort_2 <- cumsum(rowSums(parameters["nus"]*(1-parameters["propo2"])*parameters["pdeath_h"]*(out_mat[,(Hindex+1)]%*%ifr[,2])))
    cinc_mort_3 <- cumsum(rowSums(parameters["nusc"]*parameters["propo2"]*parameters["pdeath_hco"]*(out_mat[,(HCindex+1)]%*%ifr[,2])))
    cinc_mort_4 <- cumsum(rowSums(parameters["nusc"]*(1-parameters["propo2"])*parameters["pdeath_hc"]*(out_mat[,(HCindex+1)]%*%ifr[,2])))
    cinc_mort_5 <- cumsum(rowSums(parameters["nu_icu"]*parameters["propo2"]*parameters["pdeath_icuo"]*dexo2_hist*(out_mat[,(ICUindex+1)]%*%ifr[,2])))
    cinc_mort_6 <- cumsum(rowSums(parameters["nu_icu"]*(1-parameters["propo2"])*parameters["pdeath_icu"]*(out_mat[,(ICUindex+1)]%*%ifr[,2])))
    cinc_mort_7 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["propo2"]*parameters["pdeath_icuco"]*dexo2c_hist*(out_mat[,(ICUCindex+1)]%*%ifr[,2])))
    cinc_mort_8 <- cumsum(rowSums(parameters["nu_icuc"]*(1-parameters["propo2"])*parameters["pdeath_icuc"]*(out_mat[,(ICUCindex+1)]%*%ifr[,2])))
    cinc_mort_9 <- cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*dexv_hist*(out_mat[,(Ventindex+1)]%*%ifr[,2])))
    cinc_mort_10 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_mat[,(VentCindex+1)]%*%ifr[,2])))
    cinc_mort_11 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_mat[,(ICUCVindex+1)]%*%ifr[,2])))
    cinc_mort_H1 <- cinc_mort_1 + cinc_mort_2
    cinc_mort_HC1 <- cinc_mort_3 + cinc_mort_4
    cinc_mort_ICU1 <- cinc_mort_5 + cinc_mort_6
    cinc_mort_ICUC1 <- cinc_mort_7 + cinc_mort_8
    cinc_mort_Vent1 <- cinc_mort_9
    cinc_mort_VentC1 <- cinc_mort_10
    cinc_mort_ICUCV1 <- cinc_mort_11
    
    base_mort_H1 <- cumsum(rowSums(out_mat[,(Hindex+1)]%*%mort))
    base_mort_HC1 <- cumsum(rowSums(out_mat[,(HCindex+1)]%*%mort))
    base_mort_ICU1 <- cumsum(rowSums(out_mat[,(ICUindex+1)]%*%mort))
    base_mort_ICUC1 <- cumsum(rowSums(out_mat[,(ICUCindex+1)]%*%mort))
    base_mort_ICUCV1 <- cumsum(rowSums(out_mat[,(ICUCVindex+1)]%*%mort))
    base_mort_Vent1 <- cumsum(rowSums(out_mat[,(Ventindex+1)]%*%mort))
    base_mort_VentC1 <- cumsum(rowSums(out_mat[,(VentCindex+1)]%*%mort))
    base_mort_Z1 <- cumsum(rowSums(out_mat[,(Zindex+1)]%*%mort))
    base_mort_V1 <- cumsum(rowSums(out_mat[,(Vindex+1)]%*%mort))
    
    # death_sum_1 <- cinc_mort_H1 + cinc_mort_HC1 + cinc_mort_ICU1 + cinc_mort_ICUC1 + cinc_mort_Vent1 + cinc_mort_VentC1 + cinc_mort_ICUCV1
    # death_sum_2 <- base_mort_H1 + base_mort_HC1 + base_mort_ICU1 + base_mort_ICUC1 + base_mort_ICUCV1 + base_mort_Vent1 + base_mort_VentC1 + base_mort_Z1 + base_mort_V1
    ## death_sum_1 + death_sum_2 ~= rowSums(out_mat[, CMindex + 1])

    base_mort_S1 <- cumsum(rowSums(out_mat[,(Sindex+1)]%*%mort))
    base_mort_E1 <- cumsum(rowSums(out_mat[,(Eindex+1)]%*%mort))
    base_mort_I1 <- cumsum(rowSums(out_mat[,(Iindex+1)]%*%mort))
    base_mort_CL1 <- cumsum(rowSums(out_mat[,(CLindex+1)]%*%mort))
    base_mort_X1 <- cumsum(rowSums(out_mat[,(Xindex+1)]%*%mort))
    base_mort_QS1 <- cumsum(rowSums(out_mat[,(QSindex+1)]%*%mort))
    base_mort_QE1 <- cumsum(rowSums(out_mat[,(QEindex+1)]%*%mort))
    base_mort_QI1 <- cumsum(rowSums(out_mat[,(QIindex+1)]%*%mort))
    base_mort_QC1 <- cumsum(rowSums(out_mat[,(QCindex+1)]%*%mort))
    base_mort_QR1 <- cumsum(rowSums(out_mat[,(QRindex+1)]%*%mort))
    base_mort_R1 <- cumsum(rowSums(out_mat[,(Rindex+1)]%*%mort))
    

    # Fill in results
    
    results$hospital_surge_beds <- round(surge_bed_occupancy)
    results$icu_beds <- round(icu_bed_occupancy)
    results$ventilators <- round(ventilator_occupancy)
    results$normal_bed_requirement <- round(reqsurge1) #real required beds. surge_bed_occupancy above is the occupancy
    results$icu_bed_requirement <- round(reqicu1)
    results$icu_ventilator_requirement <- round(reqvent1)
    
    # results$death_natural_non_exposed <- round(base_mort_S1+base_mort_V1+base_mort_QS1)
    # results$death_natural_exposed <- round(base_mort_E1 + base_mort_I1 + base_mort_CL1 + base_mort_X1 +  
    #                                          base_mort_QE1 + base_mort_QI1 + base_mort_QC1 + base_mort_QR1 + base_mort_R1+
    #                                          base_mort_H1+base_mort_HC1+base_mort_ICU1+base_mort_ICUC1+base_mort_ICUCV1+
    #                                          base_mort_Vent1+base_mort_VentC1) 
    results$death_natural_non_exposed <- round( base_mort_S1
                                              + base_mort_V1
                                              + base_mort_QS1
                                              )
    results$death_natural_exposed <- round( base_mort_I1
                                          + base_mort_CL1
                                          + base_mort_E1
                                          + base_mort_QE1
                                          + base_mort_X1  
                                          + base_mort_QI1
                                          + base_mort_QC1
                                          + base_mort_H1
                                          + base_mort_HC1
                                          + base_mort_ICU1
                                          + base_mort_ICUC1
                                          + base_mort_ICUCV1
                                          + base_mort_Vent1
                                          + base_mort_VentC1
                                          + base_mort_Z1
                                          + base_mort_R1
                                          + base_mort_QR1
                                          ) 

    ## Attributable
    results$death_treated_hospital <- round(cinc_mort_H1)
    results$death_treated_icu <- round(cinc_mort_ICU1)
    results$death_treated_ventilator <- round(cinc_mort_Vent1)
    results$death_untreated_hospital <- round(cinc_mort_HC1)
    results$death_untreated_icu <- round(cinc_mort_ICUC1)
    results$death_untreated_ventilator <- round(cinc_mort_VentC1)+round(cinc_mort_ICUCV1)
    
    results$attributable_deaths <- results$death_treated_hospital + results$death_treated_icu + results$death_treated_ventilator +
      results$death_untreated_hospital + results$death_untreated_icu + results$death_untreated_ventilator
    
    results$attributable_deaths_end <- last(results$attributable_deaths)

    ## Reportable
    # results$reportable_death <- round(rowSums(out_mat[, CMindex + 1]))
    results$reportable_death <- results$attributable_deaths + results$death_natural_exposed
    
    ## Total
    results$total_deaths <- results$attributable_deaths + results$death_natural_non_exposed + results$death_natural_exposed
    results$total_deaths_end <- last(results$total_deaths)
    
    results$tc <- tc
    results$mortality_lag <- mortality_lag
    
    return(results)
  }
  
  results$med <- compute_results(out$mean)
  results$min <- compute_results(out$min)
  results$max <- compute_results(out$max)
  
  # Results already computed in multi_runs() ----
  # Rt
  results$med$Rt <- out$mean_Rt
  results$min$Rt <- out$min_Rt
  results$max$Rt <- out$max_Rt
  
  # proportion of the  population that has been infected at the end of the simulation
  results$med$pct_total_pop_infected <- out$mean_infections
  results$min$pct_total_pop_infected <- out$min_infections
  results$max$pct_total_pop_infected <- out$max_infections
  
  
  # Daily incidence
  ifelse(is.null(dim(out$mean_cases)), results$med$daily_incidence <- round(out$mean_cases), results$med$daily_incidence <- round(out$mean_cases[, 1]))
  ifelse(is.null(dim(out$min_cases)), results$min$daily_incidence <- round(out$min_cases), results$min$daily_incidence <- round(out$min_cases[, 1]))
  ifelse(is.null(dim(out$max_cases)), results$max$daily_incidence <- round(out$max_cases), results$max$daily_incidence <- round(out$max_cases[, 1]))
  
  # Daily total cases
  ifelse(is.null(dim(out$mean_daily_infection)), results$med$daily_total_cases <- round(out$mean_daily_infection), results$med$daily_total_cases <- round(out$mean_daily_infection[, 1]))
  ifelse(is.null(dim(out$min_daily_infection)), results$min$daily_total_cases <- round(out$min_daily_infection), results$min$daily_total_cases <- round(out$min_daily_infection[, 1]))
  ifelse(is.null(dim(out$max_daily_infection)), results$max$daily_total_cases <- round(out$max_daily_infection), results$max$daily_total_cases <- round(out$max_daily_infection[, 1]))
  
  # Doubling time (only for baseline)
  results$med$doubling_time <- round(log(2)*7 / (log(out$mean_cases[2+7] / out$mean_cases[2])), 2)
  results$min$doubling_time <- round(log(2)*7 / (log(out$min_cases[2+7] / out$min_cases[2])), 2)
  results$max$doubling_time <- round(log(2)*7 / (log(out$max_cases[2+7] / out$max_cases[2])), 2)
  
  # Variables that are only downloaded as median value
  results$hospital_surge_beds <- results$med$hospital_surge_beds
  results$icu_beds <- results$med$icu_beds
  results$ventilators <- results$med$ventilators
  results$normal_bed_requirement <- results$med$normal_bed_requirement
  results$icu_bed_requirement <- results$med$icu_bed_requirement
  results$icu_ventilator_requirement <- results$med$icu_ventilator_requirement
  
  results$death_natural_non_exposed <- results$med$death_natural_non_exposed
  results$death_natural_exposed <- results$med$death_natural_exposed
  results$death_treated_hospital <- results$med$death_treated_hospital
  results$death_treated_icu <- results$med$death_treated_icu
  results$death_treated_ventilator <- results$med$death_treated_ventilator
  results$death_untreated_hospital <- results$med$death_untreated_hospital
  results$death_untreated_icu <- results$med$death_untreated_icu
  results$death_untreated_ventilator <- results$med$death_untreated_ventilator
  results$reportable_death <- results$med$reportable_death
  
  return(results)
}
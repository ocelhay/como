# Results derived from out$min, out$max or out$mean ----
  process_ode_outcome_compute <- function(out_mat, param_used, startdate, times, ihr, ifr, mort, popstruc, intv_vector) {
  
    # IMPORTANT difference with script: replaced out_mean & out$mean with out_mat
    # IMPORTANT difference with script: replaced parameters with param_used
    # total population
    pop1<-out_mat[,(Sindex+1)]+out_mat[,(SRindex+1)]+out_mat[,(Eindex+1)]+out_mat[,(Iindex+1)]+out_mat[,(CLindex+1)]+out_mat[,(Rindex+1)]+
      out_mat[,(Xindex+1)]+out_mat[,(Vindex+1)]+out_mat[,(Zindex+1)]+out_mat[,(EVindex+1)]+out_mat[,(ERindex+1)]+out_mat[,(EVRindex+1)]+
      out_mat[,(QSindex+1)]+out_mat[,(QSRindex+1)]+out_mat[,(QEindex+1)]+out_mat[,(QIindex+1)]+out_mat[,(QCindex+1)]+out_mat[,(QRindex+1)]+
      out_mat[,(QVindex+1)]+out_mat[,(QEVindex+1)]+out_mat[,(QERindex+1)]+out_mat[,(QVRindex+1)]+out_mat[,(QEVRindex+1)]+
      out_mat[,(Hindex+1)]+out_mat[,(HCindex+1)]+out_mat[,(ICUindex+1)]+out_mat[,(ICUCindex+1)]+out_mat[,(ICUCVindex+1)]+
      out_mat[,(Ventindex+1)]+out_mat[,(VentCindex+1)]+out_mat[,(HCICUindex+1)]+out_mat[,(HCVindex+1)]
    tpop1<-rowSums(pop1)
    
    
    ##########################    AB prevalence
    ab_age<-out_mat[,(Abindex+1)]
    ab_all_ages<-rowSums(out_mat[,(Abindex+1)])
    # End snippet v16.5 lines 1629-1642 ----

    
    ##########################    CALCULATE MORTALITY 
    dexo2_hist <- rep(0,length(times))
    dexo2c_hist <- rep(0,length(times))
    dexv_hist <- rep(0,length(times))
    dexvc_hist <- rep(0,length(times))
    dm <- rep(1,length(times))
    for (tt in times) {
      if(tt < max(times)){
        if(intv_vector$dex[tt*20+1]) {
          dexo2_hist[tt+1] <- param_used["dexo2"]
          dexo2c_hist[tt+1] <- param_used["dexo2c"]
          dexv_hist[tt+1] <- param_used["dexv"]
          dexvc_hist[tt+1] <- param_used["dexvc"]
        } else {
          dexo2_hist[tt+1] <- 1
          dexo2c_hist[tt+1] <- 1
          dexv_hist[tt+1] <- 1
          dexvc_hist[tt+1] <- 1
        }
        if(intv_vector$dmod[tt*20+1]==1) {
          dm[tt+1] <- intv_vector$dmod_vector[tt*20+1]
        } 
      } else {
        dexo2_hist[tt+1] <- dexo2_hist[tt]
        dexo2c_hist[tt+1] <- dexo2c_hist[tt]
        dexv_hist[tt+1] <- dexv_hist[tt]
        dexvc_hist[tt+1] <- dexvc_hist[tt]
        dm[tt+1] <- intv_vector$dmod_vector[tt*20+1]
      }
    }
    
    cinc_mort_1 <- cumsum(rowSums(param_used["nus"]*param_used["propo2"]*param_used["pdeath_ho"]*dm*dexo2_hist*(out_mat[,(Hindex+1)]%*%ifr[,2])))
    cinc_mort_2 <- cumsum(rowSums(param_used["nus"]*(1-param_used["propo2"])*param_used["pdeath_h"]*dm*(out_mat[,(Hindex+1)]%*%ifr[,2])))
    
    cinc_mort_3 <- cumsum(rowSums(param_used["nusc"]*param_used["report_death_HC"]*param_used["propo2"]*param_used["pdeath_hco"]*dm*(out_mat[,(HCindex+1)]%*%ifr[,2])))
    cinc_mort_4 <- cumsum(rowSums(param_used["nusc"]*param_used["report_death_HC"]*(1-param_used["propo2"])*param_used["pdeath_hc"]*dm*(out_mat[,(HCindex+1)]%*%ifr[,2])))
    
    cinc_mort_5 <- cumsum(rowSums(param_used["nu_icu"]*param_used["propo2"]*param_used["pdeath_icuo"]*dm*dexo2_hist*(out_mat[,(ICUindex+1)]%*%ifr[,2])))
    cinc_mort_6 <- cumsum(rowSums(param_used["nu_icu"]*(1-param_used["propo2"])*param_used["pdeath_icu"]*dm*(out_mat[,(ICUindex+1)]%*%ifr[,2])))
    cinc_mort_7 <- cumsum(rowSums(param_used["nu_icuc"]*param_used["propo2"]*param_used["pdeath_icuco"]*dm*dexo2c_hist*(out_mat[,(ICUCindex+1)]%*%ifr[,2])))
    cinc_mort_8 <- cumsum(rowSums(param_used["nu_icuc"]*(1-param_used["propo2"])*param_used["pdeath_icuc"]*dm*(out_mat[,(ICUCindex+1)]%*%ifr[,2])))
    
    cinc_mort_9 <- cumsum(rowSums(param_used["nu_vent"]*param_used["pdeath_vent"]*dm*dexv_hist*(out_mat[,(Ventindex+1)]%*%ifr[,2])))
    cinc_mort_10 <- cumsum(rowSums(param_used["nu_ventc"]*param_used["pdeath_ventc"]*dm*dexvc_hist*(out_mat[,(VentCindex+1)]%*%ifr[,2])))
    cinc_mort_11 <- cumsum(rowSums(param_used["nu_ventc"]*param_used["pdeath_ventc"]*dm*dexvc_hist*(out_mat[,(ICUCVindex+1)]%*%ifr[,2])))
    
    cinc_mort_12 <- cumsum(rowSums(param_used["nusc"]*param_used["report_death_HC"]*param_used["propo2"]*param_used["pdeath_icu_hco"]*dm*(out_mat[,(HCICUindex+1)]%*%ifr[,2])))
    cinc_mort_13 <- cumsum(rowSums(param_used["nusc"]*param_used["report_death_HC"]*(1-param_used["propo2"])*param_used["pdeath_icu_hc"]*dm*(out_mat[,(HCICUindex+1)]%*%ifr[,2])))
    cinc_mort_14 <- cumsum(rowSums(param_used["nu_ventc"]*param_used["report_death_HC"]*param_used["pdeath_vent_hc"]*dm*(out_mat[,(HCVindex+1)]%*%ifr[,2])))
    
    cinc_mort_121 <- cumsum(rowSums(param_used["nusc"]*param_used["propo2"]*param_used["pdeath_icu_hco"]*dm*(out_mat[,(HCICUindex+1)]%*%ifr[,2])))
    cinc_mort_131 <- cumsum(rowSums(param_used["nusc"]*(1-param_used["propo2"])*param_used["pdeath_icu_hc"]*dm*(out_mat[,(HCICUindex+1)]%*%ifr[,2])))
    cinc_mort_141 <- cumsum(rowSums(param_used["nu_ventc"]*param_used["pdeath_vent_hc"]*dm*(out_mat[,(HCVindex+1)]%*%ifr[,2])))
    
    
    cinc_mort_H1 <- cinc_mort_1 + cinc_mort_2
    cinc_mort_HC1 <- cinc_mort_3 + cinc_mort_4 + cinc_mort_12 + cinc_mort_13 + cinc_mort_14
    cinc_mort_ICU1 <- cinc_mort_5 + cinc_mort_6
    cinc_mort_ICUC1 <- cinc_mort_7 + cinc_mort_8
    cinc_mort_Vent1 <- cinc_mort_9
    cinc_mort_VentC1 <- cinc_mort_10
    cinc_mort_ICUCV1 <- cinc_mort_11
    
    # all deaths due to covid19 disease - reported + unreported
    cinc_mort_all<-cinc_mort_1+cinc_mort_2+cinc_mort_3+cinc_mort_4+cinc_mort_5+cinc_mort_6+
      cinc_mort_7+cinc_mort_8+cinc_mort_9+cinc_mort_10+cinc_mort_11+cinc_mort_121+cinc_mort_131+cinc_mort_141
    
    base_mort_H1 <- cumsum(rowSums(out_mat[,(Hindex+1)]%*%mort))
    base_mort_HC1 <- cumsum(rowSums(param_used["report_death_HC"]*out_mat[,(HCindex+1)]%*%mort))
    base_mort_ICU1 <- cumsum(rowSums(out_mat[,(ICUindex+1)]%*%mort))
    base_mort_ICUC1 <- cumsum(rowSums(out_mat[,(ICUCindex+1)]%*%mort))
    base_mort_ICUCV1 <- cumsum(rowSums(out_mat[,(ICUCVindex+1)]%*%mort))
    base_mort_Vent1 <- cumsum(rowSums(out_mat[,(Ventindex+1)]%*%mort))
    base_mort_VentC1 <- cumsum(rowSums(out_mat[,(VentCindex+1)]%*%mort))
    base_mort_Z1 <- cumsum(rowSums(out_mat[,(Zindex+1)]%*%mort))
    base_mort_HCICU1 <- cumsum(rowSums(param_used["report_death_HC"]*out_mat[,(HCICUindex+1)]%*%mort))
    base_mort_HCV1 <- cumsum(rowSums(param_used["report_death_HC"]*out_mat[,(HCVindex+1)]%*%mort))
    
    base_mort_V1 <- cumsum(rowSums(out_mat[,(Vindex+1)]%*%mort))
    base_mort_S1 <- cumsum(rowSums(out_mat[,(Sindex+1)]%*%mort))
    base_mort_SR1 <- cumsum(rowSums(out_mat[,(SRindex+1)]%*%mort))
    base_mort_QS1 <- cumsum(rowSums(out_mat[,(QSindex+1)]%*%mort))
    base_mort_QSR1 <- cumsum(rowSums(out_mat[,(QSRindex+1)]%*%mort))
    base_mort_QR1 <- cumsum(rowSums(out_mat[,(QRindex+1)]%*%mort))
    base_mort_R1 <- cumsum(rowSums(out_mat[,(Rindex+1)]%*%mort))
    base_mort_QVR1 <- cumsum(rowSums(out_mat[,(QVRindex+1)]%*%mort))
    base_mort_VR1 <- cumsum(rowSums(out_mat[,(VRindex+1)]%*%mort))
    base_mort_QV1 <- cumsum(rowSums(out_mat[,(QVindex+1)]%*%mort))
    
    base_mort_E1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(Eindex+1)]%*%mort))
    base_mort_I1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(Iindex+1)]%*%mort))
    base_mort_CL1 <- cumsum(rowSums(param_used["report_natdeathCL"]*out_mat[,(CLindex+1)]%*%mort))
    base_mort_X1 <- cumsum(rowSums(param_used["report_natdeathCL"]*out_mat[,(Xindex+1)]%*%mort))
    base_mort_QE1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(QEindex+1)]%*%mort))
    base_mort_QI1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(QIindex+1)]%*%mort))
    base_mort_QC1 <- cumsum(rowSums(param_used["report_natdeathCL"]*out_mat[,(QCindex+1)]%*%mort))
    base_mort_ER1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(ERindex+1)]%*%mort))
    base_mort_EV1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(EVindex+1)]%*%mort))
    base_mort_EVR1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(EVRindex+1)]%*%mort))
    base_mort_QEV1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(QEVindex+1)]%*%mort))
    base_mort_QER1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(QERindex+1)]%*%mort))
    base_mort_QEVR1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(QEVRindex+1)]%*%mort))
    
    
    base_mort_HC11 <- cumsum(rowSums(out_mat[,(HCindex+1)]%*%mort))
    base_mort_HCICU11 <- cumsum(rowSums(out_mat[,(HCICUindex+1)]%*%mort))
    base_mort_HCV11 <- cumsum(rowSums(out_mat[,(HCVindex+1)]%*%mort))
    base_mort_E11 <- cumsum(rowSums(out_mat[,(Eindex+1)]%*%mort))
    base_mort_I11 <- cumsum(rowSums(out_mat[,(Iindex+1)]%*%mort))
    base_mort_CL11 <- cumsum(rowSums(out_mat[,(CLindex+1)]%*%mort))
    base_mort_X11 <- cumsum(rowSums(out_mat[,(Xindex+1)]%*%mort))
    base_mort_QE11 <- cumsum(rowSums(out_mat[,(QEindex+1)]%*%mort))
    base_mort_QI11 <- cumsum(rowSums(out_mat[,(QIindex+1)]%*%mort))
    base_mort_QC11 <- cumsum(rowSums(out_mat[,(QCindex+1)]%*%mort))
    base_mort_ER11 <- cumsum(rowSums(out_mat[,(ERindex+1)]%*%mort))
    base_mort_EV11 <- cumsum(rowSums(out_mat[,(EVindex+1)]%*%mort))
    base_mort_EVR11 <- cumsum(rowSums(out_mat[,(EVRindex+1)]%*%mort))
    base_mort_QEV11 <- cumsum(rowSums(out_mat[,(QEVindex+1)]%*%mort))
    base_mort_QER11 <- cumsum(rowSums(out_mat[,(QERindex+1)]%*%mort))
    base_mort_QEVR11 <- cumsum(rowSums(out_mat[,(QEVRindex+1)]%*%mort))
    
    # all deaths of infected with sars-cov-2 virus - reported + unreported
    nat_deaths_inf <- round(base_mort_E11 + base_mort_I11 + base_mort_CL11 + base_mort_X11 + 
                              base_mort_ER11 + base_mort_EV11+  base_mort_EVR11+   
                              base_mort_QE11 + base_mort_QI11 + base_mort_QC11 +  
                              base_mort_QEV11 + base_mort_QER11 + base_mort_QEVR11 + base_mort_Z1+
                              base_mort_H1+base_mort_HC11 + base_mort_ICU1 + base_mort_ICUC1 + base_mort_ICUCV1+
                              base_mort_Vent1 + base_mort_VentC1 + base_mort_HCICU11 + base_mort_HCV11)
    
    results <- list()
    results$time <- startdate + times  # dates
    results$N <- tpop1
    
    ## Ab
    results$ab_all_ages<-ab_all_ages
    results$ab<-ab_age
    
    # Rt/ FOI
    # 6-lines section not included
    

    # Hospital requirements
    previcureq1<-rowSums(out_mat[,(Hindex+1)])+ rowSums(out_mat[,(ICUCindex+1)])+rowSums(out_mat[,(ICUCVindex+1)]) # surge beds occupancy
    previcureq21<-rowSums(out_mat[,(ICUindex+1)])+rowSums(out_mat[,(VentCindex+1)])   # icu beds occupancy
    previcureq31<-rowSums(out_mat[,(Ventindex+1)])   # ventilator occupancy
    overloadH1<-rowSums(out_mat[,(HCindex+1)])       # requirement for beds
    overloadICU1<-rowSums(out_mat[,(ICUCindex+1)])+rowSums(out_mat[,(HCICUindex+1)])   # requirement for icu beds
    overloadICUV1<-rowSums(out_mat[,(ICUCVindex+1)]) # requirement for ventilators
    overloadVent1<-rowSums(out_mat[,(VentCindex+1)])+rowSums(out_mat[,(HCVindex+1)]) # requirement for ventilators
    
    results$required_beds <- round(previcureq1)  # required beds
    results$saturation <- param_used["beds_available"]  # saturation
    results$hospital_surge_beds <- round(previcureq1)
    results$icu_beds <- round(previcureq21)
    results$ventilators <- round(previcureq31)
    results$normal_bed_requirement <- round(rowSums(out_mat[,(Hindex+1)])+overloadH1)   #real required beds. previcureq1 above is the occupancy
    results$icu_bed_requirement <- round(rowSums(out_mat[,(ICUindex+1)])+overloadICU1)
    results$icu_ventilator_requirement <- round(rowSums(out_mat[,(Ventindex+1)])+overloadICUV1+overloadVent1)
    
    ### MORTALITY
    results$cum_mortality <- round(rowSums(out_mat[,(CMindex+1)]))       # cumulative mortality
    
    # DIFFERENT FROM SCRIPT:
    
    results$deaths_from_covid <- round(cinc_mort_all)
    results$deaths_with_covid <- round(nat_deaths_inf)
    
    # Start copy/paste script
    # IMPORTANT difference with script: replaced out_mean & out$mean with out_mat
    # IMPORTANT difference with script: replaced parameters with param_used
    results$death_natural_non_exposed <- round(base_mort_S1+base_mort_V1+base_mort_QS1+base_mort_QSR1+base_mort_SR1)
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
    
    
    ## AGE DEPENDENT MORTALITY
    cinc_mort_H1 <- param_used["nus"]*param_used["propo2"]*param_used["pdeath_ho"]*dm*dexo2_hist*(out_mat[,(Hindex+1)])+
      param_used["nus"]*(1-param_used["propo2"])*param_used["pdeath_h"]*dm*(out_mat[,(Hindex+1)])
    cinc_mort_HC1 <- param_used["nusc"]*param_used["report_death_HC"]*param_used["propo2"]*param_used["pdeath_hco"]*dm*(out_mat[,(HCindex+1)])+
      param_used["nusc"]*param_used["report_death_HC"]*(1-param_used["propo2"])*param_used["pdeath_hc"]*dm*(out_mat[,(HCindex+1)])
    cinc_mort_ICU1 <- param_used["nu_icu"]*param_used["propo2"]*param_used["pdeath_icuo"]*dm*dexo2_hist*(out_mat[,(ICUindex+1)])+
      param_used["nu_icu"]*(1-param_used["propo2"])*param_used["pdeath_icu"]*dm*(out_mat[,(ICUindex+1)])
    cinc_mort_ICUC1 <- param_used["nu_icuc"]*param_used["propo2"]*param_used["pdeath_icuco"]*dm*dexo2c_hist*(out_mat[,(ICUCindex+1)] )+
      param_used["nu_icuc"]*(1-param_used["propo2"])*param_used["pdeath_icuc"]*dm*(out_mat[,(ICUCindex+1)] )
    cinc_mort_Vent1  <- param_used["nu_vent"]*param_used["pdeath_vent"]*dm*dexv_hist*(out_mat[,(Ventindex+1)] )
    cinc_mort_VentC1 <- param_used["nu_ventc"]*param_used["pdeath_ventc"]*dm*dexvc_hist*(out_mat[,(VentCindex+1)] )
    cinc_mort_ICUCV1 <- param_used["nu_ventc"]*param_used["pdeath_ventc"]*dm*dexvc_hist*(out_mat[,(ICUCVindex+1)] )
    cinc_mort_HCICU1 <- param_used["nusc"]*param_used["report_death_HC"]*dm*param_used["propo2"]*param_used["pdeath_icu_hco"]*dm*(out_mat[,(HCICUindex+1)] )+
      param_used["nusc"]*param_used["report_death_HC"]*(1-param_used["propo2"])*param_used["pdeath_icu_hc"]*dm*(out_mat[,(HCICUindex+1)] )
    cinc_mort_HCV1 <- param_used["nu_ventc"]*param_used["report_death_HC"]*param_used["pdeath_vent_hc"]*dm*(out_mat[,(HCVindex+1)] )
    
    totage1<-as.data.frame(cinc_mort_H1+cinc_mort_HC1+cinc_mort_ICU1+cinc_mort_ICUC1+
                             cinc_mort_Vent1+cinc_mort_VentC1+cinc_mort_ICUCV1+cinc_mort_HCICU1+cinc_mort_HCV1)
    
    basemort_H1<-(out_mat[,(Hindex+1)])
    basemort_HC1<-param_used["report_death_HC"]*(out_mat[,(HCindex+1)])
    basemort_ICU1<-(out_mat[,(ICUindex+1)])
    basemort_ICUC1<-(out_mat[,(ICUCindex+1)])
    basemort_ICUCV1<-(out_mat[,(ICUCVindex+1)])
    basemort_Vent1<-(out_mat[,(Ventindex+1)])
    basemort_VentC1<-(out_mat[,(VentCindex+1)])
    basemort_HCICU1<-param_used["report_death_HC"]*(out_mat[,(HCICUindex+1)])
    basemort_HCV1<-param_used["report_death_HC"]*(out_mat[,(HCVindex+1)])
    basemort_I<-param_used["report_natdeathI"]*(out_mat[,(Iindex+1)])
    basemort_QI<-param_used["report_natdeathI"]*(out_mat[,(QIindex+1)])
    basemort_E<-param_used["report_natdeathI"]*(out_mat[,(Eindex+1)])
    basemort_QE<-param_used["report_natdeathI"]*(out_mat[,(QEindex+1)])
    basemort_EV<-param_used["report_natdeathI"]*(out_mat[,(EVindex+1)])
    basemort_EVR<-param_used["report_natdeathI"]*(out_mat[,(EVRindex+1)])
    basemort_ER<-param_used["report_natdeathI"]*(out_mat[,(ERindex+1)])
    basemort_QEV<-param_used["report_natdeathI"]*(out_mat[,(QEVindex+1)])
    basemort_QEVR<-param_used["report_natdeathI"]*(out_mat[,(QEVRindex+1)])
    basemort_QER<-param_used["report_natdeathI"]*(out_mat[,(QERindex+1)])
    basemort_CL<-param_used["report_natdeathCL"]*(out_mat[,(CLindex+1)])
    basemort_QC<-param_used["report_natdeathCL"]*(out_mat[,(QCindex+1)])
    basemort_X<-param_used["report_natdeathCL"]*(out_mat[,(Xindex+1)])
    
    totbase1<-as.data.frame(basemort_H1+basemort_HC1+basemort_ICU1+basemort_ICUC1+basemort_ICUCV1+
                              basemort_Vent1+basemort_VentC1+basemort_HCICU1+basemort_HCV1+ 
                              basemort_I+basemort_QI+basemort_E+basemort_QE+basemort_EV+basemort_EVR+
                              basemort_ER+basemort_QEV+basemort_QEVR+basemort_QER+basemort_CL+basemort_QC+basemort_X)
    
    tc<-c()
    for (i in 1:dim(cinc_mort_H1)[1]) {
      for (j in 1:dim(cinc_mort_H1)[2]) {
        # print(totage1[i,j]*ifr[j,2]+totbase1[i,j]*mort[j])
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
    if(nrow(out_mat) >= 30)  mortality_lag <- bind_cols(mortality_lag,
                                                         data.frame(day30 = out_mat[30,CMindex+1]/out_mat[30,Cindex+1]) %>%
                                                           mutate(day30 = ifelse(is.infinite(day30), 0, day30)))
    if(nrow(out_mat) >= 60)  mortality_lag <- bind_cols(mortality_lag,
                                                         data.frame(day60 = out_mat[60,CMindex+1]/out_mat[60,Cindex+1]) %>%
                                                           mutate(day60 = ifelse(is.infinite(day60), 0, day60)))
    if(nrow(out_mat) >= 90)  mortality_lag <- bind_cols(mortality_lag,
                                                         data.frame(day90 = out_mat[90,CMindex+1]/out_mat[90,Cindex+1]) %>%
                                                           mutate(day90 = ifelse(is.infinite(day90), 0, day90))) 
    if(nrow(out_mat) >= 120)  mortality_lag <- bind_cols(mortality_lag,
                                                          data.frame(day120 = out_mat[120,CMindex+1]/out_mat[120,Cindex+1]) %>%
                                                            mutate(day120 = ifelse(is.infinite(day120), 0, day120)))
    
    results$mortality_lag <- mortality_lag
    # End copy/paste script
    
    results$total_covid_deaths <- results$deaths_from_covid + results$deaths_with_covid
    results$reportable_deaths <- results$attributable_deaths + results$death_natural_exposed
    results$total_reportable_deaths_end <- last(results$total_reportable_deaths)
    results$total_cm_deaths_end <- round(last(results$cum_mortality))

    return(results)
  }
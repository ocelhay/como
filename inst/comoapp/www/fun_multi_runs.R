multi_runs <- function(Y, times, parameters, input, A, ihr, ifr, mort, popstruc, popbirth, ageing,
                       contact_home, contact_school, contact_work, contact_other){
  
  # Define objects to store results ----
  results <- list()
  aux <- array(0, dim = c(length(times), 22 * A + 1, parameters["iterations"]))
  results$mean <- matrix(0, nrow = length(times), ncol = 22 * A + 1)
  results$min <- matrix(0, nrow = length(times), ncol = 22 * A + 1)
  results$max <- matrix(0, nrow = length(times), ncol = 22 * A + 1)
  
  results$mean_cases <- matrix(0, nrow = length(times), ncol = 22 * A + 1)
  results$min_cases <- matrix(0, nrow = length(times), ncol = 22 * A + 1)
  results$max_cases <- matrix(0, nrow = length(times), ncol = 22 * A + 1)
  
  results$mean_cum_cases <- matrix(0, nrow = length(times), ncol = 1)
  results$min_cum_cases <- matrix(0, nrow = length(times), ncol = 1)
  results$max_cum_cases <- matrix(0, nrow = length(times), ncol = 1)
  
  results$mean_daily_infection <-
    matrix(0, nrow = length(times), ncol = 1)
  results$min_daily_infection <-
    matrix(0, nrow = length(times), ncol = 1)
  results$max_daily_infection <-
    matrix(0, nrow = length(times), ncol = 1)
  
  cases<-matrix(0, nrow=length(times),ncol=parameters["iterations"])
  cum_cases<-matrix(0, nrow=length(times),ncol=parameters["iterations"])
  day_infections<-matrix(0, nrow=length(times),ncol=parameters["iterations"])
  Rt_aux<-matrix(0, nrow=length(times),ncol=parameters["iterations"])
  infections<-matrix(0, nrow=parameters["iterations"],ncol=1)
  Rt <- NULL
  
  parameters_dup <- parameters  # duplicate parameters to add noise 
  
  for (i in 1:parameters["iterations"]) {
    showNotification(paste("Run", i, "of", parameters["iterations"]), duration = 3, type = "message")
    
    # Add noise to parameters only if there are several iterations
    if (parameters["iterations"] > 1) {
      parameters_dup[parameters_noise] <- parameters[parameters_noise] + 
        rnorm(length(parameters_noise), mean = 0, sd = parameters["noise"] * abs(parameters[parameters_noise]))
    }

    covidOdeCpp_reset()
    mat_ode <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covidOdeCpp, 
                   parms = parameters_dup, input = input, A = A,
                   contact_home=contact_home, contact_school=contact_school,
                   contact_work=contact_work, contact_other=contact_other,
                   popbirth_col2=popbirth[,2], popstruc_col2=popstruc[,2],
                   ageing=ageing, ifr_col2=ifr[,2], ihr_col2=ihr[,2], mort_col=mort)
    
    aux[, , i] <- mat_ode
    
    
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
    
    critH<-c()
    crit<-c()
    critV<-c()
    for (ii in 1:length(times)){
      critH[ii]<-min(1-fH((sum(mat_ode[ii,(Hindex+1)]))+sum(mat_ode[ii,(ICUCindex+1)])+sum(mat_ode[ii,(ICUCVindex+1)])),1)
      crit[ii]<-min(1-fICU((sum(mat_ode[ii,(ICUindex+1)]))+(sum(mat_ode[ii,(Ventindex+1)]))+(sum(mat_ode[ii,(VentCindex+1)]))))
      critV[ii]<-min(1-fVent((sum(mat_ode[ii,(Ventindex+1)]))),1)
    }
    
    # daily incidence
    incidence<-parameters_dup["report"]*parameters_dup["gamma"]*(1-parameters_dup["pclin"])*mat_ode[,(Eindex+1)]%*%(1-ihr[,2])+
      parameters_dup["reportc"]*parameters_dup["gamma"]*parameters_dup["pclin"]*mat_ode[,(Eindex+1)]%*%(1-ihr[,2])+
      parameters_dup["report"]*parameters_dup["gamma"]*(1-parameters_dup["pclin"])*mat_ode[,(QEindex+1)]%*%(1-ihr[,2])+
      parameters_dup["reportc"]*parameters_dup["gamma"]*parameters_dup["pclin"]*mat_ode[,(QEindex+1)]%*%(1-ihr[,2])
    
    incidenceh<- parameters_dup["gamma"]*mat_ode[,(Eindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters_dup["prob_icu"])*parameters_dup["reporth"]+
      parameters_dup["gamma"]*mat_ode[,(Eindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters_dup["prob_icu"])*(1-parameters_dup["reporth"])+
      parameters_dup["gamma"]*mat_ode[,(QEindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters_dup["prob_icu"])+
      parameters_dup["gamma"]*mat_ode[,(Eindex+1)]%*%ihr[,2]*critH*parameters_dup["reporth"]*(1-parameters_dup["prob_icu"])+
      parameters_dup["gamma"]*mat_ode[,(QEindex+1)]%*%ihr[,2]*critH*parameters_dup["reporth"]*(1-parameters_dup["prob_icu"])+
      parameters_dup["gamma"]*mat_ode[,(Eindex+1)]%*%ihr[,2]*parameters_dup["prob_icu"]+
      parameters_dup["gamma"]*mat_ode[,(QEindex+1)]%*%ihr[,2]*parameters_dup["prob_icu"]
    
    cases[,i]<-(rowSums(incidence)+rowSums(incidenceh))           # daily incidence cases
    cum_cases[,i]<-colSums(incidence)+colSums(incidenceh)         # cumulative incidence cases
    day_infections[,i]<- round(rowSums(parameters_dup["gamma"]*mat_ode[,(Eindex+1)]+parameters_dup["gamma"]*mat_ode[,(QEindex+1)]))
    
    # daily infections
    infections[i] <- round(100*tail(cumsum(rowSums(parameters_dup["gamma"]*mat_ode[,(Eindex+1)])),1)/sum(popstruc[,2]), 1)  # proportion of the  population that has been infected at the end of the simulation
    for (w in (ceiling(1/parameters_dup["nui"])+1):length(times)){
      Rt_aux[w,i]<-cumsum(sum(parameters_dup["gamma"]*mat_ode[w,(Eindex+1)]))/cumsum(sum(parameters_dup["gamma"]*mat_ode[(w-1/parameters_dup["nui"]),(Eindex+1)]))
      if(Rt_aux[w,i] >= 7) {Rt_aux[w,i]  <- NA}
    }
  }
  
  print("Start aggregation of results")
  if (parameters["iterations"] > 1) {
    showNotification("Aggregation of results (~ half a minute)", duration = NULL, type = "message", id = "aggregation_results")
  }
  
  time_start <- Sys.time()
  results$mean_infections<-quantile(infections,0.5)
  results$min_infections<-quantile(infections,parameters["confidence"])
  results$max_infections<-quantile(infections,(1-parameters["confidence"]))
  
  if (parameters["iterations"] == 1) {
      results$mean_cases <- cases
      results$min_cases <- cases
      results$max_cases <- cases
      
      results$mean_cum_cases <- cum_cases
      results$min_cum_cases <- cum_cases
      results$max_cum_cases <- cum_cases
      
      results$mean_daily_infection <- day_infections
      results$min_daily_infection <- day_infections
      results$max_daily_infection <- day_infections
      
      results$mean_Rt <- Rt_aux
      results$min_Rt <- Rt_aux
      results$max_Rt <- Rt_aux
      
      results$mean <- aux[, , 1]
      results$min <- aux[, , 1]
      results$max <- aux[, , 1]
  }
  
  if (parameters["iterations"] > 1) {
    for(i in 1:length(times)){
      results$mean_cases[i]<-quantile(cases[i,],0.5)
      results$min_cases[i]<-quantile(cases[i,],parameters["confidence"])
      results$max_cases[i]<-quantile(cases[i,],(1-parameters["confidence"]))
      
      results$mean_cum_cases[i]<-quantile(cum_cases[i,],0.5)
      results$min_cum_cases[i]<-quantile(cum_cases[i,],parameters["confidence"])
      results$max_cum_cases[i]<-quantile(cum_cases[i,],(1-parameters["confidence"]))
      
      results$mean_daily_infection[i]<-quantile(day_infections[i,],0.5)
      results$min_daily_infection[i]<-quantile(day_infections[i,],parameters["confidence"])
      results$max_daily_infection[i]<-quantile(day_infections[i,],(1-parameters["confidence"]))
      
      results$mean_Rt[i]<-quantile(Rt_aux[i,],0.5,na.rm = T)
      results$min_Rt[i]<-quantile(Rt_aux[i,],parameters["confidence"],na.rm = T)
      results$max_Rt[i]<-quantile(Rt_aux[i,],(1-parameters["confidence"]),na.rm = T)
      
      for (j in 1:length(mat_ode[1,])){
        results$mean[i,j]<-quantile(aux[i,j,],0.5)
        results$min[i,j]<-quantile(aux[i,j,],parameters["confidence"])
        results$max[i,j]<-quantile(aux[i,j,],(1-parameters["confidence"]))
      }
    }
  }
  
  time_end <- Sys.time()
  print(paste("step runtime:", round(time_end - time_start, 2) %>% as.numeric(), "secs"))
  if (parameters["iterations"] > 1) {
    removeNotification(id = "aggregation_results")
  }
  return(results)
}
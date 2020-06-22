multi_runs <- function(Y, times, parameters, input, A){
  
  browser()
  results <- list()
  aux<-array(0, dim=c(length(times),22*A+1,parameters["iterations"]))
  results$mean<-matrix(0,nrow = length(times),ncol = 22*A+1)
  results$min<-matrix(0,nrow = length(times),ncol = 22*A+1)
  results$max<-matrix(0,nrow = length(times),ncol = 22*A+1)
  results$mean_cases<-matrix(0,nrow = length(times),ncol = 22*A+1)
  results$min_cases<-matrix(0,nrow = length(times),ncol = 22*A+1)
  results$max_cases<-matrix(0,nrow = length(times),ncol = 22*A+1)
  results$mean_cum_cases<-matrix(0,nrow = length(times),ncol = 1)
  results$min_cum_cases<-matrix(0,nrow = length(times),ncol = 1)
  results$max_cum_cases<-matrix(0,nrow = length(times),ncol = 1)
  results$mean_daily_infection<-matrix(0,nrow = length(times),ncol = 1)
  results$min_daily_infection<-matrix(0,nrow = length(times),ncol = 1)
  results$max_daily_infection<-matrix(0,nrow = length(times),ncol = 1)
  cases<-matrix(0, nrow=length(times),ncol=parameters["iterations"])
  cum_cases<-matrix(0, nrow=length(times),ncol=parameters["iterations"])
  day_infections<-matrix(0, nrow=length(times),ncol=parameters["iterations"])
  Rt_aux<-matrix(0, nrow=length(times),ncol=parameters["iterations"])
  infections<-matrix(0, nrow=parameters["iterations"],ncol=1)
  Rt <- NULL
  
  param_vector<-parameters
  if(parameters["iterations"]>1){
    for (i in 1:parameters["iterations"]){
      param_vector[parameters_noise]<-parameters[parameters_noise]+rnorm(length(parameters_noise),mean=0,sd=noise*abs(parameters[parameters_noise]))
      out0 <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = param_vector, input=vectors0)
      aux[,,i]<-out0
      
      critH<-c()
      crit<-c()
      critV<-c()
      for (ii in 1:length(times)){
        critH[ii]<-min(1-fH((sum(out0[ii,(Hindex+1)]))+sum(out0[ii,(ICUCindex+1)])+sum(out0[ii,(ICUCVindex+1)])),1)
        crit[ii]<-min(1-fICU((sum(out0[ii,(ICUindex+1)]))+(sum(out0[ii,(Ventindex+1)]))+(sum(out0[ii,(VentCindex+1)]))))
        critV[ii]<-min(1-fVent((sum(out0[ii,(Ventindex+1)]))),1)
      }
      
      # daily incidence
      incidence<-param_vector["report"]*param_vector["gamma"]*(1-param_vector["pclin"])*out0[,(Eindex+1)]%*%(1-ihr[,2])+
        param_vector["reportc"]*param_vector["gamma"]*param_vector["pclin"]*out0[,(Eindex+1)]%*%(1-ihr[,2])+
        param_vector["report"]*param_vector["gamma"]*(1-param_vector["pclin"])*out0[,(QEindex+1)]%*%(1-ihr[,2])+
        param_vector["reportc"]*param_vector["gamma"]*param_vector["pclin"]*out0[,(QEindex+1)]%*%(1-ihr[,2])
      
      incidenceh<- param_vector["gamma"]*out0[,(Eindex+1)]%*%ihr[,2]*(1-critH)*(1-param_vector["prob_icu"])*param_vector["reporth"]+
        param_vector["gamma"]*out0[,(Eindex+1)]%*%ihr[,2]*(1-critH)*(1-param_vector["prob_icu"])*(1-param_vector["reporth"])+
        param_vector["gamma"]*out0[,(QEindex+1)]%*%ihr[,2]*(1-critH)*(1-param_vector["prob_icu"])+
        param_vector["gamma"]*out0[,(Eindex+1)]%*%ihr[,2]*critH*param_vector["reporth"]*(1-param_vector["prob_icu"])+
        param_vector["gamma"]*out0[,(QEindex+1)]%*%ihr[,2]*critH*param_vector["reporth"]*(1-param_vector["prob_icu"])+
        param_vector["gamma"]*out0[,(Eindex+1)]%*%ihr[,2]*param_vector["prob_icu"]+
        param_vector["gamma"]*out0[,(QEindex+1)]%*%ihr[,2]*param_vector["prob_icu"]
      
      cases[,i]<-(rowSums(incidence)+rowSums(incidenceh))           # daily incidence cases
      cum_cases[,i]<-colSums(incidence)+colSums(incidenceh)         # cumulative incidence cases
      day_infections[,i]<- round(rowSums(param_vector["gamma"]*out0[,(Eindex+1)]+param_vector["gamma"]*out0[,(QEindex+1)]))
      
      # daily infections
      infections[i] <- round(100*tail(cumsum(rowSums(param_vector["gamma"]*out0[,(Eindex+1)])),1)/sum(popstruc[,2]), 1)  # proportion of the  population that has been infected at the end of the simulation
      for (w in (ceiling(1/param_vector["nui"])+1):length(times)){
        Rt_aux[w,i]<-cumsum(sum(param_vector["gamma"]*out0[w,(Eindex+1)]))/cumsum(sum(param_vector["gamma"]*out0[(w-1/param_vector["nui"]),(Eindex+1)]))
        if(Rt_aux[w,i] >= 7) {Rt_aux[w,i]  <- NA}
      }
    }  
    results$mean_infections<-quantile(infections,0.5)
    results$min_infections<-quantile(infections,confidence)
    results$max_infections<-quantile(infections,(1-confidence))
    
    
    for(i in 1:length(out0[,1])){
      results$mean_cases[i]<-quantile(cases[i,],0.5)
      results$min_cases[i]<-quantile(cases[i,],confidence)
      results$max_cases[i]<-quantile(cases[i,],(1-confidence))
      results$mean_cum_cases[i]<-quantile(cum_cases[i,],0.5)
      results$min_cum_cases[i]<-quantile(cum_cases[i,],confidence)
      results$max_cum_cases[i]<-quantile(cum_cases[i,],(1-confidence))
      results$mean_daily_infection[i]<-quantile(day_infections[i,],0.5)
      results$min_daily_infection[i]<-quantile(day_infections[i,],confidence)
      results$max_daily_infection[i]<-quantile(day_infections[i,],(1-confidence))
      results$mean_Rt[i]<-quantile(Rt_aux[i,],0.5,na.rm = T)
      results$min_Rt[i]<-quantile(Rt_aux[i,],confidence,na.rm = T)
      results$max_Rt[i]<-quantile(Rt_aux[i,],(1-confidence),na.rm = T)
      
      for (j in 1:length(out0[1,])){
        results$mean[i,j]<-quantile(aux[i,j,],0.5)
        results$min[i,j]<-quantile(aux[i,j,],confidence)
        results$max[i,j]<-quantile(aux[i,j,],(1-confidence))
      }
    }
  }else{
    results$mean <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = parameters, input=vectors0)
  }
  
  return(results)
  
}
multi_runs <- function(Y, times, parameters, input, iterations, noise, confidence){
  
  results <- list()
  results$mean <- matrix(0, nrow = length(times), ncol = 22*A+1)
  results$min <- matrix(0, nrow = length(times), ncol = 22*A+1)
  results$max <- matrix(0, nrow = length(times), ncol = 22*A+1)
  
  aux <- array(0, dim = c(length(times), 22*A+1, iterations))
  
  if(iterations > 1){
    for (i in 1:iterations){
      print(paste0("Iteration: ", i))
      parameters[parameters_noise] <- parameters[parameters_noise] + 
        rnorm(length(parameters_noise), mean = 0, sd = noise*abs(parameters[parameters_noise]))
      
      covidOdeCpp_reset()
      aux[, , i] <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covidOdeCpp, parms = parameters, input=vectors0, A=A,
                 contact_home=contact_home, contact_school=contact_school,
                 contact_work=contact_work, contact_other=contact_other,
                 popbirth_col2=popbirth[,2], popstruc_col2=popstruc[,2],
                 ageing=ageing, ifr_col2=ifr[,2], ihr_col2=ihr[,2], mort_col=mort)
    }
    shiny_aux <<- aux
    
    for(i in 1:length(times)){
      for (j in (22*A+1)){
        results$mean[i,j]<-quantile(aux[i,j,],0.5)
        results$min[i,j]<-quantile(aux[i,j,],confidence)
        results$max[i,j]<-quantile(aux[i,j,],(1-confidence))
      }
    }
    # for(i in 1:length(out0[1,])){
    #   for (j in 1:length(out0[,1])){
    #     results$mean[i,j]<-quantile(aux[i,j,],0.5)
    #     results$min[i,j]<-quantile(aux[i,j,],confidence)
    #     results$max[i,j]<-quantile(aux[i,j,],(1-confidence))
    #   }
    # }
  }else{
    covidOdeCpp_reset()
    out0 <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covidOdeCpp, parms = parameters, input=vectors0, A=A,
                        contact_home=contact_home, contact_school=contact_school,
                        contact_work=contact_work, contact_other=contact_other,
                        popbirth_col2=popbirth[,2], popstruc_col2=popstruc[,2],
                        ageing=ageing, ifr_col2=ifr[,2], ihr_col2=ihr[,2], mort_col=mort)
    results$mean <- out0
  }
  return(results)
}
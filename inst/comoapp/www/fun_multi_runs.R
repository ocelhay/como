multi_runs <- function(Y, times, parameters, input, A){
  results <- list()
  
  for (i in 1:parameters["iterations"]){
    print(paste0("Iteration ", i))
    
    # add noise to parameters
    parameters[parameters_noise] <- parameters[parameters_noise] + 
      rnorm(length(parameters_noise), mean = 0, sd = parameters["noise"]*abs(parameters[parameters_noise]))
    
    covidOdeCpp_reset()
    output_ode <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covidOdeCpp, 
                      parms = parameters, input = input, A = A,
                      contact_home=contact_home, contact_school=contact_school,
                      contact_work=contact_work, contact_other=contact_other,
                      popbirth_col2=popbirth[,2], popstruc_col2=popstruc[,2],
                      ageing=ageing, ifr_col2=ifr[,2], ihr_col2=ihr[,2], mort_col=mort)
    
    results[[i]] <- process_ode_outcome(output_ode, parameters, startdate, times, ihr, ifr, mort, popstruc)
  }
  return(results)
}
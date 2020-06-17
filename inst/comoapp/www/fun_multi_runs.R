multi_runs <- function(Y, times, parameters, input, A, iterations, noise, confidence){
  
  results <- list()
  nrow <- length(times)
  ncol <- (22*A) + 1
  
  # case when one iteration
  if(iterations == 1){
    covidOdeCpp_reset()
    output_ode <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covidOdeCpp, 
                      parms = parameters, input = input, A = A,
                      contact_home=contact_home, contact_school=contact_school,
                      contact_work=contact_work, contact_other=contact_other,
                      popbirth_col2=popbirth[,2], popstruc_col2=popstruc[,2],
                      ageing=ageing, ifr_col2=ifr[,2], ihr_col2=ihr[,2], mort_col=mort)
    
    results$mean <- output_ode %>% matrix(nrow = nrow, ncol = ncol)
  }
  
  # case when several iterations
  if(iterations > 1){
    multiple_output_ode <- NULL
    
    for (i in 1:iterations){
      print(paste0("Iteration: ", i))
      
      # add noise to parameters
      parameters[parameters_noise] <- parameters[parameters_noise] + 
        rnorm(length(parameters_noise), mean = 0, sd = noise*abs(parameters[parameters_noise]))
      
      covidOdeCpp_reset()
      output_ode <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covidOdeCpp, 
                        parms = parameters, input = input, A = A,
                        contact_home=contact_home, contact_school=contact_school,
                        contact_work=contact_work, contact_other=contact_other,
                        popbirth_col2=popbirth[,2], popstruc_col2=popstruc[,2],
                        ageing=ageing, ifr_col2=ifr[,2], ihr_col2=ihr[,2], mort_col=mort)
      
      multiple_output_ode <- cbind(multiple_output_ode, output_ode %>% as.numeric())
    }
    
    results$mean <- apply(multiple_output_ode, 1, quantile, probs = 0.5) %>% matrix(nrow = nrow, ncol = ncol)
    results$min <- apply(multiple_output_ode, 1, quantile, probs = confidence) %>% matrix(nrow = nrow, ncol = ncol)
    results$max <- apply(multiple_output_ode, 1, quantile, probs = (1 - confidence)) %>% matrix(nrow = nrow, ncol = ncol)
  }
  
  return(results)
}
fun_validation_interventions <- function(dta, simul_start_date, simul_end_date, input) {
  
  validation <- list(
    validation_interventions = TRUE, 
    message_interventions = NULL)
  
  # Test if some interventions of the same nature overlap
  test <- dta  %>%
    arrange(intervention, date_start, date_end) %>% 
    group_by(intervention) %>%
    summarise(overlapping = any(date_start <= lag(date_end, 
                                                  default = first(date_end)) & row_number() != 1), .groups = "drop")
  
  if(any(test$overlapping)) {
    validation$validation_interventions <- FALSE
    validation$message_interventions <- paste0(validation$message_interventions,
                                               "<span class = 'redbold'>NEEDS RESOLUTION: some interventions of the same nature are overlapping.</span>",
                                               br())
  }
  
  # Test interventions date range versus simulation date range
  if(any(dta$date_start < simul_start_date | dta$date_end > simul_end_date)) {
    validation$message_interventions <- paste0(validation$message_interventions,
                                               "<span class = 'warn'>Take note that some intervention(s) date range are outside the date range of simulation.</span>",
                                               br())
  }
  
  # Test if a value is beyond the accepted range due to RR
  if(dta %>% filter(intervention == "Transmissibility") %>% nrow() != 0) {
    max_value <- input$p * dta %>% filter(intervention == "Transmissibility") %>% pull(value) %>% max()
    if(max_value > 0.2) {
      validation$message_interventions <- paste0(validation$message_interventions,
                                                 "<span class = 'redbold'>NEEDS RESOLUTION: After applying RR, the 'Prob. of infection given contact' is above the max authorised value of 0.2.</span>",
                                                 br())
    }
  }
  
  if(dta %>% filter(intervention == "Lethality") %>% nrow() != 0) {
    max_value <- max(c(input$pdeath_h,
                       input$pdeath_ho,
                       input$pdeath_hc,
                       input$pdeath_hco,
                       input$pdeath_icu,
                       input$pdeath_icuo,
                       input$pdeath_icuc,
                       input$pdeath_icuco,
                       input$pdeath_vent,
                       input$pdeath_ventc,
                       input$pdeath_vent_hc,
                       input$pdeath_icu_hc,
                       input$pdeath_icu_hco) * dta %>% filter(intervention == "Lethality") %>% pull(value) %>% max())
    if(max_value > 100) {
      validation$message_interventions <- paste0(validation$message_interventions,
                                                 "<span class = 'redbold'>NEEDS RESOLUTION: After applying RR, one or several 'Probability of dying' parameter is above the max authorised value of 100.</span>",
                                                 br())
    }
  }
  
  if(dta %>% filter(intervention == "Breakthrough infection probability") %>% nrow() != 0) {
    max_value <- (input$sigmaR * dta %>% filter(intervention == "Breakthrough infection probability") %>% pull(value) %>% max())
    if(max_value > 100) {
      validation$message_interventions <- paste0(validation$message_interventions,
                                                 "<span class = 'redbold'>NEEDS RESOLUTION: After applying RR, the 'Probability of infection of people that have recovered from a previous infection' is above the max authorised value of 100.</span>",
                                                 br())
    }
  }
  
  # Test if dependent intervention dates are within the range of the referent intervention
  check_date_range <- function(reference, dependant) {
    # Example:
    # reference <-  "Self-isolation if Symptomatic"
    # dependant <-  "(*Self-isolation) Screening"
    
    dates_ref <- NULL
    ref <- dta %>% filter(intervention == reference)
    
    if(nrow(ref) >= 1) {
      for (i in 1:nrow(ref)) 
        if(ref$date_start[i] <= ref$date_end[i]) dates_ref <- c(dates_ref, seq(ref$date_start[i], ref$date_end[i], 1))
    }
    
    
    dates_dep <- NULL
    dep <- dta %>% filter(intervention == dependant)
    
    if(nrow(dep) >= 1) {
      for (i in 1:nrow(dep)) 
        if(dep$date_start[i] <= dep$date_end[i]) dates_dep <- c(dates_dep, seq(dep$date_start[i], dep$date_end[i], 1))
    }
    
    
    if(!all(dates_dep %in% dates_ref)) return(glue("<span class = 'warn'>Take note that <em>{dependant}</em> has no effect unless <em>{reference}</em> is selected.</span><br>"))
  }
  
  validation$message_interventions <- paste0(validation$message_interventions, 
                                             check_date_range(reference = "Self-isolation if Symptomatic", dependant = "(*Self-isolation) Screening"))
  validation$message_interventions <- paste0(validation$message_interventions, 
                                             check_date_range(reference = "Self-isolation if Symptomatic", dependant = "(*Self-isolation) Household Isolation"))
  
  return(validation)
}

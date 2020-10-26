fun_validation_interventions <- function(dta, simul_start_date, simul_end_date) {
  
  validation <- list(
    validation_interventions = TRUE, 
    message_interventions = NULL)
  
  # Test interventions date range versus simulation date range
  if(any(dta$date_start < simul_start_date | dta$date_end > simul_end_date)) {
    validation$message_interventions <- paste0(validation$message_interventions, 
                                               "Please note that some intervention(s) date range are outside the date range of simulation. ")
  }
  
  # Test if some interventions of the same nature overlap
  test <- dta  %>%
    arrange(intervention, date_start, date_end) %>% 
    group_by(intervention) %>%
    summarise(overlapping = any(date_start <= lag(date_end, 
                                                  default = first(date_end)) & row_number() != 1), .groups = "drop")
  
  if(any(test$overlapping)) {
    validation$validation_interventions <- FALSE
    validation$message_interventions <- paste0(validation$message_interventions, 
                                               "Some interventions of the same nature are overlapping. Needs resolution. ")
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
    
    
    if(!all(dates_dep %in% dates_ref)) return(glue("'{dependant}' has no effect unless '{reference}' is selected. "))
  }
  
  validation$message_interventions <- paste0(validation$message_interventions, 
                                             check_date_range(reference = "Self-isolation if Symptomatic", dependant = "(*Self-isolation) Screening"))
  validation$message_interventions <- paste0(validation$message_interventions, 
                                             check_date_range(reference = "Self-isolation if Symptomatic", dependant = "(*Self-isolation) Household Isolation"))
  validation$message_interventions <- paste0(validation$message_interventions, 
                                             check_date_range(reference = "Vaccination", dependant = "(*Vaccination) Age Vaccine Minimum"))
  validation$message_interventions <- paste0(validation$message_interventions, 
                                             check_date_range(reference = "Vaccination", dependant = "(*Vaccination) Age Vaccine Maximum"))
  validation$message_interventions <- paste0(validation$message_interventions, 
                                             check_date_range(reference = "Mass Testing", dependant = "(*Mass Testing) Age Testing Minimum"))
  validation$message_interventions <- paste0(validation$message_interventions, 
                                             check_date_range(reference = "Mass Testing", dependant = "(*Mass Testing) Age Testing Maximum"))
  
  
  return(validation)
}

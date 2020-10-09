fun_validation_interventions <- function(dta, all_possible_interventions = all_interventions, simul_start_date, simul_end_date) {
  
  validation <- list(
    validation_interventions = TRUE, 
    message_interventions = NULL)
  
  # test interventions date versus date range
  if(any(dta$date_start < simul_start_date | dta$date_end > simul_end_date)) {
    validation$message_interventions <- paste0(validation$message_interventions, 
                                               "Please note that some intervention(s) date range are outside the date range of simulation. ")
  }
  
  # Test if screening/quarantaine is selected outside of a period of self-isolation
  ref <- dta %>% filter(intervention == "Self-isolation if Symptomatic")
  dates_ref <- NULL
  if(nrow(ref) >= 1) {
    for (i in 1:nrow(ref)) 
      if(ref$date_start[i] <- ref$date_end[i]) dates_ref <- c(dates_ref, seq(ref$date_start[i], ref$date_end[i], 1))
  }
  
  addition <- dta %>% filter(intervention %in% "(*Self-isolation) Screening")
  dates_addition <- NULL
  if(nrow(addition) >= 1) {
    for (i in 1:nrow(addition)) 
      if(addition$date_start[i] <- addition$date_end[i]) dates_addition <- c(dates_addition, seq(addition$date_start[i], addition$date_end[i], 1))
  }
  if(!all(dates_addition %in% dates_ref)) {
    validation$message_interventions <- paste0(validation$message_interventions, 
                                               "'(*Self-isolation) Screening' has no effect unless 'Self-isolation if Symptomatic' is selected. ")
  }
  
  addition <- dta %>% filter(intervention %in% "(*Self-isolation) Household Isolation")
  dates_addition <- NULL
  if(nrow(addition) >= 1) {
    for (i in 1:nrow(addition)) 
      if(addition$date_start[i] <- addition$date_end[i]) dates_addition <- c(dates_addition, seq(addition$date_start[i], addition$date_end[i], 1))
  }
  if(!all(dates_addition %in% dates_ref)) {
    validation$message_interventions <- paste0(validation$message_interventions,
                                               "'(*Self-isolation) Household Isolation' has no effect unless 'Self-isolation if Symptomatic' is selected.")
  }
  
  # Test if vaccine age minimum is selected outside of a period of vaccination
  ref <- dta %>% filter(intervention == "Vaccination")
  dates_ref <- NULL
  if(nrow(ref) >= 1) {
    for (i in 1:nrow(ref)) 
      if(ref$date_start[i] <- ref$date_end[i]) dates_ref <- c(dates_ref, seq(ref$date_start[i], ref$date_end[i], 1))
  }
  
  addition <- dta %>% filter(intervention %in% "(*Vaccination) Age Vaccine Minimum")
  dates_addition <- NULL
  if(nrow(addition) >= 1) {
    for (i in 1:nrow(addition)) 
      if(addition$date_start[i] <- addition$date_end[i]) dates_addition <- c(dates_addition, seq(addition$date_start[i], addition$date_end[i], 1))
  }
  if(!all(dates_addition %in% dates_ref)) {
    validation$message_interventions <- paste0(validation$message_interventions, 
                                               "For a given period, setting '(*Vaccination) Age Vaccine Minimum' has no effect unless 'Vaccination' is selected. ")
  }
  
  
  # Test if vaccine age minimum is selected outside of a period of vaccination
  ref <- dta %>% filter(intervention == "Mass Testing")
  dates_ref <- NULL
  if(nrow(ref) >= 1) {
    for (i in 1:nrow(ref)) 
      if(ref$date_start[i] <- ref$date_end[i]) dates_ref <- c(dates_ref, seq(ref$date_start[i], ref$date_end[i], 1))
  }
  
  addition <- dta %>% filter(intervention %in% "(*Mass Testing) Age Testing Minimum")
  dates_addition <- NULL
  if(nrow(addition) >= 1) {
    for (i in 1:nrow(addition)) 
      if(addition$date_start[i] <- addition$date_end[i]) dates_addition <- c(dates_addition, seq(addition$date_start[i], addition$date_end[i], 1))
  }
  if(!all(dates_addition %in% dates_ref)) {
    validation$message_interventions <- paste0(validation$message_interventions, 
                                               "For a given period, setting '(*Mass Testing) Age Testing Minimum' has no effect unless 'Mass Testing' is selected. ")
  }
  
  addition <- dta %>% filter(intervention %in% "(*Mass Testing) Age Testing Maximum")
  dates_addition <- NULL
  if(nrow(addition) >= 1) {
    for (i in 1:nrow(addition)) 
      if(addition$date_start[i] <- addition$date_end[i]) dates_addition <- c(dates_addition, seq(addition$date_start[i], addition$date_end[i], 1))
  }
  if(!all(dates_addition %in% dates_ref)) {
    validation$message_interventions <- paste0(validation$message_interventions, 
                                               "For a given period, setting '(*Mass Testing) Age Testing Maximum' has no effect unless 'Mass Testing' is selected. ")
  }
  
  
  # Test interventions date overlap
  test <- dta  %>%
    arrange(intervention, date_start, date_end) %>% 
    group_by(intervention) %>%
    summarise(overlapping = any(date_start < lag(date_end, 
                                                  default = first(date_end)) & row_number() != 1), .groups = "drop")
  
  if(any(test$overlapping)) {
    validation$validation_interventions <- FALSE
    validation$message_interventions <- paste0(validation$message_interventions, 
                                               "Some interventions of the same nature are overlapping. Needs resolution. ")
  }
  
  return(validation)
}

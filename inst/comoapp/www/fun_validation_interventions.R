fun_validation_interventions <- function(dta, all_possible_interventions = all_interventions, simul_start_date, simul_end_date) {
  validation <- list(validation_interventions = TRUE, 
                     message_interventions = NULL)
  
  # Test interventions date versus date range
  if(any(dta$date_start == simul_start_date)) {
    validation$validation_interventions <- FALSE
    validation$message_interventions <- paste0(validation$message_interventions, 
                                               "All intervention start dates should be at least a day after the simulation start date. Needs Resolution. ")
  }
  
  if(any(dta$date_start < simul_start_date | dta$date_end > simul_end_date)) {
    validation$validation_interventions <- FALSE
    validation$message_interventions <- paste0(validation$message_interventions, 
                                               "Some intervention(s) dates are outside the date range of simulation. Needs Resolution. ")
  }
  
  # Test if screening/quarantaine is selected outside of a period of self-isolation
  ref <- dta %>% filter(intervention == "Self-isolation if Symptomatic")
  dates_ref <- NULL
  if(nrow(ref) >= 1) {
    for (i in 1:nrow(ref)) dates_ref <- c(dates_ref, seq(ref$date_start[i], ref$date_end[i], 1))
  }
  
  addition <- dta %>% filter(intervention %in% "Screening (when S.I.)")
  dates_addition <- NULL
  if(nrow(addition) >= 1) {
    for (i in 1:nrow(addition)) dates_addition <- c(dates_addition, seq(addition$date_start[i], addition$date_end[i], 1))
  }
  if(!all(dates_addition %in% dates_ref)) {
    validation$message_interventions <- paste0(validation$message_interventions, 
                                               "Screening has no effect if Self Isolation isn’t selected. ")
  }
  
  addition <- dta %>% filter(intervention %in% "Household Isolation (when S.I.)")
  dates_addition <- NULL
  if(nrow(addition) >= 1) {
    for (i in 1:nrow(addition)) dates_addition <- c(dates_addition, seq(addition$date_start[i], addition$date_end[i], 1))
  }
  if(!all(dates_addition %in% dates_ref)) {
    validation$message_interventions <- paste0(validation$message_interventions,
                                               "Household Isolation has no effect if Self Isolation isn’t selected. ")
  }
  
  
  # Test interventions date overlap
  test <- dta  %>%
    arrange(intervention, date_start, date_end) %>% 
    group_by(intervention) %>%
    summarise(overlapping = any(date_start <= lag(date_end, 
                                                  default = first(date_end)) & row_number() != 1))
  
  if(any(test$overlapping)) {
    validation$validation_interventions <- FALSE
    validation$message_interventions <- paste0(validation$message_interventions, 
                                               "Some interventions of the same nature are overlapping. Needs resolution. ")
  }
  
  return(validation)
}

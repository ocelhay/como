fun_validation_interventions <- function(dta, all_possible_interventions = all_interventions) {
  # dta should have the following columns: intervention, date_start, date_end, coverage
  
  validation <- list(validation_interventions = TRUE, 
                     message_interventions = NULL)
  
  # Check input
  if(!all(dta$intervention %in% all_possible_interventions)) {
    validation$validation_interventions <- FALSE
    validation$message_interventions <- paste0(validation$message_interventions, 
                                               "Some intervention(s) are unrecognised. Needs resolution. ")
  }
  
  if(any(dta$coverage < 0 | dta$coverage > 100)) {
    validation$validation_interventions <- FALSE
    validation$message_interventions <- paste0(validation$message_interventions, 
                                               "Coverage value(s) not in-between 0 and 100. Needs resolution. ")
  }
  
  # Test if screening/quarantaine is selected outsdide of a period of self-isolation
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
                                               "Household Isolation has no effect if Self Isolation isn’t selected.")
  }
  
  
  # Test period overlap
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

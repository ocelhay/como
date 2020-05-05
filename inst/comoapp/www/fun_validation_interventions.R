fun_validation_interventions <- function(dta, all_possible_interventions = all_interventions) {
  # dta should have the following columns: intervention, date_start, date_end, coverage
  
  validation <- list(validation_interventions = TRUE, 
                     message_interventions = NULL)
  
  # Check input
  if(!all(dta$intervention %in% all_possible_interventions)) {
    validation$validation_interventions <- FALSE
    validation$message_interventions <- "Some intervention(s) are unrecognised. Needs resolution."
  }
  
  if(any(dta$coverage < 0 | dta$coverage > 100)) {
    validation$validation_interventions <- FALSE
    validation$message_interventions <- "Coverage value(s) not in-between 0 and 100. Needs resolution."
  }
  
  # Test period overlap
  test <- dta  %>%
    arrange(intervention, date_start, date_end) %>% 
    group_by(intervention) %>%
    summarise(overlapping = any(date_start <= lag(date_end, 
                                                 default = first(date_end)) & row_number() != 1))
  
  if(any(test$overlapping)) {
    validation$validation_interventions <- FALSE
    validation$message_interventions <- "Some interventions of the same nature are overlapping. Needs resolution."
  }
  
  return(validation)
}

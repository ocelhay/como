process_ode_outcome <- function(out, param_used, startdate, times, ihr, ifr, mort, popstruc, intv_vector){

  # Define object to return ----
  results <- list()
  results$time = startdate + times
  
  results$med <- process_ode_outcome_compute(out_mat = out$mean, param_used, startdate, times, ihr, ifr, mort, popstruc, intv_vector)
  results$min <- process_ode_outcome_compute(out_mat = out$min, param_used, startdate, times, ihr, ifr, mort, popstruc, intv_vector)
  results$max <- process_ode_outcome_compute(out_mat = out$max, param_used, startdate, times, ihr, ifr, mort, popstruc, intv_vector)
  
  # Results already computed in multi_runs() ----
  # Rt
  results$med$Rt <- out$mean_Rt
  results$min$Rt <- out$min_Rt
  results$max$Rt <- out$max_Rt
  
  # proportion of the  population that has been infected at the end of the simulation
  results$med$pct_total_pop_infected <- out$mean_infections
  results$min$pct_total_pop_infected <- out$min_infections
  results$max$pct_total_pop_infected <- out$max_infections
  
  
  # Daily incidence
  ifelse(is.null(dim(out$mean_cases)), results$med$daily_incidence <- round(out$mean_cases), results$med$daily_incidence <- round(out$mean_cases[, 1]))
  ifelse(is.null(dim(out$min_cases)), results$min$daily_incidence <- round(out$min_cases), results$min$daily_incidence <- round(out$min_cases[, 1]))
  ifelse(is.null(dim(out$max_cases)), results$max$daily_incidence <- round(out$max_cases), results$max$daily_incidence <- round(out$max_cases[, 1]))
  
  # Daily total cases
  ifelse(is.null(dim(out$mean_daily_infection)), results$med$daily_total_cases <- round(out$mean_daily_infection), results$med$daily_total_cases <- round(out$mean_daily_infection[, 1]))
  ifelse(is.null(dim(out$min_daily_infection)), results$min$daily_total_cases <- round(out$min_daily_infection), results$min$daily_total_cases <- round(out$min_daily_infection[, 1]))
  ifelse(is.null(dim(out$max_daily_infection)), results$max$daily_total_cases <- round(out$max_daily_infection), results$max$daily_total_cases <- round(out$max_daily_infection[, 1]))
  
  # Doubling time (only for baseline)
  results$med$doubling_time <- round(log(2)*7 / (log(out$mean_cases[2+7] / out$mean_cases[2])), 2)
  results$min$doubling_time <- round(log(2)*7 / (log(out$min_cases[2+7] / out$min_cases[2])), 2)
  results$max$doubling_time <- round(log(2)*7 / (log(out$max_cases[2+7] / out$max_cases[2])), 2)
  
  # Variables that are only downloaded as median value
  results$hospital_surge_beds <- results$med$hospital_surge_beds
  results$icu_beds <- results$med$icu_beds
  results$ventilators <- results$med$ventilators
  results$normal_bed_requirement <- results$med$normal_bed_requirement
  results$icu_bed_requirement <- results$med$icu_bed_requirement
  results$icu_ventilator_requirement <- results$med$icu_ventilator_requirement
  
  results$death_natural_non_exposed <- results$med$death_natural_non_exposed
  results$death_natural_exposed <- results$med$death_natural_exposed
  results$death_treated_hospital <- results$med$death_treated_hospital
  results$death_treated_icu <- results$med$death_treated_icu
  results$death_treated_ventilator <- results$med$death_treated_ventilator
  results$death_untreated_hospital <- results$med$death_untreated_hospital
  results$death_untreated_icu <- results$med$death_untreated_icu
  results$death_untreated_ventilator <- results$med$death_untreated_ventilator
  results$reportable_death <- results$med$reportable_death
  
  return(results)
}
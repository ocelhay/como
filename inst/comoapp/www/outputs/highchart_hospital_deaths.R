output$highchart_hospital_deaths_baseline <- renderHighchart({
  req(simul_baseline$baseline_available)
  req(simul_interventions$interventions_available)
  
  dta <- tibble(time = simul_baseline$results$time,
                death_treated_hospital = simul_baseline$results$death_treated_hospital,
                death_treated_icu = simul_baseline$results$death_treated_icu,
                death_treated_ventilator = simul_baseline$results$death_treated_ventilator,
                death_untreated_hospital = simul_baseline$results$death_untreated_hospital,
                death_untreated_icu = simul_baseline$results$death_untreated_icu,
                death_untreated_ventilator = simul_baseline$results$death_untreated_ventilator)
  
  max_y <- max(simul_baseline$results$death_treated_hospital, simul_baseline$results$death_treated_icu, simul_baseline$results$death_treated_ventilator,
               simul_baseline$results$death_untreated_hospital, simul_baseline$results$death_untreated_icu, simul_baseline$results$death_untreated_ventilator,
               simul_interventions$results$death_treated_hospital, simul_interventions$results$death_treated_icu, simul_interventions$results$death_treated_ventilator,
               simul_interventions$results$death_untreated_hospital, simul_interventions$results$death_untreated_icu, simul_interventions$results$death_untreated_ventilator)
  
  tt <- tooltip_table(c("Death Treated Hospital: ", "Death Treated ICU: ", "Death Treated Ventilator: ", 
                        "Death Untreated Hospital: ", "Death Untreated ICU: ", "Death Untreated Ventilator: "), 
                      c("{point.death_treated_hospital:,.0f}", "{point.death_treated_icu:,.0f}", "{point.death_treated_ventilator:,.0f}",
                        "{point.death_untreated_hospital:,.0f}", "{point.death_untreated_icu:,.0f}", "{point.death_untreated_ventilator:,.0f}"))
  
  hchart(dta, "area", hcaes(x = time, y = death_treated_hospital), color = "#fef0d9") %>%
    hc_add_series(dta, type = 'area', hcaes(x = time, y = death_treated_icu), color = "#fdd49e") %>%
    hc_add_series(dta, type = 'area', hcaes(x = time, y = death_treated_ventilator), color = "#fdbb84") %>%
    hc_add_series(dta, type = 'area', hcaes(x = time, y = death_untreated_hospital), color = "#fc8d59") %>%
    hc_add_series(dta, type = 'area', hcaes(x = time, y = death_untreated_icu), color = "#e34a33") %>%
    hc_add_series(dta, type = 'area', hcaes(x = time, y = death_untreated_ventilator), color = "#b30000") %>%
    
    hc_tooltip(pointFormat = tt, useHTML = TRUE) %>%
    hc_title(text = "Baseline Cumulative Deaths") %>%
    hc_yAxis(max = max_y, title = "Deaths") %>%
    hc_xAxis(title = "")
})

output$highchart_hospital_deaths_interventions <- renderHighchart({
  req(simul_interventions$interventions_available)
  
  dta <- tibble(time = simul_interventions$results$time,
                death_treated_hospital = simul_interventions$results$death_treated_hospital,
                death_treated_icu = simul_interventions$results$death_treated_icu,
                death_treated_ventilator = simul_interventions$results$death_treated_ventilator,
                death_untreated_hospital = simul_interventions$results$death_untreated_hospital,
                death_untreated_icu = simul_interventions$results$death_untreated_icu,
                death_untreated_ventilator = simul_interventions$results$death_untreated_ventilator)
  
  max_y <- max(simul_baseline$results$death_treated_hospital, simul_baseline$results$death_treated_icu, simul_baseline$results$death_treated_ventilator,
               simul_baseline$results$death_untreated_hospital, simul_baseline$results$death_untreated_icu, simul_baseline$results$death_untreated_ventilator,
               simul_interventions$results$death_treated_hospital, simul_interventions$results$death_treated_icu, simul_interventions$results$death_treated_ventilator,
               simul_interventions$results$death_untreated_hospital, simul_interventions$results$death_untreated_icu, simul_interventions$results$death_untreated_ventilator)
  
  tt <- tooltip_table(c("Death Treated Hospital: ", "Death Treated ICU: ", "Death Treated Ventilator: ", 
                        "Death Untreated Hospital: ", "Death Untreated ICU: ", "Death Untreated Ventilator: "), 
                      c("{point.death_treated_hospital:,.0f}", "{point.death_treated_icu:,.0f}", "{point.death_treated_ventilator:,.0f}",
                        "{point.death_untreated_hospital:,.0f}", "{point.death_untreated_icu:,.0f}", "{point.death_untreated_ventilator:,.0f}"))
  
  hchart(dta, "area", hcaes(x = time, y = death_treated_hospital), color = "#fef0d9") %>%
    hc_add_series(dta, type = 'area', hcaes(x = time, y = death_treated_icu), color = "#fdd49e") %>%
    hc_add_series(dta, type = 'area', hcaes(x = time, y = death_treated_ventilator), color = "#fdbb84") %>%
    hc_add_series(dta, type = 'area', hcaes(x = time, y = death_untreated_hospital), color = "#fc8d59") %>%
    hc_add_series(dta, type = 'area', hcaes(x = time, y = death_untreated_icu), color = "#e34a33") %>%
    hc_add_series(dta, type = 'area', hcaes(x = time, y = death_untreated_ventilator), color = "#b30000") %>%
    
    hc_tooltip(pointFormat = tt, useHTML = TRUE) %>%
    hc_title(text = "Interventions Cumulative Deaths") %>%
    hc_yAxis(max = max_y, title = "Deaths") %>%
    hc_xAxis(title = "")
})



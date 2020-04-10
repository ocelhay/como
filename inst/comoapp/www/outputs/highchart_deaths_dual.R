output$highchart_deaths_dual_baseline <- renderHighchart({
  req(simul_baseline$baseline_available)
  req(simul_interventions$interventions_available)
  
  dta <- tibble(time = simul_baseline$results$time,
                death_treated_hospital = simul_baseline$results$death_treated_hospital,
                death_treated_icu = simul_baseline$results$death_treated_icu,
                death_treated_ventilator = simul_baseline$results$death_treated_ventilator,
                death_untreated_hospital = simul_baseline$results$death_untreated_hospital,
                death_untreated_icu = simul_baseline$results$death_untreated_icu,
                death_untreated_ventilator = simul_baseline$results$death_untreated_ventilator) %>%
    mutate(total_death = death_treated_hospital + death_treated_icu + death_treated_ventilator + death_untreated_hospital + death_untreated_icu + death_untreated_ventilator)
  
  max_y <- max((simul_baseline$results$death_treated_hospital + simul_baseline$results$death_treated_icu + simul_baseline$results$death_treated_ventilator +
                  simul_baseline$results$death_untreated_hospital + simul_baseline$results$death_untreated_icu + simul_baseline$results$death_untreated_ventilator),
               (simul_interventions$results$death_treated_hospital + simul_interventions$results$death_treated_icu + simul_interventions$results$death_treated_ventilator +
                  simul_interventions$results$death_untreated_hospital + simul_interventions$results$death_untreated_icu + simul_interventions$results$death_untreated_ventilator))
  
  hchart(dta, type = "area", name = "Death Treated Hospital", color = "#66c2a5", hcaes(x = time, y = death_treated_hospital)) %>%
    hc_add_series(dta, type = 'area', name = "Death Treated ICU", color = "#3288bd", hcaes(x = time, y = death_treated_icu)) %>%
    hc_add_series(dta, type = 'area', name = "Death Treated Ventilator", color = "#5e4fa2", hcaes(x = time, y = death_treated_ventilator)) %>%
    
    hc_add_series(dta, type = 'area', name = "Death Untreated Hospital", color = "#fdae61", hcaes(x = time, y = death_untreated_hospital)) %>%
    hc_add_series(dta, type = 'area', name = "Death Untreated ICU", color = "#d53e4f", hcaes(x = time, y = death_untreated_icu)) %>%
    hc_add_series(dta, type = 'area', name = "Death Untreated Ventilator", color = "#9e0142", hcaes(x = time, y = death_untreated_ventilator)) %>%
    hc_add_series(dta, type = 'line', name = "Total Death", color = "black", hcaes(x = time, y = total_death)) %>%
    hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:,.0f} deaths<br/>",shared = TRUE) %>%
    hc_plotOptions(area = list(stacking = "normal")) %>%
    hc_title(text = "Baseline Cumulative Deaths") %>%
    hc_yAxis(max = max_y, title = "Deaths") %>%
    hc_xAxis(title = "")
})

output$highchart_deaths_dual_interventions <- renderHighchart({
  req(simul_baseline$baseline_available)
  req(simul_interventions$interventions_available)
  
  dta <- tibble(time = simul_interventions$results$time,
                death_treated_hospital = simul_interventions$results$death_treated_hospital,
                death_treated_icu = simul_interventions$results$death_treated_icu,
                death_treated_ventilator = simul_interventions$results$death_treated_ventilator,
                death_untreated_hospital = simul_interventions$results$death_untreated_hospital,
                death_untreated_icu = simul_interventions$results$death_untreated_icu,
                death_untreated_ventilator = simul_interventions$results$death_untreated_ventilator) %>%
    mutate(total_death = death_treated_hospital + death_treated_icu + death_treated_ventilator + death_untreated_hospital + death_untreated_icu + death_untreated_ventilator)
  
  max_y <- max((simul_baseline$results$death_treated_hospital + simul_baseline$results$death_treated_icu + simul_baseline$results$death_treated_ventilator +
                  simul_baseline$results$death_untreated_hospital + simul_baseline$results$death_untreated_icu + simul_baseline$results$death_untreated_ventilator),
               (simul_interventions$results$death_treated_hospital + simul_interventions$results$death_treated_icu + simul_interventions$results$death_treated_ventilator +
                  simul_interventions$results$death_untreated_hospital + simul_interventions$results$death_untreated_icu + simul_interventions$results$death_untreated_ventilator))
  
  hchart(dta, type = "area", name = "Death Treated Hospital", color = "#66c2a5", hcaes(x = time, y = death_treated_hospital)) %>%
    hc_add_series(dta, type = 'area', name = "Death Treated ICU", color = "#3288bd", hcaes(x = time, y = death_treated_icu)) %>%
    hc_add_series(dta, type = 'area', name = "Death Treated Ventilator", color = "#5e4fa2", hcaes(x = time, y = death_treated_ventilator)) %>%
    
    hc_add_series(dta, type = 'area', name = "Death Untreated Hospital", color = "#fdae61", hcaes(x = time, y = death_untreated_hospital)) %>%
    hc_add_series(dta, type = 'area', name = "Death Untreated ICU", color = "#d53e4f", hcaes(x = time, y = death_untreated_icu)) %>%
    hc_add_series(dta, type = 'area', name = "Death Untreated Ventilator", color = "#9e0142", hcaes(x = time, y = death_untreated_ventilator)) %>%
    hc_add_series(dta, type = 'line', name = "Total Death", color = "black", hcaes(x = time, y = total_death)) %>%
    hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:,.0f} deaths<br/>", shared = TRUE) %>%
    hc_plotOptions(area = list(stacking = "normal")) %>%
    hc_title(text = "Future Scenarios Cumulative Deaths") %>%
    hc_yAxis(max = max_y, title = "Deaths") %>%
    hc_xAxis(title = "")
})

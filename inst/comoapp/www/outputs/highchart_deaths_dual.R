output$highchart_deaths_dual_baseline <- renderHighchart({
  req(simul_baseline$baseline_available)
  req(simul_interventions$interventions_available)
  
  dta <- tibble(time = simul_baseline$results$time,
                death_natural_non_exposed = simul_baseline$results$med$death_natural_non_exposed,
                death_natural_exposed = simul_baseline$results$med$death_natural_exposed,
                death_treated_hospital = simul_baseline$results$med$death_treated_hospital,
                death_treated_icu = simul_baseline$results$med$death_treated_icu,
                death_treated_ventilator = simul_baseline$results$med$death_treated_ventilator,
                death_untreated_hospital = simul_baseline$results$med$death_untreated_hospital,
                death_untreated_icu = simul_baseline$results$med$death_untreated_icu,
                death_untreated_ventilator = simul_baseline$results$med$death_untreated_ventilator,
                total_deaths = simul_baseline$results$med$total_deaths)
  
  if (!input$show_all_days) dta <- dta %>% filter(wday(time) == 4)
  
  if(input$focus_natural_death == "No Focus"){
    max_y <- max(simul_baseline$results$med$total_deaths, 
                 simul_interventions$results$med$total_deaths)
  }
  
  if(input$focus_natural_death == "COVID-19 Deaths"){
    max_y <- max(simul_baseline$results$med$death_treated_icu + simul_baseline$results$med$death_treated_ventilator + 
                   simul_baseline$results$med$death_treated_hospital +  simul_baseline$results$med$death_untreated_hospital + 
                   simul_baseline$results$med$death_untreated_icu + simul_baseline$results$med$death_untreated_ventilator,
                 
                 
                   simul_interventions$results$med$death_treated_icu + simul_interventions$results$med$death_treated_ventilator + 
                   simul_interventions$results$med$death_treated_hospital +  simul_interventions$results$med$death_untreated_hospital + 
                   simul_interventions$results$med$death_untreated_icu + simul_interventions$results$med$death_untreated_ventilator)
  }
  
  hchart(dta, type = "area", name = "Natural Death, Non Exposed", color = "#636363", hcaes(x = time, y = death_natural_non_exposed)) %>%
    hc_add_series(dta, type = 'area', name = "Natural Death, Exposed", color = "#bdbdbd", hcaes(x = time, y = death_natural_exposed)) %>%
    hc_add_series(dta, type = 'area', name = "Death Treated ICU no Ventilator", color = "#3288bd", hcaes(x = time, y = death_treated_icu)) %>%
    hc_add_series(dta, type = 'area', name = "Death Treated ICU and Ventilator", color = "#5e4fa2", hcaes(x = time, y = death_treated_ventilator)) %>%
    hc_add_series(dta, type = 'area', name = "Death Treated Hospital", color = "#66c2a5", hcaes(x = time, y = death_treated_hospital)) %>%
    hc_add_series(dta, type = 'area', name = "Death Untreated Hospital", color = "#fdae61", hcaes(x = time, y = death_untreated_hospital)) %>%
    hc_add_series(dta, type = 'area', name = "Death Untreated ICU no Ventilator", color = "#d53e4f", hcaes(x = time, y = death_untreated_icu)) %>%
    hc_add_series(dta, type = 'area', name = "Death Untreated ICU and Ventilator", color = "#9e0142", hcaes(x = time, y = death_untreated_ventilator)) %>%
    hc_add_series(dta, type = 'line', name = "Total Deaths", color = "black", hcaes(x = time, y = total_deaths)) %>%
    hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:,.0f} deaths<br/>",shared = TRUE) %>%
    hc_plotOptions(area = list(stacking = "normal")) %>%
    hc_title(text = "Baseline Cumulative Deaths") %>%
    hc_yAxis(max = max_y, title = "Deaths") %>%
    hc_xAxis(title = "") %>%
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_items)))
})

output$highchart_deaths_dual_interventions <- renderHighchart({
  req(simul_baseline$baseline_available)
  req(simul_interventions$interventions_available)
  
  dta <- tibble(time = simul_interventions$results$time,
                death_natural_non_exposed = simul_interventions$results$med$death_natural_non_exposed,
                death_natural_exposed = simul_interventions$results$med$death_natural_exposed,
                death_treated_hospital = simul_interventions$results$med$death_treated_hospital,
                death_treated_icu = simul_interventions$results$med$death_treated_icu,
                death_treated_ventilator = simul_interventions$results$med$death_treated_ventilator,
                death_untreated_hospital = simul_interventions$results$med$death_untreated_hospital,
                death_untreated_icu = simul_interventions$results$med$death_untreated_icu,
                death_untreated_ventilator = simul_interventions$results$med$death_untreated_ventilator,
                total_deaths = simul_interventions$results$med$total_deaths)
  
  if (!input$show_all_days) dta <- dta %>% filter(wday(time) == 4)
  
  if(input$focus_natural_death == "No Focus"){
    max_y <- max(simul_baseline$results$med$total_deaths, 
                 simul_interventions$results$med$total_deaths)
  }
  
  if(input$focus_natural_death == "COVID-19 Deaths"){
    max_y <- max(simul_baseline$results$med$death_treated_icu + simul_baseline$results$med$death_treated_ventilator + 
                   simul_baseline$results$med$death_treated_hospital +  simul_baseline$results$med$death_untreated_hospital + 
                   simul_baseline$results$med$death_untreated_icu + simul_baseline$results$med$death_untreated_ventilator,
                 
                 
                 simul_interventions$results$med$death_treated_icu + simul_interventions$results$med$death_treated_ventilator + 
                   simul_interventions$results$med$death_treated_hospital +  simul_interventions$results$med$death_untreated_hospital + 
                   simul_interventions$results$med$death_untreated_icu + simul_interventions$results$med$death_untreated_ventilator)
  }
  
  hchart(dta, type = "area", name = "Natural Death, Non Exposed", color = "#636363", hcaes(x = time, y = death_natural_non_exposed)) %>%
    hc_add_series(dta, type = 'area', name = "Natural Death, Exposed", color = "#bdbdbd", hcaes(x = time, y = death_natural_exposed)) %>%
    hc_add_series(dta, type = 'area', name = "Death Treated ICU no Ventilator", color = "#3288bd", hcaes(x = time, y = death_treated_icu)) %>%
    hc_add_series(dta, type = 'area', name = "Death Treated ICU and Ventilator", color = "#5e4fa2", hcaes(x = time, y = death_treated_ventilator)) %>%
    hc_add_series(dta, type = 'area', name = "Death Treated Hospital", color = "#66c2a5", hcaes(x = time, y = death_treated_hospital)) %>%
    hc_add_series(dta, type = 'area', name = "Death Untreated Hospital", color = "#fdae61", hcaes(x = time, y = death_untreated_hospital)) %>%
    hc_add_series(dta, type = 'area', name = "Death Untreated ICU no Ventilator", color = "#d53e4f", hcaes(x = time, y = death_untreated_icu)) %>%
    hc_add_series(dta, type = 'area', name = "Death Untreated ICU and Ventilator", color = "#9e0142", hcaes(x = time, y = death_untreated_ventilator)) %>%
    hc_add_series(dta, type = 'line', name = "Total Deaths", color = "black", hcaes(x = time, y = total_deaths)) %>%
    hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:,.0f} deaths<br/>",shared = TRUE) %>%
    hc_plotOptions(area = list(stacking = "normal")) %>%
    hc_title(text = "Hypothetical Scenario Cumulative Deaths") %>%
    hc_yAxis(max = max_y, title = "Deaths") %>%
    hc_xAxis(title = "") %>%
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_items)))
})

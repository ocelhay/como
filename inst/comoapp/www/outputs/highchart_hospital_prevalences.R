output$highchart_hospital_prevalences_baseline <- renderHighchart({
  req(simul_baseline$baseline_available)
  req(simul_interventions$interventions_available)
  
  dta <- tibble(time = simul_baseline$results$time,
                hospital_surge_beds = simul_baseline$results$hospital_surge_beds,
                icu_beds = simul_baseline$results$icu_beds,
                ventilators = simul_baseline$results$ventilators)
  
  max_y <- max(simul_baseline$results$hospital_surge_beds, 
               simul_interventions$results$hospital_surge_beds)
  
  tt <- tooltip_table(c("Hospital Surge Beds: ", "ICU Beds", "Ventilators: "), 
                      c("{point.hospital_surge_beds:,.0f}", "{point.icu_beds:,.0f}", "{point.ventilators:,.0f}"))
  
  return(
    hchart(dta, "area", hcaes(x = time, y = hospital_surge_beds), color = "#4d4d4d") %>% 
      hc_add_series(dta, type = 'area', hcaes(x = time, y = icu_beds), color = "#f4a582") %>%
      hc_add_series(dta, type = 'area', hcaes(x = time, y = ventilators), color = "#b2182b") %>%
      
      hc_tooltip(pointFormat = tt, useHTML = TRUE) %>%
      hc_title(text = "Baseline Hospital Requirements") %>%
      hc_yAxis(max = max_y, title = "Patients") %>%
      hc_xAxis(title = "")
  )
})

output$highchart_hospital_prevalences_interventions <- renderHighchart({
  req(simul_interventions$interventions_available)
  
  dta <- tibble(time = simul_interventions$results$time,
                hospital_surge_beds = simul_interventions$results$hospital_surge_beds,
                icu_beds = simul_interventions$results$icu_beds,
                ventilators = simul_interventions$results$ventilators)
  
  max_y <- max(simul_baseline$results$hospital_surge_beds, 
               simul_interventions$results$hospital_surge_beds)
  
  tt <- tooltip_table(c("Hospital Surge Beds: ", "ICU Beds", "Ventilators: "), 
                      c("{point.hospital_surge_beds:,.0f}", "{point.icu_beds:,.0f}", "{point.ventilators:,.0f}"))
  
  return(
    hchart(dta, "area", hcaes(x = time, y = hospital_surge_beds), color = "#4d4d4d") %>% 
      hc_add_series(dta, type = 'area', hcaes(x = time, y = icu_beds), color = "#f4a582") %>%
      hc_add_series(dta, type = 'area', hcaes(x = time, y = ventilators), color = "#b2182b") %>%
      
      hc_tooltip(pointFormat = tt, useHTML = TRUE) %>%
      hc_title(text = "Interventions Hospital Requirements") %>%
      hc_yAxis(max = max_y, title = "Patients") %>%
      hc_xAxis(title = "")
  )
})
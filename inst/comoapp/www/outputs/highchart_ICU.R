output$highchart_ICU <- renderHighchart({
  req(simul_baseline$baseline_available)
  req(simul_interventions$interventions_available)
  
  dta <- left_join(
    tibble(previcureq_bas = round(simul_baseline$results$required_beds, 0),
           saturation = round(simul_baseline$results$saturation, 0),
           time = simul_baseline$results$time),
    tibble(previcureq_int = round(simul_interventions$results$required_beds, 0),
           time = simul_interventions$results$time),
    by = "time")
  
  tt <- tooltip_table(c("Baseline, Hospital Surge Beds: ", "Interventions, Hospital Surge Beds: ", "Saturation: "), 
                      c("{point.previcureq_bas:,.0f}", "{point.previcureq_int:,.0f}", "{point.saturation:,.0f}"))
  
  return(
    hchart(dta, "line", hcaes(x = time, y = previcureq_bas), color = "#bae4bc") %>% 
      hc_add_series(dta, type = 'line', hcaes(y = saturation, x = time), color = "red") %>%
      hc_add_series(dta, type = 'line', hcaes(y = previcureq_int, x = time), color = "#3182bd") %>%
      hc_tooltip(pointFormat = tt, useHTML = TRUE) %>%
      hc_title(text = "Hospital Surge Bed Requirement") %>%
      hc_yAxis(title = "Beds") %>%
      hc_xAxis(title = "")
  )
})
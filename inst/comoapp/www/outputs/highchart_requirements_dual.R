output$highchart_requirements_dual_baseline <- renderHighchart({
  req(simul_baseline$baseline_available)
  req(simul_interventions$interventions_available)
  
  dta <- tibble(time = simul_baseline$results$time,
                hospital_surge_beds = simul_baseline$results$hospital_surge_beds,
                icu_beds = simul_baseline$results$icu_beds,
                ventilators = simul_baseline$results$ventilators,
                max_beds = input$beds_available,
                max_icu_beds = input$icu_beds_available,
                max_ventilators = input$ventilators_available)
  
  if (!input$show_all_days) dta <- dta %>% filter(wday(time) == 4)
  
  if(input$focus_requirements == "No Focus") max_y <- max((simul_baseline$results$hospital_surge_beds + simul_baseline$results$icu_beds + simul_baseline$results$ventilators), 
                                                          (simul_interventions$results$hospital_surge_beds + simul_interventions$results$icu_beds + simul_interventions$results$ventilators))
  if(input$focus_requirements == "Hospital Beds") max_y <- max(simul_baseline$results$hospital_surge_beds, simul_interventions$results$hospital_surge_beds)
  if(input$focus_requirements == "ICU Beds") max_y <- max(simul_baseline$results$icu_beds, simul_interventions$results$icu_beds)
  if(input$focus_requirements == "Ventilators") max_y <- max(simul_baseline$results$ventilators, simul_interventions$results$ventilators)
  
  
  if(input$focus_requirements == "No Focus") return(
    hchart(dta, type = "area", name = "Hospital Surge Beds", color = "#66c2a5", hcaes(x = time, y = hospital_surge_beds)) %>%
      hc_add_series(dta, type = "line", name = "Max Hospital Beds", color = "#66c2a5", hcaes(x = time, y = max_beds)) %>%
      hc_add_series(dta, type = 'area', name = "ICU Beds", color = "#3288bd", hcaes(x = time, y = icu_beds)) %>%
      hc_add_series(dta, type = "line", name = "Max ICU Beds", color = "#3288bd", hcaes(x = time, y = max_icu_beds)) %>%
      hc_add_series(dta, type = 'area', name = "Ventilators", color = "#5e4fa2", hcaes(x = time, y = ventilators)) %>%
      hc_add_series(dta, type = "line", name = "Max Ventilators", color = "#5e4fa2", hcaes(x = time, y = max_ventilators)) %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:,.0f}<br/>",shared = TRUE) %>%
      hc_title(text = "Baseline Hospital Occupancy") %>%
      hc_yAxis(max = max_y, title = "") %>%
      hc_xAxis(title = "") %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_items)))
  )
  
  if(input$focus_requirements == "Hospital Beds") return(
    hchart(dta, type = "area", name = "Hospital Surge Beds", color = "#66c2a5", hcaes(x = time, y = hospital_surge_beds)) %>%
      hc_add_series(dta, type = "line", name = "Max Hospital Beds", color = "#66c2a5", hcaes(x = time, y = max_beds)) %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:,.0f}<br/>",shared = TRUE) %>%
      hc_title(text = "Baseline Hospital Occupancy") %>%
      hc_yAxis(max = max_y, title = "") %>%
      hc_xAxis(title = "") %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_items)))
  )
  
  if(input$focus_requirements == "ICU Beds") return(
    hchart(dta, type = "area", name = "ICU Beds", color = "#3288bd", hcaes(x = time, y = icu_beds)) %>%
      hc_add_series(dta, type = "line", name = "Max ICU Beds", color = "#3288bd", hcaes(x = time, y = max_icu_beds)) %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:,.0f}<br/>",shared = TRUE) %>%
      hc_title(text = "Baseline Hospital Occupancy") %>%
      hc_yAxis(max = max_y, title = "") %>%
      hc_xAxis(title = "") %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_items)))
  )
  
  if(input$focus_requirements == "Ventilators") return(
    hchart(dta, type = "area", name = "Ventilators", color = "#5e4fa2", hcaes(x = time, y = ventilators)) %>%
      hc_add_series(dta, type = "line", name = "Max Ventilators", color = "#5e4fa2", hcaes(x = time, y = max_ventilators)) %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:,.0f}<br/>",shared = TRUE) %>%
      hc_title(text = "Baseline Hospital Occupancy") %>%
      hc_yAxis(max = max_y, title = "") %>%
      hc_xAxis(title = "") %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_items)))
  )
})

output$highchart_requirements_dual_interventions <- renderHighchart({
  req(simul_interventions$interventions_available)
  
  dta <- tibble(time = simul_interventions$results$time,
                hospital_surge_beds = simul_interventions$results$hospital_surge_beds,
                icu_beds = simul_interventions$results$icu_beds,
                ventilators = simul_interventions$results$ventilators,
                max_beds = input$beds_available,
                max_icu_beds = input$icu_beds_available,
                max_ventilators = input$ventilators_available)
  
  if (!input$show_all_days) dta <- dta %>% filter(wday(time) == 4)
  
  if(input$focus_requirements == "No Focus") max_y <- max((simul_baseline$results$hospital_surge_beds + simul_baseline$results$icu_beds + simul_baseline$results$ventilators), 
                                                          (simul_interventions$results$hospital_surge_beds + simul_interventions$results$icu_beds + simul_interventions$results$ventilators))
  if(input$focus_requirements == "Hospital Beds") max_y <- max(simul_baseline$results$hospital_surge_beds, simul_interventions$results$hospital_surge_beds)
  if(input$focus_requirements == "ICU Beds") max_y <- max(simul_baseline$results$icu_beds, simul_interventions$results$icu_beds)
  if(input$focus_requirements == "Ventilators") max_y <- max(simul_baseline$results$ventilators, simul_interventions$results$ventilators)
  
  
  if(input$focus_requirements == "No Focus") return(
    hchart(dta, type = "area", name = "Hospital Surge Beds", color = "#66c2a5", hcaes(x = time, y = hospital_surge_beds)) %>%
      hc_add_series(dta, type = "line", name = "Max Hospital Beds", color = "#66c2a5", hcaes(x = time, y = max_beds)) %>%
      hc_add_series(dta, type = 'area', name = "ICU Beds", color = "#3288bd", hcaes(x = time, y = icu_beds)) %>%
      hc_add_series(dta, type = "line", name = "Max ICU Beds", color = "#3288bd", hcaes(x = time, y = max_icu_beds)) %>%
      hc_add_series(dta, type = 'area', name = "Ventilators", color = "#5e4fa2", hcaes(x = time, y = ventilators)) %>%
      hc_add_series(dta, type = "line", name = "Max Ventilators", color = "#5e4fa2", hcaes(x = time, y = max_ventilators)) %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:,.0f}<br/>",shared = TRUE) %>%
      hc_title(text = "Future Scenarios Hospital Occupancy") %>%
      hc_yAxis(max = max_y, title = "") %>%
      hc_xAxis(title = "") %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_items)))
  )
  
  if(input$focus_requirements == "Hospital Beds") return(
    hchart(dta, type = "area", name = "Hospital Surge Beds", color = "#66c2a5", hcaes(x = time, y = hospital_surge_beds)) %>%
      hc_add_series(dta, type = "line", name = "Max Hospital Beds", color = "#66c2a5", hcaes(x = time, y = max_beds)) %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:,.0f}<br/>",shared = TRUE) %>%
      hc_title(text = "Future Scenarios Hospital Occupancy") %>%
      hc_yAxis(max = max_y, title = "") %>%
      hc_xAxis(title = "") %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_items)))
  )
  
  if(input$focus_requirements == "ICU Beds") return(
    hchart(dta, type = "area", name = "ICU Beds", color = "#3288bd", hcaes(x = time, y = icu_beds)) %>%
      hc_add_series(dta, type = "line", name = "Max ICU Beds", color = "#3288bd", hcaes(x = time, y = max_icu_beds)) %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:,.0f}<br/>",shared = TRUE) %>%
      hc_title(text = "Future Scenarios Hospital Occupancy") %>%
      hc_yAxis(max = max_y, title = "") %>%
      hc_xAxis(title = "") %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_items)))
  )
  
  if(input$focus_requirements == "Ventilators") return(
    hchart(dta, type = "area", name = "Ventilators", color = "#5e4fa2", hcaes(x = time, y = ventilators)) %>%
      hc_add_series(dta, type = "line", name = "Max Ventilators", color = "#5e4fa2", hcaes(x = time, y = max_ventilators)) %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:,.0f}<br/>",shared = TRUE) %>%
      hc_title(text = "Future Scenarios Hospital Occupancy") %>%
      hc_yAxis(max = max_y, title = "") %>%
      hc_xAxis(title = "") %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_items)))
  )
})
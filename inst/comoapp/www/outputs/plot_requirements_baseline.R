output$plot_requirements_baseline <- renderPlot({
  req(simul_baseline$baseline_available)
  req(!input$dynamic_requirements_baseline)
  
  dta <- left_join(tibble(time = simul_baseline$results$time,
                          hospital_surge_beds = simul_baseline$results$med$hospital_surge_beds,
                          icu_beds = simul_baseline$results$med$icu_beds,
                          ventilators = simul_baseline$results$med$ventilators,
                          max_beds = input$beds_available,
                          max_icu_beds = input$icu_beds_available,
                          max_ventilators = input$ventilators_available),
                   cases_rv$data,
                   by = c("time" = "date"))
  
  if(input$focus_axis == "Observed")  max_x <- dta$time[last(which(!is.na(dta$cases)))] + 3
  if(input$focus_axis != "Observed")  max_x <- max(dta$time)
  dta <- dta %>% filter(time <= max_x)
  
  if(input$focus_requirements_baseline == "No Focus") return(
    ggplot(dta, aes(x = time)) +
      geom_area(aes(y = hospital_surge_beds, fill = "Hospital Surge Beds")) +
      geom_hline(aes(yintercept = max_beds, color = "Hospital Surge Beds")) +
      geom_area(aes(y = icu_beds, fill = "ICU Beds")) +
      geom_hline(aes(yintercept = max_icu_beds, color = "ICU Beds")) +
      geom_area(aes(y = ventilators, fill = "Ventilators")) +
      geom_hline(aes(yintercept = max_ventilators, color = "Ventilators")) +
      scale_fill_manual(name = "Requirement", values = c("Hospital Surge Beds" = "#66c2a5",
                                                         "ICU Beds" = "#3288bd", "Ventilators" = "#5e4fa2")) +
      scale_color_manual(name = "Maximum Capacity", values = c("Hospital Surge Beds" = "#66c2a5",
                                                               "ICU Beds" = "#3288bd", "Ventilators" = "#5e4fa2")) +
      labs(y = NULL, x = NULL, title = "Hospital Occupancy") +
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
      theme_light(base_size = 14)
  )
  
  if(input$focus_requirements_baseline == "Hospital Beds") return(
    ggplot(dta, aes(x = time)) +
      geom_area(aes(y = hospital_surge_beds, fill = "Hospital Surge Beds")) +
      geom_hline(aes(yintercept = max_beds, color = "Hospital Surge Beds")) +
      scale_fill_manual(name = "Requirement", values = c("Hospital Surge Beds" = "#66c2a5",
                                                         "ICU Beds" = "#3288bd", "Ventilators" = "#5e4fa2")) +
      scale_color_manual(name = "Maximum Capacity", values = c("Hospital Surge Beds" = "#66c2a5",
                                                               "ICU Beds" = "#3288bd", "Ventilators" = "#5e4fa2")) +
      labs(y = NULL, x = NULL, title = "Hospital Occupancy") +
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
      theme_light(base_size = 14)
  )
  
  if(input$focus_requirements_baseline == "ICU Beds") return(
    ggplot(dta, aes(x = time)) +
      geom_area(aes(y = icu_beds, fill = "ICU Beds")) +
      geom_hline(aes(yintercept = max_icu_beds, color = "ICU Beds")) +
      scale_fill_manual(name = "Requirement", values = c("Hospital Surge Beds" = "#66c2a5",
                                                         "ICU Beds" = "#3288bd", "Ventilators" = "#5e4fa2")) +
      scale_color_manual(name = "Maximum Capacity", values = c("Hospital Surge Beds" = "#66c2a5",
                                                               "ICU Beds" = "#3288bd", "Ventilators" = "#5e4fa2")) +
      labs(y = NULL, x = NULL, title = "Hospital Occupancy") +
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
      theme_light(base_size = 14)
  )
  
  if(input$focus_requirements_baseline == "Ventilators") return(
    ggplot(dta, aes(x = time)) +
      geom_area(aes(y = ventilators, fill = "Ventilators")) +
      geom_hline(aes(yintercept = max_ventilators, color = "Ventilators")) +
      scale_fill_manual(name = "Requirement", values = c("Hospital Surge Beds" = "#66c2a5",
                                                         "ICU Beds" = "#3288bd", "Ventilators" = "#5e4fa2")) +
      scale_color_manual(name = "Maximum Capacity", values = c("Hospital Surge Beds" = "#66c2a5",
                                                               "ICU Beds" = "#3288bd", "Ventilators" = "#5e4fa2")) +
      labs(y = NULL, x = NULL, title = "Hospital Occupancy") +
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
      theme_light(base_size = 14)
  )
})

output$highchart_requirements_baseline <- renderHighchart({
  req(simul_baseline$baseline_available)
  req(input$dynamic_requirements_baseline)
  
  dta <- tibble(time = simul_baseline$results$time,
                hospital_surge_beds = simul_baseline$results$med$hospital_surge_beds,
                icu_beds = simul_baseline$results$med$icu_beds,
                ventilators = simul_baseline$results$med$ventilators,
                max_beds = input$beds_available,
                max_icu_beds = input$icu_beds_available,
                max_ventilators = input$ventilators_available)
  
  if(input$focus_requirements_baseline == "No Focus") max_y <- max((simul_baseline$results$med$hospital_surge_beds + simul_baseline$results$med$icu_beds + simul_baseline$results$med$ventilators), 
                                                          (simul_interventions$results$med$hospital_surge_beds + simul_interventions$results$med$icu_beds + simul_interventions$results$med$ventilators))
  if(input$focus_requirements_baseline == "Hospital Beds") max_y <- max(simul_baseline$results$med$hospital_surge_beds, simul_interventions$results$med$hospital_surge_beds)
  if(input$focus_requirements_baseline == "ICU Beds") max_y <- max(simul_baseline$results$med$icu_beds, simul_interventions$results$med$icu_beds)
  if(input$focus_requirements_baseline == "Ventilators") max_y <- max(simul_baseline$results$med$ventilators, simul_interventions$results$med$ventilators)
  
  
  if(input$focus_requirements_baseline == "No Focus") return(
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
  
  if(input$focus_requirements_baseline == "Hospital Beds") return(
    hchart(dta, type = "area", name = "Hospital Surge Beds", color = "#66c2a5", hcaes(x = time, y = hospital_surge_beds)) %>%
      hc_add_series(dta, type = "line", name = "Max Hospital Beds", color = "#66c2a5", hcaes(x = time, y = max_beds)) %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:,.0f}<br/>",shared = TRUE) %>%
      hc_title(text = "Baseline Hospital Occupancy") %>%
      hc_yAxis(max = max_y, title = "") %>%
      hc_xAxis(title = "") %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_items)))
  )
  
  if(input$focus_requirements_baseline == "ICU Beds") return(
    hchart(dta, type = "area", name = "ICU Beds", color = "#3288bd", hcaes(x = time, y = icu_beds)) %>%
      hc_add_series(dta, type = "line", name = "Max ICU Beds", color = "#3288bd", hcaes(x = time, y = max_icu_beds)) %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:,.0f}<br/>",shared = TRUE) %>%
      hc_title(text = "Baseline Hospital Occupancy") %>%
      hc_yAxis(max = max_y, title = "") %>%
      hc_xAxis(title = "") %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_items)))
  )
  
  if(input$focus_requirements_baseline == "Ventilators") return(
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
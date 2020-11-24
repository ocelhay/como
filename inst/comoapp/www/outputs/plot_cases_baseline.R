output$plot_cases_baseline <- renderPlot({
  req(simul_baseline$baseline_available)
  req(!input$dynamic_cases_baseline)
  
  dta <- left_join(tibble(daily_incidence_min = simul_baseline$results$min$daily_incidence,
                          daily_incidence_med = simul_baseline$results$med$daily_incidence,
                          daily_incidence_max = simul_baseline$results$max$daily_incidence,
                          daily_total_cases_min = simul_baseline$results$min$daily_total_cases,
                          daily_total_cases_med = simul_baseline$results$med$daily_total_cases,
                          daily_total_cases_max = simul_baseline$results$max$daily_total_cases,
                          time = simul_baseline$results$time),
                   cases_rv$data,
                   by = c("time" = "date"))
  
  if(input$entity_tests != "_")  dta <- left_join(dta, tests %>% filter(entity == input$entity_tests), by = c("time" = "date"))
  
  # X/Y scales
  if(input$focus_axis == "Observed")  {
    max_x <- dta$time[last(which(!is.na(dta$cases)))] + 3
    if(input$entity_tests == "_")  max_y <- 1.2 * max(dta$cases, na.rm = TRUE)
    if(input$entity_tests != "_")  max_y <- 1.2 * max(dta$cases, dta$tests, na.rm = TRUE)
  }
  
  if(input$focus_axis == "Predicted Reported")  {
    max_x <- max(dta$time)
    if(input$entity_tests == "_")  max_y <- 1.2 * max(dta$daily_incidence_max, na.rm = TRUE)
    if(input$entity_tests != "_")  max_y <- 1.2 * max(dta$daily_incidence_max, dta$tests, na.rm = TRUE)
  }
  
  if(input$focus_axis == "Predicted Reported + Unreported")  {
    max_x <- max(dta$time)
    if(input$entity_tests == "_")  max_y <- 1.2 * max(dta$daily_total_cases_max, na.rm = TRUE)
    if(input$entity_tests != "_")  max_y <- 1.2 * max(dta$daily_total_cases_max, dta$tests, na.rm = TRUE)
  }
  
  plot <- dta %>%
    rename(Date = time) %>%
    filter(Date <= max_x) %>%
    ggplot(aes(x = Date)) +
    geom_ribbon(aes(ymin = daily_total_cases_min, ymax = daily_total_cases_max), fill = "#74c476", alpha = 0.7) +
    geom_line(aes(y = daily_total_cases_med, color = "Predicted Reported + Unreported"), lwd = 1.2) + 
    geom_ribbon(aes(ymin = daily_incidence_min, ymax = daily_incidence_max), fill = "#00441b", alpha = 0.7) +
    geom_line(aes(y = daily_incidence_med, color = "Predicted Reported"), lwd = 1.2) +
    geom_line(aes(y = cases, color = "Observed"), lwd = 1.2) +
    geom_point(aes(y = cases), color = "red") + 
    coord_cartesian(ylim = c(NA, max_y)) +
    labs(title = "Baseline Daily Cases", x= "", y = "") +
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
    theme_light(base_size = 14)
  
  if(input$entity_tests == "_")  plot <- plot + scale_color_manual(name = "Cases", values = c("Predicted Reported" = "#00441b", 
                                                                                              "Predicted Reported + Unreported" = "#74c476", 
                                                                                              "Observed" = "red"))
  if(input$entity_tests != "_")  plot <- plot + geom_line(data = tests %>% filter(entity == input$entity_tests), aes(x = date, y = tests, color = "Tests"), lwd = 1.2) + scale_color_manual(name = "Cases", values = c("Predicted Reported" = "#74c476", "Predicted Reported + Unreported" = "#00441b", "Observed" = "red", "Tests" = "grey"))
  
  return(plot)
})

output$highchart_cases_baseline <- renderHighchart({
  req(simul_baseline$baseline_available)
  req(input$dynamic_cases_baseline)
  
  dta <- left_join(tibble(daily_incidence_min = simul_baseline$results$min$daily_incidence,
                          daily_incidence_med = simul_baseline$results$med$daily_incidence,
                          daily_incidence_max = simul_baseline$results$max$daily_incidence,
                          daily_total_cases_min = simul_baseline$results$min$daily_total_cases,
                          daily_total_cases_med = simul_baseline$results$med$daily_total_cases,
                          daily_total_cases_max = simul_baseline$results$max$daily_total_cases,
                          time = simul_baseline$results$time),
                   cases_rv$data,
                   by = c("time" = "date"))
  
  if(input$entity_tests != "_")  dta <- left_join(dta, tests %>% filter(entity == input$entity_tests), by = c("time" = "date"))
  
  # X/Y scales
  if(input$focus_axis == "Observed")  {
    max_x <- dta$time[last(which(!is.na(dta$cases)))] + 3
    if(input$entity_tests == "_")  max_y <- 1.2 * max(dta$cases, na.rm = TRUE)
    if(input$entity_tests != "_")  max_y <- 1.2 * max(dta$cases, dta$tests, na.rm = TRUE)
  }
  
  if(input$focus_axis == "Predicted Reported")  {
    max_x <- max(dta$time)
    if(input$entity_tests == "_")  max_y <- 1.2 * max(dta$daily_incidence_max, na.rm = TRUE)
    if(input$entity_tests != "_")  max_y <- 1.2 * max(dta$daily_incidence_max, dta$tests, na.rm = TRUE)
  }
  
  if(input$focus_axis == "Predicted Reported + Unreported")  {
    max_x <- max(dta$time)
    if(input$entity_tests == "_")  max_y <- 1.2 * max(dta$daily_total_cases_max, na.rm = TRUE)
    if(input$entity_tests != "_")  max_y <- 1.2 * max(dta$daily_total_cases_max, dta$tests, na.rm = TRUE)
  }
  
  
  hc <- hchart(dta, "line", name = "Observed", hcaes(x = time, y = cases), color = "red") %>% 
    hc_add_series(dta, type = "line", hcaes(x = time, y = daily_incidence_med), id = "reported", name = "Predicted Reported", color = "#00441b") %>%
    hc_add_series(dta, type = 'arearange', name = "Predicted Reported", color = "#00441b", 
                  hcaes(x = time, low = daily_incidence_min, high = daily_incidence_max), 
                  linkedTo = "reported") %>%
    hc_add_series(dta, type = "line", hcaes(x = time, y = daily_total_cases_med), id = "pred_reported", name = "Predicted Reported + Unreported", color = "#74c476") %>%
    hc_add_series(dta, type = 'arearange', name = "Predicted Reported + Unreported", color = "#74c476", 
                  hcaes(x= time,  low = daily_total_cases_min, high = daily_total_cases_max), 
                  linkedTo = "pred_reported") %>%
    hc_title(text = "Baseline Daily Cases") %>%
    hc_yAxis(min = 0, max = max_y, title = "Cases") %>%
    hc_xAxis(title = "") %>%
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_items)))
  
  if(input$entity_tests == "_")  {
    hc <- hc %>% hc_tooltip(shared = TRUE)
  }
  
  if(input$entity_tests != "_")  {
    hc <- hc %>%
      hc_add_series(dta, type = "line", hcaes(x = time, y = tests), name = "Tests", color = "grey") %>%
      hc_tooltip(shared = TRUE)
  }
  
  return(hc)
})


output$plot_deaths_baseline <- renderPlot({
  req(simul_baseline$baseline_available)
  req(!input$dynamic_deaths_baseline)
  
  dta <- left_join(tibble(
    # change 2020-12-29
    reportable_death_min = simul_baseline$results$min$cum_mortality,
    reportable_death_med = simul_baseline$results$med$cum_mortality,
    reportable_death_max = simul_baseline$results$max$cum_mortality,
    # attributable_deaths_min = simul_baseline$results$min$attributable_deaths,
    # attributable_deaths_med = simul_baseline$results$med$attributable_deaths,
    # attributable_deaths_max = simul_baseline$results$max$attributable_deaths,
    time = simul_baseline$results$time), 
    cases_rv$data, # cumulative_death
    by = c("time" = "date"))
  
  # X/Y scales
  if(input$focus_axis == "Observed")  {
    max_x <- dta$time[last(which(!is.na(dta$cumulative_death)))] + 3
    max_y <- 1.2 * max(dta$cumulative_death, na.rm = TRUE)
  }
  if(input$focus_axis == "Predicted Reported" | input$focus_axis == "Predicted Reported + Unreported")  {
    max_x <- max(dta$time)
    max_y <- NA
  }
  
  dta %>%
    rename(Date = time) %>%
    filter(Date <= max_x) %>%
  
  ggplot(aes(x = Date)) +
    geom_ribbon(aes(ymin = reportable_death_min, ymax = reportable_death_max), fill = "#00441b", alpha = 0.7) +
    geom_line(aes(y = reportable_death_med, color = "Predicted Reported"), lwd = 1.2) + 
    
    # geom_ribbon(aes(ymin = attributable_deaths_min, ymax = attributable_deaths_max), fill = "#74c476", alpha = 0.7) +
    # geom_line(aes(y = attributable_deaths_med, color = "Attributable"), lwd = 1.2) + 
    
    geom_line(aes(y = cumulative_death, color = "Observed"), lwd = 1.2) +
    
    geom_point(aes(y = cumulative_death), color = "red") +
    coord_cartesian(ylim = c(NA, max_y)) +
    labs(title = "Baseline Cumulative Deaths", x= "", y = "") +
    theme_light(base_size = 14) +
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
    scale_x_date(date_labels =  "%b %Y") +
    scale_color_manual(name = "Cumulative Deaths", values = c("Predicted Reported" = "#00441b", "Observed" = "red"))
})


output$highchart_deaths_baseline <- renderHighchart({
  req(simul_baseline$baseline_available)
  req(input$dynamic_deaths_baseline)
  
  dta <- left_join(tibble(
    # change 2020-12-29
    reportable_death_min = simul_baseline$results$min$cum_mortality,
    reportable_death_med = simul_baseline$results$med$cum_mortality,
    reportable_death_max = simul_baseline$results$max$cum_mortality,
    # attributable_deaths_min = simul_baseline$results$min$attributable_deaths,
    # attributable_deaths_med = simul_baseline$results$med$attributable_deaths,
    # attributable_deaths_max = simul_baseline$results$max$attributable_deaths,
    time = simul_baseline$results$time), 
    cases_rv$data, # cumulative_death
    by = c("time" = "date"))
  
  # X/Y scales
  if(input$focus_axis == "Observed")  {
    max_x <- dta$time[last(which(!is.na(dta$cumulative_death)))] + 3
    max_y <- 1.2 * max(dta$cumulative_death, na.rm = TRUE)
  }
  if(input$focus_axis == "Predicted Reported" | input$focus_axis == "Predicted Reported + Unreported")  {
    max_x <- max(dta$time)
    max_y <- NA
  }
  
  dta <- dta %>%
    rename(Date = time) %>%
    filter(Date <= max_x)
  
  
  hchart(dta, "line", name = "Observed", hcaes(x = Date, y = cumulative_death), color = "red") %>% 
    hc_add_series(dta, type = "line", hcaes(x = Date, y = reportable_death_med), id = "reportable", name = "Predicted Reported", color = "#74c476") %>%
    hc_add_series(dta, type = 'arearange', name = "Predicted Reported", color = "#00441b", 
                  hcaes(x = Date, low = reportable_death_min, high = reportable_death_max), 
                  linkedTo = "reportable") %>%
    # hc_add_series(dta, type = "line", hcaes(x = Date, y = attributable_deaths_med), id = "attributable", name = "Attributable", color = "#74c476") %>%
    # hc_add_series(dta, type = 'arearange', name = "Attributable", color = "#74c476", 
    #               hcaes(x= Date,  low = attributable_deaths_min, high = attributable_deaths_max), 
    #               linkedTo = "attributable") %>%
    hc_title(text = "Baseline Cumulative Deaths") %>%
    hc_yAxis(min = 0, max = max_y, title = "") %>%
    hc_xAxis(title = "") %>%
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_items)))
})
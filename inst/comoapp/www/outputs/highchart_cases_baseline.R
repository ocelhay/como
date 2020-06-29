output$plot_cases_baseline <- renderPlot({
  req(simul_baseline$baseline_available)
  
  # dta <- left_join(tibble(daily_incidence = simul_baseline$results$med$daily_incidence,
  #                         daily_total_cases = simul_baseline$results$med$daily_total_cases,
  #                         time = simul_baseline$results$time), 
  #                  cases_rv$data, 
  #                  by = c("time" = "date"))
  # 
  # # X/Y scales
  # if(input$focus_axis == "Observed")  {
  #   max_x <- dta$time[last(which(!is.na(dta$cases)))] + 3
  #   max_y <- 1.2 * max(dta$cases, na.rm = TRUE)
  # }
  # if(input$focus_axis == "Predicted Reported")  {
  #   max_x <- max(dta$time)
  #   max_y <- 1.2 * max(simul_baseline$results$med$daily_incidence, na.rm = TRUE)
  # }
  # if(input$focus_axis == "Predicted Reported + Unreported")  {
  #   max_x <- max(dta$time)
  #   max_y <- 1.2 * max(simul_baseline$results$med$daily_total_cases, na.rm = TRUE)
  # }
  # 
  # dta2 <- dta %>%
  #   rename(Date = time) %>%
  #   filter(Date <= max_x)
  # 
  # return(
  #   hchart(dta2, "area", name = "Predicted Reported", hcaes(x = Date, y = daily_incidence), color = "#00441b") %>% 
  #     hc_add_series(dta2, type = 'area', name = "Predicted Reported + Unreported", hcaes(y = daily_total_cases, x = Date), color = "#74c476", dashStyle = "longdash") %>%
  #     hc_add_series(dta2, type = 'line', name = "Observed", hcaes(y = cases, x = Date), color = "red") %>%
  #     hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
  #            {point.y:,.0f}<br/>", shared = TRUE) %>%
  #     hc_title(text = "Baseline Cases") %>%
  #     hc_yAxis(max = max_y, title = "Cases") %>%
  #     hc_xAxis(title = "")
  # )
  
  dta <- left_join(tibble(daily_incidence_min = simul_baseline$results$min$daily_incidence,
                          daily_incidence_med = simul_baseline$results$med$daily_incidence,
                          daily_incidence_max = simul_baseline$results$max$daily_incidence,
                          daily_total_cases_min = simul_baseline$results$min$daily_total_cases,
                          daily_total_cases_med = simul_baseline$results$med$daily_total_cases,
                          daily_total_cases_max = simul_baseline$results$max$daily_total_cases,
                          time = simul_baseline$results$time),
                   cases_rv$data,
                   by = c("time" = "date"))
  
  # X/Y scales
  if(input$focus_axis == "Observed")  {
    max_x <- dta$time[last(which(!is.na(dta$cases)))] + 3
    max_y <- 1.2 * max(dta$cases, na.rm = TRUE)
  }
  
  if(input$focus_axis == "Predicted Reported")  {
    max_x <- max(dta$time)
    max_y <- 1.2 * max(dta$daily_incidence_max, na.rm = TRUE)
  }
  
  if(input$focus_axis == "Predicted Reported + Unreported")  {
    max_x <- max(dta$time)
    max_y <- 1.2 * max(dta$daily_total_cases_max, na.rm = TRUE)
  }
  
  dta2 <- dta %>%
    rename(Date = time) %>%
    filter(Date <= max_x)
  
  # hchart(dta2, type = 'arearange', name = "Predicted Reported", color = "#00441b", 
  #        hcaes(x = Date, low = daily_total_cases_min, high = daily_total_cases_max)) %>%
  #   hc_add_series(dta2, type = 'arearange', name = "Predicted Reported + Unreported", color = "#74c476", dashStyle = "longdash", 
  #                 hcaes(x = Date, low = daily_total_cases_min, high = daily_total_cases_max)) %>%
  #   hc_add_series(dta2, type = 'line', name = "Observed", hcaes(y = cases, x = Date), color = "red") %>%
  #   hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
  #            {point.y:,.0f}<br/>", shared = TRUE) %>%
  #   hc_title(text = "Baseline Cases") %>%
  #   hc_yAxis(max = max_y, title = "Cases") %>%
  #   hc_xAxis(title = "")
  
  ggplot(data = dta2, aes(x = Date)) +
    geom_ribbon(aes(ymin = daily_total_cases_min, ymax = daily_total_cases_max), fill = "#00441b") +
    geom_line(aes(y = daily_total_cases_med)) + 
    geom_ribbon(aes(ymin = daily_incidence_min, ymax = daily_incidence_max), fill = "#74c476") +
    geom_line(aes(y = daily_incidence_med)) +
    geom_line(aes(y = cases), color = "red") +
    coord_cartesian(ylim = c(NA, max_y)) +
    ggtitle("Baseline Cases") + xlab("") + ylab("Daily Cases") +
    theme_light(base_size = 14)
    
})

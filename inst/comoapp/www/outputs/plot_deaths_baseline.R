output$plot_deaths_baseline <- renderPlot({
  req(simul_baseline$baseline_available)
  
  dta <- left_join(tibble(
    cum_mortality_min = simul_baseline$results$min$cum_mortality,
    cum_mortality_med = simul_baseline$results$med$cum_mortality,
    cum_mortality_max = simul_baseline$results$max$cum_mortality,
    time = simul_baseline$results$time), 
    cases_rv$data, # cumulative_death
    by = c("time" = "date"))
  
  # X/Y scales
  if(input$focus_axis == "Observed")  {
    max_x <- dta$time[last(which(!is.na(dta$cumulative_death)))]  + 3
    max_y <- 1.2 * max(dta$cumulative_death, na.rm = TRUE)
  }
  if(input$focus_axis == "Predicted Reported" | input$focus_axis == "Predicted Reported + Unreported")  {
    max_x <- max(dta$time)
    max_y <- NA
  }
  
  dta2 <- dta %>%
    rename(Date = time) %>%
    filter(Date <= max_x)
  
  # return(
  #   hchart(dta2, "line", name = "Predicted Reported Deaths", hcaes(x = Date, y = cum_mortality), color = "#00441b") %>% 
  #     hc_add_series(dta2, type = 'line', name = "Observed", hcaes(x = Date, y = cumulative_death), color = "red") %>%
  #     hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
  #            {point.y:,.0f}<br/>", shared = TRUE) %>%
  #     hc_title(text = "Baseline Cumulative Deaths") %>%
  #     hc_yAxis(max = max_y, title = "Cases") %>%
  #     hc_xAxis(title = "")
  # )
  
  ggplot(data = dta2, aes(x = Date)) +
    geom_ribbon(aes(ymin = cum_mortality_min, ymax = cum_mortality_max), fill = "#00441b") +
    geom_line(aes(y = cum_mortality_med)) + 
    geom_line(aes(y = cumulative_death), color = "red") +
    coord_cartesian(ylim = c(NA, max_y)) +
    ggtitle("Baseline Cumulative Deaths") + xlab("") + ylab("Deaths") +
    theme_light(base_size = 14)
})
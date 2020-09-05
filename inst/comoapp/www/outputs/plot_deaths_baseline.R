output$plot_deaths_baseline <- renderPlot({
  req(simul_baseline$baseline_available)
  
  dta <- left_join(tibble(
    reportable_death_min = simul_baseline$results$min$reportable_death,
    reportable_death_med = simul_baseline$results$med$reportable_death,
    reportable_death_max = simul_baseline$results$max$reportable_death,
    attributable_deaths_min = simul_baseline$results$min$attributable_deaths,
    attributable_deaths_med = simul_baseline$results$med$attributable_deaths,
    attributable_deaths_max = simul_baseline$results$max$attributable_deaths,
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
  
  dta2 <- dta %>%
    rename(Date = time) %>%
    filter(Date <= max_x)
  
  ggplot(data = dta2, aes(x = Date)) +
    geom_ribbon(aes(ymin = reportable_death_min, ymax = reportable_death_max), fill = "#00441b", alpha = 0.7) +
    geom_line(aes(y = reportable_death_med, color = "Reportable"), lwd = 1.2) + 
    
    geom_ribbon(aes(ymin = attributable_deaths_min, ymax = attributable_deaths_max), fill = "#74c476", alpha = 0.7) +
    geom_line(aes(y = attributable_deaths_med, color = "Attributable"), lwd = 1.2) + 
    
    geom_line(aes(y = cumulative_death, color = "Observed"), lwd = 1.2) +
    
    geom_point(aes(y = cumulative_death), color = "red") +
    coord_cartesian(ylim = c(NA, max_y)) +
    ggtitle("Baseline Cumulative Deaths") + xlab("") + ylab("Deaths") +
    theme_light(base_size = 14) +
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
    scale_color_manual(name = "Cumulative Deaths", values = c("Reportable" = "#00441b", "Attributable" = "#74c476", "Observed" = "red"))
})
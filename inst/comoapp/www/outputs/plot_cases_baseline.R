output$plot_cases_baseline <- renderPlot({
  req(simul_baseline$baseline_available)
  
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
  
  dta %>%
    rename(Date = time) %>%
    filter(Date <= max_x) %>%
  ggplot(aes(x = Date)) +
    geom_ribbon(aes(ymin = daily_total_cases_min, ymax = daily_total_cases_max), fill = "#00441b", alpha = 0.7) +
    geom_line(aes(y = daily_total_cases_med, color = "Predicted Reported + Unreported"), lwd = 1.2) + 
    geom_ribbon(aes(ymin = daily_incidence_min, ymax = daily_incidence_max), fill = "#74c476", alpha = 0.7) +
    geom_line(aes(y = daily_incidence_med, color = "Predicted Reported"), lwd = 1.2) +
    geom_line(aes(y = cases, color = "Observed"), lwd = 1.2) +
    geom_point(aes(y = cases), color = "red") + 
    coord_cartesian(ylim = c(NA, max_y)) +
    labs(title = "Baseline Daily Cases", x= "", y = "") +
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
    theme_light(base_size = 14) +
    scale_color_manual(name = "Cases", values = c("Predicted Reported" = "#74c476", "Predicted Reported + Unreported" = "#00441b", "Observed" = "red"))
})

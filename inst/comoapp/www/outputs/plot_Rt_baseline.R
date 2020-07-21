output$plot_Rt_baseline <- renderPlot({
  req(simul_baseline$baseline_available)
  
  
  # end date is the date of the last data point if the focus is "Observed"
  # end date os the last day of the simulation otherwise
  end_date <- input$date_range[2]
  if(input$focus_axis == "Observed")  end_date <- cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))]
  
  dta <- tibble(
    Rt_min = simul_baseline$results$min$Rt,
    Rt_med = simul_baseline$results$med$Rt,
    Rt_max = simul_baseline$results$max$Rt,
    Date = simul_baseline$results$time,
    One = 1) %>%
    filter(Date <= end_date)
  

  ggplot(data = dta, aes(x = Date)) +
    geom_ribbon(aes(ymin = Rt_min, ymax = Rt_max), fill = "#00441b", alpha = 0.7) +
    geom_line(aes(y = Rt_med), color = "#00441b", lwd = 1.2) + 
    geom_line(aes(y = One), color = "red", lwd = 1.2) + 
    geom_label(label = "Rt = 1", x = simul_baseline$results$time[1], y = 1, color = "red", fill = "pink") + 
    ggtitle("Baseline Rt") + xlab("") + ylab("Rt") +
    theme_light(base_size = 14)
})
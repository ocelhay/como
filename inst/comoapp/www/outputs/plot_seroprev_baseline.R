output$plot_seroprev_baseline <- renderPlot({
  req(simul_baseline$baseline_available)
  
  if(input$focus_axis == "Observed")  max_x <- cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))] + 3
  if(input$focus_axis != "Observed")  max_x <- max(simul_baseline$results$time)
  
  plot <- simul_baseline$results$seroprevalence %>%
    filter(mday(Time) %in% c(1, 15)) %>%
    filter(Time <= max_x) %>%
    mutate(Ab = 100 * Ab) %>%
    ggplot() + 
    coord_cartesian(ylim = c(0, 100)) +
    geom_boxplot(aes(x = Time, y = Ab, group = Time)) +
    scale_x_date(date_labels =  "%b %Y") +
    labs(title = "Seroprevalence", 
         subtitle = glue("The plot shows the expected seroprevalence at predetermined times\nif we were to sample a user-defined number of individuals and perform an antibody test on them.\nThe prevalence levels shown are corrected for test sensitivity/specificity (resp. {input$se}/{input$sp}%).\nRed points (if any) are prevalence survey data provided in the template."), 
         x= "", y = "%") +
    theme_light(base_size = 14)
  
  
  if(! all(is.na(cases_rv$data$seroprevalence))) plot <- plot + geom_point(data = cases_rv$data, aes(x = date, y = seroprevalence), color = "red")
  
  return(plot)
})
  
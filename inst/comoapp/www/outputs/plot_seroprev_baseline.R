output$plot_seroprev_baseline <- renderPlot({
  req(simul_baseline$baseline_available)
  
  
  if(input$focus_axis == "Observed")  max_x <- cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))] + 3
  if(input$focus_axis != "Observed")  max_x <- max(simul_baseline$results$time)
    

  samp.sizes <- round(rnorm(length(simul_baseline$results$time),
                            input$sample_size,
                            input$sample_size / 5))
  
  ab <- data.frame(Time = rep(simul_baseline$results$time, 100), 
                   Ab = 0)
  
  aux <- NULL
  for (i in 1:100) {
    num.inf.samp <- rbinom(length(simul_baseline$results$time), size = samp.sizes, 
                           prob = (simul_baseline$results$med$ab_all_ages / simul_baseline$results$med$N))
    aux<-c(aux, num.inf.samp / samp.sizes)
  }
  
  se<-0.75
  sp<-0.97
  ab$Ab<-se*aux+(1-sp)*(1-aux)
  
  ab %>%
    filter(mday(Time) %in% c(1, 15)) %>%
    filter(Time <= max_x) %>%
    mutate(Ab = 100 * Ab) %>%
  ggplot(aes(x = Time, y = Ab, group = Time)) + 
    coord_cartesian(ylim = c(0, 100)) +
    geom_boxplot() +
    scale_x_date(date_labels =  "%b %Y") +
    labs(title = "Seroprevalence", 
         subtitle = "The plot shows the expected seroprevalence at predetermined times\nif we were to sample a user-defined number of individuals and perform an antibody test on them.\nThe prevalence levels shown are corrected for test sensitivity/specificity (resp. 75/97%).", 
         x= "", y = "%") +
    theme_light(base_size = 14)
})
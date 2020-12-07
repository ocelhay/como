output$plot_seroprev_baseline <- renderPlot({
  req(simul_baseline$baseline_available)
  
  
  if(input$focus_axis == "Observed")  max_x <- cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))] + 3
  if(input$focus_axis != "Observed")  max_x <- max(simul_baseline$results$time)
    

  samp.sizes <- round(rnorm(length(simul_baseline$results$time),
                            input$sample_size,
                            input$sample_size / 5))
  
  ab <- data.frame(Time = rep(simul_baseline$results$time, 1000), 
                   Ab = 0)
  
  aux <- NULL
  for (i in 1:1000) {
    num.inf.samp <- rbinom(length(simul_baseline$results$time), size = samp.sizes, 
                           prob = (simul_baseline$results$med$ab_all_ages / simul_baseline$results$med$N))
    aux<-c(aux, num.inf.samp / samp.sizes)
  }
  
  se<-0.5
  sp<-0.9
  ab$Ab<-se*aux+(1-sp)*(1-aux)
  
  ab %>%
    filter(wday(Time) == 1) %>%
    filter(Time <= max_x) %>%
    mutate(Ab = 100 * Ab) %>%
  ggplot(aes(x = Time, y = Ab, group = Time)) + 
    coord_cartesian(ylim = c(0, 100)) +
    geom_boxplot() +
    scale_x_date(date_labels =  "%b %Y") +
    labs(title = "Seroprevalence - PROVISIONAL PLOT", x= "", y = "%") +
    theme_light(base_size = 14)
})
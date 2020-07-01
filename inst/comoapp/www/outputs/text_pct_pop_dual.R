output$text_pct_pop_baseline <- renderText({
  req(simul_baseline$baseline_available)
  
  # end date is the date of the last data point if the focus is "Observed"
  # end date os the last day of the simulation otherwise
  end_date <- input$date_range[2]
  if(input$focus_axis == "Observed")  end_date <- cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))]
  
  paste0(
    as.character(
      div(class = "n_box_baseline",
          div(class = "icon_box", icon("user")),
          conf_interval(simul_baseline$results$min$pct_total_pop_infected[which(simul_baseline$results$min$time == end_date)],
                        simul_baseline$results$med$pct_total_pop_infected[which(simul_baseline$results$med$time == end_date)],
                        simul_baseline$results$max$pct_total_pop_infected[which(simul_baseline$results$max$time == end_date)],
                        unit = "%"),
          p("of the population infected from ", input$date_range[1], " to ", strong(end_date))
      )))
})

output$text_pct_pop_baseline_dup <- renderText({
  req(simul_baseline$baseline_available)
  req(simul_interventions$interventions_available)
  
  # end date is the date of the last data point if the focus is "Observed"
  # end date os the last day of the simulation otherwise
  end_date <- input$date_range[2]
  if(input$focus_axis_dup == "Observed")  end_date <- cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))]
  
  paste0(
    as.character(
      div(class = "n_box_baseline",
          div(class = "icon_box", icon("user")),
          conf_interval(simul_baseline$results$min$pct_total_pop_infected[which(simul_baseline$results$min$time == end_date)],
                        simul_baseline$results$med$pct_total_pop_infected[which(simul_baseline$results$med$time == end_date)],
                        simul_baseline$results$max$pct_total_pop_infected[which(simul_baseline$results$max$time == end_date)],
                        unit = "%"),
          p("of the population infected from ", input$date_range[1], " to ", strong(end_date))
      )))
})


output$text_pct_pop_interventions <- renderText({
  req(simul_interventions$interventions_available)
  
  # end date is the date of the last data point if the focus is "Observed"
  # end date os the last day of the simulation otherwise
  end_date <- input$date_range[2]
  if(input$focus_axis == "Observed")  end_date <- cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))]
  
  n <- simul_interventions$results$med$pct_total_pop_infected[which(simul_interventions$results$min$time == end_date)]
  reduction <- round((n - simul_baseline$results$med$pct_total_pop_infected[which(simul_baseline$results$min$time == end_date)]), 1)
  
  paste0(
    as.character(
      div(class = "n_box_interventions",
          div(class = "icon_box", h3(paste0("(", reduction, "%)"))),
          conf_interval(simul_interventions$results$min$pct_total_pop_infected[which(simul_interventions$results$min$time == end_date)],
                        simul_interventions$results$med$pct_total_pop_infected[which(simul_interventions$results$med$time == end_date)],
                        simul_interventions$results$max$pct_total_pop_infected[which(simul_interventions$results$max$time == end_date)],
                        unit = "%"),
          p("of the population infected from ", input$date_range[1], " to ", strong(end_date))
      )))
})
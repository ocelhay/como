output$text_attributable_death_baseline <- renderText({
  req(simul_baseline$baseline_available)
  
  # end date is the date of the last data point if the focus is "Observed"
  # end date os the last day of the simulation otherwise
  end_date <- input$date_range[2]
  if(input$focus_axis == "Observed")  end_date <- cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))]
  
  paste0(
    as.character(
      div(class = "n_box_baseline",
          conf_interval_deaths(
            simul_baseline$results$min$attributable_deaths[which(simul_baseline$results$min$time == end_date)],
            simul_baseline$results$med$attributable_deaths[which(simul_baseline$results$med$time == end_date)], 
            simul_baseline$results$max$attributable_deaths[which(simul_baseline$results$max$time == end_date)]
            ),
          p("Covid-19",  strong("attributable"), "deaths from ", input$date_range[1], " to ", strong(end_date))
      )))
})

output$text_attributable_death_baseline_dup <- renderText({
  req(simul_baseline$baseline_available)
  req(simul_interventions$interventions_available)
  
  # end date is the date of the last data point if the focus is "Observed"
  # end date os the last day of the simulation otherwise
  end_date <- input$date_range[2]
  if(input$focus_axis_dup == "Observed")  end_date <- cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))]
  
  paste0(
    as.character(
      div(class = "n_box_baseline",
          conf_interval_deaths(
            simul_baseline$results$min$attributable_deaths[which(simul_baseline$results$min$time == end_date)],
            simul_baseline$results$med$attributable_deaths[which(simul_baseline$results$med$time == end_date)], 
            simul_baseline$results$max$attributable_deaths[which(simul_baseline$results$max$time == end_date)]
          ),
          p("Covid-19",  strong("attributable"), "deaths from ", input$date_range[1], " to ", strong(end_date))
      )))
})

output$text_reported_death_baseline <- renderText({
  req(simul_baseline$baseline_available)
  
  # end date is the date of the last data point if the focus is "Observed"
  # end date os the last day of the simulation otherwise
  end_date <- input$date_range[2]
  if(input$focus_axis == "Observed")  end_date <- cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))]
  
  paste0(
    as.character(
      div(class = "n_box_baseline",
          conf_interval_deaths(
            simul_baseline$results$min$reportable_death[which(simul_baseline$results$min$time == end_date)],
            simul_baseline$results$med$reportable_death[which(simul_baseline$results$med$time == end_date)], 
            simul_baseline$results$max$reportable_death[which(simul_baseline$results$max$time == end_date)]
          ),
          p("Covid-19",  strong("reportable"), "deaths from ", input$date_range[1], " to ", strong(end_date))
      )))
})

output$text_reported_death_baseline_dup <- renderText({
  req(simul_baseline$baseline_available)
  req(simul_interventions$interventions_available)
  
  # end date is the date of the last data point if the focus is "Observed"
  # end date os the last day of the simulation otherwise
  end_date <- input$date_range[2]
  if(input$focus_axis_dup == "Observed")  end_date <- cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))]
  
  paste0(
    as.character(
      div(class = "n_box_baseline",
          conf_interval_deaths(
            simul_baseline$results$min$reportable_death[which(simul_baseline$results$min$time == end_date)],
            simul_baseline$results$med$reportable_death[which(simul_baseline$results$med$time == end_date)], 
            simul_baseline$results$max$reportable_death[which(simul_baseline$results$max$time == end_date)]
          ),
          p("Covid-19",  strong("reportable"), "deaths from ", input$date_range[1], " to ", strong(end_date))
      )))
})


output$text_attributable_death_interventions <- renderText({
  req(simul_interventions$interventions_available)
  
  # end date is the date of the last data point if the focus is "Observed"
  # end date os the last day of the simulation otherwise
  end_date <- input$date_range[2]
  if(input$focus_axis_dup == "Observed")  end_date <- cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))]
  
  reduction <- (simul_interventions$results$med$attributable_deaths[which(simul_interventions$results$med$time == end_date)] - 
                  simul_baseline$results$med$attributable_deaths[which(simul_baseline$results$med$time == end_date)])
  
  paste0(
    as.character(
      div(class = "n_box_interventions",
          div(class = "icon_box", h3(paste0("(", format(reduction, big.mark = ","), ")"))),
          conf_interval_deaths(
            simul_interventions$results$min$attributable_deaths[which(simul_interventions$results$min$time == end_date)],
            simul_interventions$results$med$attributable_deaths[which(simul_interventions$results$med$time == end_date)], 
            simul_interventions$results$max$attributable_deaths[which(simul_interventions$results$max$time == end_date)]
          ),
          p("Covid-19",  strong("attributable"), "deaths from ", input$date_range[1], " to ", strong(end_date))
      )))
})

output$text_reported_death_interventions <- renderText({
  req(simul_interventions$interventions_available)
  
  # end date is the date of the last data point if the focus is "Observed"
  # end date os the last day of the simulation otherwise
  end_date <- input$date_range[2]
  if(input$focus_axis_dup == "Observed")  end_date <- cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))]
  
  reduction <- (simul_interventions$results$med$reportable_death[which(simul_interventions$results$med$time == end_date)] - 
                  simul_baseline$results$med$reportable_death[which(simul_baseline$results$med$time == end_date)])
  
  paste0(
    as.character(
      div(class = "n_box_interventions",
          div(class = "icon_box", h3(paste0("(", format(reduction, big.mark = ","), ")"))),
          conf_interval_deaths(
            simul_interventions$results$min$reportable_death[which(simul_interventions$results$min$time == end_date)],
            simul_interventions$results$med$reportable_death[which(simul_interventions$results$med$time == end_date)], 
            simul_interventions$results$max$reportable_death[which(simul_interventions$results$max$time == end_date)]
          ),
          p("Covid-19",  strong("reportable"), "deaths from ", input$date_range[1], " to ", strong(end_date))
      )))
})
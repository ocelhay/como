# Baseline

output$text_pct_total_baseline <- renderText({
  req(simul_baseline$baseline_available)
  
  end_date <- input$date_range[2]
  if(input$focus_axis == "Observed") end_date <- min(cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))], input$date_range[2])
  
  paste0(
    as.character(
      div(class = "n_box_baseline_total",
          div(class = "icon_box", icon("user")),
          conf_interval(simul_baseline$results$min$pct_total_pop_infected[which(simul_baseline$results$min$time == end_date)],
                        simul_baseline$results$med$pct_total_pop_infected[which(simul_baseline$results$med$time == end_date)],
                        simul_baseline$results$max$pct_total_pop_infected[which(simul_baseline$results$max$time == end_date)],
                        unit = "%"),
          p("of the population infected from ", input$date_range[1], " to ", strong(end_date))
      )))
})

output$text_pct_total_baseline_dup <- renderText({
  req(simul_baseline$baseline_available)
  
  end_date <- input$date_range[2]
  if(input$focus_axis_dup == "Observed") end_date <- min(cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))], input$date_range[2])
  
  paste0(
    as.character(
      div(class = "n_box_baseline_total",
          div(class = "icon_box", icon("user")),
          conf_interval(simul_baseline$results$min$pct_total_pop_infected[which(simul_baseline$results$min$time == end_date)],
                        simul_baseline$results$med$pct_total_pop_infected[which(simul_baseline$results$med$time == end_date)],
                        simul_baseline$results$max$pct_total_pop_infected[which(simul_baseline$results$max$time == end_date)],
                        unit = "%"),
          p("of the population infected from ", input$date_range[1], " to ", strong(end_date))
      )))
})

output$text_death_total_baseline <- renderText({
  req(simul_baseline$baseline_available)
  
  end_date <- input$date_range[2]
  if(input$focus_axis == "Observed") end_date <- min(cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))], input$date_range[2])
  
  paste0(
    as.character(
      div(class = "n_box_baseline_total",
          conf_interval_deaths(
            simul_baseline$results$min$total_deaths[which(simul_baseline$results$min$time == end_date)],
            simul_baseline$results$med$total_deaths[which(simul_baseline$results$med$time == end_date)], 
            simul_baseline$results$max$total_deaths[which(simul_baseline$results$max$time == end_date)]
            ),
          p("Covid-19 deaths from ", input$date_range[1], " to ", strong(end_date))
      )))
})

output$text_death_total_baseline_dup <- renderText({
  req(simul_baseline$baseline_available)
  
  end_date <- input$date_range[2]
  if(input$focus_axis_dup == "Observed") end_date <- min(cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))], input$date_range[2])
  
  paste0(
    as.character(
      div(class = "n_box_baseline_total",
          conf_interval_deaths(
            simul_baseline$results$min$total_deaths[which(simul_baseline$results$min$time == end_date)],
            simul_baseline$results$med$total_deaths[which(simul_baseline$results$med$time == end_date)], 
            simul_baseline$results$max$total_deaths[which(simul_baseline$results$max$time == end_date)]
          ),
          p("Covid-19 deaths from ", input$date_range[1], " to ", strong(end_date))
      )))
})

output$text_pct_reported_baseline <- renderText({
  req(simul_baseline$baseline_available)
  end_date <- input$date_range[2]
  if(input$focus_axis == "Observed") end_date <- min(cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))], input$date_range[2])
  
  paste0(
    as.character(
      div(class = "n_box_baseline_reported",
          div(class = "icon_box", icon("user")),
          conf_interval(simul_baseline$results$min$pct_reported_pop_infected[which(simul_baseline$results$min$time == end_date)],
                        simul_baseline$results$med$pct_reported_pop_infected[which(simul_baseline$results$med$time == end_date)],
                        simul_baseline$results$max$pct_reported_pop_infected[which(simul_baseline$results$max$time == end_date)],
                        unit = "%"),
          p("of the population infected from ", input$date_range[1], " to ", strong(end_date))
      )))
})

output$text_pct_reported_baseline_dup <- renderText({
  req(simul_baseline$baseline_available)
  
  end_date <- input$date_range[2]
  if(input$focus_axis_dup == "Observed") end_date <- min(cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))], input$date_range[2])
  
  paste0(
    as.character(
      div(class = "n_box_baseline_reported",
          div(class = "icon_box", icon("user")),
          conf_interval(simul_baseline$results$min$pct_reported_pop_infected[which(simul_baseline$results$min$time == end_date)],
                        simul_baseline$results$med$pct_reported_pop_infected[which(simul_baseline$results$med$time == end_date)],
                        simul_baseline$results$max$pct_reported_pop_infected[which(simul_baseline$results$max$time == end_date)],
                        unit = "%"),
          p("of the population infected from ", input$date_range[1], " to ", strong(end_date))
      )))
})

# output$text_death_reported_baseline <- renderText({
#   req(simul_baseline$baseline_available)
#   
#   end_date <- input$date_range[2]
#   end_date <- if(input$focus_axis == "Observed") end_date <- min(cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))], input$date_range[2])
#   
#   paste0(
#     as.character(
#       div(class = "n_box_baseline_reported",
#           conf_interval_deaths(
#             simul_baseline$results$min$attributable_deaths[which(simul_baseline$results$min$time == end_date)],
#             simul_baseline$results$med$attributable_deaths[which(simul_baseline$results$med$time == end_date)], 
#             simul_baseline$results$max$attributable_deaths[which(simul_baseline$results$max$time == end_date)]
#           ),
#           p("Covid-19 deaths from ", input$date_range[1], " to ", strong(end_date))
#       )))
# })

# output$text_death_reported_baseline_dup <- renderText({
#   req(simul_baseline$baseline_available)
#   
#   end_date <- input$date_range[2]
#   end_date <- if(input$focus_axis == "Observed") end_date <- min(cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))], input$date_range[2])
#   
#   paste0(
#     as.character(
#       div(class = "n_box_baseline_reported",
#           conf_interval_deaths(
#             simul_baseline$results$min$attributable_deaths[which(simul_baseline$results$min$time == end_date)],
#             simul_baseline$results$med$attributable_deaths[which(simul_baseline$results$med$time == end_date)], 
#             simul_baseline$results$max$attributable_deaths[which(simul_baseline$results$max$time == end_date)]
#           ),
#           p("Covid-19 deaths from ", input$date_range[1], " to ", strong(end_date))
#       )))
# })


# Future ----

output$text_death_total_future <- renderText({
  req(simul_interventions$interventions_available)

  end_date <- input$date_range[2]
  if(input$focus_axis_dup == "Observed")  end_date <- min(cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))], input$date_range[2])

  reduction <- (simul_interventions$results$med$total_deaths[which(simul_interventions$results$med$time == end_date)] -
                  simul_baseline$results$med$total_deaths[which(simul_baseline$results$med$time == end_date)])

  paste0(
    as.character(
      div(class = "n_box_interventions_total",
          div(class = "icon_box", h3(paste0("(", format(reduction, big.mark = ","), ")"))),
          conf_interval_deaths(
            simul_interventions$results$min$total_deaths[which(simul_interventions$results$min$time == end_date)],
            simul_interventions$results$med$total_deaths[which(simul_interventions$results$med$time == end_date)],
            simul_interventions$results$max$total_deaths[which(simul_interventions$results$max$time == end_date)]
          ),
          p("Covid-19 deaths from ", input$date_range[1], " to ", strong(end_date))
      )))
})

# output$text_death_reported_future <- renderText({
#   req(simul_interventions$interventions_available)
#   
#   end_date <- input$date_range[2]
#   if(input$focus_axis_dup == "Observed")  end_date <- min(cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))], input$date_range[2])
#   
#   reduction <- (simul_interventions$results$med$attributable_deaths[which(simul_interventions$results$med$time == end_date)] -
#                   simul_baseline$results$med$attributable_deaths[which(simul_baseline$results$med$time == end_date)])
#   
#   paste0(
#     as.character(
#       div(class = "n_box_interventions_reported",
#           div(class = "icon_box", h3(paste0("(", format(reduction, big.mark = ","), ")"))),
#           conf_interval_deaths(
#             simul_interventions$results$min$attributable_deaths[which(simul_interventions$results$min$time == end_date)],
#             simul_interventions$results$med$attributable_deaths[which(simul_interventions$results$med$time == end_date)],
#             simul_interventions$results$max$attributable_deaths[which(simul_interventions$results$max$time == end_date)]
#           ),
#           p("Covid-19 deaths from ", input$date_range[1], " to ", strong(end_date))
#       )))
# })

output$text_pct_total_future <- renderText({
  req(simul_interventions$interventions_available)

  end_date <- input$date_range[2]
  if(input$focus_axis_dup == "Observed")  end_date <- min(cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))], input$date_range[2])

  n <- simul_interventions$results$med$pct_total_pop_infected[which(simul_interventions$results$min$time == end_date)]
  reduction <- round((n - simul_baseline$results$med$pct_total_pop_infected[which(simul_baseline$results$min$time == end_date)]), 1)

  paste0(
    as.character(
      div(class = "n_box_interventions_total",
          div(class = "icon_box", h3(paste0("(", reduction, "%)"))),
          conf_interval(simul_interventions$results$min$pct_total_pop_infected[which(simul_interventions$results$min$time == end_date)],
                        simul_interventions$results$med$pct_total_pop_infected[which(simul_interventions$results$med$time == end_date)],
                        simul_interventions$results$max$pct_total_pop_infected[which(simul_interventions$results$max$time == end_date)],
                        unit = "%"),
          p("of the population infected from ", input$date_range[1], " to ", strong(end_date))
      )))
})

output$text_pct_reported_future <- renderText({
  req(simul_interventions$interventions_available)
  
  end_date <- input$date_range[2]
  if(input$focus_axis_dup == "Observed")  end_date <- min(cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))], input$date_range[2])
  
  n <- simul_interventions$results$med$pct_reported_pop_infected[which(simul_interventions$results$min$time == end_date)]
  reduction <- round((n - simul_baseline$results$med$pct_reported_pop_infected[which(simul_baseline$results$min$time == end_date)]), 1)
  
  paste0(
    as.character(
      div(class = "n_box_interventions_reported",
          div(class = "icon_box", h3(paste0("(", reduction, "%)"))),
          conf_interval(simul_interventions$results$min$pct_reported_pop_infected[which(simul_interventions$results$min$time == end_date)],
                        simul_interventions$results$med$pct_reported_pop_infected[which(simul_interventions$results$med$time == end_date)],
                        simul_interventions$results$max$pct_reported_pop_infected[which(simul_interventions$results$max$time == end_date)],
                        unit = "%"),
          p("of the population infected from ", input$date_range[1], " to ", strong(end_date))
      )))
})
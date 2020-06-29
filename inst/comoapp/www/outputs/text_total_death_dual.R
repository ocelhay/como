output$text_attributable_death_baseline <- renderText({
  req(simul_baseline$baseline_available)
  
  paste0(
    as.character(
      div(class = "n_box_baseline",
          conf_interval_deaths(simul_baseline$results$min$attributable_deaths_end, 
                               simul_baseline$results$med$attributable_deaths_end, 
                               simul_baseline$results$max$attributable_deaths_end),
          p("Covid-19",  strong("attributable"), "deaths during the range of simulation.")
      )))
  
})

output$text_attributable_death_baseline_dup <- renderText({
  req(simul_baseline$baseline_available)
  
  paste0(
    as.character(
      div(class = "n_box_baseline",
          conf_interval_deaths(simul_baseline$results$min$attributable_deaths_end, 
                               simul_baseline$results$med$attributable_deaths_end, 
                               simul_baseline$results$max$attributable_deaths_end),
          p("Covid-19",  strong("attributable"), "deaths during the range of simulation.")
      )))
})

output$text_reported_death_baseline <- renderText({
  req(simul_baseline$baseline_available)

  paste0(
    as.character(
      div(class = "n_box_baseline",
          conf_interval_deaths(simul_baseline$results$min$total_reported_deaths_end, 
                               simul_baseline$results$med$total_reported_deaths_end, 
                               simul_baseline$results$max$total_reported_deaths_end),
          p("Covid-19",  strong("reported"), "deaths during the range of simulation.")
      )))
})

output$text_reported_death_baseline_dup <- renderText({
  req(simul_baseline$baseline_available)
  
  paste0(
    as.character(
      div(class = "n_box_baseline",
          conf_interval_deaths(simul_baseline$results$min$total_reported_deaths_end, 
                               simul_baseline$results$med$total_reported_deaths_end, 
                               simul_baseline$results$max$total_reported_deaths_end),
          p("Covid-19",  strong("reported"), "deaths during the range of simulation.")
      )))
})


output$text_attributable_death_interventions <- renderText({
  req(simul_interventions$interventions_available)
  
  reduction <- (simul_interventions$results$med$attributable_deaths_end - 
    simul_baseline$results$med$attributable_deaths_end)
  
  paste0(
    as.character(
      div(class = "n_box_interventions",
          div(class = "icon_box", h3(paste0("(", format(reduction, big.mark = ","), ")"))),
          conf_interval_deaths(simul_interventions$results$min$attributable_deaths_end, 
                               simul_interventions$results$med$attributable_deaths_end, 
                               simul_interventions$results$max$attributable_deaths_end),
          p("Covid-19",  strong("attributable"), "deaths during the range of simulation.")
      )))
})

output$text_reported_death_interventions <- renderText({
  req(simul_interventions$interventions_available)
  
  reduction <- (simul_interventions$results$med$total_reported_deaths_end - 
                  simul_baseline$results$med$total_reported_deaths_end)
  
  paste0(
    as.character(
      div(class = "n_box_interventions",
          div(class = "icon_box", h3(paste0("(", format(reduction, big.mark = ","), ")"))),
          conf_interval_deaths(simul_interventions$results$min$total_reported_deaths_end, 
                               simul_interventions$results$med$total_reported_deaths_end, 
                               simul_interventions$results$max$total_reported_deaths_end),
          p("Covid-19",  strong("reported"), "deaths during the range of simulation.")
      )))
})
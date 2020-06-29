output$text_pct_pop_baseline <- renderText({
  req(simul_baseline$baseline_available)
  
  paste0(
    as.character(
      div(class = "n_box_baseline",
          div(class = "icon_box", icon("user")),
          conf_interval(simul_baseline$results$min$pct_total_pop_infected, 
                        simul_baseline$results$med$pct_total_pop_infected,
                        simul_baseline$results$max$pct_total_pop_infected,
                        unit = "%"),
          p("of the population infected during the range of simulation.")
      )))
})

output$text_pct_pop_baseline_dup <- renderText({
  req(simul_baseline$baseline_available)
  paste0(
    as.character(
      div(class = "n_box_baseline",
          div(class = "icon_box", icon("user")),
          conf_interval(simul_baseline$results$min$pct_total_pop_infected, 
                        simul_baseline$results$med$pct_total_pop_infected,
                        simul_baseline$results$max$pct_total_pop_infected,
                        unit = "%"),
          p("of the population infected during the range of simulation.")
      )))
})


output$text_pct_pop_interventions <- renderText({
  req(simul_interventions$interventions_available)
  
  n <- simul_interventions$results$med$pct_total_pop_infected
  reduction <- round((n - simul_baseline$results$med$pct_total_pop_infected), 1)
  
  paste0(
    as.character(
      div(class = "n_box_interventions",
          div(class = "icon_box", h3(paste0("(", reduction, "%)"))),
          conf_interval(simul_interventions$results$min$pct_total_pop_infected, 
                        simul_interventions$results$med$pct_total_pop_infected,
                        simul_interventions$results$max$pct_total_pop_infected,
                        unit = "%"),
          p("of the population infected during the range of simulation.")
      )))
})
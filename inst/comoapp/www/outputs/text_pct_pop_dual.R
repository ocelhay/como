output$text_pct_pop_baseline <- renderText({
  req(simul_baseline$baseline_available)
  
  n <- simul_baseline$results$pct_total_pop_infected
  
  return(
    paste0(
      as.character(
        div(class = "n_box_baseline",
            div(class = "icon_box", icon("user")),
            h3(paste0(n, "%")),
            p("of the population infected during the range of simulation.")
        ))))
})

output$text_pct_pop_interventions <- renderText({
  req(simul_interventions$interventions_available)
  
  n <- simul_interventions$results$pct_total_pop_infected
  reduction <- round((n - simul_baseline$results$pct_total_pop_infected), 1)
  
  return(
    paste0(
      as.character(
        div(class = "n_box_interventions",
            div(class = "icon_box", h3(paste0("(", reduction, "%)"))),
            h3(paste0(n, "%")),
            p("of the population infected during the range of simulation.")
        ))))
})
output$text_interventions_pct_pop <- renderText({
  req(simul_interventions$interventions_available)
  
  n <- round(last(simul_interventions$results$pct_total_pop_infected), 1)
  reduction <- round(last(simul_interventions$results$pct_total_pop_infected - simul_baseline$results$pct_total_pop_infected), 1)
  return(
    paste0(
      as.character(
        div(class = "n_box_interventions",
            div(class = "icon_box", h3(paste0("(", reduction, "%)"))),
            h3(paste0(n, "%")),
            p("of the population will be infected during the epidemic. (Interventions)")
        ))))
})
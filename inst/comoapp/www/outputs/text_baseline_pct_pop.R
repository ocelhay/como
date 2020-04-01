output$text_baseline_pct_pop <- renderText({
  req(simul_baseline$baseline_available)
  
  n <- round(last(simul_baseline$results$pct_total_pop_infected), 1)
  return(
    paste0(
      as.character(
        div(class = "n_box_baseline",
            div(class = "icon_box", icon("user")),
            h3(paste0(n, "%")),
            p("of the population will be infected during the epidemic. (Baseline)")
        ))))
})

output$text_baseline_total_death <- renderText({
  req(simul_baseline$baseline_available)
  
  n <- round(last(simul_baseline$results$cmortality0))
  return(
    paste0(
      as.character(
        div(class = "n_box_baseline",
            h3(paste0(format(n, big.mark = ","))),
            h4("Deaths"),
            p("during the epidemic. (Baseline)")
        ))))
})
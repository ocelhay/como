output$text_interventions_total_death <- renderText({
  req(simul_interventions$interventions_available)
  
  n <- simul_interventions$results$total_deaths
  reduction <- n - simul_baseline$results$total_deaths
  
  return(
    paste0(
      as.character(
        div(class = "n_box_interventions",
            div(class = "icon_box", h3(paste0("(", format(reduction, big.mark = ","), ")"))),
            h3(paste0(format(n, big.mark = ","))),
            span(h4("Deaths"), "during the epidemic.")
        ))))
})
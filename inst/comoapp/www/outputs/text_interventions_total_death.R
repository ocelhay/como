output$text_interventions_total_death <- renderText({
  req(simul_interventions$interventions_available)
  
  n <- round(last(simul_interventions$results$cmortality0))
  reduction <- round(last(simul_interventions$results$cmortality0) - last(simul_baseline$results$cmortality0))
  return(
    paste0(
      as.character(
        div(class = "n_box_interventions",
            div(class = "icon_box", h3(paste0("(", format(reduction, big.mark = ","), ")"))),
            h3(paste0(format(n, big.mark = ","))),
            span(h4("Deaths"), "during the epidemic. (Interventions)")
        ))))
})
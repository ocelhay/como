output$text_nb_interventions_baseline <- renderText({
  req(simul_baseline$baseline_available)

  n <- interventions$baseline_nb

  return(paste0(n, " interventions for Baseline + Future Scenario (max. 30)."))
})

output$text_nb_interventions_future <- renderText({
  req(simul_baseline$baseline_available)

  n <- interventions$future_nb

  return(paste0(n, " interventions for Future Scenario (max. 30)."))
})
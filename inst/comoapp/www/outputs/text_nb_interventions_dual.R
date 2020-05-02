output$text_nb_interventions_baseline <- renderText({
  return(paste0(interventions$baseline_nb, " interventions for Baseline + Future Scenario (max. 30)."))
})

output$text_nb_interventions_future <- renderText({
  return(paste0(interventions$future_nb, " interventions for Future Scenario (max. 30)."))
})
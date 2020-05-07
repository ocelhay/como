output$text_nb_interventions_baseline <- renderText({
  return(paste0(interventions$baseline_nb, " interventions for Baseline (Calibration) (max. 30). ",
                span(class = "redbold", interventions$message_baseline_interventions)))
  
})

output$text_nb_interventions_future <- renderText({
  return(paste0(interventions$future_nb, " interventions for Hypothetical Scenario (max. 30). ",
                span(class = "redbold", interventions$message_all_interventions)))
})
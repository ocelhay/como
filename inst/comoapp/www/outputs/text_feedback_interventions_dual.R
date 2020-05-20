output$text_feedback_interventions_baseline <- renderText({
  return(paste0("", span(class = "redbold", interventions$message_baseline_interventions)))
  
})

output$text_feedback_interventions_future <- renderText({
  return(paste0("", span(class = "redbold", interventions$message_future_interventions)))
})
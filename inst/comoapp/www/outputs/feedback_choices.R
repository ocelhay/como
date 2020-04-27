output$feedback_choices <- renderText({
  return(
    paste0(
      strong("Selected Inputs:"),
      span("Cases/Deaths:", span(class = "importanttext", input$country_cases),
      ", demographics: ", span(class = "importanttext", input$country_demographic), 
      ", social contacts: ", span(class = "importanttext", input$country_contact))
      )
    )
})
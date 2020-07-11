output$feedback_choices <- renderText({
  return(
    paste0(
      strong("Selected Inputs:"),
      span("Cases/Deaths:", span(class = "redbold", input$country_cases),
      ", demographics: ", span(class = "redbold", input$country_demographic), 
      ", social contacts: ", span(class = "redbold", input$country_contact))
      )
    )
})
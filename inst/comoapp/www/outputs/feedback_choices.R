output$feedback_choices <- renderText({
  return(
    paste0(
      span("Selected Country Inputs:"), br(),
      span("Cases/Deaths:", span(class = "redbold", input$country_cases), br(),
      "Demographics: ", span(class = "redbold", input$country_demographic), br(), 
      "Social contacts: ", span(class = "redbold", input$country_contact))
      )
    )
})
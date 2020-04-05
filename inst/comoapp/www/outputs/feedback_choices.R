output$feedback_choices <- renderText({
  return(
    paste0(
      strong("Selected Inputs:"),
      tags$ul(
        tags$li("Cases/Deaths:", span(class = "importanttext", input$country_cases)),
        tags$li("Demographics: ", span(class = "importanttext", input$country)),
        tags$li("Social Contacts: ", span(class = "importanttext", input$country_contact))
      )
    )
  )
})
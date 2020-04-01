output$feedback_choices <- renderText({
  return(
    paste0(
      strong("Selected Inputs:"),
      tags$ul(
        tags$li("Cases/Deaths: ", input$country_cases),
        tags$li("Demographics: ", input$country),
        tags$li("Social Contacts: ", input$country_contact),
        tags$li("Hospital Beds:", input$country_beds)
      )
    )
  )
})
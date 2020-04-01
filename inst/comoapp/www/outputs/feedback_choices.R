output$feedback_choices <- renderText({
  return(
    paste0(
      tags$ul(
        tags$li(input$country_cases, " selected for epidemiology data"),
        tags$li(input$country, " selected for population data."),
        tags$li(input$country_contact, " selected for contact data."),
        tags$li(input$country_beds, " selected for hospital bed data."),
      )
    )
  )
})
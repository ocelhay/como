output$feedback_choices <- renderText({
  return(
    paste0(
      strong(span("Selected in [", icon('cog'), " Country]:")),
      tags$ul(tags$li("Cases/Deaths:", span(class = "redbold", input$country_cases)),
              tags$li("Demographics: ", span(class = "redbold", input$country_demographic)), 
              tags$li("Social contacts: ", span(class = "redbold", input$country_contact)))
    )
  )
})
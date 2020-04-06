output$feedback_process <- renderText({
  return(
    paste0(
      if(input$tabs == "tab_welcome") span(strong("Welcome!"), p("Please familiarise yourself with the model and sources of data.")),
      if(status_app$status == "No Baseline" & input$tabs == "tab_visualfit") p("Adjust all parameters, possibly providing your own data, and 'Run Baseline'."),
      if(status_app$status == "Ok Baseline" & input$tabs == "tab_visualfit") span(strong("Perform Visual Fit:"), p("Adjust ", em("Probability of infection given contact, Percentage of all infections "), "that are reported and the starting date in ", em("Range of dates "), "and re-run the intervention (with current and past interventions selected) until observed cases/deaths loosely fit the predicted reported cases/deaths.")),
      if(status_app$status == "Validated Baseline" & input$tabs == "tab_modelpredictions") span(strong("Build a Package of Interventions:"), p("Select/Scale-up or down interventions to simulate, adjust parameters as desired, and 'Run Interventions'")),
      if(status_app$status == "Locked Baseline" & input$tabs == "tab_modelpredictions") p("Re-run interventions or download results"),
      if(status_app$status == "Locked Baseline" & input$tabs == "tab_visualfit") p("Since interventions are already built, to change parameters here, you will need to reset the baseline and start again."),
      if(status_app$status == "Validated Baseline" & input$tabs == "tab_visualfit") p("Since interventions are already built, to change parameters here, you will need to reset the baseline and start again.")
      )
  )
})
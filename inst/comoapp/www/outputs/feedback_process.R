output$feedback_process <- renderText({
  return(
    paste0(
      if(input$tabs == "tab_welcome") span(strong("Welcome!"), p("Please familiarise yourself with the model and sources of data.")),
      if(simul_baseline$baseline_available == FALSE & input$tabs == "tab_visualfit") p("Adjust all parameters, possibly providing your own data, and 'Run Baseline'."),
      if(simul_baseline$baseline_available == TRUE & 
         simul_interventions$interventions_available == FALSE & input$tabs == "tab_visualfit") span(strong("Perform Visual Fit:"), p("Adjust ", em("Probability of infection given contact, Percentage of all infections "), "that are reported and the starting date in ", em("Range of dates "), "and re-run the intervention (with current and past interventions selected) until observed cases/deaths loosely fit the predicted reported cases/deaths.")),
      if(simul_baseline$baseline_available == TRUE & 
         simul_interventions$interventions_available == FALSE & input$tabs == "tab_modelpredictions") span(strong("Build a Package of Interventions:"), p("Select interventions to simulate, adjust parameters as desired, and 'Run Interventions'")),
      if(simul_interventions$interventions_available == TRUE & input$tabs == "tab_modelpredictions") p("You can re-run interventions."),
      if(simul_interventions$interventions_available == TRUE & input$tabs == "tab_visualfit") p("Since interventions are already built, to change parameters here, you will need to reset the baseline and start again.")
      )
  )
})
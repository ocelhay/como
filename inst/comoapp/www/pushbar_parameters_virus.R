list(
  pushbar(id = "pushbar_parameters_virus", from = "right", class = "pushbarcomp",
          br(), br(), br(),
          fluidRow(
            column(4,
                   sliderInput("rho", label = "Relative infectiousness of incubation phase:", post = "%", ticks = FALSE,
                               value = 12.5, min = 0, max = 100, step = 0.5),
                   sliderInput("gamma", label = "Average incubation period:", post = " days", ticks = FALSE,
                               value = 3.5, min = 1, max = 7, step = 0.5),
                   sliderInput("nui", label = "Average duration of symptomatic infection period:", post = " days", ticks = FALSE,
                               value = 4.5, min = 1, max = 7, step = 0.5),
                   div(id = "margin_month_slider",
                       sliderTextInput("phi", label = "Month of peak infectivity of the virus:", 
                                       choices = month.name, selected = month.name[1]),
                       p("Recommendation: select the month of peak humidity")
                   )
            ),
            column(4,  
                   sliderInput("amp", label = "Annual variation in infectivity of the virus:", post = "%", ticks = FALSE,
                               value = 0, min = 0, max = 100, step = 1),
                   sliderInput("omega", label = "Average duration of immunity:", post = " years", ticks = FALSE,
                               value = 150, min = 0.5, max = 200, step = 0.5)
                   
            ),
            column(4,  
                   sliderInput("pclin", label = "Probability upon infection of developing clinical symptoms:", post = "%", ticks = FALSE,
                               value = 55, min = 0, max = 100, step = 1),
                   sliderInput("prob_icu", label = "Probability upon hospitalisation of requiring ICU admission:", post = "%", ticks = FALSE,
                               value = 50, min = 0, max = 100, step = 1),
                   sliderInput("prob_vent", label = "Probability upon admission to the ICU of requiring a ventilator:", post = "%", ticks = FALSE,
                               value = 75, min = 0, max = 100, step = 1)
            )
          ),
          div(class = "closebutton", bsButton("close_virus_param", "Close", icon("times"), style = "danger", size = "small"))
  )
)
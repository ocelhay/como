list(
  pushbar(id = "pushbar_parameters_virus", from = "right",
          br(), br(), br(),
          fluidRow(
            column(4,
                   sliderInput("rho", label = "Relative infectiousness of incubation phase:", post = "%", ticks = FALSE,
                               value = 50, min = 0, max = 100, step = 0.5),
                   sliderInput("gamma", label = "Average incubation period:", post = " days", ticks = FALSE,
                               value = 3.5, min = 1, max = 7, step = 0.5),
                   sliderInput("nui", label = "Average duration of symptomatic infection period:", post = " days", ticks = FALSE,
                               value = 4.5, min = 1, max = 7, step = 0.5),
                   div(id = "margin_month_slider",
                       sliderTextInput("phi", label = "Month of peak infectivity of the virus:", 
                                       choices = month.name, selected = month.name[1]),
                       p("Recommendation: select the month of peak humidity")
                   ),
                   sliderInput("amp", label = "Annual variation in infectivity of the virus:", post = "%", ticks = FALSE,
                               value = 0, min = 0, max = 100, step = 1),
                   sliderInput("omega", label = "Average duration of immunity:", post = " years", ticks = FALSE,
                               value = 150, min = 0.5, max = 200, step = 0.5),
                   sliderInput("pclin", label = "Probability upon infection of developing clinical symptoms:", post = "%", ticks = FALSE,
                               value = 15, min = 0, max = 100, step = 1),
                   sliderInput("prob_icu", label = "Probability upon hospitalisation of requiring ICU admission:", post = "%", ticks = FALSE,
                               value = 25, min = 0, max = 100, step = 1),
                   sliderInput("prob_vent", label = "Probability upon admission to the ICU of requiring a ventilator:", post = "%", ticks = FALSE,
                               value = 40, min = 0, max = 100, step = 1),
                   sliderInput("propo2", label = "Proportion of hospitalised patients needing O2:", post = "%", ticks = FALSE,
                               value = 50, min = 0, max = 100, step = 1)
            ),
            column(4,
                   sliderInput("pclin_v", label = "Probability upon infection of developing clinical symptoms if previously vaccinated", value = 20, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   sliderInput("pclin_vr", label = "Probability upon infection of developing clinical symptoms if previously vaccinated and exposed", value = 0, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   sliderInput("pclin_r", label = "Probability upon infection of developing clinical symptoms if previously infected", value = 0, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   sliderInput("prob_icu_v", label = "Probability upon hospitalisation of requiring ICU admission if previously vaccinated", value = 20, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   
                   sliderInput("prob_icu_vr", label = "Probability upon hospitalisation of requiring ICU admission if previously vaccinated and exposed", value = 0, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   sliderInput("prob_icu_r", label = "Probability upon hospitalisation of requiring ICU admission if previously infected", value = 0, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   sliderInput("prob_v_v", label = "Probability upon admission to the ICU of requiring a ventilator if previously vaccinated", value = 20, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%")
            ),
            column(4,  
                   sliderInput("prob_v_vr", label = "Probability upon admission to the ICU of requiring a ventilator if previously vaccinated and exposed", value = 0, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   sliderInput("prob_v_r", label = "Probability upon admission to the ICU of requiring a ventilator if previously infected", value = 0, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   sliderInput("sigmaR", label = "Probability of infection of people that have recovered from a previous infection", value = 0, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   sliderInput("sigmaEV", label = "Change in probability of requiring hospitalisation if previously vaccinated", value = 20, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   
                   sliderInput("sigmaER", label = "Change in probability of requiring hospitalisation if previously infected", value = 0, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   sliderInput("sigmaEVR", label = "Change in probability of requiring hospitalisation if previously infected and vaccinated", value = 0, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   sliderInput("seroneg", label = "Days from seropositve to seronegative", value = 100, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%")
            )
          ),
          div(class = "closebutton", actionButton("close_virus_param", label = span(icon('times'), " Close (Esc.)"), class = "btn-danger btn-sm"))
  )
)
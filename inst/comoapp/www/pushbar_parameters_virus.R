list(
  pushbar(id = "pushbar_parameters_virus", from = "right", class = "pushbarcomp",
          br(), br(), br(),
          fluidRow(
            column(6,
                   sliderInput("rho", label = "Relative infectiousness of incubation phase:", post = "%", ticks = FALSE,
                               value = 12.5, min = 0, max = 100, step = 0.5),
                   sliderInput("gamma", label = "Average incubation period:", post = " days", ticks = FALSE,
                               value = 4.5, min = 1, max = 7, step = 0.5),
                   sliderInput("nui", label = "Average duration of symptomatic infection period:", post = " days", ticks = FALSE,
                               value = 5.5, min = 1, max = 7, step = 0.5),
                   sliderInput("nus", label = "Average duration of non-fatal severe infection:", post = " days", ticks = FALSE,
                               value = 7, min = 1, max = 21, step = 0.5),
                   div(id = "margin_month_slider",
                       sliderTextInput("phi", label = "Month of peak infectivity of the virus:", 
                                       choices = month.name, selected = month.name[1]),
                       p("Recommendation: select the month of peak humidity")
                   )
            ),
            column(6,  
                   sliderInput("ratem", label = "Average time to death for fatal infection:", post = " days", ticks = FALSE,
                               value = 7, min = 1, max = 21, step = 0.5),
                   sliderInput("amp", label = "Annual variation in infectivity of the virus:", post = "%", ticks = FALSE,
                               value = 0, min = 0, max = 100, step = 1),
                   sliderInput("rhos", label = "Relative level of contacts from severely ill patients:", post = "%", ticks = FALSE,
                               value = 10, min = 0, max = 100, step = 1),
                   sliderInput("omega", label = "Average duration of immunity:", post = " years", ticks = FALSE,
                               value = 100, min = 0.5, max = 100, step = 0.5)
                   
            )
          ),
          div(id = "closebutton", bsButton("close_virus_param", "Close", icon("times"), style = "danger", size = "small"))
  )
)
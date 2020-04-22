list(
  pushbar(id = "pushbar_parameters_hospitalisation", from = "right", class = "pushbarcomp",
          br(), br(), br(),
          fluidRow(
            column(4, 
                   numericInput("beds_available", label = "Maximum number of hospital beds:", value = 80000, min = 1),
                   numericInput("icu_beds_available", label = "Maximum number of ICU beds:", value = 8000, min = 1),
                   numericInput("ventilators_available", label = "Maximum number of ventilators:", value = 6000, min = 1),
                   sliderInput("rhos", label = "Relative percentage of regular daily contacts when hospitalised:", post = "%", ticks = FALSE,
                               value = 15, min = 0, max = 100, step = 1),
                   sliderInput("ihr_scaling", label = "Scaling factor for infection hospitalisation rate: (NOT USED IN CURRENT VERSION)", ticks = FALSE,
                               value = 1, min = 1, max = 4, step = 1)
            ),
            column(4,
                   sliderInput("pdeath_h", label = "Probability of dying when hospitalised:", value = 35, min = 0, max = 100, step = 1, post = "%", ticks = FALSE),
                   sliderInput("pdeath_hc", label = "Probability of dying when denied hospitalisation:", value = 45, min = 0, max = 100, step = 1, post = "%", ticks = FALSE),
                   sliderInput("pdeath_icu", label = "Probability of dying when admitted to ICU:", value = 55, min = 0, max = 100, step = 1, post = "%", ticks = FALSE),
                   sliderInput("pdeath_icuc", label = "Probability of dying when admission to ICU denied:", value = 80, min = 0, max = 100, step = 1, post = "%", ticks = FALSE),
                   sliderInput("pdeath_vent", label = "Probability of dying when ventilated:", value = 80, min = 0, max = 100, step = 1, post = "%", ticks = FALSE),
                   sliderInput("pdeath_ventc", label = "Probability of dying when ventilator denied:", value = 95, min = 0, max = 100, step = 1, post = "%", ticks = FALSE)
            ),
            column(4,
                   sliderInput("nus", label = "Duration of hospitalised infection:", value = 24, min = 1, max = 30, step = 0.5, post = " days", ticks = FALSE),
                   sliderInput("nusc", label = "Duration of denied hospitalisation infection:", value = 24, min = 1, max = 30, step = 0.5, post = " days", ticks = FALSE),
                   sliderInput("nu_icu", label = "Duration of ICU infection:", value = 24, min = 1, max = 30, step = 0.5, post = " days", ticks = FALSE),
                   sliderInput("nu_icuc", label = "Duration of denied ICU infection:", value = 24, min = 1, max = 30, step = 0.5, post = " days", ticks = FALSE),
                   sliderInput("nu_vent", label = "Duration of ventilated infection:", value = 25, min = 1, max = 30, step = 0.5, post = " days", ticks = FALSE),
                   sliderInput("nu_ventc", label = "Duration of denied ventilation infection:", value = 24, min = 1, max = 30, step = 0.5, post = " days", ticks = FALSE),
            )
          ),
          div(class = "closebutton", bsButton("close_hospital_param", "Close", icon("times"), style = "danger", size = "small"))
  )
)
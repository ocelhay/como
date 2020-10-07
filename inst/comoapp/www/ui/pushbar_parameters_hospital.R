list(
  pushbar(id = "pushbar_parameters_hospitalisation", from = "right",
          br(), br(), br(),
          fluidRow(
            column(4, 
                   numericInput("beds_available", label = "Maximum number of hospital surge beds:", value = 80000, min = 1),
                   numericInput("icu_beds_available", label = "Maximum number of ICU beds without ventilators:", value = 8000, min = 1),
                   numericInput("ventilators_available", label = "Maximum number of ICU beds with ventilators:", value = 6000, min = 1),
                   sliderInput("rhos", label = "Relative percentage of regular daily contacts when hospitalised:", post = "%", ticks = FALSE,
                               value = 15, min = 0, max = 100, step = 1),
                   sliderInput("ihr_scaling", label = "Scaling factor for infection hospitalisation rate:", ticks = FALSE,
                               value = 1, min = 0.1, max = 5, step = 0.1)
            ),
            column(4,
                   sliderInput("pdeath_h", label = "Probability of dying when hospitalised (not req. O2):", value = 35, min = 0, max = 100, step = 1, post = "%", ticks = FALSE),
                   sliderInput("pdeath_ho", label = "Probability of dying when hospitalised if req. O2:", value = 35, min = 0, max = 100, step = 1, post = "%", ticks = FALSE),
                   
                   sliderInput("pdeath_hc", label = "Probability of dying when denied hospitalisation (not req. O2):", value = 45, min = 0, max = 100, step = 1, post = "%", ticks = FALSE),
                   sliderInput("pdeath_hco", label = "Probability of dying when denied hospitalisation if req. O2:", value = 45, min = 0, max = 100, step = 1, post = "%", ticks = FALSE),
                   
                   sliderInput("pdeath_icu", label = "Probability of dying when admitted to ICU (not req. O2):", value = 55, min = 0, max = 100, step = 1, post = "%", ticks = FALSE),
                   sliderInput("pdeath_icuo", label = "Probability of dying when admitted to ICU if req. O2:", value = 55, min = 0, max = 100, step = 1, post = "%", ticks = FALSE),
                   
                   sliderInput("pdeath_icuc", label = "Probability of dying when admission to ICU denied (not req. O2):", value = 75, min = 0, max = 100, step = 1, post = "%", ticks = FALSE),
                   sliderInput("pdeath_icuco", label = "Probability of dying when admission to ICU denied if req. O2:", value = 75, min = 0, max = 100, step = 1, post = "%", ticks = FALSE),
                   
                   sliderInput("pdeath_vent", label = "Probability of dying when ventilated:", value = 80, min = 0, max = 100, step = 1, post = "%", ticks = FALSE),
                   sliderInput("pdeath_ventc", label = "Probability of dying when ventilator denied:", value = 95, min = 0, max = 100, step = 1, post = "%", ticks = FALSE),
                   
                   sliderInput("pdeath_vent_hc", label = "Probability of dying when ventilator required and not going to hospital:", value = 95, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE),
                   sliderInput("pdeath_icu_hc", label = "Probability of dying when icu required (not O2) and not going to hospital:", value = 95, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE),
                   sliderInput("pdeath_icu_hco", label = "Probability of dying when icu required (req O2) and not going to hospital:", value = 95, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE),
            ),
            column(4,
                   sliderInput("nus", label = "Duration of hospitalised infection:", value = 24, min = 1, max = 30, step = 0.5, post = " days", ticks = FALSE),
                   sliderInput("nu_icu", label = "Duration of ICU infection:", value = 24, min = 1, max = 30, step = 0.5, post = " days", ticks = FALSE),
                   sliderInput("nu_vent", label = "Duration of ventilated infection:", value = 25, min = 1, max = 30, step = 0.5, post = " days", ticks = FALSE),
            )
          ),
          div(class = "closebutton", actionButton("close_hospital_param", label = span(icon('times'), " Close (Esc.)"), class = "btn-danger btn-sm"))
  )
)
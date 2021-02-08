list(
  pushbar(id = "pushbar_parameters_reporting", from = "right",
          br(), br(), br(),
          fluidRow(
            column(6,
                   numericInput("init", label = "Number of exposed people at start date ", value = 10, min = 1, max = NA, width = "50%"),
                   sliderInput("pre", label = "Proportion of population with partial immunity at the start date", value = 0, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE),
                   numericInput("sample_size", "Average sample size for seroprevalence", value = 100, min = 1, max = 10000, step = 1),
                   sliderInput("reporth_g", label = "Percentage of denied hospitalisations that are reported", value = 90, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   
                   sliderInput("reporth", label = "Percentage of non-severe hospitalisations that are appropriately treated", value = 90, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   sliderInput("reporth_ICU", label = "Percentage of severe hospitalisations that are appropriately treated", value = 90, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   sliderInput("report_v", label = "Percentage of all asymptomatic infections in previously vaccinated people that are reported", value = 0, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   sliderInput("report_vr", label = "Percentage of all asymptomatic infections in previously vaccinated and exposed people that are reported", value = 0, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%")
            ),
            column(6,  
                   sliderInput("report_r", label = "Percentage of all asymptomatic infections in previously infected people that are reported", value = 0, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   sliderInput("report_cv", label = "Percentage of all symptomatic infections in previously vaccinated people that are reported", value = 0, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   sliderInput("report_cvr", label = " Percentage of all symptomatic infections in previously vaccinated and exposed people that are reported", value = 0, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   
                   sliderInput("report_cr", label = "Percentage of all symptomatic infections in previously infected people that are reported", value = 0, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   sliderInput("report_natdeathI", label = "Percentage of all people dying outside the hospital with asymptomatic infections reported as covid-deaths", value = 0, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   sliderInput("report_natdeathCL", label = "Percentage of all people dying outside the hospital with symptomatic infections reported as covid-deaths", value = 10, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%"),
                   sliderInput("report_death_HC", label = "Percentage of all people dying outside the hospital with severe infections reported as covid-deaths", value = 10, min = 0, max = 100, step = 0.1, post = "%", ticks = FALSE, width = "75%")
            )
          ),
          div(class = "closebutton", actionButton("close_reporting_param", label = span(icon('times'), " Close (Esc.)"), class = "btn-danger btn-sm"))
  )
)
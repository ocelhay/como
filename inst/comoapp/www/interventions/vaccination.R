div(class = "interventions", id = 'interventions_vaccine',
    fluidRow(
      column(width = 9,
             materialSwitch(inputId = "vaccination_switch", label = span(icon("syringe"), "Vaccination"), value = FALSE,
                            status = "danger", right = TRUE, inline = FALSE, width = NULL)
      ),
      column(width = 3,
             conditionalPanel("input.vaccination_switch",
                              dropdownButton(label = "", circle = FALSE, status = "default", size = 'sm', icon = icon("gear"), 
                                             width = "400px", tooltip = FALSE, right = FALSE, up = TRUE,
                                             fluidRow(
                                               column(12,
                                                      dateInput("date_vaccine_on", label = "Start Date", value = "2021-04-01"),
                                                      sliderInput("vac_campaign", label = "Time to reach target coverage:", value = 4, min = 1, max = 52,
                                                                  step = 1, post = " weeks", ticks = FALSE),
                                                      sliderInput("vaccine_cov", label = "Coverage of vaccine:", value = 90, min = 0, max = 100,
                                                                  step = 1, post = "%", ticks = FALSE),
                                                      sliderInput("vaccine_eff", label = "Efficacy of vaccine:", value = 0, min = 0, max = 100,
                                                                  step = 1, post = "%", ticks = FALSE)
                                               )
                                             )
                              )
             )
      )
    )
)
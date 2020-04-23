div(class = "interventions", id = 'interventions_cocoon',
    fluidRow(
      column(width = 9,
             materialSwitch(inputId = "cocoon_switch", label = span(icon("house-user"), 'Shielding the Elderly'), value = FALSE,
                            status = "danger", right = TRUE, inline = FALSE, width = NULL),
      ),
      column(width = 3,
             conditionalPanel("input.cocoon_switch",
                              dropdownButton(label = "", circle = FALSE, status = "default", size = 'sm', icon = icon("gear"), 
                                             width = "400px", tooltip = FALSE, right = FALSE, up = FALSE,
                                             fluidRow(
                                               column(12,
                                                      dateInput("date_cocoon_on", label = "Start Date:", value = "2020-04-01"),
                                                      sliderInput("cocoon_dur", label = "Duration:", value = 16, min = 1, max = 52, step = 1, post = " weeks", ticks = FALSE),
                                                      sliderInput("cocoon_cov", label = "Coverage of elderly shielding:", value = 90, min = 0, max = 100,
                                                                  step = 1, post = "%", ticks = FALSE),
                                                      sliderInput("cocoon_eff", label = "Efficacy of elderly shielding:", value = 95, min = 0, max = 100,
                                                                  step = 1, post = "%", ticks = FALSE),
                                                      sliderInput("age_cocoon", label = "Minimum age for elderly shielding:", value = 70, min = 0, max = 100,
                                                                  step = 5, post = " y.o.", ticks = FALSE)
                                               )
                                             )
                              )
             )
      )
    )
)
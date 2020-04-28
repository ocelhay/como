div(class = "interventions", id = 'interventions_quarantine',
    fluidRow(
      column(width = 9,
             materialSwitch(inputId = "quarantine_switch", label = span(icon("dungeon"), " Voluntary home quarantine"), value = FALSE,
                            status = "danger", right = TRUE, inline = FALSE, width = NULL)
      ),
      column(width = 3,
             conditionalPanel("input.quarantine_switch",
                              dropdownButton(label = "", circle = FALSE, status = "default", size = 'sm', icon = icon("gear"), 
                                             width = "600px", tooltip = FALSE, right = FALSE, up = TRUE,
                                             fluidRow(
                                               column(4,
                                                      dateInput("date_quarantine_on", label = "Start Date:", value = "2020-04-01"),
                                                      sliderInput("quarantine_dur", label = "Duration of quarantine:", value = 24, min = 1, max = 52,
                                                                  step = 1, post = " weeks", ticks = FALSE),
                                                      sliderInput("quarantine_days", label = "Days in isolation for average person:", value = 14, min = 1, max = 21,
                                                                  step = 1, post = " days", ticks = FALSE)
                                               ),
                                               column(4,
                                                      sliderInput("quarantine_cov", label = "Coverage of quarantine:", value = 70, min = 0, max = 100,
                                                                  step = 1, post = "%", ticks = FALSE),
                                                      sliderInput("quarantine_effort", label = "Days to implement maximum quarantine coverage:", value = 2, 
                                                                  min = 1, max = 5, step = 1, post = " days", ticks = FALSE)
                                                      
                                               ),
                                               column(4,
                                                      sliderInput("quarantine_eff_other", label = "Decrease in the number of other contacts when quarantined:", value = 20, 
                                                                  min = 0, max = 100, step = 5, post = "%", ticks = FALSE),
                                                      sliderInput("quarantine_eff_home", label = "Increase in the number of contacts at home when quarantined:", value = 100, 
                                                                  min = 0, max = 100, step = 5, post = "%", ticks = FALSE)
                                                      )
                                             )
                              )
             )
      )
    )
)
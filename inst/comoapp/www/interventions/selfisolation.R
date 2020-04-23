div(class = "interventions", id = 'interventions_selfis',
    fluidRow(
      column(width = 9,
             materialSwitch(inputId = "selfis_switch", label = span(icon("dungeon"), 'Self-isolation if Symptomatic'), value = FALSE, status = "danger", right = TRUE, inline = FALSE, width = NULL),
      ),
      column(width = 3,
             conditionalPanel("input.selfis_switch",
                              dropdownButton(label = "", circle = FALSE, status = "default", size = 'sm', icon = icon("gear"), 
                                             width = "400px", tooltip = FALSE, right = FALSE, up = FALSE,
                                             fluidRow(
                                               column(12,
                                                      dateInput("date_selfis_on", label = "Start Date:", value = "2020-04-01"),
                                                      sliderInput("selfis_dur", label = "Duration:", value = 12, min = 1, max = 52, step = 1, post = " weeks", ticks = FALSE),
                                                      sliderInput("selfis_cov", label = "Coverage of self-isolation:", value = 50, min = 0, max = 100,
                                                                  step = 1, post = "%", ticks = FALSE),
                                                      sliderInput("selfis_eff", label = "Adherence to self-isolation:", value = 50, min = 0, max = 100,
                                                                  step = 1, post = "%", ticks = FALSE),
                                                      # Screening ----
                                                      materialSwitch(inputId = "screen_switch", label = 'Screening', value = FALSE,
                                                                     status = "danger", right = TRUE, inline = FALSE, width = NULL),
                                                      conditionalPanel("input.screen_switch",
                                                                       dateInput("date_screen_on", label = "Start Date:", value = "2020-04-01"),
                                                                       sliderInput("screen_cov", label = "Coverage of screening:", value = 90, min = 0, max = 100,
                                                                                   step = 1, post = "%", ticks = FALSE),
                                                                       sliderInput("screen_dur", label = "Duration of screening:", value = 32, min = 1, max = 52,
                                                                                   step = 1, post = " weeks", ticks = FALSE),
                                                                       sliderInput("screen_overdispersion", label = "Overdispersion of cases around index case. If  1 likelihood same as general population:", value = 4, min = 1, max = 5,
                                                                                   step = 0.2, ticks = FALSE),
                                                                       sliderInput("screen_contacts", label = "Number of contacts screened per index case:", value = 4, min = 1, max = 10,
                                                                                   step = 1, post = " contacts", ticks = FALSE)
                                                      )
                                               )
                                             )
                              )
             )
      )
    )
)
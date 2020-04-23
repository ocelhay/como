div(class = "interventions", id = 'interventions_work',
    fluidRow(
      column(width = 9,
             materialSwitch(inputId = "work_switch", label = span(icon("house-user"), ' Working at Home'), value = FALSE,
                            status = "danger", right = TRUE, inline = FALSE, width = NULL)
      ),
      column(width = 3,
             conditionalPanel("input.work_switch",
                              dropdownButton(label = "", circle = FALSE, status = "default", size = 'sm', icon = icon("gear"), 
                                             width = "400px", tooltip = FALSE, right = FALSE, up = FALSE,
                                             fluidRow(
                                               column(12,
                                                      dateInput("date_work_on", label = "Start Date:", value = "2020-04-01"),
                                                      sliderInput("work_dur", label = "Duration:", value = 12 , min = 1, max = 52, step = 1, post = " weeks", ticks = FALSE),
                                                      sliderInput("work_cov", label = "Coverage of working from home:", value = 50, min = 0, max = 100,
                                                                  step = 1, post = "%", ticks = FALSE),
                                                      sliderInput("work_eff", label = "Efficacy of working from home:", value = 85, min = 0, max = 100,
                                                                  step = 1, post = "%", ticks = FALSE),
                                                      sliderInput("w2h", label = "Home contacts inflation due to working from home:", value = 10, min = 0, max = 100,
                                                                  step = 1, post = "%", ticks = FALSE)
                                                      
                                               )
                                             )
                              )
             )
      )
    )
)
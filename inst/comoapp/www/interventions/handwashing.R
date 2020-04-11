div(class = "interventions", id = 'interventions_hand',
    fluidRow(
      column(width = 9,
             materialSwitch(inputId = "hand_switch", label = span(icon("hand-paper"), " Handwashing"), value = FALSE,
                            status = "danger", right = TRUE, inline = FALSE, width = NULL)
      ),
      column(width = 3,
             conditionalPanel("input.hand_switch",
                              dropdownButton(label = "", circle = FALSE, status = "default", size = 'sm', icon = icon("gear"), 
                                             width = "400px", tooltip = FALSE, right = FALSE,
                                             fluidRow(
                                               column(12,
                                                      dateInput("date_hand_on", label = "Start Date:", value = "2020-04-01"),
                                                      sliderInput("hand_dur", label = "Duration:", value = 30, min = 1, max = 52, step = 1, post = " weeks", ticks = FALSE),
                                                      sliderInput("hand_eff", label = "Efficacy of hand hygiene:", value = 5, min = 0, max = 100,
                                                                  step = 1, post = "%", ticks = FALSE)
                                                      
                                               )
                                             )
                              )
             )
      )
    )
)
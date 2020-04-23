div(class = "interventions", id = 'interventions_travelban',
    fluidRow(
      column(width = 9,
             materialSwitch(inputId = "travelban_switch", label = span(icon("plane"), " Travel Ban"), value = FALSE,
                            status = "danger", right = TRUE, inline = FALSE, width = NULL),
      ),
      column(width = 3,
             conditionalPanel("input.travelban_switch",
                              dropdownButton(label = "", circle = FALSE, status = "default", size = 'sm', icon = icon("gear"), 
                                             width = "400px", tooltip = FALSE, right = FALSE, up = FALSE,
                                             fluidRow(
                                               column(12,
                                                      dateInput("date_travelban_on", label = "Start Date:", value = "2020-04-01"),
                                                      sliderInput("travelban_dur", label = "Duration of travel ban:", value = 16, min = 1, max = 52,
                                                                  step = 1, post = " weeks", ticks = FALSE),
                                                      sliderInput("travelban_eff", label = "Efficacy of travel ban:", value = 50, min = 0, max = 100,
                                                                  step = 1, post = "%", ticks = FALSE)
                                               )
                                             )
                              )
             )
      )
    )
)
div(class = "interventions", id = 'interventions_school',
    fluidRow(
      column(width = 9,
             materialSwitch(inputId = "school_switch", label = span(icon("school"), "School Closures"), value = FALSE,
                            status = "danger", right = TRUE, inline = FALSE, width = NULL)
      ),
      column(width = 3,
             conditionalPanel("input.school_switch",
                              dropdownButton(label = "", circle = FALSE, status = "default", size = 'sm', icon = icon("gear"), 
                                             width = "400px", tooltip = FALSE, right = FALSE, up = FALSE,
                                             fluidRow(
                                               column(12,
                                                      dateInput("date_school_on", label = "Start Date:", value = "2020-04-01"),
                                                      sliderInput("school_dur", label = "Duration:", value = 16, min = 1, max = 52, step = 1, post = " weeks", ticks = FALSE),
                                                      sliderInput("school_eff", label = "Efficacy of school closure:", value = 85, min = 0, max = 100,
                                                                  step = 1, post = "%", ticks = FALSE),
                                                      sliderInput("s2h", label = "Home contacts inflation due to school closure:", value = 20, min = 0, max = 100,
                                                                  step = 1, post = "%", ticks = FALSE)
                                                      
                                               )
                                             )
                              )
             )
      )
    )
)
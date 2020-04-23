div(class = "interventions", id = 'interventions_dist',
    fluidRow(
      column(width = 9,
             materialSwitch(inputId = "dist_switch", label = span(icon("people-arrows"), " Social Distancing"), value = FALSE, status = "danger", right = TRUE, inline = FALSE, width = NULL),
      ),
      column(width = 3,
             conditionalPanel("input.dist_switch",
                              dropdownButton(label = "", circle = FALSE, status = "default", size = 'sm', icon = icon("gear"), 
                                             width = "400px", tooltip = FALSE, right = FALSE, up = FALSE,
                                             fluidRow(
                                               column(12,
                                                      dateInput("date_dist_on", label = "Start Date:", value = "2020-04-01"),
                                                      sliderInput("dist_dur", label = "Duration:", value = 26, min = 1, max = 52, step = 1, post = " weeks", ticks = FALSE),
                                                      sliderInput("dist_cov", label = "Coverage of social distancing:", value = 50, min = 0, max = 100,
                                                                  step = 1, post = "%", ticks = FALSE),
                                                      sliderInput("dist_eff", label = "Adherence to social distancing:", value = 100, min = 0, max = 100,
                                                                  step = 1, post = "%", ticks = FALSE)
                                                      
                                               )
                                             )
                              )
             )
      )
    )
)
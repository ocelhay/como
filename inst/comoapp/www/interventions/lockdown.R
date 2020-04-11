div(class = "interventions", id = 'interventions_lockdown',
    fluidRow(
      column(width = 9,
             materialSwitch(inputId = "lockdown_switch", label = span(icon("lock"), " Lockdown"), value = FALSE,
                            status = "danger", right = TRUE, inline = FALSE, width = NULL),
      ),
      column(width = 3,
             conditionalPanel("input.lockdown_switch",
                              dropdownButton(label = "", circle = FALSE, status = "default", size = 'sm', icon = icon("gear"), 
                                             width = "400px", tooltip = FALSE, right = FALSE, up = FALSE,
                                             fluidRow(column(12,
                                                             h4("Choose One Lockdown:"),
                                                             img(src='images/lockdown_img.png', align = "left"),
                                                             conditionalPanel("! input.lockdown_mid_switch && ! input.lockdown_high_switch",
                                                                              materialSwitch(inputId = "lockdown_low_switch", label = span(icon("lock"), " Low Lockdown"), value = FALSE,
                                                                                             status = "danger", right = TRUE, inline = FALSE, width = NULL),
                                                                              conditionalPanel("input.lockdown_low_switch",
                                                                                               dateInput("date_lockdown_low_on", label = "Start Date:", value = "2020-04-01"),
                                                                                               sliderInput("lockdown_low_dur", label = "Duration of Lockdown:", value = 3, min = 1, max = 52,
                                                                                                           step = 1, post = " weeks", ticks = FALSE)
                                                                              )
                                                             ),
                                                             conditionalPanel("! input.lockdown_low_switch && ! input.lockdown_high_switch",
                                                                              materialSwitch(inputId = "lockdown_mid_switch", label = span(icon("lock"), " Mid Lockdown"), value = FALSE,
                                                                                             status = "danger", right = TRUE, inline = FALSE, width = NULL),
                                                                              conditionalPanel("input.lockdown_mid_switch",
                                                                                               dateInput("date_lockdown_mid_on", label = "Start Date:", value = "2020-04-01"),
                                                                                               sliderInput("lockdown_mid_dur", label = "Duration of Lockdown:", value = 3, min = 1, max = 52,
                                                                                                           step = 1, post = " weeks", ticks = FALSE)
                                                                              )
                                                             ),
                                                             conditionalPanel("! input.lockdown_low_switch && ! input.lockdown_mid_switch",
                                                                              materialSwitch(inputId = "lockdown_high_switch", label = span(icon("lock"), " High Lockdown"), value = FALSE,
                                                                                             status = "danger", right = TRUE, inline = FALSE, width = NULL),
                                                                              conditionalPanel("input.lockdown_high_switch",
                                                                                               dateInput("date_lockdown_high_on", label = "Start Date:", value = "2020-04-01"),
                                                                                               sliderInput("lockdown_high_dur", label = "Duration of Lockdown:", value = 3, min = 1, max = 52,
                                                                                                           step = 1, post = " weeks", ticks = FALSE)
                                                                              )
                                                             )
                                             )
                                             )
                              )
             )
      )
    )
)
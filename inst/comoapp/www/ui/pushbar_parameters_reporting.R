list(
  pushbar(id = "pushbar_parameters_reporting", from = "right",
          br(), br(), br(),
          fluidRow(
            column(4,
                   p("Placeholder")
            ),
            column(4,  
                   p("Placeholder")
            ),
            column(4,  
                   p("Placeholder")
            )
          ),
          div(class = "closebutton", actionButton("close_reporting_param", label = span(icon('times'), " Close (Esc.)"), class = "btn-danger btn-sm"))
  )
)
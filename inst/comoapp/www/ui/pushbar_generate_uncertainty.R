list(
  pushbar(id = "pushbar_generate_uncertainty", from = "right",
          br(),
          fluidRow(
            column(
              width = 12,
              sliderInput("iterations", "Number of model runs:", value = 1, min = 1, max = 100, post = " runs", ticks = FALSE)
            )
          ),
          conditionalPanel(
            "input.iterations > 1", 
            fluidRow(
              column(
                width = 6,
                sliderInput("noise", "Noise:", value = 0.1, min = 0.01, max = 0.2, ticks = FALSE),
              ),
              column(
                width = 6,
                sliderInput("confidence", "Confidence:", value = 1, min = 5, max = 25, post = "%", ticks = FALSE)
              )
            ),
            actionButton("run_baseline_multi", "Run Baseline w/ Generated Uncertainty", class = "btn btn-success"),
          ),
          br(),
          includeMarkdown("./www/markdown/generate_uncertainty.md"),
          div(class = "closebutton", bsButton("close_generate_uncertainty", "Close (Esc.)", icon("times"), style = "danger", size = "small"))
  )
)
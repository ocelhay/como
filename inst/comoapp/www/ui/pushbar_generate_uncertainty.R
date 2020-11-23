list(
  pushbar(id = "pushbar_generate_uncertainty", from = "right",
          br(),
          numericInput("iterations", "Number of model runs (1 to 10,000):", value = 1, min = 1, max = 10000, step = 1),
          conditionalPanel(
            "input.iterations > 1", 
            fluidRow(
              column(
                width = 4,
                sliderInput("noise", "Noise:", value = 0.1, min = 0.01, max = 0.2, ticks = FALSE),
              ),
              column(
                width = 4,
                sliderInput("confidence", "Confidence:", value = 5, min = 5, max = 25, post = "%", ticks = FALSE)
              ),
              column(
                width = 4,
                numericInput("sample_size", "Sample Size:", value = 5000, min = 1, max = 10000, step = 1)
              )
            ),
            actionButton("run_baseline_multi", "Run Baseline w/ Generated Uncertainty", class = "btn btn-success"),
          ),
          br(),
          includeMarkdown("./www/markdown/generate_uncertainty.md"),
          div(class = "closebutton", actionButton("close_generate_uncertainty", label = span(icon('times'), " Close (Esc.)"), class = "btn-danger btn-sm"))
  )
)
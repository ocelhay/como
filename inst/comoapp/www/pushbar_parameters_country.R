list(
  pushbar(id = "pushbar_parameters_country", from = "right", class = "pushbarcomp",
          br(), br(), br(),
          fluidRow(
            column(4, includeMarkdown("./www/markdown/about_country_data.md"), includeMarkdown("./www/markdown/about_data.md")),
            column(4,
                   pickerInput("country_demographic", label = "Demographic Data:", choices = countries_demographic,
                               selected = "Afghanistan", options = pickerOptions(liveSearch = TRUE)),
                   pickerInput("country_cases", label = "Cases/Deaths:", choices = countries_cases,
                               selected = "Afghanistan", options = pickerOptions(liveSearch = TRUE))
            ),
            column(4,
                   pickerInput("country_contact", label = "Social Contacts Data:", choices = countries_contact,
                               selected = "Pakistan", options = pickerOptions(liveSearch = TRUE)),
                   sliderInput("household_size", label = "Mean household size:", value = 2.5, min = 1, max = 10,
                               step = 0.1, post = " indiv.", ticks = FALSE),
                   numericInput("mean_imports", label = "Mean number of infectious migrants per day:", value = 0, min = 0, max = NA,
                                width = "50%")
            )
          ),
          div(class = "closebutton", bsButton("close_country_param", "Close", icon("times"), style = "danger", size = "small"))
  )
)
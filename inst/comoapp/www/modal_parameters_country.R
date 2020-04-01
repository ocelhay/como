bsModal("CA", "Country/Area", "open2", size = "large",
        div(
          h3("Demographic Data:"),
          fluidRow(
            column(4, includeMarkdown("./www/markdown/about_country_data.md")),
            column(4,
                   pickerInput("country", label = "Select a Country — Demographic Data", choices = c("-- Own Value ---", countries_demog),
                               selected = "Afghanistan",
                               options = pickerOptions(liveSearch = TRUE)),
                   awesomeCheckbox("subnational", "Use model at subnational/regional levels."),
                   conditionalPanel("input.subnational",
                                    # strong("Select '-- Own Value ---' to perform a subnational data analysis."),
                                    p("(1) select a country with a similar demographic distribution to your subnational/regional level,"),
                                    conditionalPanel("input.country != '-- Own Value ---'",
                                                     downloadButton("download_demographic", "Download Demographic Data")
                                    ),
                                    p("(2) Download Demographic Data and (3) edit the values in the downloaded file while keeping the same column names and formats,"),
                                    p("(4) Select '--Own Values--' in the list of countries"),
                                    conditionalPanel("input.country == '-- Own Value ---'",
                                                     p("(5) upload your own dataset of demographic data"), 
                                                     fileInput("population_file", label = NULL, accept = ".csv", multiple = FALSE)
                                    )
                   )
            ),
            column(4,
                   pickerInput("country_contact", label = "Select a Country — Contact Data", choices = countries_contact,
                               options = pickerOptions(liveSearch = TRUE), selected = "Pakistan"),
                   sliderInput("household_size", label = "Mean household size:", value = 2.5, min = 1, max = 10,
                               step = 0.1, post = " indiv.", ticks = FALSE),
                   numericInput("mean_imports", label = "Mean number of infectious migrants per day:", value = 0, min = 0, max = 500,
                                    width = "50%"),
                   pickerInput("country_beds", label = "Select a Country (or choose --Own Value--) — Hospital Beds per 1,000 Data", choices = c("-- Own Value ---", countries_beds),
                               options = pickerOptions(liveSearch = TRUE), selected = "Afghanistan"),
                   conditionalPanel("input.country_beds == '-- Own Value ---'",
                                    numericInput("beds", label = "Beds per 1,000 population", value = 2.54, min = 0, width = "50%")
                   )
            )
          ),
          hr(),
          h3("Epidemiological Data:"),
          fluidRow(
            column(4, includeMarkdown("./www/markdown/about_data.md")),
            column(4,
                   h4("Cases/Deaths:"),
                   pickerInput("country_cases", label = "Select a Country (or choose --Own Value--) — Cases/Deaths Data", choices = countries_cases,
                               selected = "Afghanistan", options = pickerOptions(liveSearch = TRUE)),
                   
                   p("To provide your own values, (1) download ", a("the template", href = "https://www.dropbox.com/s/poy5sn05ndglxxc/COVID_cases_deaths.csv?dl=1", target = "_blank"), 
                     ", (2) modify the values while keeping the same column names and formats, (3) select '--Own Value--', (4) upload your dataset."),
                   
                   conditionalPanel("input.country_cases == '-- Own Value ---'",
                                    fileInput("cases_file", label = NULL, accept = ".csv", multiple = FALSE)
                   ),
                   highchartOutput("highchart_cases_data")
            ),
            column(4, 
                   h4("Severity and Mortality by Age Category:"),
                   p("To overseed default values, download ", a("the template", href = "https://www.dropbox.com/s/h8qoo6fimn29fws/COVID_severe_mortality_age.csv?dl=1", target = "_blank"), 
                     ", modify the values while keeping the same age categories and column names, and upload it:"),
                   fileInput("severity_mortality_file", label = NULL, accept = ".csv", multiple = FALSE),
                   tableOutput("table_severity_mortality")
            )
          )
        )
)
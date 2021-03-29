# CoMo COVID-19 App
version_app <- "v18.1.0"

# To generate report with macOS standalone app (created with shinybox),
# ensure that the R session has access to pandoc installed in "/usr/local/bin".
if (Sys.info()["sysname"] == "Darwin" & 
    !grepl("/usr/local/bin", Sys.getenv("PATH"), fixed = TRUE)) {
  Sys.setenv(PATH = paste("/usr/local/bin", Sys.getenv("PATH"), sep = ":"))
}

# Load comoOdeCpp and ensure this is the correct version of comoOdeCpp.
library(comoOdeCpp)
if(packageVersion("comoOdeCpp") != "16.8.0")  stop("
Running the app requires to install the v16.8.0 of the R package comoOdeCpp.
Run:  

  remove.packages('comoOdeCpp')
  remotes::install_github('bogaotory/comoOdeCpp', ref = 'v16.8.0', subdir = 'comoOdeCpp')

in the R console to install it.")

library(bsplus)
library(deSolve)
library(glue)
library(gridExtra)
library(highcharter)
library(knitr)
library(lubridate)
library(pushbar)
library(readxl)
library(reshape2)
library(rmarkdown)
library(scales)
library(shiny)
library(shinycssloaders)
library(shinyhelper)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

# Load data and define elements used by model.
source("./www/model/model_once.R")

# Define UI ----
ui <- function(request) {
  fluidPage(
    title = "CoMo Consortium | COVID-19 App",
    theme = shinytheme("flatly"),
    includeCSS("./www/styles.css"),
    pushbar_deps(),
    useShinyjs(),
    # chooseSliderSkin('HTML5'),
    
    source("./www/ui/pushbar_parameters_reporting.R", local = TRUE)[1],
    source("./www/ui/pushbar_parameters_interventions.R", local = TRUE)[1],
    source("./www/ui/pushbar_parameters_country.R", local = TRUE)[1],
    source("./www/ui/pushbar_parameters_virus.R", local = TRUE)[1],
    source("./www/ui/pushbar_parameters_hospital.R", local = TRUE)[1],
    source("./www/ui/pushbar_generate_uncertainty.R", local = TRUE)[1],
    
    navbarPage(
      title = div(a(img(src = "CoMo-logo-medium-white_resized.png", id = "logo-top"))),
      id = "tabs", windowTitle = "CoMo Consortium | COVID-19 App", collapsible = TRUE, inverse = FALSE,
      tabPanel("Welcome", value = "tab_welcome",
               span(class = "app-version", version_app),
               # for debugging purposes, TODO: remove in prod
               # htmlOutput("diagnosis_platform"),
               fluidRow(
                 column(8,
                        span(img(src = "./como_logo.png", id = "logo"),
                             includeMarkdown("./www/markdown/welcome.md")),
                        strong("CoMo Consortium member countries’ stages of engagement with policymakers") %>%
                          helper(content = "stages_countries", colour = "red"),
                        tags$img(src = "./como_policy_makers.png", id = "map")
                 ),
                 column(4,
                        bs_accordion(id = "about") %>%
                          bs_set_opts(panel_type = "default", use_heading_link = TRUE) %>%
                          bs_append(title = "Important Disclaimer", content = includeMarkdown("./www/markdown/disclaimer.md")) %>%
                          bs_append(title = "License", content = includeMarkdown("./www/markdown/readable_license.md")) %>%
                          bs_append(title = "Countries Data", content = includeMarkdown("./www/markdown/about_country_data.md")) %>%
                          bs_append(title = "Epidemiological Data", content = includeMarkdown("./www/markdown/about_data.md")) %>%
                          bs_append(title = "Source Code", content = a(span("Source Code Respository", icon("external-link-alt")), href = "https://github.com/ocelhay/como", target = "_blank"))
                 ),
               )
      ),
      tabPanel(
        "Visual Calibration", value = "tab_visualfit",
        fluidRow(
          column(
            width = 2,
            div(class = "float_bottom_left",
                sliderInput("p", label = "Prob. of infection given contact:", min = 0.01, max = 0.08, step = 0.001,
                            value = 0.049, ticks = FALSE),
                sliderInput("report", label = span("% of all", em(" asymptomatic infections "), "reported:"), min = 0, max = 100, step = 0.1,
                            value = 2.5, post = "%", ticks = FALSE),
                sliderInput("reportc", label = span("% of all", em(" symptomatic infections "), "reported:"), min = 0, max = 100, step = 0.1,
                            value = 10, post = "%", ticks = FALSE),
                uiOutput("conditional_run_baseline"), br(),
                uiOutput("conditional_validate_baseline")
            )
          ),
          column(
            width = 10,
            div(class = "box_outputs", h4("Global Simulations Parameters")),
            fluidRow(
              column(4, 
                     h4("Set Parameters with Template"),
                     fileInput("own_data", buttonLabel = "Upload template", label = NULL, accept = ".xlsx", multiple = FALSE),
                     includeMarkdown("./www/markdown/help_upload_template.md")
              ),
              column(7, offset = 1,
                     h4("Set Parameters On The Spot"),
                     dateRangeInput("date_range", label = "Date range of simulation:", start = "2020-02-01", end = "2021-06-30", startview = "year"),
                     fluidRow(column(6, 
                                     actionButton("open_country_param", label = span(icon('cog'), " Country"), class = "btn-primary", width = "80%"),
                                     htmlOutput("feedback_choices"),
                                     actionButton("open_reporting_param", label = span(icon('cog'), " Reporting"), class = "btn-primary", width = "80%"), br(), br()
                     ),
                     column(6, 
                            actionButton("open_interventions_param", label = span(icon('cog'), " Interventions"), class = "btn-primary", width = "80%"), br(), br(),
                            actionButton("open_virus_param", label = span(icon('cog'), " Virus"), class = "btn-primary", width = "80%"), br(), br(),
                            actionButton("open_hospital_param", label = span(icon('cog'), " Hospital"), class = "btn-primary", width = "80%")
                     )
                     )
              )
            ),
            use_bs_accordion_sidebar(),
            div(class = "box_outputs", h4("Interventions for Baseline")),
            
            source("./www/ui/interventions_baseline.R", local = TRUE)$value,
            htmlOutput("text_feedback_interventions_baseline"),
            conditionalPanel(condition = paste0("!([", 
                                                paste0("input.baseline_intervention_", 1:100, collapse = ", "), 
                                                "].every( (val) => { return val === '_';} ))"),
                             fluidRow(
                               column(1, 
                                      dropdownButton(
                                        div(
                                          prettySwitch("dynamic_timevis_baseline", value = FALSE, label = "Dynamic Plot")
                                        ),
                                        circle = FALSE, status = "primary", icon = icon("gear"), size = "sm", width = "300px"
                                      )
                               ),
                               column(11,
                                      conditionalPanel("! input.dynamic_timevis_baseline",
                                                       plotOutput("timevis_baseline", height = "400px") %>% withSpinner()), 
                                      conditionalPanel("input.dynamic_timevis_baseline",
                                                       highchartOutput("timevis_baseline_hc") %>% withSpinner())
                               )
                             )
            ),
            br(), hr(),
            a(id = "anchor_results_baseline", style = "visibility: hidden", ""),
            shinyjs::hidden(
              div(id = "results_baseline",
                  div(class = "important_focus",
                      prettyRadioButtons("focus_axis", label = "Focus on:", choices = c("Observed", "Predicted Reported", "Predicted Reported + Unreported"), 
                                         selected = "Observed", inline = TRUE), 
                      p("Indicators and visualisations are based on the period of focus:"),
                      tags$ul(tags$li("Observed: time range limited to provided observed data"), 
                              tags$li("Predicted Reported: time range is the date range of simulation, visualisations y-axis with focus on reported"),
                              tags$li("Predicted Reported: time range is the date range of simulation, visualisations y-axis with focus on reported + unreported"))
                  ),
                  br(),
                  fluidRow(
                    column(6, h4("Predicted Reported")),
                    column(6, h4("Predicted Reported + Unreported (Total)"))
                  ),
                  fluidRow(
                    column(
                      6, 
                      htmlOutput("text_pct_reported_baseline") %>% withSpinner(),
                      # htmlOutput("text_death_reported_baseline") %>% withSpinner()
                    ),
                    column(
                      6, 
                      htmlOutput("text_pct_total_baseline") %>% withSpinner(),
                      htmlOutput("text_death_total_baseline") %>% withSpinner()
                    )
                  ),
                  br(),
                  fluidRow(
                    column(1, 
                           dropdownButton(
                             div(
                               prettySwitch("dynamic_cases_baseline", value = FALSE, label = "Dynamic Plot"),
                               p("Select an entity to display daily tests. (Source: Our World in Data)"),
                               selectInput("entity_tests", label = "Tests Data:", choices = entities_tests,
                                           selected = "_")
                             ),
                             circle = FALSE, status = "primary", icon = icon("gear"), size = "sm", width = "300px"
                           )
                    ),
                    column(11,
                           conditionalPanel("! input.dynamic_cases_baseline",
                                            plotOutput("plot_cases_baseline", height = "350px") %>% withSpinner()), 
                           conditionalPanel("input.dynamic_cases_baseline",
                                            highchartOutput("highchart_cases_baseline") %>% withSpinner())
                    )
                  ),
                  fluidRow(
                    column(1, 
                           dropdownButton(
                             div(
                               prettySwitch("dynamic_deaths_baseline", value = FALSE, label = "Dynamic Plot")
                             ),
                             circle = FALSE, status = "primary", icon = icon("gear"), size = "sm", width = "300px"
                           )
                    ),
                    column(11,
                           conditionalPanel("! input.dynamic_deaths_baseline",
                                            plotOutput("plot_deaths_baseline", height = "350px") %>% withSpinner()), 
                           conditionalPanel("input.dynamic_deaths_baseline",
                                            highchartOutput("highchart_deaths_baseline") %>% withSpinner())
                    )
                  ),
                  fluidRow(
                    column(1, 
                           dropdownButton(
                             div(
                               prettySwitch("dynamic_requirements_baseline", value = FALSE, label = "Dynamic Plot"),
                               prettyRadioButtons("focus_requirements_baseline", label = "Focus on:", 
                                                  choices = c("No Focus", "Hospital Beds", "ICU Beds", "Ventilators"), 
                                                  selected = "No Focus", inline = TRUE)
                             ),
                             circle = FALSE, status = "primary", icon = icon("gear"), size = "sm", width = "300px")
                    ),
                    column(11,
                           conditionalPanel("! input.dynamic_requirements_baseline",
                                            plotOutput("plot_requirements_baseline", height = "350px") %>% withSpinner()), 
                           conditionalPanel("input.dynamic_requirements_baseline",
                                            highchartOutput("highchart_requirements_baseline") %>% withSpinner())
                    )
                  ),
                  fluidRow(
                    column(7, plotOutput("plot_total_deaths_age", height = "400px") %>% withSpinner()),
                    column(5, plotOutput("plot_Rt_baseline", height = "400px") %>% withSpinner())
                  ),
                  fluidRow(
                    column(1, 
                           dropdownButton(
                             div(
                               sliderInput("se", "Test Sensitivity:", 0, 100, value = 75, post = "%", ticks = FALSE),
                               sliderInput("sp", "Test Specificty:", 0, 100, value = 97, post = "%", ticks = FALSE)
                             ),
                             circle = FALSE, status = "primary", icon = icon("gear"), size = "sm", width = "300px")
                    ),
                    column(11, 
                           downloadLink("download_seroprevalence_quant", span(icon("download"), "Download Seroprevalence Data")),
                           plotOutput("plot_seroprev_baseline", height = "400px") %>% withSpinner()
                    )
                  )
              )
            )
          )
        )
      ),
      tabPanel(
        "Model Predictions", value = "tab_modelpredictions",
        a(id = "anchor_interventions", style = "visibility: hidden", ""),
        fluidRow(
          column(2, br(),
                 div(class = "float_bottom_left",
                     actionButton("reset_baseline", span(icon("eraser"), "Reset Baseline"), class="btn btn-success"), br(), br(),
                     uiOutput("conditional_run_future"),
                     br(),
                     uiOutput("conditional_float_results")
                 )
          ),
          column(10,
                 div(class = "box_outputs", h4("Interventions for Hypothetical Scenario")),
                 source("./www/ui/interventions_future.R", local = TRUE)$value,
                 htmlOutput("text_feedback_interventions_future"),
                 conditionalPanel(condition = paste0("!([", 
                                                     paste0("input.future_intervention_", 1:100, collapse = ", "), 
                                                     "].every( (val) => { return val === '_';} ))"),
                                  fluidRow(
                                    column(1, 
                                           dropdownButton(
                                             div(
                                               prettySwitch("dynamic_timevis_future", value = FALSE, label = "Dynamic Plot")
                                             ),
                                             circle = FALSE, status = "primary", icon = icon("gear"), size = "sm", width = "300px"
                                           )
                                    ),
                                    column(11,
                                           conditionalPanel("! input.dynamic_timevis_future",
                                                            plotOutput("timevis_future", height = "400px") %>% withSpinner()), 
                                           conditionalPanel("input.dynamic_timevis_future",
                                                            highchartOutput("timevis_future_hc") %>% withSpinner())
                                    )
                                  )
                 )
          )
        ),
        br(), br(), 
        fluidRow(
          column(2, ),
          column(
            10,
            shinyjs::hidden(
              div(id = "results_interventions_1",
                  a(id = "anchor_summary", style="visibility: hidden", ""),
                  div(class = "important_focus",
                      prettyRadioButtons("focus_axis_dup", label = "Focus on:", choices = c("Observed", "Predicted Reported", "Predicted Reported + Unreported"),
                                         selected = "Predicted Reported + Unreported", inline = TRUE),
                      p("Indicators and visualisations are based on the period of focus:"),
                      tags$ul(tags$li("Observed: time range limited to provided observed data"), 
                              tags$li("Predicted Reported: time range is the date range of simulation, visualisations y-axis with focus on reported"),
                              tags$li("Predicted Reported: time range is the date range of simulation, visualisations y-axis with focus on reported + unreported"))
                  ),
                  br(),
                  fluidRow(
                    column(
                      6,
                      div(class = "box_outputs", h4("Baseline")),
                      fluidRow(
                        column(6, h4("Predicted Reported")),
                        column(6, h4("Predicted Reported + Unreported (Total)")),
                      ),
                      fluidRow(
                        column(
                          6, 
                          htmlOutput("text_pct_reported_baseline_dup") %>% withSpinner(),
                          # htmlOutput("text_death_reported_baseline_dup") %>% withSpinner()
                        ),
                        column(
                          6, 
                          htmlOutput("text_pct_total_baseline_dup") %>% withSpinner(),
                          htmlOutput("text_death_total_baseline_dup") %>% withSpinner()
                        ),
                        
                      )
                    ),
                    column(
                      6,
                      div(class = "box_outputs", h4("Hypothetical Scenario")),
                      fluidRow(
                        column(6, h4("Predicted Reported")),
                        column(6, h4("Predicted Reported + Unreported (Total)"))
                      ),
                      fluidRow(
                        column(
                          6, 
                          htmlOutput("text_pct_reported_future") %>% withSpinner(),
                          # htmlOutput("text_death_reported_future") %>% withSpinner()
                        ),
                        column(
                          6, 
                          htmlOutput("text_pct_total_future") %>% withSpinner(),
                          htmlOutput("text_death_total_future") %>% withSpinner()
                        )
                      )
                    )
                  )
              )
            )
          ),
          shinyjs::hidden(
            div(id = "results_interventions_2",
                fluidRow(
                  column(10, offset = 2,
                         br(),
                         materialSwitch(inputId = "show_all_days", label = span(icon("eye"), 'Display all days', br(), tags$small("You can either display only one data point per week i.e. Wednesday (Default) or display all days in the plots/table (Slower)."), br(), tags$small("Either way, we display daily data.")), value = FALSE,
                                        status = "danger", right = TRUE, inline = FALSE, width = "100%"),
                         br(),
                         a(id = "anchor_cases", style="visibility: hidden", "")
                  )
                ),
                fluidRow(
                  column(5, offset = 2,
                         highchartOutput("highchart_cases_dual_baseline", height = "350px") %>% withSpinner(), br()
                  ),
                  column(5,
                         highchartOutput("highchart_cases_dual_interventions", height = "350px") %>% withSpinner(), br()
                  )
                ),
                
                fluidRow(
                  column(10, offset = 2,
                         a(id = "anchor_deaths", style="visibility: hidden", ""),
                         prettyRadioButtons("focus_natural_death", label = "Focus on:", 
                                            choices = c("No Focus", "COVID-19 Deaths"), 
                                            selected = "No Focus", inline = TRUE)
                  )
                ),
                fluidRow(
                  column(5, offset = 2,
                         highchartOutput("highchart_deaths_dual_baseline", height = "350px") %>% withSpinner(), br(),
                         # plotOutput("plot_deaths_age_baseline") %>% withSpinner(), br(),
                         plotOutput("plot_total_deaths_age_baseline") %>% withSpinner(), br(),
                         plotOutput("plot_mortality_lag_baseline") %>% withSpinner(), br()
                  ),
                  column(5,
                         highchartOutput("highchart_deaths_dual_interventions", height = "350px") %>% withSpinner(), br(),
                         # plotOutput("plot_deaths_age_interventions") %>% withSpinner(), br(),
                         plotOutput("plot_total_deaths_age_interventions") %>% withSpinner(), br(),
                         plotOutput("plot_mortality_lag_interventions") %>% withSpinner(), br()
                  )
                ),
                fluidRow(
                  column(10, offset = 2,
                         a(id = "anchor_occupancy", style="visibility: hidden", ""),
                         prettyRadioButtons("focus_requirements", label = "Focus on:", 
                                            choices = c("No Focus", "Hospital Beds", "ICU Beds", "Ventilators"), 
                                            selected = "No Focus", inline = TRUE)
                  )
                ),
                fluidRow(
                  column(5, offset = 2,
                         highchartOutput("highchart_requirements_dual_baseline", height = "350px") %>% withSpinner(), br(),
                  ),
                  column(5, 
                         highchartOutput("highchart_requirements_dual_interventions", height = "350px") %>% withSpinner(), br(),
                  )
                ),
                fluidRow(
                  column(5, offset = 2,
                         a(id = "anchor_rt", style="visibility: hidden", ""),
                         highchartOutput("highchart_Rt_dual_baseline", height = "350px") %>% withSpinner(), br(),
                  ),
                  column(5, 
                         highchartOutput("highchart_Rt_dual_interventions", height = "350px") %>% withSpinner(), br(),
                  )
                )
            )
          )
        )
      )
    )
  )
}

# Define server ----
server <- function(input, output, session) {
  
  # look for PANDOC for debugging purposes - can be removed in prod
  # output$diagnosis_platform <- renderText({
  #   paste0("pandoc_available: ", pandoc_available(), "</br>",
  #          "Sys.getenv('PATH'): ", Sys.getenv("PATH"), "</br>",
  #          "find_pandoc(dir = '/usr/local/bin/')", find_pandoc(dir = "/usr/local/bin/")$version)
  # })
  
  # triggers the modal dialogs when the user clicks an icon
  observe_helpers(help_dir = "./www/markdown")
  
  # Hide tabs on app launch ----
  hideTab(inputId = "tabs", target = "tab_modelpredictions")
  
  # Pushbars for parameters/generation of uncertainty ----
  setup_pushbar(overlay = TRUE, blur = TRUE)
  observeEvent(input$open_reporting_param, ignoreInit = TRUE, pushbar_open(id = "pushbar_parameters_reporting"))  
  observeEvent(input$close_reporting_param, pushbar_close())
  observeEvent(input$open_interventions_param, ignoreInit = TRUE, pushbar_open(id = "pushbar_parameters_interventions"))  
  observeEvent(input$close_interventions_param, pushbar_close())
  observeEvent(input$open_country_param, ignoreInit = TRUE, pushbar_open(id = "pushbar_parameters_country"))  
  observeEvent(input$close_country_param, pushbar_close())
  observeEvent(input$open_virus_param, ignoreInit = TRUE, pushbar_open(id = "pushbar_parameters_virus"))  
  observeEvent(input$close_virus_param, pushbar_close())
  observeEvent(input$open_hospital_param, ignoreInit = TRUE, pushbar_open(id = "pushbar_parameters_hospitalisation"))  
  observeEvent(input$close_hospital_param, pushbar_close())
  observeEvent(input$open_generate_uncertainty, ignoreInit = TRUE, pushbar_open(id = "pushbar_generate_uncertainty"))  
  observeEvent(input$close_generate_uncertainty, pushbar_close())
  
  # Define reactiveValues elements ----
  population_rv <- reactiveValues(data = NULL)
  cases_rv <- reactiveValues(data = NULL)
  mort_sever_rv <- reactiveValues(data = mort_sever_default)
  status_app <- reactiveValues(status = "No Baseline")
  simul_baseline <- reactiveValues(results = NULL, baseline_available = FALSE)
  simul_interventions <- reactiveValues(results = NULL, interventions_available = FALSE)
  
  # Management of interventions ----
  interventions <- reactiveValues(baseline_mat = tibble(NULL), 
                                  baseline_age_groups = list(),
                                  future_mat = tibble(NULL),
                                  future_age_groups = list(),
                                  valid_baseline_interventions = TRUE, 
                                  message_baseline_interventions = NULL,
                                  valid_future_interventions = TRUE, 
                                  message_future_interventions = NULL)
  
  
  observe({
    # Create interventions tibble with input from UI ----
    interventions$baseline_mat <- tibble(
      intervention = unlist(reactiveValuesToList(input)[paste0("baseline_intervention_", 1:nb_interventions_max)]),
      date_start = do.call("c", reactiveValuesToList(input)[paste0("baseline_daterange_", 1:nb_interventions_max)])[seq(1, (2*nb_interventions_max - 1), by = 2)],
      date_end = do.call("c", reactiveValuesToList(input)[paste0("baseline_daterange_", 1:nb_interventions_max)])[seq(2, 2*nb_interventions_max, by = 2)],
      value = unlist(reactiveValuesToList(input)[paste0("baseline_coverage_", 1:nb_interventions_max)]),
      age_group = unlist(map(reactiveValuesToList(input)[paste0("baseline_age_group_", 1:nb_interventions_max)], 
                             ~ paste(str_sub(.x, 1, 2), collapse = ","))),
      Target = 1:nb_interventions_max) %>%
      mutate(unit = case_when(intervention == "(*Self-isolation) Screening" ~ " contacts",
                              intervention == "Mass Testing" ~ " thousands tests", 
                              TRUE ~ "%")) %>%
      filter(intervention != "_")
    
    
    # Fill list of age groups
    vec <- interventions$baseline_mat$age_group
    if(length(vec) > 0) {
      for (i in 1:length(vec)) {
        interventions$baseline_age_groups[[i]] <- parse_age_group(vec[i])
      }
    }
    
    interventions$future_mat <- tibble(
      intervention = unlist(reactiveValuesToList(input)[paste0("future_intervention_", 1:nb_interventions_max)]),
      date_start = do.call("c", reactiveValuesToList(input)[paste0("future_daterange_", 1:nb_interventions_max)])[seq(1, (2*nb_interventions_max - 1), by = 2)],
      date_end = do.call("c", reactiveValuesToList(input)[paste0("future_daterange_", 1:nb_interventions_max)])[seq(2, 2*nb_interventions_max, by = 2)],
      value = unlist(reactiveValuesToList(input)[paste0("future_coverage_", 1:nb_interventions_max)]),
      age_group = unlist(map(reactiveValuesToList(input)[paste0("future_age_group_", 1:nb_interventions_max)], 
                             ~ paste(str_sub(.x, 1, 2), collapse = ","))),
      Target = 1:nb_interventions_max) %>%
      mutate(unit = case_when(intervention == "(*Self-isolation) Screening" ~ " contacts",
                              intervention == "Mass Testing" ~ " thousands tests", 
                              TRUE ~ "%")) %>%
      filter(intervention != "_")
    
    # Fill list of age groups
    vec <- interventions$future_mat$age_group
    if(length(vec) > 0) {
      for (i in 1:length(vec)) {
        interventions$future_age_groups[[i]] <- parse_age_group(vec[i])
      }
    }
    
    # Validation of interventions ----
    validation_baseline <- fun_validation_interventions(dta = interventions$baseline_mat, 
                                                        simul_start_date = input$date_range[1], 
                                                        simul_end_date= input$date_range[2])
    interventions$valid_baseline_interventions <- validation_baseline$validation_interventions
    interventions$message_baseline_interventions <- validation_baseline$message_interventions
    
    validation_future <- fun_validation_interventions(dta = interventions$future_mat, 
                                                      simul_start_date = input$date_range[1], 
                                                      simul_end_date= input$date_range[2])
    interventions$valid_future_interventions <- validation_future$validation_interventions
    interventions$message_future_interventions <- validation_future$message_interventions
  })
  
  
  # To show/hide elements of the App depending on the status ----
  output$conditional_run_baseline <- renderUI({
    if(interventions$valid_baseline_interventions) {
      actionButton("run_baseline", "Calibrate Baseline", class = "btn btn-success")
    }
  })
  
  output$conditional_validate_baseline <- renderUI({
    if(simul_baseline$baseline_available){
      div(
        actionButton("open_generate_uncertainty", "Generate Uncertainty", class = "btn btn-success"),br(), br(),
        actionButton("validate_baseline", span(icon("thumbs-up"), " Validate Baseline"), class = "btn btn-success")
      )
    }
  })
  
  output$conditional_run_future <- renderUI({
    if(interventions$valid_future_interventions) {
      actionButton("run_interventions", "Run Scenario", class = "btn btn-success")
    }
  })
  
  output$conditional_float_results <- renderUI({
    if(simul_interventions$interventions_available){
      div(
        p("Go to:"),
        tags$ul(
          tags$li(a("Building Interventions", href = '#anchor_interventions')),
          tags$li(a("Summary Predictions", href = '#anchor_summary')),
          tags$li(a("Cases", href = '#anchor_cases')),
          tags$li(a("Deaths", href = '#anchor_deaths')),
          tags$li(a("Hospital Occupancy", href = '#anchor_occupancy')),
          tags$li(a("Rt", href = '#anchor_rt'))
        ),
        br(), 
        uiOutput("report_generation"), br(),
        downloadButton("download_data", "Download Data") %>% helper(type = "markdown", content = "help_legend_csv", colour = "red", size = "l")
      )
    }
  })
  
  
  
  # Manage population and cases data reactive values ----
  observeEvent(input$country_demographic, if(input$country_demographic != "-- Own Value ---"){
    population_rv$data <- population %>% filter(country == input$country_demographic)
  })
  
  observeEvent(input$country_cases, if(input$country_cases != "-- Own Value ---"){
    cases_rv$data <- cases %>%
      filter(country == input$country_cases)
  })
  
  # Source files with code to generate outputs ----
  file_list <- list.files(path = "./www/outputs", pattern = "*.R")
  for (file in file_list) source(paste0("./www/outputs/", file), local = TRUE)$value
  
  
  # Process on uploading a template ----
  observeEvent(input$own_data, {
    file_path <- input$own_data$datapath
    
    # Validation of template format
    version <- read_excel(file_path, sheet = 1)
    version_template <- names(version)[1]
    
    if(! is.character(version_template)) {
      showNotification("The uploaded file isn't in the right format.", 
                       type = "error", duration = 10)
      return(NULL)  # exit
    }
    
    if(version_template != "Template v18") {
      showNotification(HTML("The format of the file is not recognised. </br> Upload a 'v18 template' to change defaults parameters."), 
                       type = "error", duration = 10)
      return(NULL)  # exit
    }
    
    
    # Epidemiology Sheet
    dta <- read_excel(file_path, sheet = "Epidemiology")
    names(dta) <- c("date", "cases", "deaths", "seroprevalence")
    
    cases_rv$data <- dta %>%
      mutate(date = as.Date(date), cumulative_death = cumsum(deaths)) %>%
      as.data.frame()
    
    updatePickerInput(session, inputId = "country_cases", choices = c("-- Own Value ---", countries_cases), selected = "-- Own Value ---")
    updatePickerInput(session, inputId = "country_demographic", choices = c("-- Own Value ---", countries_demographic), selected = "-- Own Value ---")
    
    
    # Severity/Mortality Sheet
    dta <- read_excel(file_path, sheet = "Severity-Mortality") 
    names(dta) <- c("age_category",	"ifr",	"ihr")
    
    mort_sever_rv$data <- dta %>%
      mutate(ihr = ihr/100) %>%  # starting unit should be % - scaling to a value between 0 and 1
      mutate(ifr = ifr/max(ifr))  # starting unit should be % - scaling to a value between 0 and 1
    
    # Population Sheet
    dta <- read_excel(file_path, sheet = "Population")
    names(dta) <- c("age_category",	"pop",	"birth",	"death")
    
    population_rv$data <- dta %>%
      transmute(country = NA, age_category, pop, birth, death)
    
    updatePickerInput(session, inputId = "country_demographic", selected = "-- Own Value ---")
    
    
    # Parameters Sheets
    param <- bind_rows(read_excel(file_path, sheet = "Parameters"),
                       read_excel(file_path, sheet = "Country Area Param"),
                       read_excel(file_path, sheet = "Virus Param"),
                       read_excel(file_path, sheet = "Hospitalisation Param"),
                       read_excel(file_path, sheet = "Vaccination Param"),
                       read_excel(file_path, sheet = "Interventions Param")) %>%
      mutate(Value_Date = as.Date(Value_Date)) %>%
      drop_na(Parameter)
    
    msg_update_param <- "The following 'Global Simulations Parameters' were updated: <br><br>"
    
    # Update all sliders
    if(!is_empty(param$Parameter[param$Type == 'slider'])) {
      for (input_excel in param$Parameter[param$Type == 'slider']){
        if(param$Value[param$Parameter == input_excel] != input[[input_excel]]) {
          msg_update_param <- glue("{msg_update_param} <strong>{input_excel}</strong>: from {input[[input_excel]]} to {param$Value[param$Parameter == input_excel]} ; ")
        }
        updateSliderInput(session = session, inputId = input_excel, value = param$Value[param$Parameter == input_excel])
      }}
    
    # Update all numeric values
    if(!is_empty(param$Parameter[param$Type == 'numeric'])) {
      for (input_excel in param$Parameter[param$Type == 'numeric']){
        if(param$Value[param$Parameter == input_excel] != input[[input_excel]]) {
          msg_update_param <- glue("{msg_update_param} <strong>{input_excel}</strong>: from {input[[input_excel]]} to {param$Value[param$Parameter == input_excel]} ; ")
        }
        updateNumericInput(session = session, inputId = input_excel, value = param$Value[param$Parameter == input_excel])
      }}
    
    # Update month text slider
    if(!is_empty(param$Parameter[param$Parameter == 'phi'])) {
      if(param$Value[param$Parameter == 'phi'] != which(month.name == input[['phi']])) {
        msg_update_param <- glue("{msg_update_param} <strong>phi</strong>: from {which(month.name == input[['phi']])} to {param$Value[param$Parameter == 'phi']} ; ")
      }
      updateSliderTextInput(session = session, inputId = "phi", selected = month.name[param$Value[param$Parameter == "phi"]])
    }
    
    # Update date range of simulation
    if(!is_empty(param$Parameter[param$Type == 'date_range_simul'])) {
      if(param$Value_Date[param$Parameter == 'date_range_simul_start'] != input[['date_range']][1]) {
        msg_update_param <- glue("{msg_update_param} <strong>date_range_simul_start</strong>: from {input[['date_range']][1]} to {param$Value_Date[param$Parameter == 'date_range_simul_start']} ; ")
      }
      if(param$Value_Date[param$Parameter == 'date_range_simul_end'] != input[['date_range']][2]) {
        msg_update_param <- glue("{msg_update_param} <strong>date_range_simul_end</strong>: from {input[['date_range']][2]} to {param$Value_Date[param$Parameter == 'date_range_simul_end']} ; ")
      }
      updateDateRangeInput(session, inputId = "date_range", start = param$Value_Date[param$Parameter == "date_range_simul_start"], 
                           end = param$Value_Date[param$Parameter == "date_range_simul_end"])
    }
    
    # Update social contact
    if(!is_empty(param$Parameter[param$Type == 'picker'])) {
      if(param$Value_Country[param$Parameter == 'country_contact'] != input[['country_contact']]) {
        msg_update_param <- glue("{msg_update_param} <strong>country_contact</strong>: from {input[['country_contact']]} to {param$Value_Country[param$Parameter == 'country_contact']} ; ")
      }
      updatePickerInput(session, inputId = "country_contact", selected = param$Value_Country[param$Parameter == "country_contact"])
    }
    
    if(msg_update_param != "The following 'Global Simulations Parameters' were updated: <br><br>") {
      showModal(modalDialog(
        HTML(msg_update_param),
        title = NULL,
        footer = modalButton("Okay"),
        size = "m",
        easyClose = TRUE,
        fade = TRUE
      ))
    }
    
    if(msg_update_param == "The following 'Global Simulations Parameters' were updated: <br><br>") {
      showNotification(HTML("No 'Global Simulations Parameter' was updated."), duration = NULL)
    }
    
    # Update interventions in the UI: read "Interventions" sheet and validate
    interventions_excel <- read_excel(file_path, sheet = "Interventions") %>%
      filter(!is.na(Intervention))
    names(interventions_excel) <- c("intervention", "date_start", "date_end", "value", "unit", "age_group", "apply_to")
    
    if(all(interventions_excel$intervention %in% valid_interventions_v18)) message("Okay, all interventions are valid.")
    if(! all(interventions_excel$intervention %in% valid_interventions_v18)) stop("Stop, some interventions are not valid.")
    
    
    
    # Update interventions in the UI: baseline interventions
    interventions_excel_baseline <- interventions_excel %>% 
      filter(apply_to == "Baseline (Calibration)")
    
    nb_interventions_baseline <- interventions_excel_baseline %>% nrow()
    if(nb_interventions_baseline > 0) {
      for (i in 1:nb_interventions_baseline) {
        updateSelectInput(session, paste0("baseline_intervention_", i), selected = interventions_excel_baseline[[i, "intervention"]])
        updateDateRangeInput(session, paste0("baseline_daterange_", i), 
                             start = interventions_excel_baseline[[i, "date_start"]], 
                             end = interventions_excel_baseline[[i, "date_end"]])
        updateSliderInput(session, paste0("baseline_coverage_", i), value = interventions_excel_baseline[[i, "value"]])
        updatePickerInput(session, paste0("baseline_age_group_", i), selected = vec_age_categories[as.logical(parse_age_group(interventions_excel_baseline$age_group[i]))])
      }
    }
    
    # Update interventions in the UI: future interventions
    interventions_excel_future <- interventions_excel %>% 
      filter(apply_to == "Hypothetical Scenario")
    nb_interventions_future <- interventions_excel_future %>% nrow()
    if(nb_interventions_future > 0) {
      for (i in 1:nb_interventions_future) {
        updateSelectInput(session, paste0("future_intervention_", i), selected = interventions_excel_future[[i, "intervention"]])
        updateDateRangeInput(session, paste0("future_daterange_", i), 
                             start = interventions_excel_future[[i, "date_start"]], 
                             end = interventions_excel_future[[i, "date_end"]])
        updateSliderInput(session, paste0("future_coverage_", i), value = interventions_excel_future[[i, "value"]])
        updatePickerInput(session, paste0("future_age_group_", i), selected = vec_age_categories[as.logical(parse_age_group(interventions_excel_future$age_group[i]))])
      }
    }
  })
  
  # Process on "reset_baseline" ----
  observeEvent(input$reset_baseline, {
    simul_baseline$results <- NULL
    simul_baseline$baseline_available <- FALSE
    simul_interventions$results <- NULL
    simul_interventions$interventions_available <- FALSE
    showTab(inputId = "tabs", target = "tab_visualfit")
    hideTab(inputId = "tabs", target = "tab_modelpredictions")
    updateNavbarPage(session, "tabs", selected = "tab_visualfit")
    
    shinyjs::hide(id = "results_baseline", anim = FALSE)
    updateSliderInput(session, "iterations", value = 1)
  })
  
  # Process on "run_baseline" ----
  observeEvent(input$run_baseline, {
    # Previous results are no longer valid
    simul_interventions$results <- NULL
    
    # Create/filter objects for model that are dependent on user inputs
    source("./www/model/model_repeat.R", local = TRUE)
    parameters["iterations"] <- 1
    
    vectors <- inputs(inp, 'Baseline (Calibration)', times, startdate, stopdate)
    
    # Temporary fix the issue where the app crashes if the vaccination efficacy is 100 
    # by replacing 100 by 99.
    # It should better to fix this in the model.
    vectors$vc_vector[which(vectors$vc_vector == 100)] <- 99
    
    check_parameters_list_for_na(parameters_list = parameters)
    results <- multi_runs(Y, times, parameters, input = vectors, A = A,  ihr, ifr, mort, popstruc, popbirth, ageing,
                          contact_home = contact_home, contact_school = contact_school, 
                          contact_work = contact_work, contact_other = contact_other, 
                          age_group_vectors = interventions$baseline_age_groups)
    showNotification("Processing results", duration = NULL, id = "msg_processing")
    simul_baseline$results <- process_ode_outcome(out = results, param_used = parameters, startdate, times, ihr, 
                                                  ifr, mort, popstruc, intv_vector = vectors)
    removeNotification(id = "msg_processing")
    
    simul_baseline$baseline_available <- TRUE
    
    showNotification("Displaying results", duration = 7)
    shinyjs::show(id = "results_baseline", anim = FALSE)
    # need a small pause 
    Sys.sleep(0.2)
    runjs('document.getElementById("anchor_results_baseline").scrollIntoView();')
  })
  
  # Process on "run_baseline_multi" ----
  observeEvent(input$run_baseline_multi, {
    # Close pushbar
    pushbar_close()
    
    # Create/filter objects for model that are dependent on user inputs
    source("./www/model/model_repeat.R", local = TRUE)
    
    vectors <- inputs(inp, 'Baseline (Calibration)', times, startdate, stopdate)
    
    # Temporary fix the issue where the app crashes if the vaccination efficacy is 100 
    # by replacing 100 by 99.
    # It should better to fix this in the model.
    vectors$vc_vector[which(vectors$vc_vector == 100)] <- 99
    
    check_parameters_list_for_na(parameters_list = parameters)
    
    results <- multi_runs(Y, times, parameters, input = vectors, A = A,  ihr, ifr, mort, popstruc, popbirth, ageing,
                          contact_home = contact_home, contact_school = contact_school, 
                          contact_work = contact_work, contact_other = contact_other, 
                          age_group_vectors = interventions$baseline_age_groups)
    
    showNotification("Processing results", duration = NULL, id = "msg_processing")
    simul_baseline$results <- process_ode_outcome(out = results, param_used = parameters, startdate, times, ihr, 
                                                  ifr, mort, popstruc, intv_vector = vectors)
    simul_baseline$baseline_available <- TRUE
    
    removeNotification(id = "msg_processing")
    showNotification("Displaying results", duration = 7)
    # need a small pause 
    Sys.sleep(0.2)
    runjs('document.getElementById("anchor_results_baseline").scrollIntoView();')
  })
  
  # Process on "Validate Baseline" ----
  observeEvent(input$validate_baseline, {
    showTab(inputId = "tabs", target = "tab_modelpredictions")
    hideTab(inputId = "tabs", target = "tab_visualfit")
    updateNavbarPage(session, "tabs", selected = "tab_modelpredictions")
  })
  
  # Process on "run_interventions" ----
  observeEvent(input$run_interventions, {
    # Create/filter objects for model that are dependent on user inputs
    source("./www/model/model_repeat.R", local = TRUE)
    
    vectors <- inputs(inp, 'Hypothetical Scenario', times, startdate, stopdate)
    
    # Temporary fix the issue where the app crashes if the vaccination efficacy is 100 
    # by replacing 100 by 99.
    # It should better to fix this in the model.
    vectors$vc_vector[which(vectors$vc_vector == 100)] <- 99
    
    check_parameters_list_for_na(parameters_list = parameters)
    
    results <- multi_runs(Y, times, parameters, input = vectors, A = A,  ihr, ifr, mort, popstruc, popbirth, ageing,
                          contact_home = contact_home, contact_school = contact_school, 
                          contact_work = contact_work, contact_other = contact_other,
                          age_group_vectors = interventions$future_age_groups)
    
    showNotification("Processing results", duration = NULL, id = "msg_processing")
    simul_interventions$results <- process_ode_outcome(out = results, param_used = parameters, startdate, times, ihr, 
                                                       ifr, mort, popstruc, intv_vector = vectors)
    simul_interventions$interventions_available <- TRUE
    
    removeNotification(id = "msg_processing")
    showNotification("Displaying results", duration = 7)
    shinyjs::show(id = "results_interventions_1", anim = FALSE)
    shinyjs::show(id = "results_interventions_2", anim = FALSE)
    # need a small pause 
    Sys.sleep(0.2)
    runjs('document.getElementById("anchor_summary").scrollIntoView();')
  })
  
  
  # Generate Report ----
  output$report_generation <- renderUI({
    ifelse(pandoc_available(),
           tagList(downloadLink("report", label = span(icon("file-word"), "Generate Report Based on Current Simulation (.docx)"))),
           tagList(span("To generate a report", a("install pandoc", href = "https://pandoc.org/installing.html", target = "_blank"), " and restart the app."))
    )
  })
  
  
  output$report <- downloadHandler(
    filename = "CoMo_Model_Report.docx",
    content = function(file) {
      showNotification(HTML("Generating report."), duration = NULL, type = "message", id = "report_generation", session = session)
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      tempLogo <- file.path(tempdir(), "como_logo.png")
      file.copy("./www/report.Rmd", tempReport, overwrite = TRUE)
      file.copy("./www/como_logo.png", tempLogo, overwrite = TRUE)
      
      rmarkdown::render(tempReport, output_file = file)
      removeNotification(id = "report_generation", session = session)
      showNotification(HTML("Report Generated"), duration = 4, type = "message", id = "report_generated", session = session)
    }
  )
  
  # Downloadable csv of results ----
  results_aggregated <- reactive({
    
    # Outputs of the model ----
    dta <- tibble(
      date = simul_baseline$results$time, 
      
      # Baseline
      baseline_predicted_reported_min = simul_baseline$results$min$daily_incidence,
      baseline_predicted_reported_and_unreported_min = simul_baseline$results$min$daily_total_cases,
      baseline_normal_bed_occupancy_min = simul_baseline$results$min$hospital_surge_beds,
      baseline_icu_bed_occupancy_min = simul_baseline$results$min$icu_beds,
      baseline_icu_ventilator_occupancy_min = simul_baseline$results$min$ventilators,
      baseline_normal_bed_requirement_min = simul_baseline$results$min$normal_bed_requirement,
      baseline_icu_bed_requirement_min = simul_baseline$results$min$icu_bed_requirement,
      baseline_icu_ventilator_requirement_min = simul_baseline$results$min$icu_ventilator_requirement,
      baseline_death_natural_non_exposed_min = simul_baseline$results$min$death_natural_non_exposed,
      baseline_death_natural_exposed_min = simul_baseline$results$min$death_natural_exposed,
      baseline_death_treated_hospital_min = simul_baseline$results$min$death_treated_hospital,
      baseline_death_treated_icu_min = simul_baseline$results$min$death_treated_icu,
      baseline_death_treated_ventilator_min = simul_baseline$results$min$death_treated_ventilator,
      baseline_death_untreated_hospital_min = simul_baseline$results$min$death_untreated_hospital,
      baseline_death_untreated_icu_min = simul_baseline$results$min$death_untreated_icu,
      baseline_death_untreated_ventilator_min = simul_baseline$results$min$death_untreated_ventilator,
      baseline_death_cum_mortality_min = simul_baseline$results$min$cum_mortality,
      baseline_death_deaths_from_covid_min = simul_baseline$results$min$deaths_from_covid,
      baseline_death_deaths_with_covid_min = simul_baseline$results$min$deaths_with_covid,
      
      
      baseline_predicted_reported_med = simul_baseline$results$med$daily_incidence,
      baseline_predicted_reported_and_unreported_med = simul_baseline$results$med$daily_total_cases,
      baseline_normal_bed_occupancy_med = simul_baseline$results$med$hospital_surge_beds,
      baseline_icu_bed_occupancy_med = simul_baseline$results$med$icu_beds,
      baseline_icu_ventilator_occupancy_med = simul_baseline$results$med$ventilators,
      baseline_normal_bed_requirement_med = simul_baseline$results$med$normal_bed_requirement,
      baseline_icu_bed_requirement_med = simul_baseline$results$med$icu_bed_requirement,
      baseline_icu_ventilator_requirement_med = simul_baseline$results$med$icu_ventilator_requirement,
      baseline_death_natural_non_exposed_med = simul_baseline$results$med$death_natural_non_exposed,
      baseline_death_natural_exposed_med = simul_baseline$results$med$death_natural_exposed,
      baseline_death_treated_hospital_med = simul_baseline$results$med$death_treated_hospital,
      baseline_death_treated_icu_med = simul_baseline$results$med$death_treated_icu,
      baseline_death_treated_ventilator_med = simul_baseline$results$med$death_treated_ventilator,
      baseline_death_untreated_hospital_med = simul_baseline$results$med$death_untreated_hospital,
      baseline_death_untreated_icu_med = simul_baseline$results$med$death_untreated_icu,
      baseline_death_untreated_ventilator_med = simul_baseline$results$med$death_untreated_ventilator,
      baseline_death_cum_mortality_med = simul_baseline$results$med$cum_mortality,
      baseline_death_deaths_from_covid_med = simul_baseline$results$med$deaths_from_covid,
      baseline_death_deaths_with_covid_med = simul_baseline$results$med$deaths_with_covid,
      
      
      baseline_predicted_reported_max = simul_baseline$results$max$daily_incidence,
      baseline_predicted_reported_and_unreported_max = simul_baseline$results$max$daily_total_cases,
      baseline_normal_bed_occupancy_max = simul_baseline$results$max$hospital_surge_beds,
      baseline_icu_bed_occupancy_max = simul_baseline$results$max$icu_beds,
      baseline_icu_ventilator_occupancy_max = simul_baseline$results$max$ventilators,
      baseline_normal_bed_requirement_max = simul_baseline$results$max$normal_bed_requirement,
      baseline_icu_bed_requirement_max = simul_baseline$results$max$icu_bed_requirement,
      baseline_icu_ventilator_requirement_max = simul_baseline$results$max$icu_ventilator_requirement,
      baseline_death_natural_non_exposed_max = simul_baseline$results$max$death_natural_non_exposed,
      baseline_death_natural_exposed_max = simul_baseline$results$max$death_natural_exposed,
      baseline_death_treated_hospital_max = simul_baseline$results$max$death_treated_hospital,
      baseline_death_treated_icu_max = simul_baseline$results$max$death_treated_icu,
      baseline_death_treated_ventilator_max = simul_baseline$results$max$death_treated_ventilator,
      baseline_death_untreated_hospital_max = simul_baseline$results$max$death_untreated_hospital,
      baseline_death_untreated_icu_max = simul_baseline$results$max$death_untreated_icu,
      baseline_death_untreated_ventilator_max = simul_baseline$results$max$death_untreated_ventilator,
      baseline_death_cum_mortality_max = simul_baseline$results$max$cum_mortality,
      baseline_death_deaths_from_covid_max = simul_baseline$results$max$deaths_from_covid,
      baseline_death_deaths_with_covid_max = simul_baseline$results$max$deaths_with_covid,
      
      
      # Hypothetical scenario
      hypothetical_predicted_reported_min = simul_interventions$results$min$daily_incidence,
      hypothetical_predicted_reported_and_unreported_min = simul_interventions$results$min$daily_total_cases,
      hypothetical_normal_bed_occupancy_min = simul_interventions$results$min$hospital_surge_beds,
      hypothetical_icu_bed_occupancy_min = simul_interventions$results$min$icu_beds,
      hypothetical_icu_ventilator_occupancy_min = simul_interventions$results$min$ventilators,
      hypothetical_normal_bed_requirement_min = simul_interventions$results$min$normal_bed_requirement,
      hypothetical_icu_bed_requirement_min = simul_interventions$results$min$icu_bed_requirement,
      hypothetical_icu_ventilator_requirement_min = simul_interventions$results$min$icu_ventilator_requirement,
      hypothetical_death_natural_non_exposed_min = simul_interventions$results$min$death_natural_non_exposed,
      hypothetical_death_natural_exposed_min = simul_interventions$results$min$death_natural_exposed,
      hypothetical_death_treated_hospital_min = simul_interventions$results$min$death_treated_hospital,
      hypothetical_death_treated_icu_min = simul_interventions$results$min$death_treated_icu,
      hypothetical_death_treated_ventilator_min = simul_interventions$results$min$death_treated_ventilator,
      hypothetical_death_untreated_hospital_min = simul_interventions$results$min$death_untreated_hospital,
      hypothetical_death_untreated_icu_min = simul_interventions$results$min$death_untreated_icu,
      hypothetical_death_untreated_ventilator_min = simul_interventions$results$min$death_untreated_ventilator,
      hypothetical_death_cum_mortality_min = simul_interventions$results$min$cum_mortality,
      hypothetical_death_deaths_from_covid_min = simul_interventions$results$min$deaths_from_covid,
      hypothetical_death_deaths_with_covid_min = simul_interventions$results$min$deaths_with_covid,
      
      hypothetical_predicted_reported_med = simul_interventions$results$med$daily_incidence,
      hypothetical_predicted_reported_and_unreported_med = simul_interventions$results$med$daily_total_cases,
      hypothetical_normal_bed_occupancy_med = simul_interventions$results$med$hospital_surge_beds,
      hypothetical_icu_bed_occupancy_med = simul_interventions$results$med$icu_beds,
      hypothetical_icu_ventilator_occupancy_med = simul_interventions$results$med$ventilators,
      hypothetical_normal_bed_requirement_med = simul_interventions$results$med$normal_bed_requirement,
      hypothetical_icu_bed_requirement_med = simul_interventions$results$med$icu_bed_requirement,
      hypothetical_icu_ventilator_requirement_med = simul_interventions$results$med$icu_ventilator_requirement,
      hypothetical_death_natural_non_exposed_med = simul_interventions$results$med$death_natural_non_exposed,
      hypothetical_death_natural_exposed_med = simul_interventions$results$med$death_natural_exposed,
      hypothetical_death_treated_hospital_med = simul_interventions$results$med$death_treated_hospital,
      hypothetical_death_treated_icu_med = simul_interventions$results$med$death_treated_icu,
      hypothetical_death_treated_ventilator_med = simul_interventions$results$med$death_treated_ventilator,
      hypothetical_death_untreated_hospital_med = simul_interventions$results$med$death_untreated_hospital,
      hypothetical_death_untreated_icu_med = simul_interventions$results$med$death_untreated_icu,
      hypothetical_death_untreated_ventilator_med = simul_interventions$results$med$death_untreated_ventilator,
      hypothetical_death_cum_mortality_med = simul_interventions$results$med$cum_mortality,
      hypothetical_death_deaths_from_covid_med = simul_interventions$results$med$deaths_from_covid,
      hypothetical_death_deaths_with_covid_med = simul_interventions$results$med$deaths_with_covid,
      
      hypothetical_predicted_reported_max = simul_interventions$results$max$daily_incidence,
      hypothetical_predicted_reported_and_unreported_max = simul_interventions$results$max$daily_total_cases,
      hypothetical_normal_bed_occupancy_max = simul_interventions$results$max$hospital_surge_beds,
      hypothetical_icu_bed_occupancy_max = simul_interventions$results$max$icu_beds,
      hypothetical_icu_ventilator_occupancy_max = simul_interventions$results$max$ventilators,
      hypothetical_normal_bed_requirement_max = simul_interventions$results$max$normal_bed_requirement,
      hypothetical_icu_bed_requirement_max = simul_interventions$results$max$icu_bed_requirement,
      hypothetical_icu_ventilator_requirement_max = simul_interventions$results$max$icu_ventilator_requirement,
      hypothetical_death_natural_non_exposed_max = simul_interventions$results$max$death_natural_non_exposed,
      hypothetical_death_natural_exposed_max = simul_interventions$results$max$death_natural_exposed,
      hypothetical_death_treated_hospital_max = simul_interventions$results$max$death_treated_hospital,
      hypothetical_death_treated_icu_max = simul_interventions$results$max$death_treated_icu,
      hypothetical_death_treated_ventilator_max = simul_interventions$results$max$death_treated_ventilator,
      hypothetical_death_untreated_hospital_max = simul_interventions$results$max$death_untreated_hospital,
      hypothetical_death_untreated_icu_max = simul_interventions$results$max$death_untreated_icu,
      hypothetical_death_untreated_ventilator_max = simul_interventions$results$max$death_untreated_ventilator,
      hypothetical_death_cum_mortality_max = simul_interventions$results$max$cum_mortality,
      hypothetical_death_deaths_from_covid_max = simul_interventions$results$max$deaths_from_covid,
      hypothetical_death_deaths_with_covid_max = simul_interventions$results$max$deaths_with_covid,
    )
    
    # Cases Data ----
    dta <- left_join(
      dta, 
      cases_rv$data %>% rename(input_cases = cases, input_deaths = deaths, input_cumulative_death = cumulative_death), 
      by = "date")
    
    # Interventions ----
    startdate <- input$date_range[1]
    stopdate <- input$date_range[2]
    times <- seq(0, as.numeric(stopdate - startdate))
    inp <- bind_rows(interventions$baseline_mat %>% mutate(apply_to = "Baseline (Calibration)"),
                     interventions$future_mat %>% mutate(apply_to = "Hypothetical Scenario"))
    
    vectors0 <- inputs(inp, 'Baseline (Calibration)', times, startdate, stopdate)
    vectors0_cbind <- do.call(cbind, vectors0)
    vectors0_reduced <- vectors0_cbind[seq(from=0,to=nrow(vectors0_cbind),by=20),]
    vectors0_reduced <- as.data.frame(rbind(rep(0,ncol(vectors0_reduced)),vectors0_reduced))
    vectors0_reduced <- vectors0_reduced[,1:12] #subsetting only the coverages - total of 12 different interventions
    names(vectors0_reduced) <- paste0("interventions_baseline_",names(vectors0_reduced))
    
    vectors <- inputs(inp, 'Hypothetical Scenario', times, startdate, stopdate)
    vectors_cbind <- do.call(cbind, vectors)
    vectors_reduced <- vectors_cbind[seq(from=0,to=nrow(vectors_cbind),by=20),]
    vectors_reduced <- as.data.frame(rbind(rep(0,ncol(vectors_reduced)),vectors_reduced))
    vectors_reduced <- vectors_reduced[,1:12] #subsetting only the coverages - total of 12 different interventions
    names(vectors_reduced) <- paste0("interventions_hypothetical_",names(vectors_reduced))
    
    intv_vectors <- as_tibble(cbind(date=simul_baseline$results$time, vectors0_reduced, vectors_reduced))
    intv_vectors$date <- as.Date(intv_vectors$date)
    dta <- left_join(dta,intv_vectors, by="date")
    
    return(dta)
  })
  
  output$download_data <- downloadHandler(
    filename = "COVID19_App_Data.csv",
    content = function(file) {
      write.csv(results_aggregated(), file, row.names = FALSE)
    }
  )
  
  output$download_seroprevalence_quant <- downloadHandler(
    filename = "Baseline_Seroprevalence_Quantiles.csv",
    content = function(file) {
      write.csv(simul_baseline$results$seroprevalence_quantile, file, row.names = FALSE)
    }
  )
}

# Run the App ----
shinyApp(ui = ui, server = server)


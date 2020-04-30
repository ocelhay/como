# CoMo COVID-19 App
version_app <- "v13.1"

# Load packages and data
source("./www/source_on_inception.R")

# Define UI ----
ui <- function(request) {
  fluidPage(
    theme = shinytheme("flatly"),
    includeCSS("./www/styles.css"),
    pushbar_deps(),
    chooseSliderSkin('HTML5'),
    title = "CoMo COVID-19 App",
    
    source("./www/pushbar_parameters_interventions.R", local = TRUE)[1],
    source("./www/pushbar_parameters_country.R", local = TRUE)[1],
    source("./www/pushbar_parameters_virus.R", local = TRUE)[1],
    source("./www/pushbar_parameters_hospital.R", local = TRUE)[1],
    
    fluidRow(
      # column left ----
      column(4, 
             div(id = "css_feedback_process", htmlOutput("feedback_process")),
             br(), 
             conditionalPanel("input.tabs != 'tab_welcome' && output.status_app_output == 'No Baseline' | output.status_app_output == 'Ok Baseline'", 
                              p("Use customised data/update default parameters: ", br(), a("download 'Template_CoMo_App.xlsx'", href = "https://github.com/ocelhay/como/blob/master/Template_CoMoCOVID-19App.xlsx", target = "_blank"), 
                                ", edit it and upload it."),
                              fileInput("own_data", label = span("Upload your ", span("v13", class = "red"), " template:"), accept = ".xlsx", multiple = FALSE),
                              tabsetPanel(
                                tabPanel("Global Parameters",
                                         div(class = "baseline_left",
                                             dateRangeInput("date_range", label = "Date range of simulation:", start = "2020-02-10", end = "2020-09-01"),
                                             bsButton("open_interventions_param", label = "Interventions", icon = icon('cog'), style = "danger", type = "action", value = FALSE, 
                                                      block = TRUE), br(),
                                             bsButton("open_country_param", label = "Country", icon = icon('cog'), style = "primary", type = "action", value = FALSE, 
                                                      block = TRUE), br(),
                                             bsButton("open_virus_param", label = "Virus", icon = icon('cog'), style = "primary", type = "action", value = FALSE, 
                                                      block = TRUE), br(), 
                                             bsButton("open_hospital_param", label = "Hospital", icon = icon('cog'), style = "primary", type = "action", value = FALSE, 
                                                      block = TRUE), br(), 
                                             sliderInput("p", label = "Probability of infection given contact:", min = 0, max = 0.2, step = 0.001,
                                                         value = 0.049, ticks = FALSE),
                                             sliderInput("report", label = span("Percentage of all", strong(" asymptomatic infections "), "that are reported:"), min = 0, max = 100, step = 0.1,
                                                         value = 2.5, post = "%", ticks = FALSE),
                                             sliderInput("reportc", label = span("Percentage of all", strong(" symptomatic infections "), "that are reported:"), min = 0, max = 100, step = 0.1,
                                                         value = 5, post = "%", ticks = FALSE),
                                             sliderInput("reporth", label = span("Percentage of all hospitalisations that are reported:"), min = 0, max = 100, step = 0.1,
                                                         value = 100, post = "%", ticks = FALSE)
                                         )
                                ),
                                # V13
                                tabPanel("[Baseline + FS] Interventions",
                                         source("./www/ui_interventions_baseline.R", local = TRUE)$value
                                )
                              )
             ),
             # V13
             conditionalPanel("(output.status_app_output == 'Validated Baseline' | output.status_app_output == 'Locked Baseline') && input.tabs == 'tab_modelpredictions'",
                              source("./www/ui_interventions_future.R", local = TRUE)$value
             ),
             br(), br(), br(), br(), br(), br(), br(), br(),
             conditionalPanel("input.tabs != 'tab_welcome'",
                              div(id = "float_action",
                                  conditionalPanel("output.status_app_output == 'No Baseline' | output.status_app_output == 'Ok Baseline'",
                                                   htmlOutput("feedback_choices")
                                  ),
                                  fluidRow(
                                    column(6, 
                                           conditionalPanel("output.status_app_output == 'No Baseline' | output.status_app_output == 'Ok Baseline'",
                                                            div(class = "front_btn", actionButton("run_baseline", "Run Baseline", class="btn btn-success"))
                                           )
                                    ),
                                    column(6, 
                                           conditionalPanel("output.status_app_output == 'Ok Baseline'",
                                                            actionButton("validate_baseline", span(icon("thumbs-up"), " Validate Baseline"), class="btn btn-success"),
                                           )
                                    )
                                  ),
                                  conditionalPanel("(output.status_app_output == 'Validated Baseline' | output.status_app_output == 'Locked Baseline')  && input.tabs == 'tab_visualfit'", 
                                                   actionButton("reset_baseline", span(icon("eraser"), "Reset the Baseline"), class="btn btn-success")
                                  ),
                                  conditionalPanel("(output.status_app_output == 'Validated Baseline' | output.status_app_output == 'Locked Baseline') && input.tabs == 'tab_modelpredictions'",
                                                   actionButton("run_interventions", "Run Future Scenarios", class="btn btn-success")
                                  ),
                              )
             )
      ),
      
      # column right ----
      column(8,
             navbarPage(NULL, id = "tabs", windowTitle = "CoMo COVID-19 App", collapsible = TRUE, inverse = FALSE,
                        tabPanel("Welcome", value = "tab_welcome",
                                 h3("CoMo COVID-19 App"),
                                 h4(version_app),
                                 br(),
                                 fluidRow(
                                   column(6, 
                                          div(class = "box_outputs", h4("Important Disclaimer:")),
                                          includeMarkdown("./www/markdown/disclaimer.md")
                                   ),
                                   column(6,
                                          div(class = "box_outputs", h4("Sources of Data:")),
                                          includeMarkdown("./www/markdown/about_country_data.md"),
                                          includeMarkdown("./www/markdown/about_data.md"),
                                   )
                                 )
                        ),
                        tabPanel("Visual Calibration", value = "tab_visualfit",
                                 div(class = "box_outputs", h4("Timeline")),
                                 plotOutput("timevis_baseline"),
                                 dataTableOutput("tab_inputs"),
                                 conditionalPanel("output.status_app_output == 'Ok Baseline' | output.status_app_output == 'Validated Baseline'",
                                                  br(), br(), br(), br(),
                                                  fluidRow(
                                                    column(8, br(), prettyRadioButtons("focus_axis", label = "Focus on:", choices = c("Observed", "Predicted Reported", "Predicted Reported + Unreported"), 
                                                                                       selected = "Observed", inline = TRUE)),
                                                    column(3, offset = 1, htmlOutput("text_doubling_time"))
                                                  ),
                                                  highchartOutput("highchart_cases", height = "350px") %>% withSpinner(), 
                                                  highchartOutput("highchart_deaths", height = "350px") %>% withSpinner()
                                 )
                        ),
                        tabPanel("Model Predictions", value = "tab_modelpredictions",
                                 br(), br(),
                                 div(class = "box_outputs", h4("Timeline")),
                                 plotOutput("timevis_future"),
                                 conditionalPanel("output.status_app_output == 'Locked Baseline'",
                                                  conditionalPanel("output.status_app_output == 'Locked Baseline'",
                                                                   fluidRow(
                                                                     column(6,
                                                                            div(class = "box_outputs", h4("Baseline")),
                                                                            htmlOutput("text_pct_pop_baseline") %>% withSpinner(), br(),
                                                                            htmlOutput("text_total_death_baseline") %>% withSpinner(),
                                                                     ),
                                                                     column(6,
                                                                            div(class = "box_outputs", h4("Future Scenarios")),
                                                                            htmlOutput("text_pct_pop_interventions") %>% withSpinner(), br(),
                                                                            htmlOutput("text_total_death_interventions") %>% withSpinner()
                                                                     ),
                                                                   ),
                                                                   br(),
                                                                   materialSwitch(inputId = "show_all_days", label = span(icon("eye"), 'Display all days', br(), tags$small("You can either display only one data point per week i.e. Wednesday (Default) or display all days in the plots/table (Slower)."), br(), tags$small("Either way, we display daily data.")), value = FALSE,
                                                                                  status = "danger", right = TRUE, inline = FALSE, width = "100%"),
                                                                   br(),
                                                                   prettyRadioButtons("focus_axis_dup", label = "Focus on:", choices = c("Observed", "Predicted Reported", "Predicted Reported + Unreported"),
                                                                                      selected = "Predicted Reported + Unreported", inline = TRUE),
                                                                   fluidRow(
                                                                     column(6,
                                                                            highchartOutput("highchart_cases_dual_baseline", height = "350px") %>% withSpinner(), br()
                                                                     ),
                                                                     column(6,
                                                                            highchartOutput("highchart_cases_dual_interventions", height = "350px") %>% withSpinner(), br()
                                                                     )
                                                                   ),
                                                                   prettyRadioButtons("focus_natural_death", label = "Focus on:", 
                                                                                      choices = c("No Focus", "COVID-19 Deaths"), 
                                                                                      selected = "No Focus", inline = TRUE),
                                                                   fluidRow(
                                                                     column(6,
                                                                            highchartOutput("highchart_deaths_dual_baseline", height = "350px") %>% withSpinner(), br(),
                                                                            plotOutput("plot_deaths_age_baseline") %>% withSpinner(), br(),
                                                                            plotOutput("plot_mortality_lag_baseline") %>% withSpinner(), br()
                                                                     ),
                                                                     column(6,
                                                                            highchartOutput("highchart_deaths_dual_interventions", height = "350px") %>% withSpinner(), br(),
                                                                            plotOutput("plot_deaths_age_interventions") %>% withSpinner(), br(),
                                                                            plotOutput("plot_mortality_lag_interventions") %>% withSpinner(), br()
                                                                     )
                                                                   ),
                                                                   
                                                                   prettyRadioButtons("focus_requirements", label = "Focus on:", 
                                                                                      choices = c("No Focus", "Hospital Beds", "ICU Beds", "Ventilators"), 
                                                                                      selected = "No Focus", inline = TRUE),
                                                                   fluidRow(
                                                                     column(6, 
                                                                            highchartOutput("highchart_requirements_dual_baseline", height = "350px") %>% withSpinner(), br(),
                                                                     ),
                                                                     column(6, 
                                                                            highchartOutput("highchart_requirements_dual_interventions", height = "350px") %>% withSpinner(), br(),
                                                                     )
                                                                   ),
                                                                   fluidRow(
                                                                     column(6, 
                                                                            highchartOutput("highchart_Rt_dual_baseline", height = "350px") %>% withSpinner(), br(),
                                                                     ),
                                                                     column(6, 
                                                                            highchartOutput("highchart_Rt_dual_interventions", height = "350px") %>% withSpinner(), br(),
                                                                     )
                                                                   ),
                                                                   div(class = "box_outputs", h4("Model Output Table")),
                                                                   DTOutput("table_results")
                                                                   
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
  # Hide tabs on app launch ----
  hideTab(inputId = "tabs", target = "tab_modelpredictions")
  
  # Pushbar for parameters ----
  setup_pushbar(overlay = TRUE, blur = TRUE)
  observeEvent(input$open_interventions_param, ignoreInit = TRUE, pushbar_open(id = "pushbar_parameters_interventions"))  
  observeEvent(input$close_interventions_param, pushbar_close())
  observeEvent(input$open_country_param, ignoreInit = TRUE, pushbar_open(id = "pushbar_parameters_country"))  
  observeEvent(input$close_country_param, pushbar_close())
  observeEvent(input$open_virus_param, ignoreInit = TRUE, pushbar_open(id = "pushbar_parameters_virus"))  
  observeEvent(input$close_virus_param, pushbar_close())
  observeEvent(input$open_hospital_param, ignoreInit = TRUE, pushbar_open(id = "pushbar_parameters_hospitalisation"))  
  observeEvent(input$close_hospital_param, pushbar_close())
  
  # Define reactiveValues elements ----
  population_rv <- reactiveValues(data = NULL)
  cases_rv <- reactiveValues(data = NULL)
  mort_sever_rv <- reactiveValues(data = mort_sever_default)
  status_app <- reactiveValues(status = "No Baseline")
  simul_baseline <- reactiveValues(results = NULL, baseline_available = FALSE)
  simul_interventions <- reactiveValues(results = NULL, interventions_available = FALSE)
  
  
  # START CODE V13 ----
  interventions <- reactiveValues(baseline_nb = 1, baseline_mat = tibble(NULL), future_nb = 1, future_mat = tibble(NULL))
  output$baseline_nb <- reactive(interventions$baseline_nb)
  outputOptions(output, "baseline_nb", suspendWhenHidden = FALSE)
  output$future_nb <- reactive(interventions$future_nb)
  outputOptions(output, "future_nb", suspendWhenHidden = FALSE)
  
  observeEvent(input$add_intervention_baseline, 
               interventions$baseline_nb <- min(interventions$baseline_nb + 1, nb_interventions_max))
  observeEvent(input$remove_intervention_baseline, 
               interventions$baseline_nb <- max(interventions$baseline_nb - 1, nb_interventions_min))
  observeEvent(input$add_intervention_future, 
               interventions$future_nb <- min(interventions$future_nb + 1, nb_interventions_max))
  observeEvent(input$remove_intervention_future, 
               interventions$future_nb <- max(interventions$future_nb - 1, nb_interventions_min))
  
  observe({
    interventions$baseline_mat <- tibble(index = 1:5, 
                                         intervention = c(input$baseline_intervention_1, input$baseline_intervention_2, 
                                                          input$baseline_intervention_3, input$baseline_intervention_4,
                                                          input$baseline_intervention_5),
                                         date_start = c(input$baseline_daterange_1[1], input$baseline_daterange_2[1],
                                                        input$baseline_daterange_3[1], input$baseline_daterange_4[1],
                                                        input$baseline_daterange_5[1]),
                                         date_end = c(input$baseline_daterange_1[2], input$baseline_daterange_2[2],
                                                      input$baseline_daterange_3[2], input$baseline_daterange_4[2],
                                                      input$baseline_daterange_5[2]),
                                         coverage = c(input$baseline_coverage_1, input$baseline_coverage_2,
                                                      input$baseline_coverage_3, input$baseline_coverage_4,
                                                      input$baseline_coverage_5)
    ) %>%
      filter(index <= interventions$baseline_nb)
    
    interventions$future_mat <- tibble(index = 1:5, 
                                       intervention = c(input$future_intervention_1, input$future_intervention_2, 
                                                        input$future_intervention_3, input$future_intervention_4,
                                                        input$future_intervention_5),
                                       date_start = c(input$future_daterange_1[1], input$future_daterange_2[1],
                                                      input$future_daterange_3[1], input$future_daterange_4[1],
                                                      input$future_daterange_5[1]),
                                       date_end = c(input$future_daterange_1[2], input$future_daterange_2[2],
                                                    input$future_daterange_3[2], input$future_daterange_4[2],
                                                    input$future_daterange_5[2]),
                                       coverage = c(input$future_coverage_1, input$future_coverage_2,
                                                    input$future_coverage_3, input$future_coverage_4,
                                                    input$future_coverage_5)
    ) %>%
      filter(index <= interventions$future_nb)
  })
  # END CODE V13 ----
  
  
  
  # Manage population and cases data reactive values ----
  observeEvent(input$country_demographic, if(input$country_demographic != "-- Own Value ---"){
    population_rv$data <- population %>% filter(country == input$country_demographic)
  })
  observeEvent(input$country_cases, if(input$country_cases != "-- Own Value ---"){
    cases_rv$data <- cases %>% filter(country == input$country_cases) %>%
      select(-country)
  })
  
  
  # Source code to generate outputs ----
  file_list <- list.files(path = "./www/outputs", pattern = "*.R")
  for (file in file_list) source(paste0("./www/outputs/", file), local = TRUE)$value
  
  # To show/hide elements of the App depending on the status ----
  output$status_app_output <- reactive({
    return(status_app$status)
  })
  outputOptions(output, "status_app_output", suspendWhenHidden = FALSE)
  
  # Process on uploading a file of data/parameters
  observeEvent(input$own_data, {
    file_path <- input$own_data$datapath
    
    # Cases
    dta <- read_excel(file_path, sheet = "Cases")
    names(dta) <- c("date", "cases", "deaths")
    
    cases_rv$data <- dta %>%
      mutate(date = as.Date(date), cumulative_death = cumsum(deaths)) %>%
      as.data.frame()
    
    updatePickerInput(session, inputId = "country_cases", choices = c("-- Own Value ---", countries_cases), selected = "-- Own Value ---")
    updatePickerInput(session, inputId = "country_demographic", choices = c("-- Own Value ---", countries_demographic), selected = "-- Own Value ---")
    
    
    # Severity/Mortality
    dta <- read_excel(file_path, sheet = "Severity-Mortality") 
    names(dta) <- c("age_category",	"ifr",	"ihr")
    
    mort_sever_rv$data <- dta %>%
      mutate(ihr = ihr/100) %>% # starting unit should be % - scaling to a value between 0 and 1
      mutate(ifr = ifr/max(ifr))  # starting unit should be % - scaling to a value between 0 and 1
    
    # Population
    dta <- read_excel(file_path, sheet = "Population")
    names(dta) <- c("age_category",	"pop",	"birth",	"death")
    
    population_rv$data <- dta %>%
      transmute(country = NA, age_category, pop, birth, death)
    
    updatePickerInput(session, inputId = "country_demographic", selected = "-- Own Value ---")
    
    
    # Parameters
    param <- bind_rows(read_excel(file_path, sheet = "Parameters"),
                       read_excel(file_path, sheet = "Country Area Param"),
                       read_excel(file_path, sheet = "Virus Param"),
                       read_excel(file_path, sheet = "Hospitalisation Param"),
                       read_excel(file_path, sheet = "Interventions Param")) %>%
      mutate(Value_Date = as.Date(Value_Date)) %>%
      drop_na(Parameter)
    
    # Update all sliders
    if(!is_empty(param$Parameter[param$Type == 'slider'])) {
      for (input_excel in param$Parameter[param$Type == 'slider']){
        updateSliderInput(session = session, inputId = input_excel, value = param$Value[param$Parameter == input_excel])
      }}
    
    # Update all numeric values
    if(!is_empty(param$Parameter[param$Type == 'numeric'])) {
      for (input_excel in param$Parameter[param$Type == 'numeric']){
        updateNumericInput(session = session, inputId = input_excel, value = param$Value[param$Parameter == input_excel])
      }}
    
    # Update month text slider
    if(!is_empty(param$Parameter[param$Parameter == 'phi'])) {
      updateSliderTextInput(session = session, inputId = "phi", selected = month.name[param$Value[param$Parameter == "phi"]])
    }
    
    # Update date range of simulation
    if(!is_empty(param$Parameter[param$Type == 'date_range_simul'])) {
      updateDateRangeInput(session, inputId = "date_range", start = param$Value_Date[param$Parameter == "date_range_simul_start"], 
                           end = param$Value_Date[param$Parameter == "date_range_simul_end"])
    }
    
    # Update social contact
    if(!is_empty(param$Parameter[param$Type == 'picker'])) {
      updatePickerInput(session, inputId = "country_contact", selected = param$Value_Country[param$Parameter == "country_contact"])
    }
    
    # START CODE V13 ----
    interventions_excel <- read_excel(file_path, sheet = "Interventions")
    names(interventions_excel) <- c("intervention", "date_start", "date_end", "coverage", "apply_to")
    interventions_excel <- interventions_excel %>%
      mutate(date_start = as.Date(date_start), date_end = as.Date(date_end))
    
    
    interventions_excel_baseline <- interventions_excel %>% 
      filter(apply_to == "Baseline + Future Scenario")
    interventions_excel_future <- interventions_excel %>% 
      filter(apply_to == "Future Scenario")
    
    interventions$baseline_nb <- min(interventions_excel_baseline %>% nrow(), 50)
    interventions$future_nb <- min(interventions_excel_future %>% nrow(), 50)
    
    for (i in 1:interventions$baseline_nb) {
      updateSelectInput(session, paste0("baseline_intervention_", i), selected = interventions_excel_baseline[[i, "intervention"]])
      updateDateRangeInput(session, paste0("baseline_daterange_", i), start = interventions_excel_baseline[[i, "date_start"]], end = interventions_excel_baseline[[i, "date_end"]])
      updateNumericInput(session, paste0("baseline_coverage_", i), value = interventions_excel_baseline[[i, "coverage"]])
    }
    for (i in 1:interventions$future_nb) {
      updateSelectInput(session, paste0("future_intervention_", i), selected = interventions_excel_future[[i, "intervention"]])
      updateDateRangeInput(session, paste0("future_daterange_", i), start = interventions_excel_future[[i, "date_start"]], end = interventions_excel_future[[i, "date_end"]])
      updateNumericInput(session, paste0("future_coverage_", i), value = interventions_excel_future[[i, "coverage"]])
    }
    # END CODE V13 ----
  })
  
  # Process on "reset_baseline" ----
  observeEvent(input$reset_baseline, {
    simul_baseline$results <- NULL
    simul_baseline$baseline_available <- FALSE
    simul_interventions$results <- NULL
    simul_interventions$interventions_available <- FALSE
    status_app$status <- "No Baseline"
    hideTab(inputId = "tabs", target = "tab_modelpredictions")
  })
  
  # Process on "run_baseline" ----
  observeEvent(input$run_baseline, {
    showNotification(span(h4(icon("hourglass-half"), "Running the Baseline..."), "typically runs in 10 secs."),
                     duration = NULL, type = "message", id = "model_run_notif")
    
    # Reset simul_interventions and elements of the UI
    simul_interventions$results <- NULL
    
    # source("./www/model.R", local = TRUE)
    # if(! input$lockdown_low_switch) parameters["lockdown_low_on"] <- 10e5
    # if(! input$lockdown_mid_switch) parameters["lockdown_mid_on"] <- 10e5
    # if(! input$lockdown_high_switch) parameters["lockdown_high_on"] <- 10e5
    # if(! input$selfis_switch) parameters["selfis_on"] <- 10e5
    # if(! input$dist_switch) parameters["dist_on"] <- 10e5
    # if(! input$hand_switch) parameters["hand_on"] <- 10e5
    # if(! input$work_switch) parameters["work_on"] <- 10e5
    # if(! input$school_switch) parameters["school_on"] <- 10e5
    # if(! input$cocoon_switch) parameters["cocoon_on"] <- 10e5
    # if(! input$travelban_switch) parameters["travelban_on"] <-10e5
    # if(! input$screen_switch) parameters["screen_on"] <- 10e5
    # if(! input$quarantine_switch) parameters["quarantine_on"] <- 10e5
    # if(! input$vaccination_switch) parameters["vaccine_on"] <- 10e5
    # 
    # out <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = parameters)
    # simul_baseline$results <- process_ode_outcome(out)
    
    removeNotification(id = "model_run_notif", session = session)
    status_app$status <- "Ok Baseline"
    simul_baseline$baseline_available <- TRUE
    shiny_simul_baseline <<- simul_baseline$results  # for development only
  })
  
  observeEvent(input$validate_baseline, {
    status_app$status <- "Validated Baseline"
    showTab(inputId = "tabs", target = "tab_modelpredictions")
    updateNavbarPage(session, "tabs",selected = "tab_modelpredictions")
  }
  )
  
  # Process on "run_interventions" ----
  observeEvent(input$run_interventions, {
    showNotification(span(h4(icon("hourglass-half"), "Running Future Scenarios..."), "typically runs in 10 secs."),
                     duration = NULL, type = "message", id = "run_interventions_notif")
    
    # source("./www/model.R", local = TRUE)
    # if(! input$lockdown_low_switch) parameters["lockdown_low_on"]<-10e5
    # if(! input$lockdown_mid_switch) parameters["lockdown_mid_on"]<-10e5
    # if(! input$lockdown_high_switch) parameters["lockdown_high_on"]<-10e5
    # if(! input$selfis_switch) parameters["selfis_on"] <- 10e5
    # if(! input$dist_switch) parameters["dist_on"] <- 10e5
    # if(! input$hand_switch) parameters["hand_on"] <- 10e5
    # if(! input$work_switch) parameters["work_on"] <- 10e5
    # if(! input$school_switch) parameters["school_on"] <- 10e5
    # if(! input$cocoon_switch) parameters["cocoon_on"] <- 10e5
    # if(! input$travelban_switch) parameters["travelban_on"] <-10e5
    # if(! input$screen_switch) parameters["screen_on"] <- 10e5
    # if(! input$quarantine_switch) parameters["quarantine_on"] <- 10e5
    # if(! input$vaccination_switch) parameters["vaccine_on"] <- 10e5
    # 
    # out <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = parameters)
    # simul_interventions$results <- process_ode_outcome(out)
    
    removeNotification(id = "run_interventions_notif", session = session)
    status_app$status <- "Locked Baseline"
    simul_interventions$interventions_available <- TRUE
    shiny_simul_interventions <<- simul_interventions$results  # for development only
  })
  
  
  # Export Data ----
  results_aggregated <- reactive({
    
    dta_baseline <- tibble(
      date = simul_baseline$results$time,
      baseline_daily_incidence = simul_baseline$results$daily_incidence,
      baseline_daily_total_cases = simul_baseline$results$daily_total_cases,
      baseline_required_beds = simul_baseline$results$required_beds,
      baseline_cum_mortality = simul_baseline$results$cum_mortality,
      baseline_hospital_surge_beds = simul_baseline$results$hospital_surge_beds,
      baseline_icu_beds = simul_baseline$results$icu_beds,
      baseline_ventilators = simul_baseline$results$ventilators,
      baseline_death_treated_hospital = simul_baseline$results$death_treated_hospital,
      baseline_death_treated_icu = simul_baseline$results$death_treated_icu,
      baseline_death_treated_ventilator = simul_baseline$results$death_treated_ventilator,
      baseline_death_untreated_hospital = simul_baseline$results$death_untreated_hospital,
      baseline_death_untreated_icu = simul_baseline$results$death_untreated_icu,
      baseline_death_untreated_ventilator = simul_baseline$results$death_untreated_ventilator)
    
    dta <- left_join(dta_baseline, 
                     cases_rv$data %>% rename(input_cases = cases,
                                              input_deaths = deaths,
                                              input_cumulative_death = cumulative_death), by = "date")
    
    if(simul_interventions$interventions_available){ 
      
      dta_interventions <- tibble(
        date = simul_interventions$results$time,
        future_scenario_daily_incidence = simul_interventions$results$daily_incidence,
        future_scenario_daily_total_cases = simul_interventions$results$daily_total_cases,
        future_scenario_required_beds = simul_interventions$results$required_beds,
        future_scenario_cum_mortality = simul_interventions$results$cum_mortality,
        future_scenario_hospital_surge_beds = simul_interventions$results$hospital_surge_beds,
        future_scenario_icu_beds = simul_interventions$results$icu_beds,
        future_scenario_ventilators = simul_interventions$results$ventilators,
        future_scenario_death_treated_hospital = simul_interventions$results$death_treated_hospital,
        future_scenario_death_treated_icu = simul_interventions$results$death_treated_icu,
        future_scenario_death_treated_ventilator = simul_interventions$results$death_treated_ventilator,
        future_scenario_death_untreated_hospital = simul_interventions$results$death_untreated_hospital,
        future_scenario_death_untreated_icu = simul_interventions$results$death_untreated_icu,
        future_scenario_death_untreated_ventilator = simul_interventions$results$death_untreated_ventilator)
      
      dta <- left_join(dta, dta_interventions, by = "date") }
    
    if (!input$show_all_days) dta <- dta %>% filter(wday(date) == 2)
    
    return(dta)
  })
}

# Run the App ----
shinyApp(ui = ui, server = server)


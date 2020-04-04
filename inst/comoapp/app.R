# COVID19 App

# Load packages
source("./www/load_packages.R")

# Import/Process data
source("./www/prepare_data.R")

# Define UI ----
ui <- function(request) {
  fluidPage(
    theme = shinytheme("flatly"),
    includeCSS("./www/styles.css"),
    pushbar_deps(),
    chooseSliderSkin('HTML5'),
    title = "CoMo COVID-19 App",
    div(id = "float_feedback_process",
        htmlOutput("feedback_process"),
        
        conditionalPanel("output.show_results_interventions && input.tabs == 'tab_modelpredictions'",
                         downloadButton("export", label = "Download Results (.csv)", class="btn btn-primary")
        ),
    ),
    navbarPage(NULL, id = "tabs", windowTitle = "CoMo COVID-19 App", collapsible = TRUE, inverse = FALSE,
               tabPanel("Welcome", value = "tab_welcome",
                        h3("CoMo COVID-19 App"),
                        h4("v11.3"),
                        br(),
                        fluidRow(
                          column(5, 
                                 div(class = "box_outputs",
                                     h4("Important Disclaimer:")
                                 ),
                                 includeMarkdown("./www/markdown/disclaimer.md")
                          ),
                          column(5, offset = 2,
                                 div(class = "box_outputs",
                                     h4("Sources of Data:")
                                 ),
                                 includeMarkdown("./www/markdown/about_country_data.md"),
                                 includeMarkdown("./www/markdown/about_data.md"),
                          )
                        )
               ),
               tabPanel("Visual Fit", value = "tab_visualfit",
                        fluidRow(
                          column(4, 
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Parameters",
                                                      conditionalPanel("! output.show_results_interventions",
                                                                       div(class = "baseline_left",
                                                                           br(), 
                                                                           p("Use customised data/ update default parameters:", a("download the template", href = "https://www.dropbox.com/s/eslw1x267iq6p5p/Template_data_comomodel.xlsx?dl=1", target = "_blank"), 
                                                                             ", edit it and upload below. (Any subsequent input in the App will overseed provided input value)."),
                                                                           fileInput("own_data", label = NULL, accept = ".xlsx", multiple = FALSE),
                                                                           bsButton("open_country_param", label = "Country/Area Data", icon = icon('cog'), style = "primary", type = "action", value = FALSE, 
                                                                                    block = TRUE), br(),
                                                                           bsButton("open_virus_param", label = "Virus Parameters", icon = icon('cog'), style = "primary", type = "action", value = FALSE, 
                                                                                    block = TRUE), br(), 
                                                                           bsButton("open_hospitalisation_param", label = "Hospitalisation Parameters", icon = icon('cog'), style = "primary", type = "action", value = FALSE, 
                                                                                    block = TRUE), br(), 
                                                                           sliderInput("p", label = "Probability of infection given contact:", min = 0, max = 0.2, step = 0.001,
                                                                                       value = 0.049, ticks = FALSE),
                                                                           sliderInput("report", label = span("Percentage of all", strong(" asymptomatic infections "), "that are reported:"), min = 0, max = 100, step = 0.1,
                                                                                       value = 2.5, post = "%", ticks = FALSE),
                                                                           sliderInput("reportc", label = span("Percentage of all", strong(" symptomatic infections "), "that are reported:"), min = 0, max = 100, step = 0.1,
                                                                                       value = 5, post = "%", ticks = FALSE),
                                                                           
                                                                           dateRangeInput("date_range", label = "Range of dates", start = "2020-02-10", end = "2020-09-01"),
                                                                           br(), 
                                                                           htmlOutput("feedback_choices"),
                                                                           div(class = "floating-button",
                                                                               actionButton("run_baseline", "Run Baseline", class="btn btn-success")
                                                                           ),
                                                                           hr()
                                                                       ),
                                                                       source("./www/pushbar_parameters_country.R", local = TRUE)[1],
                                                                       source("./www/pushbar_parameters_virus.R", local = TRUE)[1],
                                                                       source("./www/pushbar_parameters_hospitalisation.R", local = TRUE)[1]
                                                      ),
                                                      conditionalPanel("output.show_results_interventions", 
                                                                       br(),
                                                                       bsButton("reset_baseline", label = "Reset the Baseline", icon = icon('cog'), style = "primary", type = "action", value = FALSE, 
                                                                                block = TRUE)
                                                      )
                                             ),
                                             tabPanel("Past/Current Interventions",
                                                      HTML("TBC")
                                             )
                                             
                                 )
                          ),
                          column(8,
                                 conditionalPanel("output.show_results_baseline",
                                                  br(), br(), br(), br(),
                                                  fluidRow(
                                                    column(8, br(), prettyRadioButtons("focus_axis", label = "Focus on:", choices = c("Observed", "Predicted Reported", "Predicted Reported + Unreported"), 
                                                                                       selected = "Observed", inline = TRUE)),
                                                    column(3, offset = 1, htmlOutput("text_doubling_time"))
                                                  ),
                                                  highchartOutput("highchart_confirmed", height = "300px"), 
                                                  highchartOutput("highchart_mortality", height = "300px")
                                 )
                          )
                        )
               ),
               tabPanel("Model Predictions", value = "tab_modelpredictions",
                        fluidRow(
                          column(4, 
                                 div(class = "interventions_left",
                                     h4("Lockdown:"),
                                     source("./www/interventions/lockdown.R", local = TRUE)$value,
                                     h4("Interventions available:"),
                                     # Self Isolation ----
                                     source("./www/interventions/selfisolation.R", local = TRUE)$value,
                                     bsPopover("interventions_selfis", title='Self-isolation', "<p>Coverages gives the proportion of people whom self-isolate after testing positive for coronavirus. The efficacy indicates how many less infectious contacts isolating people have (across all contact matrices).</p>", 
                                               "top", trigger='hover', options = list(container = "body")),
                                     # Social Distancing ----
                                     source("./www/interventions/socialdistancing.R", local = TRUE)$value,
                                     bsPopover("interventions_dist", title='Social distancing', "<p>Coverage reflects the proportion of people that reduce their societal contacts (excluding those at home, work and school). Efficacy changes the percent reduction in those contacts.</p>", 
                                               "top", trigger='hover', options = list(container = "body")),
                                     # Handwashing ----
                                     source("./www/interventions/handwashing.R", local = TRUE)$value,
                                     bsPopover("interventions_hand", title='Hand washing', "<p>This indicates improvements in personal hygiene and reduction in risk behaviours (touching face, nose, mouth). It reduces the risk of infection by efficacy for the duration of the intervention.</p>", 
                                               "top", trigger='hover', options = list(container = "body")),
                                     # Working from Home ----
                                     source("./www/interventions/work.R", local = TRUE)$value,
                                     bsPopover("interventions_work", title='Working at home', "<p>Sets the proportion of workers working from home. Those who don’t, have a reduction in contacts at work defined by efficacy. Those who work at home have increased contacts at home defined by w2h.</p>", 
                                               "top", trigger='hover', options = list(container = "body")),
                                     # School Closure ----
                                     source("./www/interventions/school.R", local = TRUE)$value,
                                     bsPopover("interventions_school", title='School closure', "<p>We assume all schools close at the same time. Efficacy defines the reduction of contacts between school children when school is closed. Children at home have increased home contacts given by s2h.</p>", 
                                               "top", trigger='hover', options = list(container = "body")),
                                     # Cocooning the elderly ----
                                     source("./www/interventions/cocoon.R", local = TRUE)$value,
                                     bsPopover("interventions_cocoon", title='Cocooning the elderly', "<p>Defining an age cut-off, this intervention is designed to isolate a proportion (coverage) of the elderly population and reduce their overall contacts by efficacy.</p>", 
                                               "top", trigger='hover', options = list(container = "body")),
                                     # Travel Ban ----
                                     source("./www/interventions/travel.R", local = TRUE)$value,
                                     bsPopover("interventions_travelban", title='Travel ban', "<p>Reduced the number of imported cases per day by a percentage given by efficacy.</p>", 
                                               "bottom", trigger='hover', options = list(container = "body")),
                                     # Quarantine ----
                                     source("./www/interventions/quarantine.R", local = TRUE)$value,
                                     bsPopover("interventions_quarantine", title='Quarantine', "<p>This indicates how many people will self-isolate for X days if a person they live with tests positive. As such, coverage depends on the number of people isolating and the number of persons per household. During their isolation period, these people will have increased contacts at home and reduced societal contacts.</p>", 
                                               "bottom", trigger='hover', options = list(container = "body")),
                                     # Vaccination ----
                                     h4("Interventions not yet available:"),
                                     source("./www/interventions/vaccination.R", local = TRUE)$value,
                                     br(),
                                     # ),
                                     div(class = "floating-button",
                                         actionButton("run_interventions", "Run Interventions", class="btn btn-success")
                                     ),
                                     hr()
                                 )
                          ),
                          column(8,
                                 br(), br(),
                                 div(class = "box_outputs", h4("Timeline")),
                                 timevisOutput("timeline"),
                                 conditionalPanel("output.show_results_interventions",
                                                  fluidRow(
                                                    column(6, 
                                                           div(class = "box_outputs", h4("Baseline")),
                                                           htmlOutput("text_baseline_pct_pop"), br(),
                                                           htmlOutput("text_baseline_total_death"),
                                                    ),
                                                    column(6, 
                                                           div(class = "box_outputs", h4("Interventions")),
                                                           htmlOutput("text_interventions_pct_pop"), br(),
                                                           htmlOutput("text_interventions_total_death")
                                                    ),
                                                  ),
                                                  br(),
                                                  prettyRadioButtons("focus_axis_dup", label = "Focus on:", choices = c("Observed", "Predicted Reported", "Predicted Reported + Unreported"), 
                                                                     selected = "Predicted Reported + Unreported", inline = TRUE),
                                                  highchartOutput("highchart_confirmed_dup", height = "300px"), 
                                                  highchartOutput("highchart_mortality_dup", height = "300px"),
                                                  highchartOutput("highchart_ICU", height = "300px"), br()
                                 )
                          )
                        )
               )
    )
  )
}

# Define server ----
server <- function(input, output, session) {
  # On deployment only:
  # Stop the shiny app when the browser window is closed
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # Hide tabs on app launch ----
  hideTab(inputId = "tabs", target = "tab_modelpredictions")
  
  # Pushbar for parameters ----
  setup_pushbar(overlay = TRUE, blur = TRUE)
  observeEvent(input$open_country_param, ignoreInit = TRUE, pushbar_open(id = "pushbar_parameters_country"))  
  observeEvent(input$close_country_param, pushbar_close())
  observeEvent(input$open_virus_param, ignoreInit = TRUE, pushbar_open(id = "pushbar_parameters_virus"))  
  observeEvent(input$close_virus_param, pushbar_close())
  observeEvent(input$open_hospitalisation_param, ignoreInit = TRUE, pushbar_open(id = "pushbar_parameters_hospitalisation"))  
  observeEvent(input$close_hospitalisation_param, pushbar_close())
  
  # Reactive elements
  population_rv <- reactiveValues(data = NULL)
  cases_rv <- reactiveValues(data = NULL)
  mort_sever <- reactiveValues(data = mort_sever_default)
  simul_baseline <- reactiveValues(results = NULL, baseline_available = FALSE)
  simul_interventions <- reactiveValues(results = NULL, interventions_available = FALSE)
  
  # Manage cases data reactive values
  observeEvent(input$country, if(input$country != "-- Own Value ---"){
               population_rv$data <- population %>% filter(country == input$country)
               
               shiny_population_rv_data_1 <<- population_rv$data
  })
  
  observeEvent(input$country_cases, if(input$country != "-- Own Value ---"){
               cases_rv$data <- cases %>% filter(country == input$country_cases) %>%
                 select(-country)
               
               shiny_cases_rv_data_1 <<- cases_rv$data
  })
  
  
  
  # Outputs
  # Source code to generate outputs ----
  file_list <- list.files(path = "./www/outputs", pattern = "*.R")
  for (file in file_list) source(paste0("./www/outputs/", file), local = TRUE)$value
  
  # To show/hide results ----
  output$show_results_baseline <- reactive({
    return(simul_baseline$baseline_available & !simul_interventions$interventions_available)
  })
  outputOptions(output, "show_results_baseline", suspendWhenHidden = FALSE)
  
  output$show_results_interventions <- reactive({
    return(simul_interventions$interventions_available)
  })
  outputOptions(output, "show_results_interventions", suspendWhenHidden = FALSE)
  
  # Process on uploading a file of data/parameters
  observeEvent(input$own_data, {
    file_path <- input$own_data$datapath
    
    # file_path <- "/Users/olivier/Dropbox/Linked files — do not edit/comomodel/Template_data_comomodel.xlsx"
    
    # Cases
    cases_rv$data <- read_excel(file_path, sheet = "Cases") %>%
      mutate(date = as.Date(date), cumulative_death = cumsum(deaths)) %>%
      as.data.frame()
    
    shiny_cases_rv_data <<- cases_rv$data
    
    updatePickerInput(session, inputId = "country_cases", selected = "-- Own Value ---")

    
    # Severity/Mortality
    mort_sever$data <- read_excel(file_path, sheet = "Severity-Mortality") %>%
      rename(ihr = severity, ifr = mortality) %>%
      mutate(ihr = 4*ihr) %>%
      mutate(ifr = ifr/max(ifr))
    
    # Population
    population_rv$data <- read_excel(file_path, sheet = "Population") %>%
      transmute(country = NA, age_category, pop, birth, death)
    
    shiny_population_rv_data <<- population_rv$data
    updatePickerInput(session, inputId = "country", selected = "-- Own Value ---")
    
    # Parameters
    param <- bind_rows(read_excel(file_path, sheet = "Country Area Parameters"),
                            read_excel(file_path, sheet = "Virus Parameters"),
                            read_excel(file_path, sheet = "Hospitalisation Parameters"))

    shiny_param <<- param

    # Update all sliders with one value
    if(!is_empty(param$Parameter[param$Type == 'slider'])) {
      for (input_excel in param$Parameter[param$Type == 'slider']){
        updateSliderInput(session = session, inputId = input_excel, value = param$Value[param$Parameter == input_excel])
      }}

    # Update all numeric values
    if(!is_empty(param$Parameter[param$Type == 'numeric'])) {
      for (input_excel in param$Parameter[param$Type == 'numeric']){
        updateNumericInput(session = session, inputId = input_excel, value = param$Value[param$Parameter == input_excel])
      }}
  })
  
  # Process on "reset_baseline" ----
  observeEvent(input$reset_baseline, {
    simul_baseline$results <- NULL
    simul_baseline$baseline_available <- FALSE
    simul_interventions$results <- NULL
    simul_interventions$interventions_available <- FALSE
    updateMaterialSwitch(session, "selfis_switch", value = FALSE)
    updateMaterialSwitch(session, "dist_switch", value = FALSE)
    updateMaterialSwitch(session, "hand_switch", value = FALSE)
    updateMaterialSwitch(session, "work_switch", value = FALSE)
    updateMaterialSwitch(session, "school_switch", value = FALSE)
    updateMaterialSwitch(session, "cocoon_switch", value = FALSE)
    updateMaterialSwitch(session, "travelban_switch", value = FALSE)
    updateMaterialSwitch(session, "screen_switch", value = FALSE)
    updateMaterialSwitch(session, "vaccination_switch", value = FALSE)
    updateMaterialSwitch(session, "quarantine_switch", value = FALSE)
    hideTab(inputId = "tabs", target = "tab_modelpredictions")
  })
  
  # Process on "run_baseline" ----
  observeEvent(input$run_baseline, {
    showNotification(span(h4(icon("hourglass-half"), "Running the Baseline..."), "typically runs in 10 secs."),
                     duration = NULL, type = "message", id = "model_run_notif")
    
    # Reset simul_interventions and elements of the UI
    simul_interventions$results <- NULL
    simul_interventions$interventions_available <- FALSE
    updateMaterialSwitch(session, "selfis_switch", value = FALSE)
    updateMaterialSwitch(session, "dist_switch", value = FALSE)
    updateMaterialSwitch(session, "hand_switch", value = FALSE)
    updateMaterialSwitch(session, "work_switch", value = FALSE)
    updateMaterialSwitch(session, "school_switch", value = FALSE)
    updateMaterialSwitch(session, "cocoon_switch", value = FALSE)
    updateMaterialSwitch(session, "travelban_switch", value = FALSE)
    updateMaterialSwitch(session, "screen_switch", value = FALSE)
    updateMaterialSwitch(session, "vaccination_switch", value = FALSE)
    updateMaterialSwitch(session, "quarantine_switch", value = FALSE)
    updateMaterialSwitch(session, "lockdown_low_switch", value = FALSE)
    updateMaterialSwitch(session, "lockdown_mid_switch", value = FALSE)
    updateMaterialSwitch(session, "lockdown_high_switch", value = FALSE)
    
    source("./www/model.R", local = TRUE)
    
    # Make sure that interventions are not used
    parameters["selfis_on"]<-10e5
    parameters["dist_on"]<-10e5
    parameters["hand_on"]<-10e5
    parameters["work_on"]<-10e5
    parameters["school_on"]<-10e5
    parameters["cocoon_on"]<-10e5
    parameters["travelban_on"]<-10e5
    parameters["screen_on"]<-10e5
    parameters["vaccine_on"]<-10e5
    parameters["quarantine_on"]<-10e5
    parameters["lockdown_low_on"]<-10e5
    parameters["lockdown_mid_on"]<-10e5
    parameters["lockdown_high_on"]<-10e5
    
    
    out <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = parameters)
    simul_baseline$results <- process_ode_outcome(out)
    simul_baseline$baseline_available <- TRUE
    
    removeNotification(id = "model_run_notif", session = session)
    
    showTab(inputId = "tabs", target = "tab_modelpredictions")
  })
  
  # Process on "run_interventions" ----
  observeEvent(input$run_interventions, {
    showNotification(span(h4(icon("hourglass-half"), "Running the Interventions..."), "typically runs in 10 secs."),
                     duration = NULL, type = "message", id = "run_interventions_notif")
    
    source("./www/model.R", local = TRUE)
    if(! input$selfis_switch) parameters["selfis_on"] <- 10e5
    if(! input$dist_switch) parameters["dist_on"] <- 10e5
    if(! input$hand_switch) parameters["hand_on"] <- 10e5
    if(! input$work_switch) parameters["work_on"] <- 10e5
    if(! input$school_switch) parameters["school_on"] <- 10e5
    if(! input$cocoon_switch) parameters["cocoon_on"] <- 10e5
    if(! input$travelban_switch) parameters["travelban_on"] <-10e5
    if(! input$screen_switch) parameters["screen_on"] <- 10e5
    if(! input$vaccination_switch) parameters["vaccine_on"] <- 10e5
    if(! input$quarantine_switch) parameters["quarantine_on"] <- 10e5
    if(! input$lockdown_low_switch) parameters["lockdown_low_on"]<-10e5
    if(! input$lockdown_mid_switch) parameters["lockdown_mid_on"]<-10e5
    if(! input$lockdown_high_switch) parameters["lockdown_high_on"]<-10e5
    
    out <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = parameters)
    simul_interventions$results <- process_ode_outcome(out)
    simul_interventions$interventions_available <- TRUE
    
    removeNotification(id = "run_interventions_notif", session = session)
  })
  
  
  # Export Data ----
  results_aggregated <- reactive({
    dta <- tibble(
      date = simul_baseline$results$time,
      baseline_daily_incidence = round(simul_baseline$results$dailyinc0),
      baseline_cumulative_total_cases = round(simul_baseline$results$daily_total_cases),
      baseline_bed_requirement = round(simul_baseline$results$previcureq0),
      baseline_saturation_bed = round(simul_baseline$results$saturation),
      baseline_cumulative_death = round(simul_baseline$results$cmortality0))
    
    dta <- left_join(dta, 
                     cases_rv$data %>% rename(input_country = country,
                                              input_cases = cases,
                                              input_deaths = deaths,
                                              input_cumulative_death = cumulative_death), by = "date")
    
    if(simul_interventions$interventions_available){ 
      
      dta_inter <- tibble(
        date = simul_interventions$results$time,
        interventions_daily_incidence = round(simul_interventions$results$dailyinc0),
        interventions_cumulative_total_cases = round(simul_interventions$results$daily_total_cases),
        interventions_bed_requirement = round(simul_interventions$results$previcureq0),
        interventions_saturation_bed = round(simul_interventions$results$saturation),
        interventions_cumulative_death = round(simul_interventions$results$cmortality0))
      
      dta <- left_join(dta, dta_inter, by = "date") }
    
    return(dta)
  })
  
  output$export <- downloadHandler(
    filename = paste0("Results_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(results_aggregated(), file, row.names = FALSE)
    }
  )
}

# Run the App ----
shinyApp(ui = ui, server = server)
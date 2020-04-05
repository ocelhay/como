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
        
        conditionalPanel("output.status_app_output == 'Locked Baseline' && input.tabs == 'tab_modelpredictions'",
                         downloadButton("export", label = "Download Results (.csv)", class="btn btn-primary")
        ),
    ),
    
    
    fluidRow(
      # column left ----
      column(4, 
             conditionalPanel("input.tabs == 'tab_welcome'",
                              br()
             ),
             conditionalPanel("input.tabs != 'tab_welcome'",
                              br(), br(), br(), br(), br(), br(),
                              conditionalPanel("output.status_app_output == 'No Baseline' | output.status_app_output == 'Ok Baseline'", 
                                               p("Use customised data/ update default parameters:", a("download the template", href = "https://www.dropbox.com/s/eslw1x267iq6p5p/Template_data_comomodel.xlsx?dl=1", target = "_blank"), 
                                                 ", edit it and upload below."),
                                               fileInput("own_data", label = NULL, accept = ".xlsx", multiple = FALSE),
                                               hr()
                              ),
                              fluidRow(
                                column(6,
                                       conditionalPanel("output.status_app_output == 'No Baseline' | output.status_app_output == 'Ok Baseline'",
                                                        br(), 
                                                        div(class = "baseline_left",
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
                                                            br()
                                                        ),
                                                        source("./www/pushbar_parameters_country.R", local = TRUE)[1],
                                                        source("./www/pushbar_parameters_virus.R", local = TRUE)[1],
                                                        source("./www/pushbar_parameters_hospitalisation.R", local = TRUE)[1]
                                       ),
                                       conditionalPanel("(output.status_app_output == 'Validated Baseline' | output.status_app_output == 'Locked Baseline')  && input.tabs == 'tab_visualfit'", 
                                                        br(),
                                                        bsButton("reset_baseline", label = "Reset the Baseline", icon = icon('cog'), style = "primary", type = "action", value = FALSE, 
                                                                 block = TRUE)
                                       ),
                                ),
                                column(6,
                                       div(class = "interventions_left",
                                           conditionalPanel("! ((output.status_app_output == 'Validated Baseline' || output.status_app_output == 'Locked Baseline') && input.tabs == 'tab_visualfit')",
                                                            h4("Interventions Available:"),
                                                            # Lockdown ----
                                                            source("./www/interventions/lockdown.R", local = TRUE)$value,
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
                                                            h4("Not Yet Available:"),
                                                            source("./www/interventions/vaccination.R", local = TRUE)$value
                                           )
                                       )
                                )
                              ),
                              conditionalPanel("input.tabs == 'tab_visualfit' & !output.status_app_output == 'Locked Baseline'",
                                               htmlOutput("feedback_choices")
                              ),
                              
                              div(class = "marginleft",
                                  conditionalPanel("output.status_app_output == 'No Baseline' | output.status_app_output == 'Ok Baseline'",
                                                   actionButton("run_baseline", "Run Baseline", class="btn btn-success"),
                                                   br(),
                                  ),
                                  conditionalPanel("output.status_app_output == 'Ok Baseline'",
                                                   p("Satisfactory visual fit?"),
                                                   actionButton("validate_baseline", span(icon("thumbs-up"), " Validate Baseline"), class="btn btn-success"),
                                  ),
                                  conditionalPanel("output.status_app_output == 'Validated Baseline' && input.tabs == 'tab_modelpredictions'",
                                                   actionButton("run_interventions", "Run Interventions", class="btn btn-success")
                                  )
                              )
             )
      ),
      
      # column right ----
      column(8,
             navbarPage(NULL, id = "tabs", windowTitle = "CoMo COVID-19 App", collapsible = TRUE, inverse = FALSE,
                        tabPanel("Welcome", value = "tab_welcome",
                                 h3("CoMo COVID-19 App"),
                                 h4("v11.4"),
                                 br(),
                                 fluidRow(
                                   column(6, 
                                          div(class = "box_outputs",
                                              h4("Important Disclaimer:")
                                          ),
                                          includeMarkdown("./www/markdown/disclaimer.md")
                                   ),
                                   column(6,
                                          div(class = "box_outputs",
                                              h4("Sources of Data:")
                                          ),
                                          includeMarkdown("./www/markdown/about_country_data.md"),
                                          includeMarkdown("./www/markdown/about_data.md"),
                                   )
                                 )
                        ),
                        tabPanel("Visual Calibration", value = "tab_visualfit",
                                 
                                 conditionalPanel("output.status_app_output == 'Ok Baseline' | output.status_app_output == 'Validated Baseline'",
                                                  br(), br(), br(), br(),
                                                  fluidRow(
                                                    column(8, br(), prettyRadioButtons("focus_axis", label = "Focus on:", choices = c("Observed", "Predicted Reported", "Predicted Reported + Unreported"), 
                                                                                       selected = "Observed", inline = TRUE)),
                                                    column(3, offset = 1, htmlOutput("text_doubling_time"))
                                                  ),
                                                  highchartOutput("highchart_confirmed", height = "300px"), 
                                                  highchartOutput("highchart_mortality", height = "300px")
                                 )
                        ),
                        tabPanel("Model Predictions", value = "tab_modelpredictions",
                                 br(), br(),
                                 div(class = "box_outputs", h4("Timeline")),
                                 timevisOutput("timeline"),
                                 conditionalPanel("output.status_app_output == 'Locked Baseline'",
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
  
  # Define reactiveValues elements ----
  population_rv <- reactiveValues(data = NULL)
  cases_rv <- reactiveValues(data = NULL)
  mort_sever_rv <- reactiveValues(data = mort_sever_default)
  status_app <- reactiveValues(status = "No Baseline")
  simul_baseline <- reactiveValues(results = NULL, baseline_available = FALSE)
  simul_interventions <- reactiveValues(results = NULL, interventions_available = FALSE)
  
  # Manage population and cases data reactive values ----
  observeEvent(input$country, if(input$country != "-- Own Value ---"){
    population_rv$data <- population %>% filter(country == input$country)
  })
  observeEvent(input$country_cases, if(input$country != "-- Own Value ---"){
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
    cases_rv$data <- read_excel(file_path, sheet = "Cases") %>%
      mutate(date = as.Date(date), cumulative_death = cumsum(deaths)) %>%
      as.data.frame()
    
    updatePickerInput(session, inputId = "country_cases", selected = "-- Own Value ---")
    
    
    # Severity/Mortality
    dta <- read_excel(file_path, sheet = "Severity-Mortality") 
    names(dta) <- c("age_category",	"ifr",	"ihr")
    
    mort_sever_rv$data <- dta %>%
      mutate(ihr = 4*ihr) %>%
      mutate(ifr = ifr/max(ifr))
    
    # Population
    dta <- read_excel(file_path, sheet = "Population")
    names(dta) <- c("age_category",	"pop",	"birth",	"death")
    
    population_rv$data <- dta %>%
      transmute(country = NA, age_category, pop, birth, death)
    
    updatePickerInput(session, inputId = "country", selected = "-- Own Value ---")
    
    # Parameters
    param <- bind_rows(read_excel(file_path, sheet = "Country Area Parameters"),
                       read_excel(file_path, sheet = "Virus Parameters"),
                       read_excel(file_path, sheet = "Hospitalisation Parameters"),
                       read_excel(file_path, sheet = "Interventions")) %>%
      mutate(Value_Date = as.Date(Value_Date))
    
    
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
    if(!is_empty(param$Parameter[param$Type == 'slider_text'])) {
      updateSliderTextInput(session = session, inputId = "phi", selected = month.name[param$Value[param$Parameter == input_excel]])
    }
    
    # Update switch
    if(!is_empty(param$Parameter[param$Type == 'switch'])) {
      for (input_excel in param$Parameter[param$Type == 'switch']){
        updateMaterialSwitch(session = session, inputId = input_excel, value = param$Value_Logical[param$Parameter == input_excel])
      }}
    
    # Update dates
    if(!is_empty(param$Parameter[param$Type == 'date'])) {
      for (input_excel in param$Parameter[param$Type == 'date']){
        updateDateInput(session = session, inputId = input_excel, value = param$Value_Date[param$Parameter == input_excel])
      }}
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
    
    source("./www/model.R", local = TRUE)
    if(! input$lockdown_low_switch) parameters["lockdown_low_on"]<-10e5
    if(! input$lockdown_mid_switch) parameters["lockdown_mid_on"]<-10e5
    if(! input$lockdown_high_switch) parameters["lockdown_high_on"]<-10e5
    if(! input$selfis_switch) parameters["selfis_on"] <- 10e5
    if(! input$dist_switch) parameters["dist_on"] <- 10e5
    if(! input$hand_switch) parameters["hand_on"] <- 10e5
    if(! input$work_switch) parameters["work_on"] <- 10e5
    if(! input$school_switch) parameters["school_on"] <- 10e5
    if(! input$cocoon_switch) parameters["cocoon_on"] <- 10e5
    if(! input$travelban_switch) parameters["travelban_on"] <-10e5
    if(! input$screen_switch) parameters["screen_on"] <- 10e5
    if(! input$quarantine_switch) parameters["quarantine_on"] <- 10e5
    if(! input$vaccination_switch) parameters["vaccine_on"] <- 10e5
    
    out <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = parameters)
    simul_baseline$results <- process_ode_outcome(out)
    
    removeNotification(id = "model_run_notif", session = session)
    
    
    status_app$status <- "Ok Baseline"
    simul_baseline$baseline_available <- TRUE
  })
  
  observeEvent(input$validate_baseline, {
    status_app$status <- "Validated Baseline"
    showTab(inputId = "tabs", target = "tab_modelpredictions")
    updateNavbarPage(session, "tabs",selected = "tab_modelpredictions")
  }
  )
  
  # Process on "run_interventions" ----
  observeEvent(input$run_interventions, {
    showNotification(span(h4(icon("hourglass-half"), "Running the Interventions..."), "typically runs in 10 secs."),
                     duration = NULL, type = "message", id = "run_interventions_notif")
    
    source("./www/model.R", local = TRUE)
    if(! input$lockdown_low_switch) parameters["lockdown_low_on"]<-10e5
    if(! input$lockdown_mid_switch) parameters["lockdown_mid_on"]<-10e5
    if(! input$lockdown_high_switch) parameters["lockdown_high_on"]<-10e5
    if(! input$selfis_switch) parameters["selfis_on"] <- 10e5
    if(! input$dist_switch) parameters["dist_on"] <- 10e5
    if(! input$hand_switch) parameters["hand_on"] <- 10e5
    if(! input$work_switch) parameters["work_on"] <- 10e5
    if(! input$school_switch) parameters["school_on"] <- 10e5
    if(! input$cocoon_switch) parameters["cocoon_on"] <- 10e5
    if(! input$travelban_switch) parameters["travelban_on"] <-10e5
    if(! input$screen_switch) parameters["screen_on"] <- 10e5
    if(! input$quarantine_switch) parameters["quarantine_on"] <- 10e5
    if(! input$vaccination_switch) parameters["vaccine_on"] <- 10e5
    
    out <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = parameters)
    simul_interventions$results <- process_ode_outcome(out)
    
    removeNotification(id = "run_interventions_notif", session = session)
    status_app$status <- "Locked Baseline"
    simul_interventions$interventions_available <- TRUE
  })
  
  
  # Export Data ----
  results_aggregated <- reactive({
    dta <- tibble(
      date = simul_baseline$results$time,
      baseline_daily_incidence = simul_baseline$results$daily_incidence,
      baseline_daily_total_cases = simul_baseline$results$daily_total_cases,
      baseline_required_beds = simul_baseline$results$required_beds,
      baseline_cum_mortality = simul_baseline$results$cum_mortality)
    
    dta <- left_join(dta, 
                     cases_rv$data %>% rename(input_cases = cases,
                                              input_deaths = deaths,
                                              input_cumulative_death = cumulative_death), by = "date")
    
    if(simul_interventions$interventions_available){ 
      
      dta_inter <- tibble(
        date = simul_interventions$results$time,
        predictions_daily_incidence = simul_interventions$results$daily_incidence,
        predictions_daily_total_cases = simul_interventions$results$daily_total_cases,
        predictions_required_beds = simul_interventions$results$required_beds,
        predictions_cum_mortality = simul_interventions$results$cum_mortality)
      
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
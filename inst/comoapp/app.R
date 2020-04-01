# COVID19 App

# Load packages
source("./www/load_packages.R")

# Import/Process data
source("./www/prepare_data.R")

# Define UI ----
ui <- function(request) {
  fluidPage(
    theme = shinytheme("simplex"),
    includeCSS("./www/styles.css"),
    chooseSliderSkin('HTML5'),
    title = "COVID-19 App",
    br(),
    div(class = "box_outputs",
        h4("About COVID-19 App v9.20")
    ),
    h4("Important Disclaimer:"),
    includeMarkdown("./www/markdown/disclaimer.md"),
    
    fluidRow(
      column(3, 
             br(),
             tabsetPanel(id = "left_panel",
                         # UI Tab Baseline ----
                         tabPanel(value = 'baseline', title = h5("Baseline"), 
                                  conditionalPanel("! output.interventions_okay",
                                                   br(),
                                                   bsButton("open2", label = "Country/Area Data", icon = icon('cog'), style = "default", type = "action", value = FALSE, 
                                                            block = TRUE), br(),
                                                   bsButton("open", label = "Virus Parameters", icon = icon('cog'), style = "default", type = "action", value = FALSE, 
                                                            block = TRUE), br(), 
                                                   # Modal dialogs for parameters ----
                                                   source("./www/modal_parameters_virus.R", local = TRUE)$value,
                                                   source("./www/modal_parameters_country.R", local = TRUE)$value,
                                                   
                                                   sliderInput("p", label = "Probability of infection given contact:", min = 0, max = 0.2, step = 0.001,
                                                               value = 0.035, ticks = FALSE),
                                                   div(id = "margin_month_slider",
                                                       sliderTextInput("phi", label = "Month of peak infectivity of the virus:", 
                                                                       choices = month.name, selected = month.name[1])
                                                   ),
                                                   bsPopover("phi", title = "Recommendation", content = "Select the month of peak humidity",
                                                             "right", options = list(container = "body")),
                                                   
                                                   sliderInput("report", label = "Percentage of all infections that are reported:", min = 0, max = 100, step = 0.1,
                                                               value = 12.5, post = "%", ticks = FALSE),
                                                   
                                                   dateRangeInput("date_range", label = "Range of dates", start = "2020-01-31", end = "2020-12-31"),
                                                   br(), 
                                                   htmlOutput("feedback_choices"),
                                                   div(class = "floating-button",
                                                       actionButton("run_baseline", "Run Baseline", class="btn btn-success")
                                                   ),
                                                   hr()
                                                   
                                  ),
                                  
                                  conditionalPanel("output.interventions_okay", 
                                                   br(),
                                                   p("Since interventions are already built, to change parameters here, you will need to reset the baseline and start again."),
                                                   bsButton("reset_baseline", label = "Reset the Baseline", icon = icon('cog'), style = "default", type = "action", value = FALSE, 
                                                            block = TRUE), 
                                  )
                         ),
                         # UI Tab Interventions ----
                         tabPanel(value = 'interventions', title = h5("Interventions"), 
                                  br(), 
                                  # prettyRadioButtons("package_interventions", label = "Package of Interventions:", 
                                  #                    choices = c("Low", "Medium", "High"), 
                                  #                    selected = NULL, inline = TRUE),
                                  # prettyCheckbox("custom_interventions", label = "Customise Package", value = FALSE),
                                  # 
                                  # conditionalPanel("input.custom_interventions", 
                                  
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
             )
      ),
      # UI Outputs ----
      column(9, 
             br(), br(), 
             div(class = "box_outputs", h4("Timeline")),
             timevisOutput("timeline"),
             br(), 
             conditionalPanel("output.show_results",
                              div(class = "box_outputs", h4("Predictions")),
                              fluidRow(
                                column(9,
                                       prettyRadioButtons("focus_axis", label = "Focus on:", choices = c("Observed", "Predicted Reported", "Predicted Reported + Unreported"), 
                                                          selected = "Predicted Reported + Unreported", inline = TRUE)
                                ),
                                column(3, downloadButton("export", label = "Download Results (.csv)"))
                              ),
                              fluidRow(
                                column(6,
                                       highchartOutput("highchart_confirmed", height = "350px"), br(),
                                       highchartOutput("highchart_mortality", height = "350px")
                                ),
                                column(6,
                                       highchartOutput("highchart_ICU", height = "350px"), br(),
                                       htmlOutput("text_doubling_time"), br(),
                                       fluidRow(
                                         column(6,
                                                htmlOutput("text_baseline_pct_pop"), br(),
                                                htmlOutput("text_baseline_total_death")
                                         ),
                                         column(6, 
                                                htmlOutput("text_interventions_pct_pop"), br(),
                                                htmlOutput("text_interventions_total_death")
                                         )
                                       )
                                )
                              ),
                              br()
             )
      )
    )
  )
}

# Define server ----
server <- function(input, output, session) {
  # On deployment only:
  # Stop the shiny app when the browser window is closed
  # session$onSessionEnded(function() {
  #   stopApp()
  # })
  
  # Show disclaimer on startup
  # observe({
  #   showNotification(id = "disclaimer", ui = div(includeMarkdown("./www/markdown/disclaimer.md")),
  #                    duration = NULL, closeButton = FALSE, type = "message",
  #                    action = actionButton("understand", "I understand and wish to continue."))
  # })
  # observeEvent(input$understand, removeNotification("disclaimer"))
  
  # Hide tabs on app launch ----
  hideTab(inputId = "left_panel", target = "interventions")
  
  # # Provide default values for package of interventions
  # observeEvent(input$package_interventions, {
  #   if(input$package_interventions == "Low"){
  #     updateMaterialSwitch(session, "selfis_switch", value = TRUE)
  #     updateMaterialSwitch(session, "dist_switch", value = TRUE)
  #     updateMaterialSwitch(session, "hand_switch", value = TRUE)
  #     updateMaterialSwitch(session, "work_switch", value = TRUE)
  #     updateMaterialSwitch(session, "school_switch", value = TRUE)
  #     updateMaterialSwitch(session, "cocoon_switch", value = TRUE)
  #     updateMaterialSwitch(session, "travelban_switch", value = TRUE)
  #     updateMaterialSwitch(session, "screen_switch", value = TRUE)
  #     updateMaterialSwitch(session, "quarantine_switch", value = TRUE)
  #     updateSliderInput(session, "selfis_cov", value = 50)
  #     updateSliderInput(session, "dist_cov", value = 25)
  #     updateSliderInput(session, "hand_eff", value = 5)
  #     updateSliderInput(session, "work_cov", value = 25)
  #     updateSliderInput(session, "school_eff", value = 0)
  #     updateSliderInput(session, "cocoon_cov", value = 90)
  #     updateSliderInput(session, "travelban_eff", value = 0)
  #     updateSliderInput(session, "quarantine_cov", value = 0)
  #   }
  #   if(input$package_interventions == "Medium"){
  #     updateMaterialSwitch(session, "selfis_switch", value = TRUE)
  #     updateMaterialSwitch(session, "dist_switch", value = TRUE)
  #     updateMaterialSwitch(session, "hand_switch", value = TRUE)
  #     updateMaterialSwitch(session, "work_switch", value = TRUE)
  #     updateMaterialSwitch(session, "school_switch", value = TRUE)
  #     updateMaterialSwitch(session, "cocoon_switch", value = TRUE)
  #     updateMaterialSwitch(session, "travelban_switch", value = TRUE)
  #     updateMaterialSwitch(session, "screen_switch", value = TRUE)
  #     updateMaterialSwitch(session, "quarantine_switch", value = TRUE)
  #     updateSliderInput(session, "selfis_cov", value = 75)
  #     updateSliderInput(session, "dist_cov", value = 75)
  #     updateSliderInput(session, "hand_eff", value = 5)
  #     updateSliderInput(session, "work_cov", value = 50)
  #     updateSliderInput(session, "school_eff", value = 85)
  #     updateSliderInput(session, "cocoon_cov", value = 90)
  #     updateSliderInput(session, "travelban_eff", value = 0)
  #     updateSliderInput(session, "quarantine_cov", value = 25)
  #   }
  #   if(input$package_interventions == "High"){
  #     updateMaterialSwitch(session, "selfis_switch", value = TRUE)
  #     updateMaterialSwitch(session, "dist_switch", value = TRUE)
  #     updateMaterialSwitch(session, "hand_switch", value = TRUE)
  #     updateMaterialSwitch(session, "work_switch", value = TRUE)
  #     updateMaterialSwitch(session, "school_switch", value = TRUE)
  #     updateMaterialSwitch(session, "cocoon_switch", value = TRUE)
  #     updateMaterialSwitch(session, "travelban_switch", value = TRUE)
  #     updateMaterialSwitch(session, "screen_switch", value = TRUE)
  #     updateMaterialSwitch(session, "quarantine_switch", value = TRUE)
  #     updateSliderInput(session, "selfis_cov", value = 95)
  #     updateSliderInput(session, "dist_cov", value = 95)
  #     updateSliderInput(session, "hand_eff", value = 5)
  #     updateSliderInput(session, "work_cov", value = 75)
  #     updateSliderInput(session, "school_eff", value = 85)
  #     updateSliderInput(session, "cocoon_cov", value = 90)
  #     updateSliderInput(session, "travelban_eff", value = 100)
  #     updateSliderInput(session, "quarantine_cov", value = 90)
  #   }
  # }, ignoreNULL = FALSE, ignoreInit = TRUE)
  # 
  # observeEvent(input$left_panel, {
  #   print(input$left_panel)
  #   if(input$left_panel == "interventions") updatePrettyRadioButtons(session, "package_interventions", selected = "Medium")
  # })
  
  # Reactive elements
  population_rv <- reactiveValues(data = NULL)
  cases_rv <- reactiveValues(data = NULL)
  mort_sever <- reactiveValues(data = mort_sever_default)
  simul_baseline <- reactiveValues(results = NULL, baseline_available = FALSE)
  simul_interventions <- reactiveValues(results = NULL, interventions_available = FALSE)
  
  # Outputs
  # Source code to generate outputs ----
  file_list <- list.files(path = "./www/outputs", pattern = "*.R")
  for (file in file_list) source(paste0("./www/outputs/", file), local = TRUE)$value
  
  # To show/hide baseline reset ----
  output$interventions_okay <- reactive({
    return(simul_interventions$interventions_available)
  })
  outputOptions(output, "interventions_okay", suspendWhenHidden = FALSE)
  
  # To show/hide results ----
  output$show_results <- reactive({
    return(simul_baseline$baseline_available)
  })
  outputOptions(output, "show_results", suspendWhenHidden = FALSE)
  
  # Process on uploading a file of severity/mortality
  observeEvent(input$severity_mortality_file, {
    mort_sever_upload <- read.csv(input$severity_mortality_file$datapath, header = TRUE)
    
    if (any(mort_sever_upload$mortality > mort_sever_upload$severity)) {
      warning("CFR > CSR for at least one age class!")
      mort_sever_upload %>% mutate(severity = max(mortality, severity))
    }
    
    mort_sever$data <- mort_sever_upload
  })
  
  # Manage cases data reactive values
  observeEvent(input$country_cases, {
    cases_rv$data <- cases %>% filter(country == input$country_cases)
  })
  
  observeEvent(input$cases_file, {
    cases_file_upload <- read_csv(input$cases_file$datapath, col_names = TRUE,
                                  col_types = "cnnn") %>%
      mutate(date = ymd(date))
    
    cases_rv$data <- cases_file_upload
    cases_rv$data <- cases %>% filter(country == input$country_cases)
  })
  
  # Manage population reactive values
  observeEvent(input$country, 
               population_rv$data <- population %>% filter(country == input$country)
  )
  observeEvent(input$population_file, {
    population_file_upload <- read_csv(input$population_file$datapath, col_names = TRUE,
                                       col_types = "ccnnn")
    
    population_rv$data <- population_file_upload
  })
  
  # Downloadable csv of demographics
  output$download_demographic <- downloadHandler(
    filename = "demographics.csv",
    content = function(file) {
      write.csv(population %>% filter(country == input$country), file, row.names = FALSE)
    }
  )
  
  
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
    hideTab(inputId = "left_panel", target = "interventions")
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
    
    
    out <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = parameters)
    simul_baseline$results <- process_ode_outcome(out)
    simul_baseline$baseline_available <- TRUE
    
    removeNotification(id = "model_run_notif", session = session)
    
    showTab(inputId = "left_panel", target = "interventions")
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
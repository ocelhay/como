output$timeline <- renderTimevis({
  req(cases_rv$data)
  input$reset_baseline
  
  dta <- tibble(id = 10, content = "Range of simulation", start = input$date_range[1], end = input$date_range[2], type = "range", group = 2)
  min_x <- cases_rv$data$date[first(which(!is.na(cases_rv$data$cases)))]
  max_x <- cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))]
  dta <- bind_rows(dta, tibble(id = 11, content = "Observed Cases/Deaths", start = min_x, end = max_x, type = "range", group = 1,
                               style = "border-color: #F991A3; background-color: pink; color: red;"))
  
  if(input$selfis_switch) {
    dta <- bind_rows(dta, tibble(id = 1, content = "Self Isolation if Symptomatic", start = input$date_selfis_on, 
                                end = as.Date(input$date_selfis_on + 7*input$selfis_dur), type = "range", group = 3))
  }
  if(input$dist_switch) {
    dta <- bind_rows(dta, tibble(id = 2, content = as.character(span(icon("user-friends"), " Social Distancing")), start = input$date_dist_on, 
                            end = as.Date(input$date_dist_on + 7*input$dist_dur), type = "range", group = 3))
  }
  if(input$hand_switch) {
    dta <- bind_rows(dta, tibble(id = 3, content = as.character(span(icon("hand-paper"), " Handwashing")), start = input$date_hand_on, 
                                 end = as.Date(input$date_hand_on + 7*input$hand_dur), type = "range", group = 3))
  }
  if(input$work_switch) {
    dta <- bind_rows(dta, tibble(id = 4, content = "Working at Home", start = input$date_work_on, 
                                 end = as.Date(input$date_work_on + 7*input$work_dur), type = "range", group = 3))
  }
  if(input$school_switch) {
    dta <- bind_rows(dta, tibble(id = 5, content = as.character(span(icon("school"), "School Closures")), start = input$date_school_on, 
                                 end = as.Date(input$date_school_on + 7*input$school_dur), type = "range", group = 3))
  }
  if(input$cocoon_switch) {
    dta <- bind_rows(dta, tibble(id = 6, content = "Cocooning the Elderly", start = input$date_cocoon_on, 
                                 end = as.Date(input$date_cocoon_on + 7*input$cocoon_dur), type = "range", group = 3))
  }
  if(input$travelban_switch) {
    dta <- bind_rows(dta, tibble(id = 7, content = as.character(span(icon("plane"), " Travel Ban")), start = input$date_travelban_on, 
                                 end = as.Date(input$date_travelban_on + 7*input$travelban_dur), type = "range", group = 3))
  }
  if(input$vaccination_switch) {
    dta <- bind_rows(dta, tibble(id = 8, content = as.character(span(icon("syringe"), "Vaccination")), start = input$date_vaccine_on, 
                                 end = as.Date(input$date_vaccine_on + 7*input$vac_campaign), type = "range", group = 3))
  }
  if(input$quarantine_switch) {
    dta <- bind_rows(dta, tibble(id = 9, content = "Quarantine", start = input$date_quarantine_on, 
                                 end = as.Date(input$date_quarantine_on + 7*input$quarantine_dur), type = "range", group = 3))
  }
  
  timevis(dta, groups = data.frame(id = 1:3, content = c("Epidemiology", "Baseline", "Interventions")), options = list(editable = FALSE))
})


output$timevis <- renderPlot({
  input$reset_baseline

  dta <- tibble(id = 1, content = "Range of simulation", 
                start = input$date_range[1], end = input$date_range[2], type = "range")
  
  if(input$lockdown_low_switch) {
    dta <- dta %>% add_row(id = 3, content = "Low Lockdown", 
                           start = input$date_lockdown_low_on, end = as.Date(input$date_lockdown_low_on + 7*input$lockdown_low_dur), type = "intervention")
  }
  
  if(input$lockdown_mid_switch) {
    dta <- dta %>% add_row(id = 3, content = "Mid Lockdown", 
                           start = input$date_lockdown_mid_on, end = as.Date(input$date_lockdown_mid_on + 7*input$lockdown_mid_dur), type = "intervention")
  }
  
  if(input$lockdown_high_switch) {
    dta <- dta %>% add_row(id = 3, content = "High Lockdown", 
                           start = input$date_lockdown_high_on, end = as.Date(input$date_lockdown_high_on + 7*input$lockdown_high_dur), type = "intervention")
  }

  if(input$selfis_switch) {
    dta <- dta %>% add_row(id = 3, content = "Self Isolation if Symptomatic", start = input$date_selfis_on,
                                 end = as.Date(input$date_selfis_on + 7*input$selfis_dur), type = "intervention")
  }
  if(input$dist_switch) {
    dta <- dta %>% add_row(id = 3, content = "Social Distancing", start = input$date_dist_on,
                                 end = as.Date(input$date_dist_on + 7*input$dist_dur), type = "intervention")
  }
  if(input$hand_switch) {
    dta <- dta %>% add_row(id = 3, content = "Handwashing", start = input$date_hand_on,
                                 end = as.Date(input$date_hand_on + 7*input$hand_dur), type = "intervention")
  }
  if(input$work_switch) {
    dta <- dta %>% add_row(id = 3, content = "Working at Home", start = input$date_work_on,
                                 end = as.Date(input$date_work_on + 7*input$work_dur), type = "intervention")
  }
  if(input$school_switch) {
    dta <- dta %>% add_row(id = 3, content = "School Closures", start = input$date_school_on,
                                 end = as.Date(input$date_school_on + 7*input$school_dur), type = "intervention")
  }
  if(input$cocoon_switch) {
    dta <- dta %>% add_row(id = 3, content = "Shielding the Elderly", start = input$date_cocoon_on,
                                 end = as.Date(input$date_cocoon_on + 7*input$cocoon_dur), type = "intervention")
  }
  if(input$travelban_switch) {
    dta <- dta %>% add_row(id = 3, content = "Travel Ban", start = input$date_travelban_on,
                                 end = as.Date(input$date_travelban_on + 7*input$travelban_dur), type = "intervention")
  }
  if(input$vaccination_switch) {
    dta <- dta %>% add_row(id = 3, content = "Vaccination", start = input$date_vaccine_on,
                                 end = as.Date(input$date_vaccine_on + 7*input$vac_campaign), type = "intervention")
  }
  if(input$quarantine_switch) {
    dta <- dta %>% add_row(id = 3, content = "Quarantine", start = input$date_quarantine_on,
                                 end = as.Date(input$date_quarantine_on + 7*input$quarantine_dur), type = "intervention")
  }
  
  timeline_colours <- list("range" = "#E6E6E6", "intervention" = "#D65745")
  
  dta %>% 
    mutate(date_range = paste0(start, " to ", end),
           content = fct_reorder(content, -id)) %>%
    ggplot(aes(x = start, xend = end, y = content, yend = content, color = type)) + 
    geom_segment(size = 10) + 
    geom_text(aes(label = date_range), color = "black", hjust = 0, vjust = 0.5, size = 5, fontface = "italic") + 
    geom_vline(xintercept = Sys.Date(), lty = 2) + 
    annotate("text", x = Sys.Date(), y = 0, label = paste0("Today: ", Sys.Date()), vjust = -1, hjust = 0, fontface = "italic") +
    xlab("") + ylab("") +
    scale_colour_manual(name = "", values = timeline_colours) +
    theme_classic(base_size = 17) +
    theme(legend.position = "none")
})

output$table_demographic_data <- renderTable({
  req(input$country_demographic != "-- Own Value ---")
  
  dta <- population %>% filter(country == input$country_demographic) %>%
    transmute(
      `Age category` = age_category,
      `Population`= pop, 
      `Number of births per person`= birth,
      `Deaths per person per day` = death)
  
  dta}, hover = TRUE, digits = 10)
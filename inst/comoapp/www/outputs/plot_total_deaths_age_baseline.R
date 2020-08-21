output$plot_total_deaths_age <- renderPlot({
  req(simul_baseline$baseline_available)
  
  # end date is the date of the last data point if the focus is "Observed"
  # end date os the last day of the simulation otherwise
  end_date <- input$date_range[2]
  if(input$focus_axis == "Observed")  end_date <- cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))]
  
  dta <- simul_baseline$results$med$tc %>%
    filter(Date <= end_date) %>% 
    group_by(age_cat) %>%
    summarise(total_deaths = round(sum(value))) %>% 
    mutate(freq = round(100 * total_deaths / sum(total_deaths), 1))
  
  ggplot(data = dta, aes(x = age_cat, y = total_deaths, fill = age_cat))+ 
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(total_deaths, " (", freq, "%)")), vjust = -0.2, size = 4) + 
    scale_fill_brewer(palette = "BrBG") + 
    ylab("Total Deaths") + xlab("") +
    theme_minimal(base_size = 14) + 
    theme(legend.title = element_blank(), legend.position = "bottom") + 
    labs(title = "Total Covid-19 Deaths per Age Category", subtitle = "Baseline")
})
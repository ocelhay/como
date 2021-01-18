output$plot_total_deaths_age <- renderPlot({
  req(simul_baseline$baseline_available)
  
  # end_date is the date of the last data point if the focus is "Observed",
  # otherwise it is the last day of the simulation
  end_date <- input$date_range[2]
  if(input$focus_axis == "Observed")  end_date <- cases_rv$data$date[last(which(!is.na(cases_rv$data$cases)))]
  
  
  dta <- simul_baseline$results$med$tc %>%
    filter(Date <= end_date) %>%
    group_by(age_cat) %>%
    summarise(total_deaths = round(sum(value)), .groups = "drop") %>% 
    mutate(freq = round(100 * total_deaths / sum(total_deaths), 0))
  
  ggplot(data = dta, aes(x = age_cat, y = total_deaths, fill = age_cat))+ 
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(format(total_deaths, big.mark = ",", scientific = FALSE), " (", freq, "%)")), 
              size = 4, fontface= 2, vjust = -0.2) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
    scale_fill_brewer(palette = "BrBG") + 
    ylab("") + xlab("") +
    theme_minimal(base_size = 14) + 
    theme(legend.title = element_blank(), legend.position = "bottom") + 
    labs(title = "Total Reported Covid-19 Deaths per Age Category", subtitle = "Baseline")
})
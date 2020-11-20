output$plot_total_deaths_age_baseline <- renderPlot({
  req(simul_baseline$baseline_available)
  req(simul_interventions$interventions_available)
  
  dta <- simul_baseline$results$med$tc %>%
    group_by(age_cat) %>%
    summarise(total_deaths = round(sum(value)), .groups = "drop") %>% 
    mutate(freq = round(100 * total_deaths / sum(total_deaths), 1))
  
  ggplot(data = dta, aes(x = age_cat, y = total_deaths, fill = age_cat))+ 
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(format(total_deaths, big.mark = ",", scientific = FALSE), " (", freq, "%)")), vjust = -0.2, size = 4) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
    scale_fill_brewer(palette = "BrBG") + 
    ylab("Total Deaths") + xlab("") +
    theme_minimal(base_size = 14) + 
    theme(legend.title = element_blank(), legend.position = "bottom") + 
    labs(title = "Total Covid-19 Deaths per Age Category", subtitle = "Baseline")
})

output$plot_total_deaths_age_interventions <- renderPlot({
  req(simul_interventions$interventions_available)
  
  dta <- simul_interventions$results$med$tc %>%
    group_by(age_cat) %>%
    summarise(total_deaths = round(sum(value)), .groups = "drop") %>% 
    mutate(freq = round(100 * total_deaths / sum(total_deaths), 1))
  
  ggplot(data = dta, aes(x = age_cat, y = total_deaths, fill = age_cat))+ 
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(format(total_deaths, big.mark = ",", scientific = FALSE), " (", freq, "%)")), vjust = -0.2, size = 4) + 
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
    scale_fill_brewer(palette = "BrBG") + 
    ylab("Total Deaths") + xlab("") +
    theme_minimal(base_size = 14) + 
    theme(legend.title = element_blank(), legend.position = "bottom") + 
    labs(title = "Total Covid-19 Deaths per Age Category", subtitle = "Hypothetical Scenario")
})
output$plot_deaths_age_baseline <- renderPlot({
  
  ggplot(data=simul_baseline$results$tc, aes(x=Date,y=value,fill=age_cat))+ 
    geom_bar(stat = "identity",position="fill", width=1)+
    scale_fill_brewer(palette = "BrBG") + 
    ylab("Proportion of Deaths") + xlab("") +
    theme_minimal(base_size = 14) + 
    theme(legend.title = element_blank(), legend.position="bottom") + 
    labs(title="Proportion of Covid-19 Deaths per Age Category", subtitle = "Baseline")
})

output$plot_deaths_age_interventions <- renderPlot({
  
  ggplot(data=simul_interventions$results$tc, aes(x=Date,y=value,fill=age_cat))+ 
    geom_bar(stat = "identity",position="fill", width=1)+
    scale_fill_brewer(palette = "BrBG") + 
    ylab("Proportion of Deaths") + xlab("") +
    theme_minimal(base_size = 14) +
    theme(legend.title = element_blank(), legend.position="bottom") + 
    labs(title="Proportion of Covid-19 Deaths per Age Category", subtitle = "Future Scenarios")
})
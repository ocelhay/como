# output$plot_deaths_age_baseline <- renderPlot({
#   req(simul_baseline$baseline_available)
#   req(simul_interventions$interventions_available)
# 
#   ggplot(data=simul_baseline$results$med$tc, aes(x=Date,y=value,fill=age_cat))+ 
#     geom_bar(stat = "identity",position="fill", width=1)+
#     scale_fill_brewer(palette = "BrBG") + 
#     ylab("Proportion of Deaths") + xlab("") +
#     theme_minimal(base_size = 14) + 
#     theme(legend.title = element_blank(), legend.position="bottom") + 
#     labs(title="Proportion of Covid-19 Deaths per Age Category", subtitle = "Baseline")
# })
# 
# output$plot_deaths_age_interventions <- renderPlot({
#   
#   req(simul_interventions$interventions_available)
#   ggplot(data=simul_interventions$results$med$tc, aes(x=Date,y=value,fill=age_cat))+ 
#     geom_bar(stat = "identity",position="fill", width=1)+
#     scale_fill_brewer(palette = "BrBG") + 
#     ylab("Proportion of Deaths") + xlab("") +
#     theme_minimal(base_size = 14) +
#     theme(legend.title = element_blank(), legend.position="bottom") + 
#     labs(title="Proportion of Covid-19 Deaths per Age Category", subtitle = "Hypothetical Scenario")
# })
output$timevis_baseline <- renderPlot({
  dta <- interventions$baseline_mat %>%
    bind_rows(tibble(index = nrow(interventions$baseline_mat) + 1, intervention = "Range of simulation", 
                     date_start = input$date_range[1], date_end = input$date_range[2], coverage = NA)) %>%
    mutate(date_range = paste0(date_start, " to ", date_end))
  
  dta %>% 
    ggplot(aes(x = date_start, xend = date_end, y = intervention, yend = intervention, color = intervention)) + 
    geom_segment(size = 10) + 
    geom_text(aes(label = date_range), color = "black", hjust = 0, vjust = 0.5, size = 5, fontface = "italic") + 
    geom_vline(xintercept = Sys.Date(), lty = 2) + 
    annotate("text", x = Sys.Date(), y = 0, label = paste0("Today: ", Sys.Date()), vjust = -1, hjust = 0, fontface = "italic") +
    xlab("") + ylab("") +
    theme_classic(base_size = 17) +
    theme(legend.position = "none")
})

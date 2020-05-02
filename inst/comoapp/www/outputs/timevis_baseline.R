output$timevis_baseline <- renderPlot({
  req(interventions$baseline_nb >= 1)
  
  mid_date <- input$date_range[1] + floor((input$date_range[2] - input$date_range[1])/2)
  range <- paste0("Range of Simulation: ", input$date_range[1], " to ", input$date_range[2])
  
  interventions$baseline_mat %>% 
    mutate(label_coverage = paste0(coverage, "%"),
           date_mid = date_start + floor((date_end - date_start)/2)) %>%
    
    ggplot(aes(x = date_start, xend = date_end, y = intervention, yend = intervention)) + 
    geom_segment(aes(color = coverage), size = 12) + 
    geom_text(aes(x  = date_mid, label = label_coverage), size = 4.5) +
    # geom_vline(xintercept = Sys.Date(), lty = 2) + 
    geom_segment(x = input$date_range[1], xend = input$date_range[2], size = 2,
                 y = 0.6, yend = 0.6, arrow = arrow(length = unit(0.30, "cm") , ends = "both", type = "closed")) +
    geom_label(x = mid_date, y = 0.6, label = range, size = 4.5) +
    labs(title = "Baseline + Future Scenario Interventions", 
         color = "Coverage (%)", x = NULL, y = NULL) +
    scale_x_date(date_breaks = "1 month", labels = date_format("%b %y")) +
    scale_colour_distiller(palette = "RdYlBu", limits = c(0, 100), breaks = c(0, 40, 60, 80, 100)) +
    guides(size = FALSE, color = guide_colourbar(barwidth = 15, barheight = 1)) + 
    theme_bw(base_size = 17) +
    theme(legend.position = "bottom", legend.title = element_text(),
          panel.border = element_blank(),
          panel.grid.major.y = element_blank(),  panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(size = 0.5, colour="grey80"),
          axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text.x=element_text(angle = 45, hjust = 1))
})

output$timevis_baseline_2 <- renderPlot({
  req(interventions$baseline_nb >= 1)
  
  # input <- list()
  # input$date_range <- c(as.Date("2020-01-01"), as.Date("2020-01-12"))
  # interventions <- list()
  # interventions$baseline_mat <- shiny_interventions_baseline_mat
  
  range <- paste0("Range of Simulation: ", input$date_range[1], " to ", input$date_range[2])
  # n_different_interventions <- unique(interventions$baseline_mat$intervention) + 0.4
  
  
  dta <- interventions$baseline_mat %>%
    mutate(date_end = date_end + 1,
           label = paste0(difftime(date_end, date_start, units = "days"), " days - ", coverage, "%"))
  
  ggplot(dta) + 
    geom_rect(aes(xmin = date_start , xmax = date_end, ymin = 0, ymax = coverage), alpha = 0.4) +
    geom_point(aes(x = date_start, y = coverage)) +
    geom_segment(aes(x = date_start, xend = date_end, y = coverage, yend = coverage),
                 arrow = arrow(length = unit(0.3, "cm") , ends = "last", type = "closed")) +
    geom_text(aes(x = date_start, y = coverage, label = label), nudge_x = -0.1, nudge_y = -0.1, 
              hjust = -0.1, vjust = 1.8, inherit.aes = FALSE) + 
    geom_vline(xintercept = Sys.Date(), lty = 2, alpha = 0.7) +
    labs(title = "Baseline + Future Scenario", subtitle = range,
         color = "Coverage (%)", x = NULL, y = NULL) +
    scale_x_date(date_breaks = "2 months", labels = date_format("%b %y")) +
    ylim(c(0, 100)) +
    theme_bw(base_size = 17) +
    theme(legend.position = "bottom", legend.title = element_text(),
          panel.border = element_blank(),
          panel.grid.major.y = element_blank(),  panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(size = 0.5, colour="grey80"),
          axis.line = element_blank(), axis.ticks = element_blank()) +
    facet_wrap(~intervention, ncol = 1)
})

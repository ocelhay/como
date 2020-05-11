output$timevis_future <- renderPlot(execOnResize = TRUE, {
  req(interventions$future_nb >= 1)
  
  dta <- interventions$future_mat %>%
    mutate(date_end = date_end + 1,
           label = paste0(coverage, "%\n", difftime(date_end, date_start, units = "days") - 1, "d."))
  
  
  ggplot(dta) + 
    geom_rect(aes(xmin = date_start , xmax = date_end, ymin = 0, ymax = coverage), fill = "#e74c3c", alpha = 0.2) +
    geom_point(aes(x = date_start, y = coverage)) +
    geom_segment(aes(x = date_start, xend = date_end, y = coverage, yend = coverage),
                 arrow = arrow(length = unit(0.3, "cm") , ends = "last", type = "closed")) +
    geom_text(aes(x = date_start, y = coverage, label = label), 
              position = position_jitter(width = 1, height = 1),  
              hjust = -0.1, vjust = 1.1, inherit.aes = FALSE) + 
    
    geom_vline(aes(colour = "Today", xintercept = Sys.Date()), lty = 2, alpha = 0.7, lwd = 1.5) +
    geom_vline(aes(colour = "Simulation Range", xintercept = input$date_range[1]), lty = 2, lwd = 1.5) +
    geom_vline(aes(colour = "Simulation Range", xintercept = input$date_range[2]), lty = 2, lwd = 1.5) +
    scale_colour_manual(name="Line Color", values = c(`Today` = "#c0392b", `Simulation Range` = "#2980b9")) +
    
    labs(x = NULL, y = NULL) +
    guides(fill = guide_legend(title = NULL), colour = guide_legend(title = NULL)) +
    scale_x_date(labels = date_format("%b' %y")) +
    scale_y_continuous(labels = label_percent(scale = 1)) + 
    theme_bw(base_size = 19) +
    theme(legend.position = "top", legend.title = element_text(), legend.text = element_text(size = 13),
          panel.border = element_blank(),
          panel.grid.major.y = element_blank(),  panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(size = 0.5, colour="grey80"),
          axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text = element_text(color = "grey20", size = 12)) +
    facet_wrap(~ intervention, ncol = 2, strip.position = "top", scales = "fixed")
  })
  
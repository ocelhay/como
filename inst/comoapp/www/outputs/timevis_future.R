output$timevis_future <- renderPlot(execOnResize = TRUE, {
  req(interventions$future_mat %>% nrow() >= 1)
  
  dta <- interventions$future_mat %>%
    mutate(date_end = date_end + 1,
           label = paste0(value, unit, " ", difftime(date_end, date_start, units = "days") - 1, "d."))
  
  
  ggplot(dta) + 
    geom_rect(aes(xmin = date_start , xmax = date_end, ymin = 0, ymax = value), fill = "#e74c3c", alpha = 0.2) +
    geom_point(aes(x = date_start, y = value)) +
    geom_segment(aes(x = date_start, xend = date_end, y = value, yend = value),
                 arrow = arrow(length = unit(0.3, "cm") , ends = "last", type = "closed")) +
    geom_segment(aes(colour = "Today", x = Sys.Date(), xend = Sys.Date(), y = 0, yend = 100), lty = 2, alpha = 1, lwd = 1.3) +
    geom_segment(aes(colour = "Simulation Range", x = input$date_range[1], xend = input$date_range[1], y = 0, yend = 100), lty = 2, alpha = 1, lwd = 1.3) +
    geom_segment(aes(colour = "Simulation Range", x = input$date_range[2], xend = input$date_range[2], y = 0, yend = 100), lty = 2, alpha = 1, lwd = 1.3) +
    scale_colour_manual(name="Line Color", values = c(`Today` = "#c0392b", `Simulation Range` = "#2980b9")) +
    geom_label(aes(x = date_start, y = value, label = label), 
               hjust = -0.1, vjust = 1.2, inherit.aes = FALSE, fill = "grey80", alpha = 1, size = 4) + 
    
    labs(x = NULL, y = NULL) +
    guides(fill = guide_legend(title = NULL), colour = guide_legend(title = NULL)) +
    scale_x_date(labels = date_format("%b' %y")) +
    scale_y_continuous(limits = c(0, 100)) + 
    theme_bw(base_size = 19) +
    theme(legend.position = "top", legend.title = element_text(), legend.text = element_text(size = 13),
          panel.border = element_blank(),
          panel.grid.major.y = element_blank(),  panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(size = 0.5, colour= "grey80"),
          axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text = element_text(color = "grey20", size = 12)) +
    facet_wrap(~ intervention, ncol = 2, strip.position = "top", scales = "fixed")
  })
  
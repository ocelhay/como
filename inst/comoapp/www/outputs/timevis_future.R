output$timevis_future <- renderPlot(execOnResize = TRUE, {
  
  dta <- interventions$future_mat %>%
    mutate(date_end = date_end + 1,
           label = paste0(value, unit, " ", difftime(date_end, date_start, units = "days") - 1, "d."))
  
  # completing with all "real" interventions
  dta <- bind_rows(
    dta,
    tibble(intervention = setdiff(real_interventions, dta$intervention))) %>%
    filter(intervention %in% real_interventions) %>% 
    arrange(intervention)
  
  dta$index <- 11 - as.numeric(factor(dta$intervention))
  
  if(interventions$future_mat %>% nrow() == 0) return({
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.8, paste("No Intervention has been selected"), 
         cex = 1.5, col = "black")
  })
  
  if(interventions$future_mat %>% nrow() > 0) return({
    ggplot(dta) + 
      geom_rect(aes(xmin = date_start , xmax = date_end, ymin = index, ymax = (index + 0.95 * 100/100), fill = intervention), alpha = 0.4) +
      geom_rect(aes(xmin = date_start , xmax = date_end, ymin = index, ymax = (index + 0.95 * value/100), fill = intervention)) +
      geom_segment(data = dta, aes(colour = "Today", x = Sys.Date(), xend = Sys.Date(), y = 0, yend = 11), lty = 2, alpha = 1, lwd = 1.3) +
      geom_segment(data = dta, aes(colour = "Simulation Range", x = input$date_range[1], xend = input$date_range[1], y = 0, yend = 11), lty = 2, alpha = 1, lwd = 1.3) +
      geom_segment(data = dta, aes(colour = "Simulation Range", x = input$date_range[2], xend = input$date_range[2], y = 0, yend = 11), lty = 2, alpha = 1, lwd = 1.3) +
      scale_colour_manual(name="Line Color", values = c(`Today` = "#c0392b", `Simulation Range` = "#2980b9")) +
      geom_text(aes(x = date_start, y = index, label = label),
                hjust = -0.1, vjust = -1, inherit.aes = FALSE, alpha = 1, size = 4) +
      labs(x = NULL, y = NULL) +
      guides(fill = guide_legend(title = NULL), colour = guide_legend(title = NULL)) +
      scale_x_date(labels = date_format("%b' %y")) +
      scale_y_continuous(labels = NULL) +
      scale_fill_brewer(palette = "Set3") +
      theme_bw(base_size = 19) +
      theme(legend.position = "right", legend.title = element_text(), legend.text = element_text(size = 13),
            panel.border = element_blank(),
            panel.grid.major.y = element_blank(),  panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(size = 0.5, colour= "grey80"),
            axis.line = element_blank(), axis.ticks = element_blank(),
            axis.text = element_text(color = "grey20", size = 12))
  })
})

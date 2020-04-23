output$table_results <- renderDT(results_aggregated(), rownames = FALSE, style = "bootstrap", filter = "top",
                                 extensions = 'Buttons', 
                                 options = list(scrollX = TRUE, scrollY = 300, paging = FALSE,
                                                dom = 'Bfrtip', buttons = c('csv', 'excel')))
output$timevis_future_hc <- renderHighchart({
  
  if(interventions$future_mat %>% nrow() == 0) return(NULL)
  
  if(interventions$future_mat %>% nrow() > 0) return({
    dta <- interventions$future_mat %>%
      filter(intervention %in% real_interventions) %>%
      mutate(date_end = date_end + 1,
             label = paste0(intervention, ": ", value, unit, " for ", difftime(date_end, date_start, units = "days") - 1, " days"))
    dta <- mutate_if(dta, is.Date, datetime_to_timestamp)

    # add intervention_num
    dta <- left_join(dta,
                     tibble(intervention = unique(dta$intervention),
                            intervention_num = 0:(n_distinct(dta$intervention) - 1)),
                     by = "intervention"
    )
    
    cat_interventions <-  dta %>% pull(intervention) %>% unique()
    
    hchart(dta, "xrange",
           hcaes(x = date_start, x2 = date_end, y = intervention_num), # partialFill = value/100),
           dataLabels = list(enabled = TRUE, format = "{point.value}")) %>% 
      hc_tooltip(pointFormat = "{point.label}") %>%
      hc_xAxis(title = FALSE, type = "datetime") %>% 
      hc_yAxis(title = FALSE, categories = cat_interventions) %>%
      hc_colors(c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5'))
  })
})
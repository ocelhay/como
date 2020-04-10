output$highchart_cases <- renderHighchart({
  req(simul_baseline$baseline_available)
  
  dta <- left_join(tibble(daily_incidence = simul_baseline$results$daily_incidence,
                          daily_total_cases = simul_baseline$results$daily_total_cases,
                          time = simul_baseline$results$time), 
                   cases_rv$data, 
                   by = c("time" = "date"))
  
  # X/Y scales
  if(input$focus_axis == "Observed")  {
    max_x <- dta$time[last(which(!is.na(dta$cases)))]
    max_y <- 1.2 * max(dta$cases, na.rm = TRUE)
  }
  if(input$focus_axis == "Predicted Reported")  {
    max_x <- max(dta$time)
    max_y <- 1.2 * max(simul_baseline$results$daily_incidence, na.rm = TRUE)
  }
  if(input$focus_axis == "Predicted Reported + Unreported")  {
    max_x <- max(dta$time)
    max_y <- 1.2 * max(simul_baseline$results$daily_total_cases, na.rm = TRUE)
  }
  
  dta2 <- dta %>%
    rename(Date = time) %>%
    filter(Date <= max_x)
    
    
      return(
        hchart(dta2, "area", name = "Predicted Reported", hcaes(x = Date, y = daily_incidence), color = "#00441b") %>% 
          hc_add_series(dta2, type = 'area', name = "Predicted Reported + Unreported", hcaes(y = daily_total_cases, x = Date), color = "#74c476", dashStyle = "longdash") %>%
          hc_add_series(dta2, type = 'line', name = "Observed", hcaes(y = cases, x = Date), color = "red") %>%
          hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:,.0f}<br/>", shared = TRUE) %>%
          hc_title(text = "Baseline Cases") %>%
          hc_yAxis(max = max_y, title = "Cases") %>%
          hc_xAxis(title = "")
      )
})
  
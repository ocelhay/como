output$highchart_mortality_dup <- renderHighchart({
  req(simul_baseline$baseline_available)
  
  # Baseline only ----
  if(!simul_interventions$interventions_available){

    dta <- left_join(tibble(cmortality = simul_baseline$results$cum_mortality,
                            time = simul_baseline$results$time), 
                     cases_rv$data, 
                     by = c("time" = "date"))
    
    # X/Y scales
    if(input$focus_axis_dup == "Observed")  {
      max_x <- dta$time[last(which(!is.na(dta$cumulative_death)))]
      max_y <- 1.2 * max(dta$cumulative_death, na.rm = TRUE)
    }
    if(input$focus_axis_dup == "Predicted Reported")  {
      max_x <- max(dta$time)
      max_y <- NA
    }
    if(input$focus_axis_dup == "Predicted Reported + Unreported")  {
      max_x <- max(dta$time)
      max_y <- NA
    }
    
    dta2 <- dta %>%
      transmute(Date = time, 
                CD = round(cmortality),
                D = cumulative_death) %>%
      filter(Date <= max_x)
    
    tt <- tooltip_table(c("Baseline, Cumulative Deaths: ", "Observed: "), 
                        c("{point.CD:,.0f}", "{point.D:,.0f}"))
    return(
      hchart(dta2, "line", hcaes(x = Date, y = CD), color = "#bae4bc") %>% 
        hc_add_series(dta2, type = 'line', hcaes(y = CD, x = Date), color = "#bae4bc", dashStyle = "longdash") %>%
        hc_add_series(dta2, type = 'line', hcaes(y = D, x = Date), color = "red") %>%
        hc_tooltip(pointFormat = tt, useHTML = TRUE) %>%
        hc_title(text = "COVID-19 Deaths") %>%
        hc_yAxis(max = max_y, title = "Deaths") %>%
        hc_xAxis(title = "")
    )
  }
  
  
  # Baseline & Interventions
  if(simul_interventions$interventions_available){
    dta <- left_join(
      tibble(cmortality_bas = simul_baseline$results$cum_mortality,
             time = simul_baseline$results$time),
      tibble(cmortality_int = simul_interventions$results$cum_mortality,
             time = simul_interventions$results$time),
      by = "time")
    
    dta <- left_join(dta, 
                     cases_rv$data, 
                     by = c("time" = "date"))
    
    # X/Y scales
    if(input$focus_axis_dup == "Observed")  {
      max_x <- dta$time[last(which(!is.na(dta$cumulative_death)))]
      max_y <- 1.2 * max(dta$cumulative_death, na.rm = TRUE)
    }
    if(input$focus_axis_dup == "Predicted Reported")  {
      max_x <- max(dta$time)
      max_y <- NA
    }
    if(input$focus_axis_dup == "Predicted Reported + Unreported")  {
      max_x <- max(dta$time)
      max_y <- NA
    }
    
    dta2 <- dta %>%
      transmute(Date = time, 
                CDB = round(cmortality_bas),
                CDI = round(cmortality_int),
                D = cumulative_death) %>%
      filter(Date <= max_x)
    
    tt <- tooltip_table(c("Baseline, Cumulative Deaths: ", "Interventions, Cumulative Deaths: ", "Observed: "), 
                        c("{point.CDB:,.0f}", "{point.CDI:,.0f}", "{point.D:,.0f}"))
    return(
      hchart(dta2, "line", hcaes(x = Date, y = CDB), color = "#bae4bc") %>% 
        hc_add_series(dta2, type = 'line', hcaes(y = CDB, x = Date), color = "#bae4bc") %>%
        hc_add_series(dta2, type = 'line', hcaes(y = CDI, x = Date), color = "#3182bd") %>%
        hc_add_series(dta2, type = 'line', hcaes(y = D, x = Date), color = "red") %>%
        hc_tooltip(pointFormat = tt, useHTML = TRUE) %>%
        hc_title(text = "COVID-19 Deaths") %>%
        hc_yAxis(max = max_y, title = "Deaths") %>%
        hc_xAxis(title = "")
    )
  }
})
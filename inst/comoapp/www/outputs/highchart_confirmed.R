output$highchart_confirmed <- renderHighchart({
  req(simul_baseline$baseline_available)
  
  # Baseline only ----
  if(!simul_interventions$interventions_available){

    dta <- left_join(tibble(dailyinc = simul_baseline$results$dailyinc0,
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
      max_y <- 1.2 * max(dta$dailyinc, na.rm = TRUE)
    }
    if(input$focus_axis == "Predicted Reported + Unreported")  {
      max_x <- max(dta$time)
      max_y <- NA
    }
    
    dta2 <- dta %>%
      transmute(Date = time, 
                BRUC = round(daily_total_cases),
                BRC = round(dailyinc), 
                ORC = cases) %>%
      filter(Date <= max_x)
    
    tt <- tooltip_table(c("Baseline, Reported + Unreported: ", "Baseline, Reported: ", "Observed: "), 
                        c("{point.BRUC:,.0f}", "{point.BRC:,.0f}", "{point.ORC:,.0f}"))
    return(
      hchart(dta2, "line", hcaes(x = Date, y = BRC), color = "#bae4bc") %>% 
        hc_add_series(dta2, type = 'line', hcaes(y = BRUC, x = Date), color = "#bae4bc", dashStyle = "longdash") %>%
        hc_add_series(dta2, type = 'line', hcaes(y = ORC, x = Date), color = "red") %>%
        hc_tooltip(pointFormat = tt, useHTML = TRUE) %>%
        hc_title(text = "COVID-19 Cases") %>%
        hc_yAxis(max = max_y, title = "Cases") %>%
        hc_xAxis(title = "")
    )
  }
  
  
  # Baseline & Interventions
  if(simul_interventions$interventions_available){
    dta <- left_join(
      tibble(dailyinc_bas = simul_baseline$results$dailyinc0,
             daily_total_cases_bas = simul_baseline$results$daily_total_cases,
             time = simul_baseline$results$time),
      tibble(dailyinc_int = simul_interventions$results$dailyinc0,
             daily_total_cases_int = simul_interventions$results$daily_total_cases,
             time = simul_interventions$results$time),
      by = "time")
    
    dta <- left_join(dta, 
                     cases_rv$data, 
                     by = c("time" = "date"))
    
    # X/Y scales
    if(input$focus_axis == "Observed")  {
      max_x <- dta$time[last(which(!is.na(dta$cases)))]
      max_y <- 1.2 * max(dta$cases, na.rm = TRUE)
    }
    if(input$focus_axis == "Predicted Reported")  {
      max_x <- max(dta$time)
      max_y <- 1.2 * max(dta$dailyinc_bas, na.rm = TRUE)
    }
    if(input$focus_axis == "Predicted Reported + Unreported")  {
      max_x <- max(dta$time)
      max_y <- NA
    }
    
    dta2 <- dta %>%
      transmute(Date = time, 
                BRUC = round(daily_total_cases_bas),
                BRC = round(dailyinc_bas),
                IRUC = round(daily_total_cases_int),
                IRC = round(dailyinc_int),
                ORC = cases) %>%
      filter(Date <= max_x)
    
    tt <- tooltip_table(c("Baseline, Reported + Unreported: ", "Baseline, Reported: ",
                          "Interven, Reported + Unreported: ", "Interven, Reported: ", "Observed: "), 
                        c("{point.BRUC:,.0f}", "{point.BRC:,.0f}", "{point.IRUC:,.0f}", "{point.IRC:,.0f}", "{point.ORC:,.0f}"))
    return(
      hchart(dta2, "line", hcaes(x = Date, y = BRC), color = "#bae4bc") %>% 
        hc_add_series(dta2, type = 'line', hcaes(y = BRUC, x = Date), color = "#bae4bc", dashStyle = "longdash") %>%
        hc_add_series(dta2, type = 'line', hcaes(y = IRC, x = Date), color = "#3182bd") %>%
        hc_add_series(dta2, type = 'line', hcaes(y = IRUC, x = Date), color = "#3182bd", dashStyle = "longdash") %>%
        hc_add_series(dta2, type = 'line', hcaes(y = ORC, x = Date), color = "red") %>%
        hc_tooltip(pointFormat = tt, useHTML = TRUE) %>%
        hc_title(text = "COVID-19 Cases") %>%
        hc_yAxis(max = max_y, title = "Cases") %>%
        hc_xAxis(title = "")
    )
  }
})
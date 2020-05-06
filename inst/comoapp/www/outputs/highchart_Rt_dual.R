output$highchart_Rt_dual_baseline <- renderHighchart({
  req(simul_baseline$baseline_available)
  req(simul_interventions$interventions_available)
  
  dta <- tibble(Rt = simul_baseline$results$Rt,
                Date = simul_baseline$results$time,
                One = 1)
  
  if (!input$show_all_days) dta <- dta %>% filter(wday(Date) == 4)
  
  max_y <- 1.2 * max(simul_baseline$results$Rt, simul_baseline$results$Rt, na.rm = TRUE)
  
  return(
    hchart(dta, "line", name = "Rt", hcaes(x = Date, y = Rt), color = "#74c476") %>%
      hc_add_series(dta, type = "line", name = "Threshold", color = "red", hcaes(x = Date, y = One)) %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:,.2f}<br/>", shared = TRUE) %>%
      hc_title(text = "Baseline Rt") %>%
      hc_yAxis(max = max_y, title = "Rt") %>%
      hc_xAxis(title = "") %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_items)))
  )
})

output$highchart_Rt_dual_interventions <- renderHighchart({
  req(simul_baseline$baseline_available)
  req(simul_interventions$interventions_available)
  
  dta <- tibble(Rt = simul_interventions$results$Rt,
                Date = simul_interventions$results$time,
                One = 1)
  
  if (!input$show_all_days) dta <- dta %>% filter(wday(Date) == 4)
  
  max_y <- 1.2 * max(simul_interventions$results$Rt, simul_interventions$results$Rt, na.rm = TRUE)
  
  return(
    hchart(dta, "line", name = "Rt", hcaes(x = Date, y = Rt), color = "#6baed6") %>%
      hc_add_series(dta, type = "line", name = "Threshold", color = "red", hcaes(x = Date, y = One)) %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:,.2f}<br/>", shared = TRUE) %>%
      hc_title(text = "Future Scenarios Rt") %>%
      hc_yAxis(max = max_y, title = "Rt") %>%
      hc_xAxis(title = "") %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_items)))
  )
})
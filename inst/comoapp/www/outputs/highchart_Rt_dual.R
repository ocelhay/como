output$highchart_Rt_dual_baseline <- renderHighchart({
  req(simul_baseline$baseline_available)
  req(simul_interventions$interventions_available)
  
  dta <- tibble(Rt = simul_baseline$results$Rt,
                Date = simul_baseline$results$time)
  
  max_y <- 1.2 * max(simul_baseline$results$Rt, simul_baseline$results$Rt, na.rm = TRUE)
  
  return(
    hchart(dta, "line", name = "Rt", hcaes(x = Date, y = Rt), color = "#74c476") %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:,.2f}<br/>", shared = TRUE) %>%
      hc_title(text = "Baseline Rt") %>%
      hc_yAxis(max = max_y, title = "Rt") %>%
      hc_xAxis(title = "")
  )
})

output$highchart_Rt_dual_interventions <- renderHighchart({
  req(simul_baseline$baseline_available)
  req(simul_interventions$interventions_available)
  
  dta <- tibble(Rt = simul_interventions$results$Rt,
                Date = simul_interventions$results$time)
  
  max_y <- 1.2 * max(simul_interventions$results$Rt, simul_interventions$results$Rt, na.rm = TRUE)
  
  return(
    hchart(dta, "line", name = "Rt", hcaes(x = Date, y = Rt), color = "#6baed6") %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             {point.y:,.2f}<br/>", shared = TRUE) %>%
      hc_title(text = "Future Scenarios Rt") %>%
      hc_yAxis(max = max_y, title = "Rt") %>%
      hc_xAxis(title = "")
  )
})
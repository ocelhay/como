output$plot_cases_data <- renderHighchart({
  req(cases_rv$data)
  req(nrow(cases_rv$data) > 0)
  
  hchart(cases_rv$data, type = 'line', hcaes(y = cases, x = date)) %>%
    hc_colors("red") %>%
    hc_tooltip(headerFormat = "", pointFormat = "{point.date}: <br>{point.cases} new cases")
})
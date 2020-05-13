output$text_total_death_baseline <- renderText({
  req(simul_baseline$baseline_available)
  
  n <- simul_baseline$results$total_deaths_end
  
  return(
    paste0(
      as.character(
        div(class = "n_box_baseline",
            h3(paste0(format(n, big.mark = ","))),
            p("Covid-19 attributable deaths during the range of simulation.")
        ))))
})

output$text_total_death_baseline_dup <- renderText({
  req(simul_baseline$baseline_available)
  
  n <- simul_baseline$results$total_deaths_end
  
  return(
    paste0(
      as.character(
        div(class = "n_box_baseline",
            h3(paste0(format(n, big.mark = ","))),
            p("Covid-19 attributable deaths during the range of simulation.")
        ))))
})

output$text_total_death_interventions <- renderText({
  req(simul_interventions$interventions_available)
  
  n <- simul_interventions$results$total_deaths_end
  reduction <- n - simul_baseline$results$total_deaths_end
  
  return(
    paste0(
      as.character(
        div(class = "n_box_interventions",
            div(class = "icon_box", h3(paste0("(", format(reduction, big.mark = ","), ")"))),
            h3(paste0(format(n, big.mark = ","))),
            p("Covid-19 attributable deaths during the range of simulation.")
        ))))
})
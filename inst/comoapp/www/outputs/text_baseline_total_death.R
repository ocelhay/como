output$text_baseline_total_death <- renderText({
  req(simul_baseline$baseline_available)
  
  n <- simul_baseline$results$total_deaths
  
  return(
    paste0(
      as.character(
        div(class = "n_box_baseline",
            h3(paste0(format(n, big.mark = ","))),
            h4("Deaths"),
            p("during the epidemic.")
        ))))
})
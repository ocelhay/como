output$text_doubling_time <- renderText({
  req(simul_baseline$baseline_available)
  
  paste0(
    as.character(
      div(class = "n_box_baseline",
          div(class = "icon_box", icon("draft2digital")),
          conf_interval(simul_baseline$results$doubling_time_min, 
                        simul_baseline$results$doubling_time_mean,
                        simul_baseline$results$doubling_time_max,
                        unit = " days"),
          p("to double the number of infections at inception.")
      )))
})
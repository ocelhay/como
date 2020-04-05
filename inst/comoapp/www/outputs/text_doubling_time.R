output$text_doubling_time <- renderText({
  req(simul_baseline$baseline_available)
  
  n <- simul_baseline$results$doubling_time
  
  return(
    paste0(
      as.character(
        div(class = "n_box",
            div(class = "icon_box", icon("draft2digital")),
            h3(paste0(n, " days")),
            p("to double the number of infections at inception.")
        ))))
})
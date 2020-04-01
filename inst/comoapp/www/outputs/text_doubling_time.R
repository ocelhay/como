output$text_doubling_time <- renderText({
  req(simul_baseline$baseline_available)
  
  doub0 <- round(log(2)*7/(log(simul_baseline$results$dailyinc0[2+7] / simul_baseline$results$dailyinc0[2])), 2)
  
  return(
    paste0(
      as.character(
        div(class = "n_box",
            div(class = "icon_box", icon("draft2digital")),
            h3(paste0(doub0, " days")),
            p("to double the number of infections at inception.")
        ))))
})
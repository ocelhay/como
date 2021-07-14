output$plot_rr_parameters_baseline <- renderPlot({
  req(interventions$valid_baseline_interventions)
  req(interventions$baseline_mat %>% filter(intervention %in% real_rr_interventions) %>% nrow() > 0)
  
  param <- tibble(date = seq(input$date_range[1], input$date_range[2], by = 1),
                  p = input$p,
                  sigmaR = input$sigmaR,
                  pdeath_h = input$pdeath_h,
                  pdeath_icu = input$pdeath_icu,
                  pdeath_vent = input$pdeath_vent)
  
  # Transmissibility ----
  trans <- interventions$baseline_mat %>%
    filter(intervention == "Transmissibility")
  
  if(nrow(trans) >= 1) {
    
    trans_tib <- tibble(date = NULL, value = NULL)
    
    for (i in 1:nrow(trans)) {
      trans_tib <- bind_rows(trans_tib,
                             tibble(date = seq(trans[[i , "date_start"]], trans[[i , "date_end"]], by = 1),
                                    Transmissibility = trans[[i, "value"]])
      )
    }
    
    trans_tib <- trans_tib %>% 
      distinct(date, .keep_all = TRUE)
    
    param <- left_join(param, trans_tib, by = "date") %>%
      replace_na(list(Transmissibility = 1)) %>%
      mutate(p = Transmissibility * p)
  }
  
  # Lethality ----
  leth <- interventions$baseline_mat %>%
    filter(intervention == "Lethality")
  
  if(nrow(leth) >= 1) {
    leth_tib <- tibble(date = NULL, value = NULL)
    
    for (i in 1:nrow(leth)) {
      leth_tib <- bind_rows(leth_tib,
                            tibble(date = seq(leth[[i , "date_start"]], leth[[i , "date_end"]], by = 1),
                                   Lethality = leth[[i, "value"]])
      )
    }
    
    leth_tib <- leth_tib %>% distinct(date, .keep_all = TRUE)
    
    param <- left_join(param, leth_tib, by = "date") %>%
      replace_na(list(Lethality = 1)) %>%
      mutate(pdeath_h = Lethality * pdeath_h,
             pdeath_icu = Lethality * pdeath_icu,
             pdeath_vent = Lethality * pdeath_vent)
  }
  
  # Breakthrough infection probability ----
  bip <- interventions$baseline_mat %>%
    filter(intervention == "Breakthrough infection probability")
  
  if(nrow(bip) >= 1) {
    bip_tib <- tibble(date = NULL, value = NULL)
    
    for (i in 1:nrow(bip)) {
      bip_tib <- bind_rows(bip_tib,
                           tibble(date = seq(bip[[i , "date_start"]], bip[[i , "date_end"]], by = 1),
                                  Breakthrough = bip[[i, "value"]])
      )
    }
    
    bip_tib <- bip_tib %>% 
      distinct(date, .keep_all = TRUE)
    
    param <- left_join(param, bip_tib, by = "date") %>%
      replace_na(list(Breakthrough = 1)) %>%
      mutate(sigmaR = Breakthrough * sigmaR)
  }
  
  param <- param %>% 
    select(date, p:pdeath_vent) %>%
    pivot_longer(cols =  p:pdeath_vent, values_to = "value")
  
  ggplot(param, aes(x = date, y = value)) +
    geom_line() +
    facet_wrap(vars(name), scales = "free", ncol = 2)
})
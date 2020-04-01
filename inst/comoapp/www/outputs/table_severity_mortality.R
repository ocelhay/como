output$table_severity_mortality <- renderTable(
  mort_sever$data %>%
    rename(`Age Category` = age_category, `Probability of dying per infection` = mortality, `Probability of severe case per infection` = severity)
)
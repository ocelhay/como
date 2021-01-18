js_1 <- "].every( (val) => { return val === '_';} ))"
js_2 <- "['Mass Testing', 'Vaccination']"
js_3 <- "['School Closures']"

# Generate interventions_baseline.R ----
sink("/Users/olivier/Desktop/interventions_baseline.R")

cat(
'div(
  br(),
  fluidRow(
    column(1, h5("#")),
    column(3, h5("Nature of Intervention:")),
    column(3, h5("Date Range:")),
    column(2, h5("Value:") %>% helper(type = "markdown", content = "help_value_unit_interventions", colour = "red")),
    column(3, "Age Groups (when applicable)")
  ),
  '
)

i <- 1
print(
  glue(
    'conditionalPanel(condition = "true",
                   fluidRow(
                     column(1, "#{i}"),
                     column(3, selectInput("baseline_intervention_{i}", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("baseline_daterange_{i}", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("baseline_coverage_{i}", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("{js_2}.includes(input.baseline_intervention_{i})", 
                                                pickerInput("baseline_age_group_{i}", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)),
                               conditionalPanel("{js_3}.includes(input.baseline_intervention_{i})", 
                                                pickerInput("baseline_age_group_{i}", NULL, choices = vec_age_categories[1:4], selected = vec_age_categories[1:4], options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE))
                                                )
                   )
  ),'
  )
)

for (i in 2:100) {
  print(
    glue(
      'conditionalPanel(condition = paste0("!([", 
                                      paste0("input.baseline_intervention_", ({i}-1):100, collapse = ", "), 
                                      "{js_1}"),
                   fluidRow(
                     column(1, "#{i}"),
                     column(3, selectInput("baseline_intervention_{i}", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("baseline_daterange_{i}", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("baseline_coverage_{i}", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("{js_2}.includes(input.baseline_intervention_{i})", 
                                                pickerInput("baseline_age_group_{i}", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)),
                               conditionalPanel("{js_3}.includes(input.baseline_intervention_{i})", 
                                                pickerInput("baseline_age_group_{i}", NULL, choices = vec_age_categories[1:4], selected = vec_age_categories[1:4], options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE))
                                                )
                   )
  ),'
    )
  )
}

cat(")")
sink()

# Generate interventions_future.R ----
sink("/Users/olivier/Desktop/interventions_future.R")

cat(
  'div(
  br(),
  fluidRow(
    column(1, h5("#")),
    column(3, h5("Nature of Intervention:")),
    column(3, h5("Date Range:")),
    column(2, h5("Value:") %>% helper(type = "markdown", content = "help_value_unit_interventions", colour = "red")),
    column(3, "Age Groups (when applicable)")
  ),
  '
)

i <- 1
print(
  glue(
    'conditionalPanel(condition = "true",
                   fluidRow(
                     column(1, "#{i}"),
                     column(3, selectInput("future_intervention_{i}", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_{i}", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_{i}", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("{js_2}.includes(input.future_intervention_{i})", 
                                                pickerInput("future_age_group_{i}", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)),
                              conditionalPanel("{js_3}.includes(input.future_intervention_{i})", 
                                                pickerInput("future_age_group_{i}", NULL, choices = vec_age_categories[1:4], selected = vec_age_categories[1:4], options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE))
                                                )
                   )
  ),'
  )
)

for (i in 2:100) {
  print(
    glue(
      'conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", ({i}-1):100, collapse = ", "), 
                                      "{js_1}"),
                   fluidRow(
                     column(1, "#{i}"),
                     column(3, selectInput("future_intervention_{i}", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_{i}", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_{i}", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("{js_2}.includes(input.future_intervention_{i})", 
                                                pickerInput("future_age_group_{i}", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)),
                              conditionalPanel("{js_3}.includes(input.future_intervention_{i})", 
                                                pickerInput("future_age_group_{i}", NULL, choices = vec_age_categories[1:4], selected = vec_age_categories[1:4], options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE))
                                                )
                   )
  ),'
    )
  )
}

cat(")")
sink()
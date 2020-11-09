div(
  br(),
  fluidRow(
    column(1, h5("#")),
    column(3, h5("Nature of Intervention:")),
    column(3, h5("Date Range:")),
    column(2, h5("Value:") %>% helper(type = "markdown", content = "help_value_unit_interventions", colour = "red")),
    column(3, "Age Groups (when applicable)")
  ),
  conditionalPanel(condition = "true",
                   fluidRow(
                     column(1, "#1"),
                     column(3, selectInput("future_intervention_1", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_1", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_1", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_1)", 
                                                pickerInput("future_age_group_1", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (2-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#2"),
                     column(3, selectInput("future_intervention_2", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_2", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_2", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_2)", 
                                                pickerInput("future_age_group_2", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (3-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#3"),
                     column(3, selectInput("future_intervention_3", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_3", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_3", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_3)", 
                                                pickerInput("future_age_group_3", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (4-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#4"),
                     column(3, selectInput("future_intervention_4", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_4", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_4", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_4)", 
                                                pickerInput("future_age_group_4", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (5-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#5"),
                     column(3, selectInput("future_intervention_5", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_5", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_5", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_5)", 
                                                pickerInput("future_age_group_5", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (6-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#6"),
                     column(3, selectInput("future_intervention_6", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_6", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_6", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_6)", 
                                                pickerInput("future_age_group_6", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (7-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#7"),
                     column(3, selectInput("future_intervention_7", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_7", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_7", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_7)", 
                                                pickerInput("future_age_group_7", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (8-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#8"),
                     column(3, selectInput("future_intervention_8", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_8", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_8", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_8)", 
                                                pickerInput("future_age_group_8", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (9-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#9"),
                     column(3, selectInput("future_intervention_9", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_9", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_9", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_9)", 
                                                pickerInput("future_age_group_9", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (10-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#10"),
                     column(3, selectInput("future_intervention_10", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_10", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_10", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_10)", 
                                                pickerInput("future_age_group_10", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (11-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#11"),
                     column(3, selectInput("future_intervention_11", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_11", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_11", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_11)", 
                                                pickerInput("future_age_group_11", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (12-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#12"),
                     column(3, selectInput("future_intervention_12", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_12", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_12", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_12)", 
                                                pickerInput("future_age_group_12", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (13-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#13"),
                     column(3, selectInput("future_intervention_13", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_13", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_13", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_13)", 
                                                pickerInput("future_age_group_13", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (14-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#14"),
                     column(3, selectInput("future_intervention_14", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_14", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_14", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_14)", 
                                                pickerInput("future_age_group_14", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (15-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#15"),
                     column(3, selectInput("future_intervention_15", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_15", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_15", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_15)", 
                                                pickerInput("future_age_group_15", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (16-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#16"),
                     column(3, selectInput("future_intervention_16", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_16", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_16", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_16)", 
                                                pickerInput("future_age_group_16", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (17-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#17"),
                     column(3, selectInput("future_intervention_17", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_17", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_17", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_17)", 
                                                pickerInput("future_age_group_17", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (18-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#18"),
                     column(3, selectInput("future_intervention_18", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_18", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_18", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_18)", 
                                                pickerInput("future_age_group_18", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (19-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#19"),
                     column(3, selectInput("future_intervention_19", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_19", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_19", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_19)", 
                                                pickerInput("future_age_group_19", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (20-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#20"),
                     column(3, selectInput("future_intervention_20", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_20", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_20", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_20)", 
                                                pickerInput("future_age_group_20", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (21-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#21"),
                     column(3, selectInput("future_intervention_21", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_21", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_21", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_21)", 
                                                pickerInput("future_age_group_21", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (22-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#22"),
                     column(3, selectInput("future_intervention_22", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_22", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_22", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_22)", 
                                                pickerInput("future_age_group_22", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (23-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#23"),
                     column(3, selectInput("future_intervention_23", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_23", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_23", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_23)", 
                                                pickerInput("future_age_group_23", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (24-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#24"),
                     column(3, selectInput("future_intervention_24", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_24", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_24", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_24)", 
                                                pickerInput("future_age_group_24", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (25-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#25"),
                     column(3, selectInput("future_intervention_25", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_25", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_25", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_25)", 
                                                pickerInput("future_age_group_25", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (26-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#26"),
                     column(3, selectInput("future_intervention_26", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_26", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_26", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_26)", 
                                                pickerInput("future_age_group_26", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (27-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#27"),
                     column(3, selectInput("future_intervention_27", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_27", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_27", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_27)", 
                                                pickerInput("future_age_group_27", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (28-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#28"),
                     column(3, selectInput("future_intervention_28", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_28", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_28", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_28)", 
                                                pickerInput("future_age_group_28", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (29-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#29"),
                     column(3, selectInput("future_intervention_29", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_29", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_29", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_29)", 
                                                pickerInput("future_age_group_29", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (30-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#30"),
                     column(3, selectInput("future_intervention_30", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_30", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_30", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_30)", 
                                                pickerInput("future_age_group_30", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (31-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#31"),
                     column(3, selectInput("future_intervention_31", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_31", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_31", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_31)", 
                                                pickerInput("future_age_group_31", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (32-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#32"),
                     column(3, selectInput("future_intervention_32", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_32", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_32", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_32)", 
                                                pickerInput("future_age_group_32", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (33-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#33"),
                     column(3, selectInput("future_intervention_33", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_33", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_33", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_33)", 
                                                pickerInput("future_age_group_33", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (34-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#34"),
                     column(3, selectInput("future_intervention_34", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_34", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_34", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_34)", 
                                                pickerInput("future_age_group_34", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (35-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#35"),
                     column(3, selectInput("future_intervention_35", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_35", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_35", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_35)", 
                                                pickerInput("future_age_group_35", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (36-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#36"),
                     column(3, selectInput("future_intervention_36", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_36", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_36", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_36)", 
                                                pickerInput("future_age_group_36", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (37-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#37"),
                     column(3, selectInput("future_intervention_37", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_37", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_37", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_37)", 
                                                pickerInput("future_age_group_37", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (38-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#38"),
                     column(3, selectInput("future_intervention_38", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_38", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_38", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_38)", 
                                                pickerInput("future_age_group_38", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (39-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#39"),
                     column(3, selectInput("future_intervention_39", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_39", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_39", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_39)", 
                                                pickerInput("future_age_group_39", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (40-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#40"),
                     column(3, selectInput("future_intervention_40", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_40", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_40", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_40)", 
                                                pickerInput("future_age_group_40", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (41-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#41"),
                     column(3, selectInput("future_intervention_41", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_41", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_41", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_41)", 
                                                pickerInput("future_age_group_41", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (42-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#42"),
                     column(3, selectInput("future_intervention_42", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_42", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_42", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_42)", 
                                                pickerInput("future_age_group_42", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (43-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#43"),
                     column(3, selectInput("future_intervention_43", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_43", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_43", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_43)", 
                                                pickerInput("future_age_group_43", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (44-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#44"),
                     column(3, selectInput("future_intervention_44", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_44", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_44", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_44)", 
                                                pickerInput("future_age_group_44", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (45-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#45"),
                     column(3, selectInput("future_intervention_45", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_45", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_45", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_45)", 
                                                pickerInput("future_age_group_45", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (46-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#46"),
                     column(3, selectInput("future_intervention_46", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_46", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_46", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_46)", 
                                                pickerInput("future_age_group_46", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (47-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#47"),
                     column(3, selectInput("future_intervention_47", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_47", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_47", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_47)", 
                                                pickerInput("future_age_group_47", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (48-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#48"),
                     column(3, selectInput("future_intervention_48", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_48", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_48", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_48)", 
                                                pickerInput("future_age_group_48", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (49-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#49"),
                     column(3, selectInput("future_intervention_49", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_49", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_49", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_49)", 
                                                pickerInput("future_age_group_49", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (50-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#50"),
                     column(3, selectInput("future_intervention_50", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_50", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_50", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_50)", 
                                                pickerInput("future_age_group_50", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (51-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#51"),
                     column(3, selectInput("future_intervention_51", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_51", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_51", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_51)", 
                                                pickerInput("future_age_group_51", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (52-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#52"),
                     column(3, selectInput("future_intervention_52", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_52", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_52", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_52)", 
                                                pickerInput("future_age_group_52", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (53-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#53"),
                     column(3, selectInput("future_intervention_53", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_53", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_53", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_53)", 
                                                pickerInput("future_age_group_53", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (54-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#54"),
                     column(3, selectInput("future_intervention_54", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_54", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_54", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_54)", 
                                                pickerInput("future_age_group_54", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (55-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#55"),
                     column(3, selectInput("future_intervention_55", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_55", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_55", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_55)", 
                                                pickerInput("future_age_group_55", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (56-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#56"),
                     column(3, selectInput("future_intervention_56", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_56", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_56", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_56)", 
                                                pickerInput("future_age_group_56", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (57-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#57"),
                     column(3, selectInput("future_intervention_57", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_57", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_57", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_57)", 
                                                pickerInput("future_age_group_57", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (58-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#58"),
                     column(3, selectInput("future_intervention_58", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_58", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_58", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_58)", 
                                                pickerInput("future_age_group_58", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (59-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#59"),
                     column(3, selectInput("future_intervention_59", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_59", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_59", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_59)", 
                                                pickerInput("future_age_group_59", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (60-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#60"),
                     column(3, selectInput("future_intervention_60", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_60", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_60", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_60)", 
                                                pickerInput("future_age_group_60", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (61-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#61"),
                     column(3, selectInput("future_intervention_61", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_61", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_61", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_61)", 
                                                pickerInput("future_age_group_61", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (62-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#62"),
                     column(3, selectInput("future_intervention_62", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_62", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_62", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_62)", 
                                                pickerInput("future_age_group_62", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (63-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#63"),
                     column(3, selectInput("future_intervention_63", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_63", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_63", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_63)", 
                                                pickerInput("future_age_group_63", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (64-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#64"),
                     column(3, selectInput("future_intervention_64", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_64", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_64", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_64)", 
                                                pickerInput("future_age_group_64", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (65-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#65"),
                     column(3, selectInput("future_intervention_65", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_65", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_65", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_65)", 
                                                pickerInput("future_age_group_65", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (66-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#66"),
                     column(3, selectInput("future_intervention_66", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_66", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_66", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_66)", 
                                                pickerInput("future_age_group_66", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (67-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#67"),
                     column(3, selectInput("future_intervention_67", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_67", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_67", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_67)", 
                                                pickerInput("future_age_group_67", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (68-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#68"),
                     column(3, selectInput("future_intervention_68", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_68", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_68", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_68)", 
                                                pickerInput("future_age_group_68", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (69-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#69"),
                     column(3, selectInput("future_intervention_69", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_69", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_69", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_69)", 
                                                pickerInput("future_age_group_69", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (70-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#70"),
                     column(3, selectInput("future_intervention_70", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_70", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_70", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_70)", 
                                                pickerInput("future_age_group_70", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (71-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#71"),
                     column(3, selectInput("future_intervention_71", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_71", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_71", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_71)", 
                                                pickerInput("future_age_group_71", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (72-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#72"),
                     column(3, selectInput("future_intervention_72", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_72", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_72", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_72)", 
                                                pickerInput("future_age_group_72", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (73-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#73"),
                     column(3, selectInput("future_intervention_73", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_73", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_73", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_73)", 
                                                pickerInput("future_age_group_73", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (74-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#74"),
                     column(3, selectInput("future_intervention_74", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_74", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_74", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_74)", 
                                                pickerInput("future_age_group_74", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (75-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#75"),
                     column(3, selectInput("future_intervention_75", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_75", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_75", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_75)", 
                                                pickerInput("future_age_group_75", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (76-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#76"),
                     column(3, selectInput("future_intervention_76", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_76", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_76", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_76)", 
                                                pickerInput("future_age_group_76", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (77-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#77"),
                     column(3, selectInput("future_intervention_77", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_77", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_77", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_77)", 
                                                pickerInput("future_age_group_77", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (78-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#78"),
                     column(3, selectInput("future_intervention_78", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_78", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_78", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_78)", 
                                                pickerInput("future_age_group_78", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (79-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#79"),
                     column(3, selectInput("future_intervention_79", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_79", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_79", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_79)", 
                                                pickerInput("future_age_group_79", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (80-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#80"),
                     column(3, selectInput("future_intervention_80", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_80", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_80", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_80)", 
                                                pickerInput("future_age_group_80", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (81-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#81"),
                     column(3, selectInput("future_intervention_81", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_81", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_81", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_81)", 
                                                pickerInput("future_age_group_81", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (82-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#82"),
                     column(3, selectInput("future_intervention_82", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_82", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_82", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_82)", 
                                                pickerInput("future_age_group_82", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (83-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#83"),
                     column(3, selectInput("future_intervention_83", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_83", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_83", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_83)", 
                                                pickerInput("future_age_group_83", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (84-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#84"),
                     column(3, selectInput("future_intervention_84", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_84", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_84", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_84)", 
                                                pickerInput("future_age_group_84", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (85-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#85"),
                     column(3, selectInput("future_intervention_85", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_85", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_85", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_85)", 
                                                pickerInput("future_age_group_85", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (86-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#86"),
                     column(3, selectInput("future_intervention_86", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_86", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_86", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_86)", 
                                                pickerInput("future_age_group_86", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (87-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#87"),
                     column(3, selectInput("future_intervention_87", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_87", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_87", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_87)", 
                                                pickerInput("future_age_group_87", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (88-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#88"),
                     column(3, selectInput("future_intervention_88", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_88", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_88", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_88)", 
                                                pickerInput("future_age_group_88", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (89-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#89"),
                     column(3, selectInput("future_intervention_89", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_89", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_89", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_89)", 
                                                pickerInput("future_age_group_89", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (90-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#90"),
                     column(3, selectInput("future_intervention_90", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_90", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_90", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_90)", 
                                                pickerInput("future_age_group_90", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (91-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#91"),
                     column(3, selectInput("future_intervention_91", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_91", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_91", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_91)", 
                                                pickerInput("future_age_group_91", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (92-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#92"),
                     column(3, selectInput("future_intervention_92", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_92", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_92", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_92)", 
                                                pickerInput("future_age_group_92", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (93-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#93"),
                     column(3, selectInput("future_intervention_93", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_93", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_93", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_93)", 
                                                pickerInput("future_age_group_93", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (94-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#94"),
                     column(3, selectInput("future_intervention_94", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_94", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_94", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_94)", 
                                                pickerInput("future_age_group_94", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (95-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#95"),
                     column(3, selectInput("future_intervention_95", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_95", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_95", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_95)", 
                                                pickerInput("future_age_group_95", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (96-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#96"),
                     column(3, selectInput("future_intervention_96", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_96", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_96", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_96)", 
                                                pickerInput("future_age_group_96", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (97-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#97"),
                     column(3, selectInput("future_intervention_97", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_97", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_97", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_97)", 
                                                pickerInput("future_age_group_97", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (98-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#98"),
                     column(3, selectInput("future_intervention_98", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_98", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_98", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_98)", 
                                                pickerInput("future_age_group_98", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (99-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#99"),
                     column(3, selectInput("future_intervention_99", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_99", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_99", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_99)", 
                                                pickerInput("future_age_group_99", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
  conditionalPanel(condition = paste0("!([", 
                                      paste0("input.future_intervention_", (100-1):100, collapse = ", "), 
                                      "].every( (val) => { return val === '_';} ))"),
                   fluidRow(
                     column(1, "#100"),
                     column(3, selectInput("future_intervention_100", NULL, all_interventions, selected = new_intervention_value)),
                     column(3, dateRangeInput("future_daterange_100", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                     column(2, sliderInput("future_coverage_100", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE)),
                     column(3, conditionalPanel("['Mass Testing', 'School Closures', 'Vaccination'].includes(input.future_intervention_100)", 
                                                pickerInput("future_age_group_100", NULL, choices = vec_age_categories, selected = vec_age_categories, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"), multiple = TRUE)))
                   )
  ),
)
div(
  br(),
  conditionalPanel("input.nb_interventions_future >= 1", 
                   fluidRow(
                     column(4, h5("Interventions:")),
                     column(5, h5("Date range:")),
                     column(3, h5("Value:") %>% helper(type = "markdown", content = "help_value_unit_interventions", colour = "red"))
                   )
  ),
  conditionalPanel("input.nb_interventions_future >= 1", fluidRow(
    column(4, selectInput("future_intervention_1", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_1", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_1", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 2", fluidRow(
    column(4, selectInput("future_intervention_2", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_2", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_2", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 3", fluidRow(
    column(4, selectInput("future_intervention_3", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_3", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_3", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 4", fluidRow(
    column(4, selectInput("future_intervention_4", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_4", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_4", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 5", fluidRow(
    column(4, selectInput("future_intervention_5", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_5", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_5", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 6", fluidRow(
    column(4, selectInput("future_intervention_6", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_6", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_6", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 7", fluidRow(
    column(4, selectInput("future_intervention_7", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_7", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_7", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 8", fluidRow(
    column(4, selectInput("future_intervention_8", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_8", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_8", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 9", fluidRow(
    column(4, selectInput("future_intervention_9", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_9", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_9", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 10", fluidRow(
    column(4, selectInput("future_intervention_10", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_10", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_10", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 11", fluidRow(
    column(4, selectInput("future_intervention_11", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_11", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_11", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 12", fluidRow(
    column(4, selectInput("future_intervention_12", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_12", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_12", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 13", fluidRow(
    column(4, selectInput("future_intervention_13", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_13", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_13", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 14", fluidRow(
    column(4, selectInput("future_intervention_14", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_14", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_14", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 15", fluidRow(
    column(4, selectInput("future_intervention_15", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_15", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_15", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 16", fluidRow(
    column(4, selectInput("future_intervention_16", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_16", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_16", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 17", fluidRow(
    column(4, selectInput("future_intervention_17", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_17", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_17", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 18", fluidRow(
    column(4, selectInput("future_intervention_18", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_18", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_18", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 19", fluidRow(
    column(4, selectInput("future_intervention_19", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_19", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_19", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 20", fluidRow(
    column(4, selectInput("future_intervention_20", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_20", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_20", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 21", fluidRow(
    column(4, selectInput("future_intervention_21", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_21", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_21", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 22", fluidRow(
    column(4, selectInput("future_intervention_22", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_22", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_22", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 23", fluidRow(
    column(4, selectInput("future_intervention_23", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_23", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_23", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 24", fluidRow(
    column(4, selectInput("future_intervention_24", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_24", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_24", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 25", fluidRow(
    column(4, selectInput("future_intervention_25", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_25", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_25", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 26", fluidRow(
    column(4, selectInput("future_intervention_26", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_26", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_26", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 27", fluidRow(
    column(4, selectInput("future_intervention_27", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_27", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_27", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 28", fluidRow(
    column(4, selectInput("future_intervention_28", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_28", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_28", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 29", fluidRow(
    column(4, selectInput("future_intervention_29", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_29", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_29", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 30", fluidRow(
    column(4, selectInput("future_intervention_30", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_30", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_30", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  
  conditionalPanel("input.nb_interventions_future >= 31", fluidRow(
    column(4, selectInput("future_intervention_31", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_31", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_31", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 32", fluidRow(
    column(4, selectInput("future_intervention_32", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_32", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_32", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 33", fluidRow(
    column(4, selectInput("future_intervention_33", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_33", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_33", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 34", fluidRow(
    column(4, selectInput("future_intervention_34", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_34", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_34", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 35", fluidRow(
    column(4, selectInput("future_intervention_35", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_35", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_35", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 36", fluidRow(
    column(4, selectInput("future_intervention_36", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_36", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_36", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 37", fluidRow(
    column(4, selectInput("future_intervention_37", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_37", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_37", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 38", fluidRow(
    column(4, selectInput("future_intervention_38", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_38", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_38", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 39", fluidRow(
    column(4, selectInput("future_intervention_39", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_39", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_39", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 40", fluidRow(
    column(4, selectInput("future_intervention_40", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_40", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_40", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  
  conditionalPanel("input.nb_interventions_future >= 41", fluidRow(
    column(4, selectInput("future_intervention_41", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_41", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_41", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 42", fluidRow(
    column(4, selectInput("future_intervention_42", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_42", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_42", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 43", fluidRow(
    column(4, selectInput("future_intervention_43", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_43", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_43", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 44", fluidRow(
    column(4, selectInput("future_intervention_44", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_44", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_44", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 45", fluidRow(
    column(4, selectInput("future_intervention_45", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_45", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_45", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 46", fluidRow(
    column(4, selectInput("future_intervention_46", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_46", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_46", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 47", fluidRow(
    column(4, selectInput("future_intervention_47", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_47", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_47", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 48", fluidRow(
    column(4, selectInput("future_intervention_48", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_48", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_48", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 49", fluidRow(
    column(4, selectInput("future_intervention_49", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_49", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_49", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  )),
  conditionalPanel("input.nb_interventions_future >= 50", fluidRow(
    column(4, selectInput("future_intervention_50", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("future_daterange_50", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
    column(3, sliderInput("future_coverage_50", NULL, min = 0, max = 100, value = new_coverage_value, ticks = FALSE))
  ))
)
div(
  br(),
  conditionalPanel("output.baseline_nb >= 1", 
                   fluidRow(
                     column(4, h5("Interventions:")),
                     column(5, h5("Date Range (YY-MM-DD):")),
                     column(3, h5("Coverage:"))
                   )
  ),
  conditionalPanel("output.baseline_nb >= 1", fluidRow(
    column(4, selectInput("baseline_intervention_1", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_1", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_1", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 2", fluidRow(
    column(4, selectInput("baseline_intervention_2", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_2", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_2", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 3", fluidRow(
    column(4, selectInput("baseline_intervention_3", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_3", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_3", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 4", fluidRow(
    column(4, selectInput("baseline_intervention_4", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_4", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_4", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 5", fluidRow(
    column(4, selectInput("baseline_intervention_5", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_5", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_5", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 6", fluidRow(
    column(4, selectInput("baseline_intervention_6", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_6", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_6", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 7", fluidRow(
    column(4, selectInput("baseline_intervention_7", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_7", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_7", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 8", fluidRow(
    column(4, selectInput("baseline_intervention_8", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_8", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_8", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 9", fluidRow(
    column(4, selectInput("baseline_intervention_9", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_9", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_9", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 10", fluidRow(
    column(4, selectInput("baseline_intervention_10", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_10", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_10", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 11", fluidRow(
    column(4, selectInput("baseline_intervention_11", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_11", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_11", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 12", fluidRow(
    column(4, selectInput("baseline_intervention_12", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_12", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_12", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 13", fluidRow(
    column(4, selectInput("baseline_intervention_13", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_13", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_13", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 14", fluidRow(
    column(4, selectInput("baseline_intervention_14", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_14", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_14", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 15", fluidRow(
    column(4, selectInput("baseline_intervention_15", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_15", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_15", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 16", fluidRow(
    column(4, selectInput("baseline_intervention_16", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_16", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_16", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 17", fluidRow(
    column(4, selectInput("baseline_intervention_17", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_17", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_17", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 18", fluidRow(
    column(4, selectInput("baseline_intervention_18", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_18", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_18", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 19", fluidRow(
    column(4, selectInput("baseline_intervention_19", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_19", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_19", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 20", fluidRow(
    column(4, selectInput("baseline_intervention_20", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_20", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_20", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 21", fluidRow(
    column(4, selectInput("baseline_intervention_21", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_21", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_21", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 22", fluidRow(
    column(4, selectInput("baseline_intervention_22", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_22", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_22", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 23", fluidRow(
    column(4, selectInput("baseline_intervention_23", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_23", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_23", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 24", fluidRow(
    column(4, selectInput("baseline_intervention_24", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_24", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_24", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 25", fluidRow(
    column(4, selectInput("baseline_intervention_25", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_25", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_25", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 26", fluidRow(
    column(4, selectInput("baseline_intervention_26", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_26", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_26", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 27", fluidRow(
    column(4, selectInput("baseline_intervention_27", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_27", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_27", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 28", fluidRow(
    column(4, selectInput("baseline_intervention_28", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_28", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_28", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 29", fluidRow(
    column(4, selectInput("baseline_intervention_29", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_29", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_29", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  conditionalPanel("output.baseline_nb >= 30", fluidRow(
    column(4, selectInput("baseline_intervention_30", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_30", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yy-mm-dd", startview = "year")),
    column(3, sliderInput("baseline_coverage_30", NULL, min = 0, max = 100, value = new_coverage_value, post = "%", ticks = FALSE))
  )),
  div(class = "buttons_interventions",
      actionBttn("add_intervention_baseline", icon("plus"), size = "sm", color = "danger", style = "jelly"),
      actionBttn("remove_intervention_baseline", icon("minus"), size = "sm", color = "danger", style = "jelly")
  )
)
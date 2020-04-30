div(
  br(),
  conditionalPanel("output.baseline_nb >= 1", 
                   fluidRow(
                     column(4, h5("Interventions:")),
                     column(5, h5("Date Range:")),
                     column(3, h5("Coverage (%):"))
                   )
  ),
  conditionalPanel("output.baseline_nb >= 1", fluidRow(
    column(4, selectInput("baseline_intervention_1", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_1", NULL, start = new_daterange_value[1], end = new_daterange_value[2])),
    column(3, numericInput("baseline_coverage_1", NULL, min = 0, max = 100, value = new_coverage_value))
  )),
  conditionalPanel("output.baseline_nb >= 2", fluidRow(
    column(4, selectInput("baseline_intervention_2", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_2", NULL, start = new_daterange_value[1], end = new_daterange_value[2])),
    column(3, numericInput("baseline_coverage_2", NULL, min = 0, max = 100, value = new_coverage_value))
  )),
  conditionalPanel("output.baseline_nb >= 3", fluidRow(
    column(4, selectInput("baseline_intervention_3", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_3", NULL, start = new_daterange_value[1], end = new_daterange_value[2])),
    column(3, numericInput("baseline_coverage_3", NULL, min = 0, max = 100, value = new_coverage_value))
  )),
  conditionalPanel("output.baseline_nb >= 4", fluidRow(
    column(4, selectInput("baseline_intervention_4", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_4", NULL, start = new_daterange_value[1], end = new_daterange_value[2])),
    column(3, numericInput("baseline_coverage_4", NULL, min = 0, max = 100, value = new_coverage_value))
  )),
  conditionalPanel("output.baseline_nb >= 5", fluidRow(
    column(4, selectInput("baseline_intervention_5", NULL, all_interventions, selected = new_intervention_value)),
    column(5, dateRangeInput("baseline_daterange_5", NULL, start = new_daterange_value[1], end = new_daterange_value[2])),
    column(3, numericInput("baseline_coverage_5", NULL, min = 0, max = 100, value = new_coverage_value))
  )),
  br(),
  actionButton("add_intervention_baseline", icon("plus")),
  actionButton("remove_intervention_baseline", icon("minus")),
)
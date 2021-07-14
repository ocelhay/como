div(
  br(),
  fluidRow(
    column(1, h5("#")),
    column(3, h5("Nature of RR:")),
    column(3, h5("Date Range:")),
    column(2, h5("Value:") %>% helper(type = "markdown", content = "help_value_unit_interventions", colour = "red")),
    column(3, "")
  ),
  conditionalPanel(condition = "true",
                 fluidRow(
                   column(1, paste0("#", 101-100)),
                   column(3, selectInput("baseline_intervention_101", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_101", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_101", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (102-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 102-100)),
                   column(3, selectInput("baseline_intervention_102", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_102", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_102", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (103-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 103-100)),
                   column(3, selectInput("baseline_intervention_103", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_103", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_103", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (104-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 104-100)),
                   column(3, selectInput("baseline_intervention_104", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_104", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_104", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (105-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 105-100)),
                   column(3, selectInput("baseline_intervention_105", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_105", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_105", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (106-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 106-100)),
                   column(3, selectInput("baseline_intervention_106", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_106", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_106", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (107-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 107-100)),
                   column(3, selectInput("baseline_intervention_107", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_107", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_107", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (108-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 108-100)),
                   column(3, selectInput("baseline_intervention_108", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_108", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_108", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (109-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 109-100)),
                   column(3, selectInput("baseline_intervention_109", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_109", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_109", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (110-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 110-100)),
                   column(3, selectInput("baseline_intervention_110", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_110", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_110", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (111-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 111-100)),
                   column(3, selectInput("baseline_intervention_111", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_111", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_111", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (112-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 112-100)),
                   column(3, selectInput("baseline_intervention_112", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_112", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_112", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (113-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 113-100)),
                   column(3, selectInput("baseline_intervention_113", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_113", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_113", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (114-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 114-100)),
                   column(3, selectInput("baseline_intervention_114", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_114", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_114", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (115-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 115-100)),
                   column(3, selectInput("baseline_intervention_115", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_115", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_115", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (116-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 116-100)),
                   column(3, selectInput("baseline_intervention_116", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_116", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_116", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (117-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 117-100)),
                   column(3, selectInput("baseline_intervention_117", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_117", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_117", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (118-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 118-100)),
                   column(3, selectInput("baseline_intervention_118", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_118", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_118", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (119-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 119-100)),
                   column(3, selectInput("baseline_intervention_119", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_119", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_119", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (120-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 120-100)),
                   column(3, selectInput("baseline_intervention_120", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_120", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_120", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (121-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 121-100)),
                   column(3, selectInput("baseline_intervention_121", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_121", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_121", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (122-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 122-100)),
                   column(3, selectInput("baseline_intervention_122", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_122", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_122", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (123-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 123-100)),
                   column(3, selectInput("baseline_intervention_123", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_123", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_123", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (124-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 124-100)),
                   column(3, selectInput("baseline_intervention_124", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_124", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_124", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (125-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 125-100)),
                   column(3, selectInput("baseline_intervention_125", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_125", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_125", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (126-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 126-100)),
                   column(3, selectInput("baseline_intervention_126", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_126", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_126", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (127-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 127-100)),
                   column(3, selectInput("baseline_intervention_127", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_127", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_127", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (128-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 128-100)),
                   column(3, selectInput("baseline_intervention_128", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_128", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_128", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (129-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 129-100)),
                   column(3, selectInput("baseline_intervention_129", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_129", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_129", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (130-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 130-100)),
                   column(3, selectInput("baseline_intervention_130", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_130", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_130", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (131-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 131-100)),
                   column(3, selectInput("baseline_intervention_131", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_131", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_131", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (132-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 132-100)),
                   column(3, selectInput("baseline_intervention_132", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_132", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_132", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (133-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 133-100)),
                   column(3, selectInput("baseline_intervention_133", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_133", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_133", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (134-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 134-100)),
                   column(3, selectInput("baseline_intervention_134", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_134", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_134", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (135-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 135-100)),
                   column(3, selectInput("baseline_intervention_135", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_135", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_135", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (136-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 136-100)),
                   column(3, selectInput("baseline_intervention_136", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_136", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_136", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (137-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 137-100)),
                   column(3, selectInput("baseline_intervention_137", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_137", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_137", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (138-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 138-100)),
                   column(3, selectInput("baseline_intervention_138", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_138", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_138", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (139-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 139-100)),
                   column(3, selectInput("baseline_intervention_139", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_139", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_139", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (140-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 140-100)),
                   column(3, selectInput("baseline_intervention_140", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_140", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_140", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (141-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 141-100)),
                   column(3, selectInput("baseline_intervention_141", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_141", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_141", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (142-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 142-100)),
                   column(3, selectInput("baseline_intervention_142", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_142", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_142", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (143-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 143-100)),
                   column(3, selectInput("baseline_intervention_143", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_143", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_143", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (144-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 144-100)),
                   column(3, selectInput("baseline_intervention_144", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_144", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_144", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (145-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 145-100)),
                   column(3, selectInput("baseline_intervention_145", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_145", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_145", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (146-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 146-100)),
                   column(3, selectInput("baseline_intervention_146", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_146", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_146", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (147-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 147-100)),
                   column(3, selectInput("baseline_intervention_147", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_147", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_147", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (148-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 148-100)),
                   column(3, selectInput("baseline_intervention_148", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_148", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_148", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (149-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 149-100)),
                   column(3, selectInput("baseline_intervention_149", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_149", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_149", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (150-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 150-100)),
                   column(3, selectInput("baseline_intervention_150", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_150", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_150", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (151-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 151-100)),
                   column(3, selectInput("baseline_intervention_151", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_151", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_151", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (152-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 152-100)),
                   column(3, selectInput("baseline_intervention_152", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_152", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_152", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (153-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 153-100)),
                   column(3, selectInput("baseline_intervention_153", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_153", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_153", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (154-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 154-100)),
                   column(3, selectInput("baseline_intervention_154", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_154", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_154", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (155-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 155-100)),
                   column(3, selectInput("baseline_intervention_155", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_155", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_155", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (156-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 156-100)),
                   column(3, selectInput("baseline_intervention_156", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_156", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_156", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (157-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 157-100)),
                   column(3, selectInput("baseline_intervention_157", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_157", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_157", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (158-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 158-100)),
                   column(3, selectInput("baseline_intervention_158", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_158", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_158", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (159-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 159-100)),
                   column(3, selectInput("baseline_intervention_159", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_159", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_159", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (160-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 160-100)),
                   column(3, selectInput("baseline_intervention_160", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_160", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_160", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (161-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 161-100)),
                   column(3, selectInput("baseline_intervention_161", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_161", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_161", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (162-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 162-100)),
                   column(3, selectInput("baseline_intervention_162", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_162", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_162", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (163-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 163-100)),
                   column(3, selectInput("baseline_intervention_163", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_163", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_163", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (164-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 164-100)),
                   column(3, selectInput("baseline_intervention_164", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_164", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_164", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (165-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 165-100)),
                   column(3, selectInput("baseline_intervention_165", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_165", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_165", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (166-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 166-100)),
                   column(3, selectInput("baseline_intervention_166", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_166", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_166", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (167-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 167-100)),
                   column(3, selectInput("baseline_intervention_167", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_167", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_167", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (168-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 168-100)),
                   column(3, selectInput("baseline_intervention_168", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_168", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_168", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (169-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 169-100)),
                   column(3, selectInput("baseline_intervention_169", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_169", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_169", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (170-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 170-100)),
                   column(3, selectInput("baseline_intervention_170", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_170", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_170", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (171-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 171-100)),
                   column(3, selectInput("baseline_intervention_171", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_171", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_171", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (172-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 172-100)),
                   column(3, selectInput("baseline_intervention_172", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_172", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_172", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (173-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 173-100)),
                   column(3, selectInput("baseline_intervention_173", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_173", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_173", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (174-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 174-100)),
                   column(3, selectInput("baseline_intervention_174", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_174", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_174", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (175-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 175-100)),
                   column(3, selectInput("baseline_intervention_175", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_175", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_175", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (176-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 176-100)),
                   column(3, selectInput("baseline_intervention_176", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_176", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_176", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (177-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 177-100)),
                   column(3, selectInput("baseline_intervention_177", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_177", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_177", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (178-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 178-100)),
                   column(3, selectInput("baseline_intervention_178", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_178", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_178", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (179-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 179-100)),
                   column(3, selectInput("baseline_intervention_179", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_179", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_179", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (180-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 180-100)),
                   column(3, selectInput("baseline_intervention_180", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_180", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_180", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (181-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 181-100)),
                   column(3, selectInput("baseline_intervention_181", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_181", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_181", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (182-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 182-100)),
                   column(3, selectInput("baseline_intervention_182", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_182", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_182", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (183-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 183-100)),
                   column(3, selectInput("baseline_intervention_183", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_183", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_183", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (184-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 184-100)),
                   column(3, selectInput("baseline_intervention_184", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_184", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_184", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (185-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 185-100)),
                   column(3, selectInput("baseline_intervention_185", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_185", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_185", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (186-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 186-100)),
                   column(3, selectInput("baseline_intervention_186", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_186", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_186", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (187-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 187-100)),
                   column(3, selectInput("baseline_intervention_187", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_187", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_187", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (188-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 188-100)),
                   column(3, selectInput("baseline_intervention_188", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_188", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_188", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (189-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 189-100)),
                   column(3, selectInput("baseline_intervention_189", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_189", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_189", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (190-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 190-100)),
                   column(3, selectInput("baseline_intervention_190", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_190", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_190", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (191-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 191-100)),
                   column(3, selectInput("baseline_intervention_191", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_191", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_191", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (192-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 192-100)),
                   column(3, selectInput("baseline_intervention_192", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_192", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_192", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (193-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 193-100)),
                   column(3, selectInput("baseline_intervention_193", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_193", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_193", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (194-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 194-100)),
                   column(3, selectInput("baseline_intervention_194", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_194", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_194", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (195-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 195-100)),
                   column(3, selectInput("baseline_intervention_195", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_195", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_195", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (196-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 196-100)),
                   column(3, selectInput("baseline_intervention_196", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_196", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_196", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (197-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 197-100)),
                   column(3, selectInput("baseline_intervention_197", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_197", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_197", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (198-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 198-100)),
                   column(3, selectInput("baseline_intervention_198", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_198", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_198", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (199-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 199-100)),
                   column(3, selectInput("baseline_intervention_199", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_199", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_199", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
conditionalPanel(condition = paste0("!([", 
                                    paste0("input.baseline_intervention_", (200-1):200, collapse = ", "), 
                                    "].every( (val) => { return val === '_';} ))"),
                 fluidRow(
                   column(1, paste0("#", 200-100)),
                   column(3, selectInput("baseline_intervention_200", NULL, rr_interventions, selected = "_")),
                   column(3, dateRangeInput("baseline_daterange_200", NULL, start = new_daterange_value[1], end = new_daterange_value[2], format = "yyyy-mm-dd", startview = "year")),
                   column(2, sliderInput("baseline_coverage_200", NULL, min = 0, max = 3, value = 0, ticks = FALSE)),
                   column(3, "")
                 )
),
)
conf_interval <- function(min, med, max, unit) {
  if (min == max) return(span(style = "font-size: 1.6em;", paste0(round(med, 1), unit)))
  
  HTML(paste0(
    span(style = "font-size: 1.6em;", paste0(round(med, 1), unit)),
    span(style = "font-size: 1.1em;", paste0(" (", round(min, 1), ", ", round(max, 1), ")"))
  ))
}

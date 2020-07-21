conf_interval <- function(min, med, max, unit) {
  if (min == max) return(span(style = "font-size: 1.6em;", paste0(round(med, 1), unit)))
  
  HTML(paste0(
    span(style = "font-size: 1.6em;", paste0(round(med, 1), unit)),
    span(style = "font-size: 1.1em;", paste0(" (", round(min, 1), ", ", round(max, 1), ")"))
  ))
}


conf_interval_deaths <- function(min, med, max) {
  if (min == max) return(span(style = "font-size: 1.6em;", format(round(med, 1), big.mark = ",")))
  
  HTML(paste0(
    span(style = "font-size: 1.6em;", format(round(med, 1), big.mark = ",")),
    span(style = "font-size: 1.1em;", paste0(" (", format(round(min, 1), big.mark = ","), 
                                             ", ", 
                                             format(round(max, 1), big.mark = ","), 
                                             ")"))
  ))
}

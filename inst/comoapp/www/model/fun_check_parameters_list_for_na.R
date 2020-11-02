check_parameters_list_for_na <- function(parameters_list) {
  any_missing_parameter <- any(is.na(parameters_list))
  
  if(any_missing_parameter) stop("At least one parameter is a missing value.")
  if(!any_missing_parameter)  message("The list of parameters has no missing value.")
}
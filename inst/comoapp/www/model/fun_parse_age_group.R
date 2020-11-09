parse_age_group <- function(vec) {

  regx_str <- "^(([0]*([1-9]|1[0-9]|2[0-1]))[\\,\\:])*([0]*([1-9]|1[0-9]|2[0-1]))$"
  output <- rep(FALSE, 21)
  
  if (is.na(vec)) {
    return(output)
  }
  
  vec <- str_replace_all(vec, "-", ":")
  vec <- str_replace_all(vec, ";", ",")

  vec <- str_replace_all(vec, "[:space:]", "")
  vec <- str_replace_all(vec, "[:alpha:]", "")

  if (!str_detect(vec, regx_str)) {
    return(output)
  }
  
  vec <- paste0("c(", vec, ")")
  vec2 <- eval(parse(text = vec))
  
  output[vec2] <- TRUE
  return(output)
}

# test_strings <- c(
#   "1,3; 2;8-9,21",
#   " 1a ,a6:age7",
#   " ",
#   " 001, 021;11-10",
#   "10, ",
#   "a1,g20",
#   "10,-1",
#   "2",
#   "2,22",
#   "5,3,1",
#   "1-21"
# )
# 
# for(str in test_strings) {
#   print("input:")
#   print(str)
#   print("output:")
#   print(parse_age_group(str))
# }  
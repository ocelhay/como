names(parameters)[! names(parameters) %in% names(shiny_parameters)]

compare <- left_join(
  tibble(names = names(shiny_parameters), shiny = shiny_parameters),
  tibble(names = names(parameters), ricardo = parameters),
  by = "names") %>%
  mutate(diff = shiny - ricardo)


print(compare, n = 100, width = 100)

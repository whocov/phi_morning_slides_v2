
slide_title <- function(pres, data) {
  
  # working_date <- as_date(max(data$modified_date, na.rm = TRUE))
  
  # as_of_date <- weekend_parse(working_date)$max
  as_of_date <- as_date(Sys.Date())
  
  h1 <-glue::glue("WHE Daily Signals and Events")
  
  # h1 <- unordered_list(
  #   level_list = 1,
  #   str_list = h1,
  #   style = list(
  #     fp_text(font.size = 33, bold = T, font.family = "Segoe Condensed", color = "black"),
  #   ))
  
  h2 <- format(as_of_date, "%d %B %Y")
  
  # h2 <- unordered_list(
  #   level_list = 1,
  #   str_list = h2,
  #   style = list(
  #     fp_text(font.size = 33, bold = T, font.family = "Segoe Condensed", color = "black"),
  #   ))
  
  
  
  pres <- pres %>% 
    # ph_with(h1, value = h1, location = ph_location()) %>% 
    add_slide(layout = "Title Layout", master = "WHE") %>% 
    ph_with(value = h1, location = ph_location_type(type = "title")) %>% 
    ph_with(value = h2, location = ph_location_type(type = "body", id = 1))
  
  move_slide(pres, index = 6, to = 1)
  
  pres <- on_slide(pres, 5)

  pres
}
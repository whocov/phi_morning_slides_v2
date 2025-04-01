
slide_signals_rtm <- function(pres, sig.detail) {
  
  h1 <- glue::glue("Signals under real time monitoring (RTM) in the last 14 days")
  
  # h2 <- glue::glue("From of {format(date_min, '%d %b %Y')} to {format(date_max, '%d %b %Y')}")
  # h2 <- glue::glue("") #Placeholder left in case we want to add a subtitle later
  h2 <- paste('as of 5pm,', format(Sys.Date()-1, "%d %b %Y"))
  h3 <- paste('This slide only reflects the information that has been acted upon by the HQ PHI unit and does not reflect work being undertaken by other teams at HQ or across the other levels of the organization')
  
  headercolumn <- unordered_list(
    level_list = c(1, 1, 1),
    str_list = c(h1,h2,h3),
    style = list(
      fp_text(font.size = 24, bold = TRUE, font.family = "Segoe Condensed", color = "white"),
      fp_text(font.size = 14, bold = TRUE, font.family = "Segoe Condensed", color = "white"),
      fp_text(font.size = 11, bold = FALSE, font.family = "Segoe Condensed", color = "white")
    ))
  
  row1 <- sig.detail %>%
    filter(date >= (as.Date(Sys.Date())-13)) %>%
    filter(categorization == 'RTM') %>%
    filter(is.na(closed_rtm_only)) %>%
    select(hazard.country) %>%
    mutate(hazard.country = str_replace_all(hazard.country, 'Floods', 'Flood')) %>% 
    group_by(hazard.country) %>%
    summarise(n = n()) %>% 
    ungroup() %>% 
    mutate(hazard.country = ifelse(n > 1, paste0(hazard.country, ' (', n, ')'), hazard.country))
  
  if(nrow(row1) == 0){
    text <- 'No signals under real-time monitoring'
  } else{
    text <- row1 %>%
      pull(hazard.country) %>% 
      paste('- ', ., collapse = '\n')
  }
  
  
  # Placeholder tables
  # table.left <- data.frame(
  #   Category = c('Real-time monitoring', 
  #                'TT/RO awareness', 
  #                'TT/RO verification', 
  #                'Discarded'),
  #   # Title = c('Hazard, Member State', NA, NA, NA)) %>%
  #   Title = c(row1, row2, row3, row4)) %>%
  #   flextable() %>%
  #   add_header_lines(values = paste0('Signals/events (N=', nrow(sig.detail), ')'), top = TRUE) %>%
  #   fontsize(size = 18, part='all') %>%
  #   font(fontname ='Calibri', part='all') %>%
  #   bold(i=2, part='header') %>%
  #   # italic(j=2, part='body') %>%
  #   width(width= c(5.36, 9), unit='cm') %>%
  #   align(align='left', part='all') %>%
  #   hline_top(border = fp_border_default(width = 0), part = "header") %>%
  #   theme_zebra(odd_header = "transparent",
  #               odd_body = "#CFD5EA",
  #               even_header = "#4472C4",
  #               even_body = "#E9EBF5") %>%
  #   border_inner(border = fp_border_default(color='white', width = 1), part = "all") %>%
  #   hline(i=2, border = fp_border_default(color='white', width = 3), part = "header") %>%
  #   color(i=2, color='white', part='header')
  # table.left
  
  
  
  # Print slides
  pres <- pres %>% 
    add_slide(layout = "Title and Content Custom", master = "WHE") %>%
    ph_with(value = headercolumn, location = ph_location_type(type = "title")) %>%
    ph_with(text, location = ph_location_type(type = "body"))
  
  pres
}

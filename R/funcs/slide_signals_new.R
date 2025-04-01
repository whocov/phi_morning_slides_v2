
slide_signals_new <- function(pres, sig.detail) {
  
  h1 <- glue::glue("PHI signal/event categorization for the last 24 hours, following initial risk assessment")
  
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
  
  
  sig.detail <- sig.detail %>% 
    filter(date >= (as.Date(Sys.Date())-1))
  
  
  row1 <- sig.detail %>%
    # filter(!is.na(rtm_4)) %>%
    filter(categorization == 'Verify') %>%
    pull(hazard.country) %>% 
    paste('- ', ., collapse = '\n')
  
  row2 <- sig.detail %>%
    # filter(!is.na(tra)) %>%
    filter(categorization == 'RFI') %>%
    pull(hazard.country) %>% 
    paste('- ', ., collapse = '\n')
  
  row3 <- sig.detail %>%
    # filter(!is.na(trv)) %>%
    filter(categorization == 'RTM') %>%
    pull(hazard.country) %>% 
    paste('- ', ., collapse = '\n')
  
  row4 <- sig.detail %>%
    # filter(!is.na(disc)) %>%
    filter(categorization == 'SFA') %>%
    pull(hazard.country) %>% 
    paste('- ', ., collapse = '\n')
  
  row5 <- sig.detail %>%
    # filter(!is.na(disc)) %>%
    filter(categorization == 'DHQP') %>%
    pull(hazard.country) %>% 
    paste('- ', ., collapse = '\n')
  
  
  # Total number of signals - excluding duplicates
  n.signals <- nrow(sig.detail %>% 
                      select(date, hazard, country, rationale) %>% 
                      distinct_all())
  
  
  # Placeholder tables
  table.left <- data.frame(
    Category = c('Request for verification', 
                 'Request for information', 
                 'Real-time monitoring', 
                 'Shared for awareness',
                 'Discard for HQ PHI'),
    # Title = c('Hazard, Member State', NA, NA, NA)) %>%
    Title = c(row1, row2, row3, row4, row5)) %>%
    flextable() %>%
    add_header_lines(values = paste0('Signals/events (N=', n.signals, ')'), top = TRUE) %>%
    fontsize(size = 18, part='all') %>%
    font(fontname ='Calibri', part='all') %>%
    bold(i=2, part='header') %>%
    # italic(j=2, part='body') %>%
    width(width= c(5.36, 9), unit='cm') %>%
    align(align='left', part='all') %>%
    hline_top(border = fp_border_default(width = 0), part = "header") %>%
    theme_zebra(odd_header = "transparent",
                odd_body = "#CFD5EA",
                even_header = "#4472C4",
                even_body = "#E9EBF5") %>%
    border_inner(border = fp_border_default(color='white', width = 1), part = "all") %>%
    hline(i=2, border = fp_border_default(color='white', width = 3), part = "header") %>%
    color(i=2, color='white', part='header')
  # table.left
  
  if(nrow(sig.detail) > 10){
    table.left <- table.left %>% 
      fontsize(size = 12, part='all')
  }
  
  
  # EMS data
  source('R/funcs/ems_signals.R')
  
  if(nrow(ems.tab) == 0) {
    table.right <- tibble(
      `Event ID` = NA,
      `Disease/Condition` = NA,
      Country = NA
    ) %>% 
      flextable() %>%
      add_header_lines(values = paste0('New events in EMS (N=',nrow(ems.tab),')'), top = TRUE) %>%
      fontsize(size = 18, part='all') %>%
      font(fontname ='Calibri', part='all') %>%
      bold(i=2, part='header') %>%
      # italic(j=2, part='body') %>%
      width(width= c(3.56, 5.71, 5), unit='cm') %>%
      hline_top(border = fp_border_default(width = 0), part = "header") %>%
      theme_zebra(odd_header = "transparent",
                  odd_body = "#CFD5EA",
                  even_header = "#4472C4",
                  even_body = "#E9EBF5") %>%
      border_inner(border = fp_border_default(color='white', width = 1), part = "all") %>%
      hline(i=2, border = fp_border_default(color='white', width = 3), part = "header") %>%
      color(i=2, color='white', part='header') %>%
      align(align='left', part='all')
  } else{
    table.right <- ems.tab %>% 
      flextable() %>%
      add_header_lines(values = paste0('New events in EMS (N=',nrow(ems.tab),')'), top = TRUE) %>%
      # set_header_labels(values = list(
      #   event = 'Event ID',
      #   disease = 'Disease/Condition'
      # )) %>%
      delete_columns(4) %>% 
      merge_h(i = 1, part = "header") %>%
      fontsize(size = 18, part='all') %>%
      font(fontname ='Calibri', part='all') %>%
      bold(i=2, part='header') %>%
      # italic(j=2, part='body') %>%
      width(width= c(3.56, 5.71, 5), unit='cm') %>%
      hline_top(border = fp_border_default(width = 0), part = "header") %>%
      theme_zebra(odd_header = "transparent",
                  odd_body = "#CFD5EA",
                  even_header = "#4472C4",
                  even_body = "#E9EBF5") %>%
      border_inner(border = fp_border_default(color='white', width = 1), part = "all") %>%
      hline(i=2, border = fp_border_default(color='white', width = 3), part = "header") %>%
      color(i=2, color='white', part='header') %>%
      align(align='left', part='all') %>% 
      compose(j = 1, value = as_paragraph(hyperlink_text(x = `Event ID`, url = link)))
  }  
  # Print slides
  pres <- pres %>% 
    add_slide(layout = "Title and Image Custom", master = "WHE") %>%
    ph_with(value = headercolumn, location = ph_location_type(type = "title")) %>%
    ph_with(table.left, location = ph_location_left()) %>%
    ph_with(table.right, location = ph_location_right())
  
  pres
}

slide_signals <- function(pres, data) {
  
  h1 <-glue::glue("List of signals/events in the past 24 hours")
  
  h2 <- paste('as of 5pm,', format(Sys.Date()-1, "%d %b %Y"))

  headercolumn <- unordered_list(
    level_list = c(1, 1),
    str_list = c(h1,h2),
    style = list(
      fp_text(font.size = 33, bold = TRUE, font.family = "Segoe Condensed", color = "white"),
      fp_text(font.size = 16, bold = TRUE, font.family = "Segoe Condensed", color = "white")
    ))
  
  if(!is.null(data)){
    
    working_date <- as_date(max(data$modified_date, na.rm = TRUE))
    
    date_lims <- weekend_parse(working_date)
    date_max <- date_lims$max
    date_min <- date_lims$min
    
    time_int <- date_max - date_min 
    time_int <- as.double(time_int) * 24
    
    
    bp_header <- "Signals and events included in daily list of signals:"
    
    data <- data %>%
      mutate(md = as_date(modified_date),
             is_updated = ifelse(as_date(created_date) < md, TRUE, FALSE)) %>%
      group_by(md) %>%
      arrange(is_updated) %>%
      group_split()
    
    ## function to create a signal bullet
    signal_bp <- function(data, row_num) {
      
      data <- slice(data, row_num)
      
      #entry_date <- pull(data, )
      entry_country <- pull(data, countries_areas) 
      entry_disease <- pull(data, title)

      
      is_updated <- pull(data, is_updated)
      

      full_entry <- glue::glue("{entry_disease}{ifelse(is_updated, ' (Updated)', '')}")
      
      entry_style <- fp_text(font.size = 24, font.family = "Segoe Condensed" 
                             # bold = is_hl, color = ifelse(is_hl, "red", "black")
      )
      
      return(list("text" = full_entry, "style" = entry_style, "level" = 2))
    }
    
    
    ## function to create a date bullet and signal bullet
    run_bps <- function(data) {
      
      map(1:nrow(data), ~signal_bp(data, .x))

    }
    
    
    bps <- map(data, run_bps) %>% 
      reduce(append)
    
    bps_levels <- c(
      1,
      map_dbl(bps, ~pluck(.x, "level"))
    )
    
    bps_text <- c(
      bp_header,
      map_chr(bps, ~pluck(.x, "text"))
    )
    
    bps_style <- list(
      fp_text(font.size = 28, font.family = "Segoe Condensed", bold = FALSE)
    ) %>% append(
      map(bps, ~pluck(.x, "style"))
    )
    
    bps <- unordered_list(
      level_list = bps_levels,
      str_list = bps_text,
      style = bps_style)
    
    pres <- pres %>% 
      add_slide(layout = "Title and Content Custom", master = "WHE") %>% 
      ph_with(value = headercolumn, location = ph_location_type(type = "title")) %>%
      ph_with(value = bps, location = ph_location_type(type = "body"))
    
  }
  
  if(is.null(data)){
    bps <- unordered_list(
      level_list = 1,
      str_list = paste0('No relevant signals/events for this forum.'),
      style = fp_text(font.size = 28, font.family = "Segoe Condensed", bold = FALSE))
    
    
    pres <- pres %>% 
      add_slide(layout = "Title and Content Custom", master = "WHE") %>% 
      ph_with(value = headercolumn, location = ph_location_type(type = "title")) %>%
      ph_with(value = bps, location = ph_location_type(type = "body"))
  }
  
  
  # Moves the categorization slide to show after the potential signals
  move_slide(pres, index = 8, to = 7)
  move_slide(pres, index = 9, to = 8)
  move_slide(pres, index = 10, to = 9)
  move_slide(pres, index = 11, to = 10)
  
  pres
}
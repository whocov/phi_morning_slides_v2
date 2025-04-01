slide_products <- function(pres, data, data2) {
  
  h1 <- glue::glue("Status of products during the current week")
  
  h2 <- glue::glue("From of {format(Sys.Date() - 7, '%d %b %Y')} to {format(Sys.Date()-1, '%d %b %Y')}")
  
  headercolumn <- unordered_list(
    level_list = c(1, 1),
    str_list = c(h1,h2),
    style = list(
      fp_text(font.size = 33, bold = T, font.family = "Segoe Condensed", color = "white"),
      fp_text(font.size = 16, bold = T, font.family = "Segoe Condensed", color = "white")
    ))
  
  
  date.range <- interval(Sys.Date()-7, Sys.Date())
  
  prod.tracker <- data 
  prod2 <- data2
  
  if (is.null(prod2)){
    
    # Published
    pub <- prod.tracker %>% 
      select(disease_condition, country, countries_if_multiple_or_region, rra_pub_date, eis_pub_date, don_pub_date) %>%
      # filter(!is.na(rra_pub_date) | !is.na(eis_pub_date) | !is.na(don_pub_date)) %>%
      filter(rra_pub_date %within% date.range | eis_pub_date %within% date.range | don_pub_date %within% date.range) %>%
      mutate(country = ifelse(country == 'Multiple', countries_if_multiple_or_region, country),
             rra_pub_date2 = ifelse(rra_pub_date %within% date.range, 'X', ''),
             eis_pub_date2 = ifelse(eis_pub_date %within% date.range, 'X', ''),
             don_pub_date2 = ifelse(don_pub_date %within% date.range, 'X', '')) %>%
      mutate(across(rra_pub_date:don_pub_date, ~ ifelse(is.na(.x), '',.x))) %>%
      select(-countries_if_multiple_or_region, -rra_pub_date, -eis_pub_date, -don_pub_date)
    
    
    #In progress
    inprogress <- prod.tracker %>%
      filter(status=='Open') %>% 
      select(disease_condition, country, countries_if_multiple_or_region,
             need_rra, rra_pub_date, need_eis, eis_pub_date, need_don, don_pub_date,
             rra_start_date, eis_start_hq_receive_date, don_start_date) %>%
      filter(is.na(rra_pub_date) | is.na(eis_pub_date) | is.na(don_pub_date)) %>%
      mutate(country = ifelse(country == 'Multiple', countries_if_multiple_or_region, country),
             rra_pub_date2 = ifelse(need_rra & is.na(rra_start_date) & is.na(rra_pub_date), 'X*', ''),
             eis_pub_date2 = ifelse(need_eis & is.na(eis_start_hq_receive_date) & is.na(eis_pub_date), 'X*', ''),
             don_pub_date2 = ifelse(need_don & is.na(don_start_date) & is.na(don_pub_date), 'X*', '')) %>%
      mutate(rra_pub_date2 = ifelse(need_rra & is.na(rra_pub_date) & !is.na(rra_start_date), 'X', rra_pub_date2),
             eis_pub_date2 = ifelse(need_eis & is.na(eis_pub_date) & !is.na(eis_start_hq_receive_date), 'X', eis_pub_date2),
             don_pub_date2 = ifelse(need_don & is.na(don_pub_date) & !is.na(don_start_date), 'X', don_pub_date2)) %>%
      select(-countries_if_multiple_or_region,
             -need_rra, -need_eis, -need_don, 
             -rra_pub_date, -eis_pub_date, -don_pub_date,
             -rra_start_date, -eis_start_hq_receive_date, -don_start_date)
    
    
    # Final product table
    prod.table <- rbind(c('Published','',length(which(pub$rra_pub_date2=='X')),
                          length(which(pub$eis_pub_date2=='X')),
                          length(which(pub$don_pub_date2=='X'))), 
                        pub, 
                        c('In progress','',length(which(inprogress$rra_pub_date2=='X' | inprogress$rra_pub_date2=='X*')),
                          length(which(inprogress$eis_pub_date2=='X' | inprogress$eis_pub_date2=='X*')),
                          length(which(inprogress$don_pub_date2=='X' | inprogress$don_pub_date2=='X*'))), 
                        inprogress) %>% 
      as.data.frame()
    
    prod.table <- prod.table %>%
      flextable() %>%
      style(pr_t=fp_text(font.size=14), part='all') %>%
      bold(i=1, part='header') %>%
      bold(i=c(1, which(prod.table$disease_condition=='In progress'))) %>%
      hline(i=c(1,which(prod.table$disease_condition=='In progress')-1, which(prod.table$disease_condition=='In progress'))) %>%
      merge_h_range(i=c(1, which(prod.table$disease_condition=='In progress')),
                    j1 = 1, j2 = 2) %>%
      align(i=c(1, which(prod.table$disease_condition=='In progress')), align='center') %>%
      align(j=3:5, align='center', part='all') %>%
      align(i=1, align='center', part='header') %>%
      width(j=1:2, width=4.5) %>%
      # width(j=3:5, width=1.5) %>%
      set_header_labels(values=list(
        disease_condition='Product',
        country='Country',
        rra_pub_date2='RRA',
        eis_pub_date2='EIS',
        don_pub_date2='DON'
      )) %>%
      add_footer_lines(value = '*Product planned, but not yet started.')
  }
  
  
  # IF THERE IS A TRACKER REPORT
  if(!is.null(prod2)){
    
    # Published
    pub <- prod.tracker %>% 
      select(disease_condition, country, countries_if_multiple_or_region, 
             rra_pub_date, eis_pub_date, don_pub_date) %>%
      # filter(!is.na(rra_pub_date) | !is.na(eis_pub_date) | !is.na(don_pub_date)) %>%
      filter(rra_pub_date %within% date.range | eis_pub_date %within% date.range | don_pub_date %within% date.range) %>%
      mutate(country = ifelse(country == 'Multiple', countries_if_multiple_or_region, country),
             rra_pub_date2 = ifelse(rra_pub_date %within% date.range, 'X', ''),
             eis_pub_date2 = ifelse(eis_pub_date %within% date.range, 'X', ''),
             don_pub_date2 = ifelse(don_pub_date %within% date.range, 'X', '')) %>%
      mutate(across(rra_pub_date:don_pub_date, ~ ifelse(is.na(.x), '',.x))) %>%
      select(-countries_if_multiple_or_region, -rra_pub_date, -eis_pub_date, -don_pub_date) %>% 
      mutate(other = '') %>% 
      rbind(
        prod2 %>% 
          filter(status== 'Published',
                 date_published %within% date.range) %>%
          rename(disease_condition = report_name) %>% 
          select(disease_condition, country) %>% 
          mutate(#country = '',
            rra_pub_date2 = '',
            eis_pub_date2 = '',
            don_pub_date2 = '',
            other = 'X'))
    
    # In progress
    inprogress <- prod.tracker %>%
      filter(status=='Open') %>% 
      select(disease_condition, country, countries_if_multiple_or_region,
             need_rra, rra_pub_date, need_eis, eis_pub_date, need_don, don_pub_date,
             rra_start_date, eis_start_hq_receive_date, don_start_date) %>%
      filter(is.na(rra_pub_date) | is.na(eis_pub_date) | is.na(don_pub_date)) %>%
      mutate(country = ifelse(country == 'Multiple', countries_if_multiple_or_region, country),
             rra_pub_date2 = ifelse(need_rra & is.na(rra_start_date) & is.na(rra_pub_date), 'X*', ''),
             eis_pub_date2 = ifelse(need_eis & is.na(eis_start_hq_receive_date) & is.na(eis_pub_date), 'X*', ''),
             don_pub_date2 = ifelse(need_don & is.na(don_start_date) & is.na(don_pub_date), 'X*', '')) %>%
      mutate(rra_pub_date2 = ifelse(need_rra & is.na(rra_pub_date) & !is.na(rra_start_date), 'X', rra_pub_date2),
             eis_pub_date2 = ifelse(need_eis & is.na(eis_pub_date) & !is.na(eis_start_hq_receive_date), 'X', eis_pub_date2),
             don_pub_date2 = ifelse(need_don & is.na(don_pub_date) & !is.na(don_start_date), 'X', don_pub_date2)) %>%
      select(-countries_if_multiple_or_region,
             -need_rra, -need_eis, -need_don, 
             -rra_pub_date, -eis_pub_date, -don_pub_date,
             -rra_start_date, -eis_start_hq_receive_date, -don_start_date) %>% 
      mutate(other = '') %>% 
      rbind(
        prod2 %>% 
          filter(status != 'Published') %>%
          rename(disease_condition = report_name) %>% 
          select(disease_condition, country) %>% 
          mutate(#country = '',
            rra_pub_date2 = '',
            eis_pub_date2 = '',
            don_pub_date2 = '',
            other = 'X'))
    
    # Final product table
    prod.table <- rbind(c('Published','',length(which(pub$rra_pub_date2=='X')),
                          length(which(pub$eis_pub_date2=='X')),
                          length(which(pub$don_pub_date2=='X')),
                          length(which(pub$other=='X'))), 
                        pub, 
                        c('In progress','',length(which(inprogress$rra_pub_date2=='X' | inprogress$rra_pub_date2=='X*')),
                          length(which(inprogress$eis_pub_date2=='X' | inprogress$eis_pub_date2=='X*')),
                          length(which(inprogress$don_pub_date2=='X' | inprogress$don_pub_date2=='X*')),
                          length(which(inprogress$other=='X'))), 
                        inprogress) %>% 
      as.data.frame()
    
    prod.table <- prod.table %>%
      flextable() %>%
      style(pr_t=fp_text(font.size=14), part='all') %>%
      bold(i=1, part='header') %>%
      bold(i=c(1, which(prod.table$disease_condition=='In progress'))) %>%
      hline(i=c(1,which(prod.table$disease_condition=='In progress')-1, which(prod.table$disease_condition=='In progress'))) %>%
      merge_h_range(i=c(1, which(prod.table$disease_condition=='In progress')),
                    j1 = 1, j2 = 2) %>%
      align(i=c(1, which(prod.table$disease_condition=='In progress')), align='center') %>%
      align(j=3:6, align='center', part='all') %>%
      align(i=1, align='center', part='header') %>%
      width(j=1:2, width=4.5) %>%
      # width(j=3:5, width=1.5) %>%
      set_header_labels(values=list(
        disease_condition='Disease/Hazard/Condition',
        country='Country',
        rra_pub_date2='RRA',
        eis_pub_date2='EIS',
        don_pub_date2='DON',
        other='Sit. Rep'
      )) %>%
      add_footer_lines(value = '*Product planned, but not yet started.')
  }
  
  
  # Print to the slide
  pres <- pres %>% 
    add_slide(layout = "Title and Content Custom", master = "WHE") %>% 
    ph_with(value = headercolumn, location = ph_location_type(type = "title")) %>%
    ph_with(value = prod.table, location = ph_location_type(type = 'body'))
  
  pres
}
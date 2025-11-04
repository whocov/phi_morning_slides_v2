
read_file <- function(x) {
  
  
  if (is.null(x)) return(NULL)
  
  excel_raw <-  read_excel(
    x$datapath
  )
  
  excel_raw
  
}




read_file_pt <- function(x, sheets = c("product tracker", "Dropdownlist")) {
  
  if (is.null(x)) return(NULL)
  
  excel_raw <-   purrr::map(
    sheets,
    ~read_excel(
      x$datapath, 
      sheet = .x #, 
      #   col_types = c("text", 
      #                 "text", "text", "text", "text", "text", 
      #                 "date", "date", "date", "text", "text", 
      #                 "text", "text", "date", "text", "text", 
      #                 "text", "date", "date", "date", "text", 
      #                 "text", "text", "text", "text", "text", 
      #                 "text", "date", "date", "date", "date", 
      #                 "date", "date", "text", "date", "date", 
      #                 "text", "text", "numeric", "numeric", 
      #                 "numeric", "numeric", "text")
      # )
    )
  )
  
  excel_raw
}



read_file_pt2 <- function(x) {
  
  if (is.null(x)) return(NULL)
  
  excel_raw <-   read_excel(
    x$datapath, 
    col_types = c("text", "text", "guess", "text", "date")
  ) %>% 
    janitor::clean_names()
  
  excel_raw
}



proc_file_dl <- function(x) {
  
  
  if (is.null(x)) return(NULL)
  
  signals_pres <- signal_clean(x)
  signals_pres
}




proc_file_pt <- function(x) {
  x %>% 
    janitor::clean_names() %>%
    mutate(
      across(
        c("pq_closure_date",
          "rra_decision_date",
          "rra_start_date",
          "rra_pub_date",
          "eis_decision_date",
          "eis_start_hq_receive_date",
          "eis_pub_date",
          "don_start_date",
          "daily_signal_tracker", 
          "daily_list",
          "ems_entry"
        ),
        ~as_date(as.double(.x), origin = ymd("1899-12-30"))),
      across(starts_with("need"), ~ifelse(str_to_upper(.) == "YES", TRUE, FALSE) %>% 
               replace_na(FALSE))) 
  # %>%
  #   filter(status=='Open')
}


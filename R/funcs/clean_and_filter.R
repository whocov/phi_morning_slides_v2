#' @details clean dataset from raw form
#' @param x - the dataset 
signal_clean <- function(x) {
  
  if (is.null(x)) return(NULL)
  
  x <- x %>% 
    janitor::clean_names() %>% 
    relocate(created_app, .before=created_date) %>%
    mutate(
      across(c(created_date, modified_date), ymd_hm),
      across(where(is.logical), ~replace_na(., FALSE)),
      region = replace_na(region, "OTHER"),
      monitoring = as.logical(ifelse(monitoring == "Ongoing", TRUE, FALSE)),
      across(c(phi_list), ~as.logical(replace_na(., 0))),
      # across(c(phi_list, whedaemm), ~as.logical(replace_na(., 0)))
      across(c(phi_list), ~as.logical(replace_na(., 0)))
    ) %>% 
    filter(modified_date <= Sys.Date(),
           # created_app == 'Signal HQ',
           # need to add RO awareness when available
           # monitoring | phi_list | whedaemm) %>%
           # monitoring |
           grepl('Signal HQ PHI', ownership),
             phi_list) %>%
    distinct(created_date, created_by, title, region, .keep_all = TRUE)

  x
}



#' @details clean dataset from raw form
#' @param x - the dataset 
triaged_signal_clean <- function(x) {
  
  if (is.null(x)) return(NULL)
  
  x <- x %>% 
    janitor::clean_names() %>% 
    relocate(created_app, .before=created_date) %>%
    mutate(
      across(c(created_date, modified_date), ymd_hm),
      across(where(is.logical), ~replace_na(., FALSE)),
      region = replace_na(region, "OTHER"),
      monitoring = as.logical(ifelse(monitoring == "Ongoing", TRUE, FALSE)),
      across(c(phi_list), ~as.logical(replace_na(., 0))),
      across(c(phi_list), ~as.logical(replace_na(., 0)))
    ) %>% 
    filter(modified_date <= Sys.Date(),
           # created_app == 'Signal HQ'
           grepl('Signal HQ PHI', ownership)) %>%
    distinct(created_date, created_by, title, region, .keep_all = TRUE)
  
  x
}


#' @details filter the dataset for the signals within the time frame 
#' @param x - the dataset (cleaned)
#' @param date - date to be used
signal_filter_date <- function(x, fil_date = NULL) {
  
  if (is.null(x)) return(NULL)

  if (is.null(fil_date)) fil_date <- as_date(Sys.Date())-1

  x <- x %>% 
    filter(as_date(created_date) %in% range(as_date(fil_date)-1, as_date(Sys.Date())))
  
  if (nrow(x) == 0) return(NULL)
  
  x 
}



#' @details filter the dataset for the highlighted signals 
#' @param x - the dataset (filtered)
signal_filter_hl <- function(x) {
  
  if (is.null(x)) return(NULL)
  
  x <- x 
  # %>% 
  #   filter(whedaemm)
  if (nrow(x) == 0) return(NULL)
  
  x 
}



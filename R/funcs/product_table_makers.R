

product_table <- function(x) {
  
  pub_date <- x$fd[1] - days(1)
  
  x <- x %>% 
    transmute(
      topic = glue::glue("{disease_condition} - {country}"),
      rra_pub = case_when(is.na(rra_pub_date) ~ "NO", need_rra & rra_pub_date <= pub_date ~ "YES", TRUE ~ "NO"),
      eis_pub = case_when(is.na(eis_pub_date) ~ "NO", need_eis & eis_pub_date <= pub_date ~ "YES", TRUE ~ "NO"),
      don_pub = case_when(is.na(don_pub_date) ~ "NO", need_don & don_pub_date <= pub_date ~ "YES", TRUE ~ "NO"),
      across(starts_with("need_"), list("wip" = ~ifelse(.x & status == "open", "YES", "NO")))
    ) %>% 
    rename_with(~str_remove(., "need_"), starts_with("need_")) %>% 
    pivot_longer(
      cols = matches("_pub$|_wip$"),
      names_to = "temp_measure",
      values_to = "is_complete"
    ) %>% 
    separate(temp_measure, into = c("temp_type", "is_pub"), sep = "_") %>% 
    pivot_wider(names_from = temp_type,
                values_from = is_complete) %>% 
    mutate(is_pub = 
             fct_rev(as_factor(
               case_when(
                 is_pub == "pub" ~ "Published",
                 is_pub == "wip" ~ "Currently in progress"
               )))
    ) %>% 
    filter(rra == "YES" | eis == "YES" | don == "YES")
  
  if (!"Published" %in% x$is_pub) {
    x <- bind_rows(
      tibble(
        "topic" = "None",
        "is_pub" = "Published"
      ),
      x
    )
  }

  x
  
}



product_gt <- function(x, num_hours = 24) {
  
  if ("Published" %in% x$is_pub) {
    pub_sec <- TRUE
  } else {
    pub_sec <- FALSE
  }
  
  x_out <- x %>% 
    mutate(across(c(rra, eis, don), ~case_when(.x == "YES" ~ "\u2713",
                                               .x == "NO" ~ "",
                                               is.na(.x) ~ ""))) %>% 
    rename_with(str_to_upper, everything()) %>% 
    rename(Topic = TOPIC) %>% 
    group_by(IS_PUB) %>% 
    gt() %>% 
    cols_align(
      align = "center",
      columns = everything()
    ) %>% 
    cols_align(
      align = "left",
      columns = Topic
    ) %>% 
    tab_spanner(
      label = "Product",
      columns = c(RRA, EIS, DON)
    ) %>% 
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = list(
        cells_column_labels(), cells_column_spanners()
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color ="#fc9272")
      ),
      locations = cells_row_groups()
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color ="#fee5d9")
      ),
      locations = cells_body()
    ) %>% 
    tab_style(
      style = list(
        cell_borders(sides = "top", color = "lightgrey")
      ),
      locations = cells_body()
    ) %>% 
    tab_style(
      style = list(
        cell_borders(sides = "left", color = "lightgrey")
      ),
      locations = cells_body(columns = c(RRA, EIS, DON),)
    ) %>% 
    opt_footnote_marks(marks = c("*", "+")) %>% 
    tab_options(table.width = pct(70))
  
  if (pub_sec) {
    x_out <- x_out %>% 
      tab_footnote(
        footnote = glue::glue("In the previous {num_hours} hours"),
        locations = cells_row_groups("Published")
      ) %>%
      tab_style(
        style = list(
          cell_fill(color ="#a6bddb")
        ),
        locations = cells_row_groups("Published")
      ) %>% 
      tab_style(
        style = list(
          cell_fill(color ="#f1eef6")
        ),
        locations = cells_body(columns = everything(), rows = IS_PUB == "Published")
      ) 
      
  }
  
  x_out
  
}

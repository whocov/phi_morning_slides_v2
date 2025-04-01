
slide_intro <- function(pres, data, eios_num = 0, inbox_num = 0, signals_num = 0) {
  
  # working_date <- as_date(max(data$modified_date, na.rm = TRUE))
  # 
  # date_lims <- weekend_parse(working_date)
  # date_max <- date_lims$max
  # date_min <- date_lims$min
  # 
  # time_int <- date_max - date_min 
  # time_int <- as.double(time_int) * 24
  
  # h1 <- glue::glue("PHI indicators for the last {round(time_int)} hours")
  h1 <- glue::glue("PHI indicators for the last 24 hours")
                  
  # h2 <- glue::glue("From of {format(date_min, '%d %b %Y')} to {format(date_max, '%d %b %Y')}")
  # h2 <- glue::glue("as of {format(Sys.Date(), "%d %b %Y")}") # Placeholder left in case we want to add a subtitle later
  h2 <- paste('as of 5pm,', format(Sys.Date()-1, "%d %b %Y"))

  headercolumn <- unordered_list(
    level_list = c(1, 1),
    str_list = c(h1,h2),
    style = list(
      fp_text(font.size = 33, bold = T, font.family = "Segoe Condensed", color = "white"),
      fp_text(font.size = 16, bold = T, font.family = "Segoe Condensed", color = "white")
    ))
  
  base_style <- fp_text(font.family = "Segoe Condensed", bold = F, font.size = 28)
  bold_style <- fp_text(font.family = "Segoe Condensed", bold = T, font.size = 28)
  base_style_red <- fp_text(font.family = "Segoe Condensed", bold = F, font.size = 28, color = "red")
  bold_style_red <- fp_text(font.family = "Segoe Condensed", bold = T, font.size = 28, color = "red")

  bp_1 <- fpar(ftext("Raw signals scanned and triangulated", base_style))
  #Weekday switch for Mondays
  # if (weekday=='Monday') {
  #   bp_1 <- fpar(ftext("Pieces of information scanned in 72hr period", base_style))
  # }

  # if (weekday=='Tuesday'){
  #   bp_1_1 <- fpar(ftext(glue::glue("EIOS: {scales::comma(eios_num)} pieces of information (covering information in EIOS from the weekend as well as Monday)"), base_style))
  # }
  # else{
  #   bp_1_1 <- fpar(ftext(glue::glue("EIOS: {scales::comma(eios_num)} pieces of information"), base_style))
  # }
  bp_1_1 <- fpar(ftext(glue::glue("EIOS: {scales::comma(eios_num)} articles"), base_style))
  bp_1_2 <- fpar(ftext(glue::glue("Outbreak inbox: {scales::comma(inbox_num)} emails"), base_style))


  # bp_1 <- fpar(ftext("***INSERT NUMBER***", bold_style),
  #              ftext(glue::glue(" pieces of information screened in EIOS within the last {round(time_int)} hours"), base_style))
  
  # events_detected <- nrow(data)
  events_detected <- signals_num
  
  bp_2 <- fpar(ftext(events_detected, bold_style),
               ftext(" potential signals/signals/events assessed and triaged", base_style))
  
  
  # bp_3 <- fpar(ftext("***INSERT NUMBER***", bold_style),
  #              ftext(" of signals/events under verification and follow up with ROs and partners", base_style))
  
  sm_events <- ifelse(is.null(data),
                      0,
                      nrow(data))
  
  bp_3 <- fpar(ftext(sm_events, bold_style),
               ftext(" signals/events relevant for daily list of signals", base_style))
  
  
  # hl_events <- nrow(data)
  
  # bp_4 <- fpar(ftext(hl_events, base_style_red),
  #              ftext(" (highlighted) ", base_style_red),
  #              ftext("signals/events for discussion with senior management", base_style))
  
  bps <- block_list(bp_1, bp_1_1, bp_1_2, bp_2, bp_3
                    # , bp_4
                    )
  
  
  footnote_text <- glue::glue(' '
    #Averages were obtained over the last month and are routinely reviewed by PHI team to ensure that it is accurate.
    #"* 'signals/events detected/received' refer to those identified through EIOS media monitoring, received in 'Outbreak' email inbox, or published in EMS."
  )

  footnote_style <- fp_text(font.family = "Segoe Condensed", italic = TRUE, font.size = 10)
  footnote <- unordered_list(
    level_list = 0, str_list = footnote_text, style = list(footnote_style)
  )
  
  pres <- pres %>% 
    add_slide(layout = "Title and Content Custom", master = "WHE") %>% 
    ph_with(value = headercolumn, location = ph_location_type(type = "title")) %>%
    ph_with(value = bps,
            level_list = c(1, 2, 2, 1, 1, 1),
            location = ph_location_type(type = "body")) %>% 
    ph_with(value = footnote, location = ph_location(
      width = 21/2.54,
      height = 1/2.54,
      left = 1/2.54,
      right = 27/2.54,
      top = 16/2.54)
    )
  # ph_with(value = bp, location = ph_location_type(type = "body"))
  #
  
  pres
}

weekend_parse <- function(x) {
  if (wday(x, week_start = 1) %in% c(5, 6, 7)) { # need to be updated just to consider 3 days window only for mondays
    date_max <- ceiling_date(x, "week", week_start = 1)
    date_min <- date_max - days(3)
  }else if (wday(x, week_start = 1) == 1) {
    date_max <- floor_date(x, "week", week_start = 1)+1 #ceiling_date(x, "week", week_start = 1)-6
    date_min <- date_max - days(4)
  } else {
    date_max <- x + days(1)
    date_min <- x
  }
  return(list("max" = date_max, "min" = date_min))
}

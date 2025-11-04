
weekend_parse <- function(x) {
  if (wday(x, week_start = 1) %in% c(5, 6, 7)) {
    date_max <- ceiling_date(x, "week", week_start = 1)
    date_min <- date_max - days(3)
  } else {
    date_max <- x + days(1)
    date_min <- x
  }
  return(list("max" = date_max, "min" = date_min))
}
# Global parameters
pacman::p_load(shiny, shinyjs,
               DT, janitor,
               readxl, tidyverse,
               flextable, lubridate,
               officer, glue, 
               gt,
               ghql, AzureAuth, jsonlite)


funcs <- list.files(here::here("R", "funcs"), pattern = ".R$", full.names = TRUE)

walk(funcs, source)


# set the number of eios articles scanned 
eios_val <- 1318

# set the number of outbreak inbox emails scanned
inbox_val <- 124

# set the number of signals assessed
signals_val <- 0

# Weekday
weekday <- weekdays(Sys.Date())


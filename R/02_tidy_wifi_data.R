# combine and tidy up the wifi data from weekly csv files

library(tidyverse)
library(here)

recode_building <- function(site){
  case_when(
    site == 'dms' ~ 'DMS',
    site == 'lpr' ~ 'LPR',
    site == 'montpetit' ~ 'MNT',
    site == 'tabaret' ~ 'TBT',
    site == 'morisset' ~ 'MRT',
    TRUE ~ NA_character_
  )
}

parse_weekly_wifi <- function(path){
  raw <- read_csv(path, show_col_types = F)
  stopifnot(all(names(raw) == c('_time', 'Building', 'Clients')))

  raw |>
    set_names(c('date', 'site', 'clients')) |>
    mutate(
      date = date |>
        parse_date_time(orders = '%a %b %d %H:%M:%S %Y') |>
        as_date(),
      site = tolower(site) |> recode_building()
    ) |>
    filter(!is.na(site), !is.na(date), !is.na(clients))
}


parse_older_wifi <- function(path){
  raw <- read_csv(path, show_col_types = F)
  stopifnot(all(names(raw) %in% c('_time', 'Building', 'Clients')))
  raw |>
    relocate(`_time`) |>
    set_names(c('date', 'site', 'clients')) |>
    mutate(
      date = as_date(date),
      site = tolower(site) |> recode_building()
    ) |>
    filter(!is.na(date), !is.na(site), !is.na(clients))
}

parse_wifi <- function(path, weekly){
  if (weekly) return(parse_weekly_wifi(path))
  parse_older_wifi(path)
}


wifi_files <-
  here('data/wifi_raw') |>
  fs::dir_info() |>
  select(path) |>
  mutate(weekly = str_detect(path, '2022-..-..\\.csv')) |>
  mutate(data = map2(path, weekly, parse_wifi),
         rows = map_int(data, nrow))

wifi <-
  wifi_files |>
  pull(data) |>
  bind_rows() |>
  arrange(desc(date), site)


write_rds(wifi, here('data/uo_wifi.rds'))

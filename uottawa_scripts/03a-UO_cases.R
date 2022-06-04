## use this script to update data for UO cases.

# It reads UO_cases_to_date.rdsh
# Add cases to .csv file; clean up missing values and conform to
# existing data types; need to do by hand as source is unpredictable!
# add an indicator for 'new' data  -> filter these new cases for addition to .rds data


library(tidyverse)
library(here)
library(lubridate)

detect_sites <- function(building_list){
  sites <- c('90U' = '90 UP|90U',
             'DMS' = 'DMS',
             'LPR' = 'LPR',
             'MNT' = 'MNT',
             'MRT' = 'MRT',
             'TBT' = 'TBT'
  )
  map(tolower(sites), ~str_detect(tolower(building_list), .x)) |>
    as_tibble()
}


# read in hand-cleaned cases data
cases <-
  read_csv(here('data/uo_cases_to_date.csv')) |>
  select(-new) |>
  mutate(locations = map(building_list, detect_sites),
         role = role |> str_to_sentence() |> as_factor()) |>
  unnest_wider(locations)

cases |> glimpse()

# find min date in data
md <-
  cases |>
  select(where(~is.Date(.x))) |>
  pivot_longer(cols = everything()) |>
  pull(value) |>
  as.numeric() |>
  min(na.rm = T)

# create difftime since earliest date
cases <- cases |>
  mutate(across(
    .cols = where(~is.Date(.x)),
    .fns = ~{as.numeric(.x - md)},
    .names = 'dt_{.col}',
    )) |> glimpse()

pos_mod <- lm(dt_positive_result ~ dt_symptoms_began + dt_isolation_end,
              data = cases)
pos_mod2 <- lm(dt_positive_result ~ dt_symptoms_began, data = cases)
pos_mod3 <- lm(dt_positive_result ~ dt_isolation_end, data = cases)

list(pos_mod, pos_mod2, pos_mod3) |> map(broom::glance)
list(pos_mod, pos_mod2, pos_mod3) |> map(broom::tidy)

cases <- cases |>
  mutate(
    imp1 = predict(pos_mod, newdata = cases),
    imp2 = predict(pos_mod2, newdata = cases),
    imp3 = predict(pos_mod3, newdata = cases),
  ) |>
  mutate(across(starts_with('imp'), ~as_date(.x + md))) |>
  group_by(case) |>
  mutate(imp = mean(c(imp1, imp2, imp3), na.rm = T)) |>
  ungroup() |>
  mutate(
    imputed = ifelse(is.na(positive_result), T, F),
    positive_result = ifelse(is.na(positive_result), imp, positive_result) |> as_date(),
  )


imputed_dates_fig <- cases |>
  filter(positive_result > '2021-08-01') |>
  ggplot(aes(x = positive_result, y = imp)) +
  # geom_smooth(se = FALSE, color = 'gray') +
  geom_abline(slope = 1, color = 'gray') +
  geom_point(alpha = 0.5, size = 0.5, aes(color = imputed)) +
  ggplot2::coord_equal() +
  labs(x = 'Positive Result Date', y = 'Imputed Date',
       caption = 'grey line shows where imputated = actual') +
  theme_light()

cases |> select(-matches('imp$|imp[0-9]+'), -starts_with('dt'))

write_rds(cases, here('data/latest_uo_cases.rds'))
write_rds(imputed_dates_fig, here('fig/imputed_uo_case_dates.rds'))



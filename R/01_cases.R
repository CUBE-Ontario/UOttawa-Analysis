## use this script to update data for UO cases.

# It reads UO_cases_to_date.rds
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

# find min date in data
min_date <-
  cases |>
  select(where(~is.Date(.x))) |>
  unlist() |>
  as.numeric() |>
  min(na.rm = T)

# create difftime since earliest date
cases <- cases |>
  mutate(across(
    .cols = where(~is.Date(.x)),
    .fns = ~{as.numeric(.x - min_date)},
    .names = 'dt_{.col}',
    )) |> glimpse()

# fit a model for each combination of data
pos_mod <- lm(dt_positive_result ~ dt_symptoms_began + dt_isolation_end,
              data = cases)
pos_mod2 <- lm(dt_positive_result ~ dt_symptoms_began, data = cases)
pos_mod3 <- lm(dt_positive_result ~ dt_isolation_end, data = cases)

#
# check differences
list(pos_mod, pos_mod2, pos_mod3) |> map(broom::glance)
list(pos_mod, pos_mod2, pos_mod3) |> map(broom::tidy)

cases

# imputed values would be similar to
# adding 4 days to symptoms began to get result date.
# subtracting 14 days from isolation end to get result date.

cases_imputed <- cases |>
  mutate(
    imp1 = predict(pos_mod, newdata = cases),
    imp2 = predict(pos_mod2, newdata = cases),
    imp3 = predict(pos_mod3, newdata = cases),
  ) |>
  mutate(across(starts_with('imp'), ~as_date(.x + min_date))) |>
  group_by(case) |>
  mutate(imputed_result_date = mean(c(imp1, imp2, imp3), na.rm = T)) |>
  ungroup() |>
  mutate(
    imputed = ifelse(is.na(positive_result), T, F),
    positive_result = ifelse(
      is.na(positive_result),
      imputed_result_date,
      positive_result
    ) |>
      as_date(),
  )

#
# imputed_dates_fig <- cases_imputed |>
#   filter(positive_result > '2021-08-01') |>
#   ggplot(aes(x = positive_result, y = imputed_result_date)) +
#   # geom_smooth(se = FALSE, color = 'gray') +
#   geom_abline(slope = 1, color = 'gray') +
#   geom_point(alpha = 0.5, size = 0.5, aes(color = imputed)) +
#   ggplot2::coord_equal() +
#   labs(x = 'Positive Result Date', y = 'Imputed Date',
#        caption = 'grey line shows where imputated = actual') +
#   theme_light()

cases_imputed |> select(-matches('imp$|imp[0-9]+'), -starts_with('dt'))

write_rds(cases_imputed, here('data/latest_uo_cases.rds'))
# write_rds(imputed_dates_fig, here('fig/imputed_uo_case_dates.rds'))



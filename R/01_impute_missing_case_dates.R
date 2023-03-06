library(tidyverse)
library(here)
library(patchwork)

# take buildings list and convert to bool columns for sites of interest
detect_sites <- function(building_list){
  lst('UC' = '90u|90 u',
      'DMS' = 'dms',
      'LPR' = 'lpr',
      'MNT' = 'mnt',
      'MRT' = 'mrt',
      'TBT' = 'tbt') |>
    map(~str_detect(building_list, .)) |>
    as_tibble()
}

# read in hand-cleaned cases data from July 2020 - early Feb 2022
cases <-
  read_csv(here::here('data/uo_cases_to_date.csv'),
           show_col_types = FALSE) |>
  mutate(
    role = str_to_lower(role),
    role = case_when(
      role %in% c('student', 'student athlete', 'resident',
                  'student & employee', 'ggs camp attendee') ~ 'Student',
      role %in% c('contractor', 'employee') ~ 'Employee',
      TRUE ~ NA_character_
    ) |> as_factor(),
    locations = building_list |>
      str_to_lower() |>
      str_replace_all('90 u', '90u') |>
      map(detect_sites)
  ) |>
  unnest_wider(locations) |>
  select(-new, -building_list) |>
  glimpse()


cases_imp <- cases |>
  mutate(
    # if pos result missing, use avail dates + offset
    case_date = case_when(
      !is.na(positive_result) ~ positive_result,
      !is.na(isolation_end) ~ isolation_end - 5,
      !is.na(tested) ~ tested,
      !is.na(symptoms_began) ~ symptoms_began + 3,
      TRUE ~ NA_Date_
    ))

# save for later analysis
write_rds(cases_imp, 'data/cases_rule_imputed.rds')



## inspect missing data -----

# 28 cases missing 'positive result date', mostly later in 2021/2022
cases |>
  summarise(across(everything(), ~sum(is.na(.)))) |>
  pivot_longer(everything(),
               names_to = 'variable',
               values_to = 'n_missing')

# 216 cases total; 184 missing some values;
# 13% missing result date, 3% isolation end, 77% tested, 53% symptoms
cases |> nrow()
cases |> naniar::n_case_miss()
cases |> select(positive_result) |>  naniar::n_case_miss()
cases |> naniar::vis_miss()


# roughly 1/4 of cases were never tested, just self-dx
cases |> count(confirmed_by_test)

# roughly 1/3 of cases were never tested during study period
cases_imp |> filter(case_date > '2021-08-26') |> count(confirmed_by_test)


# relationship between dates is not constant over time
a <- cases_imp |>
  select(positive_result, isolation_end) |>
  mutate(res_to_iso = as.numeric(isolation_end - positive_result)) |>
  ggplot(aes(isolation_end, days_since_res)) +
  geom_point() +
  geom_smooth()

b <- cases_imp |>
  select(positive_result, symptoms_began) |>
  mutate(days_since_symptoms = as.numeric(symptoms_began - positive_result)) |>
  ggplot(aes(symptoms_began, days_since_symptoms)) +
  geom_point() +
  geom_smooth()

c <- cases_imp |>
  select(positive_result, tested) |>
  mutate(days_since_tested = as.numeric(tested - positive_result)) |>
  ggplot(aes(tested, days_since_tested)) +
  geom_point() +
  geom_smooth()

(a / b / c)


# difference between pos res and iso end (mode is 5d)
a <- cases_imp |>
  filter(case_date > '2021-08-26') |>
  mutate(diff = isolation_end - positive_result) |>
  ggplot(aes(as.numeric(diff))) +
  geom_histogram() +
  labs(x = "isolation_end - positive_result")

# difference between pos res and symptoms (mode 3d)
b <- cases_imp |>
  filter(case_date > '2021-08-26') |>
  mutate(diff = symptoms_began - positive_result) |>
  ggplot(aes(as.numeric(diff))) +
  geom_histogram() +
  labs(x = "symptoms_began - positive_result")

# difference between pos res and tested (mode 0d)
c <- cases_imp |>
  filter(case_date > '2021-08-26') |>
  mutate(diff = tested - positive_result) |>
  ggplot(aes(as.numeric(diff))) +
  geom_histogram() +
  labs(x = "tested - positive_result")

(a / b / c)
rm(a, b, c)
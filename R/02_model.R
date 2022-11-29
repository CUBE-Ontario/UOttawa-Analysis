# model.R
## this script
# - creates summaries of swab results and cases at UOttawa
# - creates a mixed effects model with a random intercept for sites
# - outputs summaries and diagnostic plots
#

library(tidyverse)
library(here)
library(broom.mixed)
library(patchwork)
library(lubridate)


## data wrangling functions -----


tidy_swabs_data <- function(swabs) {
  # take UO data, remove neg. controls
  swabs_tidy <-
    swabs |>
    filter(type == "University",
           !str_detect(location, 'Air Swab|Site 2: Air')) |>
    select(site, location, date_swab, pcr_positive) |>
    mutate(
      # simplify site names
      location =  ifelse(
        str_detect(location, 'Site 1'),
        'high_traffic',
        'low_traffic'
      ),
      pcr_positive = case_when(
        pcr_positive == 'Positive' ~ TRUE,
        pcr_positive == 'Negative' ~ FALSE,
      ),
      # fix building names to match cases data
      site = str_remove(site, 'UO_'),
      # add an indicator for week
      week = week(date_swab),
      week = ifelse(year(date_swab) > 2021, week + 52, week)
    )
  return(swabs_tidy)
}

summarise_swabs_data <- function(swabs_tidy) {
  # summarize proportion of positive swabs per (site, week) group
  swab_summary <- swabs_tidy |>
    group_by(site, week) |>
    mutate(n = n()) |>
    ungroup() |>
    pivot_wider(
      id_cols = c(site, date_swab, week, n),
      names_from = location,
      values_from = c(pcr_positive)
    ) |>
    group_by(site, week) |>
    # create proportion of positive samples by (site, week)
    summarise(
      detection = (sum(high_traffic, na.rm = T) + sum(low_traffic, na.rm = T)) / n,
      n = unique(n),
      .groups = 'drop'
    ) |>
    select(site, week, detection) |>
    distinct()

  # fill in any missing (week, site) observations with 0 value for detection
  swab_summary <-
    expand_grid(week = unique(swabs_tidy$week),
                site = unique(swab_summary$site)) |>
    arrange(week, site) |>
    left_join(swab_summary, c("week", "site")) |>
    filter(!is.na(detection))

  return(swab_summary)
}

tidy_cases_data <- function(cases) {
  cases_tidy <- cases |>
    # subset data
    select(case, positive_result, `90U`:TBT) |>
    # make long-form using building indicators, remove empty rows
    pivot_longer(`90U`:TBT, names_to = 'site') |>
    filter(value) |>
    select(-value) |>
    # create week variable for grouping
    mutate(week = ifelse(
      test = year(positive_result) == 2022,
      yes = 52 + week(positive_result),
      no = week(positive_result)
    ))

  return(cases_tidy)
}

summarise_cases_data <- function(cases_tidy, swab_summary) {
  # create a grid of all (site, week) pairs to join with cases data
  case_summary <-
    expand_grid(week = seq(min(swab_summary$week), 57),
                site = unique(swab_summary$site)) |>
    arrange(site) |>
    # join with tallied case numbers
    left_join(
      cases_tidy |>
        group_by(site, week) |>
        filter(week %in% seq(
          min(swab_summary$week), max(swab_summary$week)
        )) |>
        summarise(cases = n(), .groups = 'drop'),
      by = c("week", "site")
    ) |>
    # replace any missing cases fields -> 0 cases
    mutate(cases = ifelse(is.na(cases), 0, cases))

  return(case_summary)
}

summarise_cases_and_swabs_data <- function(swabs, cases) {
  swab_summary <- swabs |>
    tidy_swabs_data() |>
    summarise_swabs_data()
  case_summary <- cases |>
    tidy_cases_data() |>
    summarise_cases_data(swab_summary = swab_summary)

  # join cases and swabs
  # fill in any missing case data with 0 cases
  # make building a factor for random effects
  ds <-
    case_summary |>
    left_join(swab_summary, by = c("week", "site")) |>
    mutate(cases = ifelse(is.na(cases), 0, cases),
           site = as_factor(site))

  # add the next week for each site without any cases or detection
  study_period <-
    crossing(week = min(ds$week):max(ds$week) + 1,
             site = unique(ds$site))

  # add lagging variables
  ds <- ds |>
    full_join(study_period, by = c('week', 'site')) |>
    group_by(site) |>
    mutate(
      detection_lag1w = lag(detection, 1),
      cases_bin =  ifelse(cases >= 1, yes = 1, no = 0)
    ) |>
    ungroup() |>
    arrange(site, week) |>
    relocate(week, site, contains('detection'))

  return(ds)
}



## plot functions ----

plot_logit_curves <- function(uo_pred, swab_model, sites) {
  newdata <- expand_grid(site = sites,
                         detection_lag1w = seq(0, 1, len = 500))
  newdata$cases_bin <-
    predict(swab_model, newdata = newdata, type = 'response')

  plt <- uo_pred |>
    ggplot(aes(y = cases_bin, x = detection_lag1w, color = site)) +
    geom_jitter(alpha = 0.5,
                width = 0.05,
                height = 0.05) +
    geom_line(data = newdata, aes(y = cases_bin, x = detection_lag1w, color = site)) +
    rcartocolor::scale_color_carto_d() +
    theme_light() +
    labs(y = 'Cases (binary)', x = 'Positive swabs (proportion)',
         title = 'Regression curves for each site')

  return(plt)
}

scale_color_pcr_dots <- function(plot) {
  scale_color_manual(
    values = viridis::inferno(
      n = 3,
      direction = 1,
      begin = 0.9,
      end = 0.2
    ),
    drop = FALSE,
    guide = guide_legend(override.aes = list(size = 3, alpha = 1))
  )
}

plot_site <- function(uo_pred, uo_swabs, uo_cases, selected) {
  swab <- uo_swabs |> filter(site == selected)
  case <- uo_cases |>
    bind_rows(
      list(
        case = NA,
        positive_result = NA,
        isolation_period_end = NA,
        transmission_start = NA,
        site = site_names,
        value = NA
      )
    ) |>
    filter(site == selected) |>
    mutate(location = 'Cases')

  # upper panel: model prediction of cases for next week
  p1 <- uo_pred |>
    filter(site == selected) |>
    ggplot(aes(x = date, y = pred)) +
    geom_col() +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_date(limits = as_date(c(min(
      c(uo_swabs$date_swab, uo_cases$positive_result), na.rm = T
    ) - 7,
    max(
      c(uo_swabs$date_swab, uo_cases$positive_result), na.rm = T
    ) + 14))) +
    labs(y = 'prob', x = NULL, color = "PCR") +
    annotate('text',
             y = 0.93,
             label = selected,
             x = min(c(
               uo_swabs$date_swab, uo_cases$positive_result
             ), na.rm = T) + 5) +
    theme_bw() +
    theme(
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 9)
    )

  # lower panel: swabs and cases
  p2 <- swab |>
    ggplot() +
    geom_point(aes(x = date_swab, y = location, color = pcr_positive),
               size = 2) +
    geom_segment(
      data = case,
      aes(
        x = positive_result - 3,
        xend = positive_result,
        y = location,
        yend = location
      ),
      position = position_jitter(width = 0.4),
      size = 5,
      color = 'black',
      alpha = 0.25
    ) +
    scale_color_pcr_dots() +
    scale_y_discrete(breaks = c('Site 1', 'Site 2', 'Cases')) +
    scale_x_date(limits = as_date(c(min(
      c(uo_swabs$date_swab, uo_cases$positive_result), na.rm = T
    ) - 5,
    max(
      c(uo_swabs$date_swab, uo_cases$positive_result), na.rm = T
    ) + 14))) +
    labs(y = NULL, x = NULL, color = "PCR Positive") +
    theme_bw()

  p1 / p2 & plot_annotation(subtitle = selected)
}





## prep data -----

# parse UO swabs and cases data; only use the flock swabs for model results
swabs <- read_rds(here('data/cube.rds')) |>
  filter(swab_type != 'sponge', !negative_control)

cases <- read_rds(here('data/latest_uo_cases.rds')) |>
  filter(positive_result > '2021-08-20', !is.na(positive_result))

last_case_date <- max(cases$positive_result)
week_last_case_date <- week(last_case_date) + 52

# prepare data for modelling
uo_sites <-
  summarise_cases_and_swabs_data(swabs, cases)

# preview summarized data
uo_sites |>
  ggplot(aes(week, detection_lag1w)) +
  geom_path(na.rm = T) +
  geom_point(aes(color = factor(cases_bin)), na.rm = T) +
  facet_grid(site ~ .)

# orignial mixed-effect logit model / binary outcome and numeric predictor
# singularity an issue unless MRT removed
#'` swab_model <-`
#'`   lme4::glmer(`
#'`     cases_bin ~ detection_lag1w + (1 | site),`
#'`     data =  uo_sites,`
#'`     family = binomial`
#'`   )`

# alternately, use bayesian framework mixed model
swab_model <-
  blme::bglmer(
    cases_bin ~ detection_lag1w + (1 | site),
    data =  uo_sites,
    family = binomial
  )

glance(swab_model)
tidy(swab_model)
tidy(swab_model, effects = 'ran_vals')
summary(swab_model)

write_rds(swab_model, here('model/uo_swab_model.rds'))


## could use a glm to get basically the same model; ^ random effects are minimal

#' ` swab_model2 <-`
#' `   glm(cases_bin ~ detection_lag1w + site, data =  uo_sites,`
#' `       family = binomial)`
#' ``
#' ` glance(swab_model2)`
#' ` tidy(swab_model2)`
#' ` tidy(swab_model2, effects = 'ran_vals')`
#' ` summary(swab_model2)`
#' ``
#' ` write_rds(swab_model2, 'model/uo_swab_model2.rds')`


## Preds + plots -----


uo_swabs <-
  swabs |>
  tidy_swabs_data() |>
  summarise_swabs_data() |>
  group_by(site) |>
  mutate(detection_lag1w = lag(detection)) |>
  ungroup() |>
  filter(!is.na(detection_lag1w))

uo_pred <-
  uo_swabs |>
# convert week of year variable back to dates.
  mutate(date = ymd('2021-01-01') + weeks(week) - days(1)) |>
# add predictions to dataset.
  bind_cols(pred = predict(swab_model, newdata = uo_swabs, type = 'response')) |>
  select(site, week, date, pred)

write_rds(uo_pred, here('model/uo_pred.rds'))

plt_curves <- plot_logit_curves(uo_sites, swab_model,
                                sites = unique(uo_sites$site))

# swab data for plot...
uo_swabs <-
  read_rds(here('data/cube.rds')) |>
  filter(type == "University", location != 'Air Swab') |>
  mutate(
    pcr_positive = as_factor(pcr_positive),
    site = str_remove(site, 'UO_'),
    location = str_extract(location, "Site [12]|Air Swab")
  )

# with row for each (case,study site) (and extra for whole campus)
## time range starts just before swabbing
uo_cases <-
  read_rds(here('data/latest_uo_cases.rds')) |>
  mutate(isolation_end = as_date(isolation_end)) |>
  select(case, matches('isolation|positive'), c(`90U`:TBT)) |>
  mutate(transmission_start = positive_result - days(3)) |>
  pivot_longer(c(`90U`:TBT), names_to = 'site') |>
  filter(value, positive_result > min(uo_swabs$date_swab) - 5)


site_names <- unique(uo_swabs$site) |> sort()
plots <- map(site_names, ~plot_site(uo_pred, uo_swabs, uo_cases, selected = .x))
uo_prediction_figure <- wrap_plots(plots, guides = 'collect', ncol = 2) &
  plot_annotation(
    subtitle = 'Swab results and cases (lower panels) aligned with\n probability of cases according to our model (top panels)'
  )

print(uo_prediction_figure)
print(plt_curves)

write_rds(uo_prediction_figure, here('fig/uo_prediction_figure.rds'))
write_rds(plt_curves, here('fig/plt_model_resp_curves.rds'))


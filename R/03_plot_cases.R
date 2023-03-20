library(tidyverse)
library(here)
library(ggiraph)
library(glue)
library(lubridate)
library(patchwork)

scale_color_pcr_dots <- function(){
  scale_color_manual(
    values = viridis::inferno(
      n = 3,
      direction = 1,
      begin = 0.9,
      end = 0.2
    ),
    drop = FALSE,
    guide = guide_legend(
      override.aes = list(size = 3, alpha = 1)
    )
  )
}

# swabs data
swabs <-
  read_rds(here('data/cube.rds')) |>
  filter(type == "University", swab_type == 'flock') |>
  mutate(site = str_remove(site, 'UO_'))



cases_raw <- read_rds(here('./data/latest_uo_cases.rds'))

# lots of missing data
naniar::vis_miss(cases_raw)
cases_raw |>
  glimpse()


# uo cases data
cases <-
  read_rds(here('./data/latest_uo_cases.rds')) |>
  filter(!is.na(positive_result),
         positive_result > as_date('2021-09-14')) |>
  select(
    case, role, confirmed_by_test, positive_result,
    c(`90U`:TBT),
  ) |>
  mutate(
    `Campus Total` = TRUE,
    # 3 days transmissibility
    transmission_start = positive_result - days(3)
    )

cases |> glimpse()

# Add lead / lag event assumptions -----


## longer data table ----
# with row for each (case,study site) (and extra for whole campus)
## time range starts just before swabbing
cases_long <-
  cases |>
  pivot_longer(`90U`:`Campus Total`, names_to = 'site') |>
  filter(value) |>
  mutate(site = fct_relevel(site, "Campus Total", after = 0L))

## plot1: swabs at each site (6 plots) -----
plt_sites <-
  swabs |>
  mutate(site = str_remove(site, 'UO_')) |>
  filter(location != 'Air Swab', !negative_control) |>
  mutate(location = str_extract(location, "Site [12]|Air Swab")) |>
  ggplot() +
  geom_point(aes(x = date_swab, y = location, color = pcr_positive),
             size = 1.1) +
  geom_rug(
    data = cases_long |> filter(site != "Campus Total"),
    aes(x = positive_result), color = 'black',
    alpha = 0.5, length = unit(0.2, "npc")) +
  scale_color_manual(
    values = viridis::inferno(
      n = 3,
      direction = 1,
      begin = 0.9,
      end = 0.2
    ),
    drop = FALSE,
    guide = guide_legend(
      override.aes = list(size = 3, alpha = 1)
    )
  ) +
  facet_wrap(~site, strip.position = 'left', ncol = 1) +
  scale_y_discrete(position = 'right') +
  scale_x_date(limits = c(min(swabs$date_swab) - 5,
                          max(swabs$date_swab) + 1)) +
  labs(y = NULL, x = NULL, color = "Swab PCR", subtitle = 'Swab results and cases by building') +
  theme_bw() +
  theme(
    strip.text = element_text(angle = 0),
    strip.background = element_rect(color = 'white',fill = 'white')
    )

## plot2: campus cases histogram and rug plot of cases (1 plot) -----
plt_campus <-
  cases_long |>
  mutate(presumed = ifelse(confirmed_by_test, 'confirmed', 'presumed')) |>
  filter(site == "Campus Total") |>
  ggplot() +
  geom_histogram(aes(x = positive_result, fill = presumed),
                 color = 'white') +
  rcartocolor::scale_fill_carto_d() +
  geom_rug(
    aes(x = positive_result, y = 0),
    sides = "b",
    color = 'black',
    alpha = 0.3,
    length = unit(0.15, "npc"),
    position = 'jitter',
    ) +
  scale_y_continuous(expand = c(0.25, 0.15), position = 'right') +
  scale_x_date(limits = c(min(swabs$date_swab) - 5,
                          max(swabs$date_swab) + 1)) +
  labs(x = NULL, fill = NULL, y = NULL, subtitle = "Campus-wide COVID-19 cases") +
  theme_bw()

## Combine swabs and cases panels
plt_cases_swabs <-
  wrap_plots(plt_campus, plt_sites) &
  plot_layout(ncol = 1, heights = c(1,4))


print(plt_cases_swabs)
write_rds(plt_cases_swabs, here('fig/plt_cases_swabs.rds'))
write_rds(plt_campus, here('fig/plt_cases_campus.rds'))



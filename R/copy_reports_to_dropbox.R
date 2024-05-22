# Copy reports to dropbox
#
# Copies files to dropbox/CUBE_Study/Data Analysis folder
# so that other team members can stay up to date.
#
# Copy figures and results to .../uOttawa paper/

library(tidyverse)

# copy reports to dropbox
rmarkdown::render(
  input = 'cube_uottawa_report.Rmd',
  output_dir = '~/Dropbox/CUBE Study/uOttawa paper/'
)

# rmarkdown::render(
#   input = 'cube_uottawa_eda.Rmd',
#   output_dir = '~/Dropbox/CUBE Study/Data Analysis/'
# )

# copy figures to dropbox
fs::dir_info(here::here('fig')) |>
  select(path) |>
  filter(str_detect(path, '/figure_')) |>
  mutate(
    file = str_extract(path, '([^/]+$){1}'),
    db = '~/Dropbox/CUBE Study/uOttawa paper/figures',
    new = str_glue('{db}/{file}')
    ) |>
  select(path, new) |>
  mutate(exec = walk2(
    path, new,
    ~fs::file_copy(.x, .y, overwrite = T)
    ))


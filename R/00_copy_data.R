# copy from data sources

# swabs
fs::file_copy(
  path = "~/Documents/LTRI/CUBE_project/cube_data_analysis/data/cube.rds",
  new_path = here::here('data/cube.rds'),
  overwrite = T
)

# UOttawa waste water
fs::file_copy(
  path = "~/Dropbox/CUBE Study/Data Analysis/External Data Sources/uOttawa wastewater sampling data 2022-08-24.xlsx",
  new_path = here::here('data/ww_university.xlsx'),
  overwrite = T
  )

# ottawa city waste water readings from cube_data_analysis/
fs::file_copy(
  path = "~/Documents/LTRI/CUBE_project/cube_data_analysis/data/ww_ottawa.rds",
  new_path = here::here('data/ww_ottawa.rds'),
  overwrite = T
)

# wifi - daily peak
fs::file_copy(
  path = "~/Documents/LTRI/CUBE_project/cube_data_analysis/data/uo_wifi.rds",
  new_path = here('data/uo_wifi.rds'),
  overwrite = T
)

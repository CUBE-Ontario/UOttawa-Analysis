# Copy reports to dropbox
#
# Copies files to dropbox/CUBE_Study/Data Analysis folder
# so that other team members can stay up to date.
#
rmarkdown::render(
  input = 'cube_uottawa_report.Rmd',
  output_dir = '~/Dropbox/CUBE Study/Data Analysis/'
)

rmarkdown::render(
  input = 'cube_uottawa_eda.Rmd',
  output_dir = '~/Dropbox/CUBE Study/Data Analysis/'
)

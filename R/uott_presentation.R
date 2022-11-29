

#### libs ------
library(tidyverse)
library(here)
library(ggtext)
library(patchwork)
library(rcartocolor)
library(glue)


## Plot fx ------------

scale_color_pcr_dots <- function(){
  scale_color_manual(
    values = viridis::inferno(
      n = 3,
      direction = 1,
      begin = 0.9,
      end = 0.2
    ),
    breaks = c('Negative', 'Positive', ''),
    drop = FALSE,
    guide = guide_legend(
      override.aes = list(size = 1.2, alpha = 1)
    )
  )
}



### Plot swab pcr results (upper plot of site panel)
plot_swabs <- function(df, title, legend, pcr_labs){

  plt <- ggplot(df,
         aes(
           y = fct_rev(locname),
           x = date,
           color = pcr_positive
         )) +
    geom_point(size = 1) +
    scale_color_pcr_dots() +
    scale_x_date() +
    labs(
      x = NULL,
      y = NULL,
      color = NULL,
      subtitle = title
    ) +
    theme_bw() +
    theme(
      # legend.margin = margin(10,10,10,10),
      legend.spacing.y = unit(2, 'mm'),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
      )
  if (!legend) {
    plt <- plt + theme(legend.position = "none")
  }
  if (!pcr_labs) {
    plt <- plt + theme(axis.text.y = element_blank())
  }

  return(plt)
}



### Plot both CO2 and wifi data (low half of site panel)
plot_co2_and_wifi <- function(df_co2, df_wifi, legend, axis_titles){
  co2_plt <-
    ggplot(df_co2, aes(x = date)) +
    geom_line(aes(y = co2, color = locname), alpha = 0.6) +
    geom_point(aes(y = co2, color = locname), size = 0.8, alpha = 0.7) +
    geom_point(
      data = df_wifi |> filter(date %in% df_co2$date),
      aes(x = date, y = clients, color = 'Wifi'),
      size  = 0.8,
      alpha = 0.7
    ) +
    geom_line(data = df_wifi,
              aes(x = date, y = clients, color = 'Wifi'),
              alpha = 0.6) +
    scale_color_carto_d() +
    scale_x_date() +
    ylim(c(0, 1300)) +
    labs(y = "CO<sub>2</sub> (ppm)<br><span style='color: darkgrey'>Wifi: Peak Users</span>",
         color = NULL, x = NULL) +
    theme_bw() +
    theme(
      axis.title.y = element_markdown(),
      legend.text = element_markdown())
  if (!legend) {
    co2_plt <- co2_plt + theme(legend.position = "none")
  }
  if (!axis_titles) {
    co2_plt <- co2_plt +
      theme(axis.title.y.left = element_blank(),
            axis.title.y.right = element_blank())
  }
  return(co2_plt)

}


### Plot CO2 without wifi data (low half of site panel)

plot_co2 <- function(df_swab, legend, axis_titles){
  co2_plt <- df_swab |>
    ggplot(aes(x = date)) +
    geom_line(aes(y = co2, color = locname), alpha = 0.6) +
    geom_point(aes(y = co2, color = locname), size = 0.8, alpha = 0.7) +
    ylim(c(0, 1300)) +
    scale_x_date() +
    scale_color_carto_d() +
    labs(y = "CO<sub>2</sub> (ppm)<br><span style='color: darkgrey'>Wifi: Peak Users</span>",
         color = 'Data Type', x = NULL) +
    theme_bw() +
    theme(axis.title.y = element_markdown(),
          legend.text = element_markdown())

  if (!legend) {
    co2_plt <- co2_plt + theme(legend.position = "none")
  }
  if (!axis_titles) {
    co2_plt <- co2_plt +
      theme(axis.title.y.left = element_blank())
  }
  return(co2_plt)
}




# Combination plot functions ----------

make_plots <- function(df_swab,
                       df_co2,
                       df_wifi,
                       title,
                       legend = FALSE,
                       pcr_labs = FALSE,
                       axis_titles = TRUE,
                       line_legend = FALSE) {
  pcr_plt <- plot_swabs(df = df_swab, title, legend, pcr_labs)
  co2_wifi_plt <- plot_co2_and_wifi(df_co2, df_wifi, legend = line_legend,
                                    axis_titles = axis_titles)
  return(
    (pcr_plt / co2_wifi_plt) &
           plot_layout(
             ncol = 1,
             widths = c(1, 1),
             heights = c(0.5, 2)
           )
    )
}

## when there is no wifi data??
make_plots2 <- function(df_swab,
                        title,
                        legend,
                        pcr_labs = FALSE,
                        axis_titles = TRUE) {

  pcr_plt <- plot_swabs(df = df_swab, title, legend, pcr_labs)
  co2_plt <- plot_co2(df_swab, legend, axis_titles)

  return((pcr_plt / co2_plt) &
           plot_layout(
             ncol = 1,
             widths = c(1, 1),
             heights = c(0.5, 2)
           ))
}

###



make_uo_panel <- function() {

#### data ------
  swabs_df <-
    read_rds(here('data/cube.rds')) |>
    filter(type == 'University', city == 'Ottawa', str_detect(site, '^UO')) |>
    mutate(date = date_swab) |>

    filter(!negative_control) |>
    mutate(
      locname = str_extract(location, 'Site [0-9]'),
      locname = ifelse(locname == 'Site 1', 'High Traffic', 'Low Traffic')
    )

  co2_df <- swabs_df |>
    mutate(locname = glue('CO<sub>2</sub> {locname}')) |>
    group_by(site, locname)

  wifi <-
    read_rds(here('data/uo_wifi.rds')) |>
    filter(date >= min(swabs_df$date))


  #### create plots for each site ------
  # u90
  u90 <- make_plots2(
    df_swab = swabs_df |> filter(site == 'UO_90U'),
    title = "90 University",
    legend = FALSE,
    pcr_labs = TRUE,
    axis_titles = TRUE
  )
  # dms
  dms <- make_plots(
    df_swab = swabs_df |> filter(site == 'UO_DMS'),
    df_co2 = co2_df |> filter(site == 'UO_DMS'),
    df_wifi = wifi |> filter(site == 'UO_DMS'),
    title = "DMS",
    legend = FALSE,
    axis_titles =  FALSE
  )

  # lpr
  lpr <- make_plots(
    df_swab = swabs_df |> filter(site == 'UO_LPR'),
    df_co2 = co2_df |> filter(site == 'UO_LPR'),
    df_wifi = wifi |> filter(site == 'UO_LPR'),
    title = "LPR",
    legend = TRUE,
    line_legend = TRUE,
    pcr_labs = FALSE,
    axis_titles = FALSE
  )

  # mnt
  mnt <- make_plots(
    df_swab = swabs_df |> filter(site == 'UO_MNT'),
    df_co2 = co2_df |> filter(site == 'UO_MNT'),
    df_wifi = wifi |> filter(site == 'UO_MNT'),
    title = "MNT",
    legend = FALSE,
    pcr_labs = TRUE,
    axis_titles =  TRUE
  )

  # mrt
  mrt <- make_plots(
    df_swab = swabs_df |> filter(site == 'UO_MRT'),
    df_co2 = co2_df |> filter(site == 'UO_MRT'),
    df_wifi = wifi |> filter(site == 'UO_MRT'),
    title = "MRT",
    legend = FALSE,
    axis_titles =  FALSE
  )

  # tbt
  tbt <- make_plots(
    df_swab = swabs_df |> filter(site == 'UO_TBT'),
    df_co2 = co2_df |> filter(site == 'UO_TBT'),
    df_wifi = wifi |> filter(site == 'UO_TBT'),
    title = "TBT",
    pcr_labs = FALSE,
    axis_titles = FALSE
  )
  wrap_plots(u90, dms, lpr, mnt, mrt, tbt) &
    plot_layout(guides = 'collect')

}



## return plot
uo_multivariate <- make_uo_panel()


write_rds(uo_multivariate, here('fig/uo_multivariate.rds'))

rm(make_plots, make_plots2, make_uo_panel, plot_co2, plot_co2_and_wifi, plot_swabs)


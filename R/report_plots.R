library(tidyverse)
library(ggiraph)
library(lubridate)
library(glue)


# Scales ----
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

scale_fill_pcr_dots <- function(){
  scale_fill_manual_interactive(
    values = viridis::inferno(
      n = 3,
      direction = 1,
      begin = 0.9,
      end = 0.1
    ),
    drop = FALSE,
    guide = guide_legend(
      override.aes = list(size = 3, alpha = 1)
    )
  )
}

# Report plots ----
plot_timeseries_summary <- function(swabs, ncol=2, ...) {

  # modified time-series data without negative controls....
  ts_data <-
    swabs |>
    filter(!negative_control) |>
    mutate(pcr = pcr_positive) |>
    group_by(site, date_swab) |>
    summarise(
      Positive = sum(pcr == 'Positive'),
      Negative = sum(pcr == 'Negative'),
      Spoiled = sum(pcr == 'Spoiled'),
      .groups = 'drop'
    ) |>
    mutate(
      row_sum = Negative + Positive + Spoiled,
      label_txt = str_glue(
        'Site: {site}<br>',
        'Date: {date_swab}<br>',
        '{row_sum} swabs collected:<br>',
        '<ul><li>{Positive} PCR positive</li>',
        '<li>{Negative} PCR negative</li>',
        '<li>{Spoiled} spoiled samples</li>',
        '</ul>'
      )) |>
    select(-row_sum) |>
    pivot_longer(-c(site, date_swab, label_txt),
                 names_to = 'pcr',
                 values_to = 'n') |>
    mutate(pcr = factor(pcr, levels = c('Negative', 'Positive', 'Spoiled')))

  # bar chart time series by location, without negative controls
  plt1 <- ts_data  |>
    ggplot() +
    geom_col_interactive(
      aes(
        x = date_swab,
        y = n,
        fill = pcr,
        tooltip = label_txt
      ),
      position = 'stack',
      width = 1.25
    ) +
    facet_wrap(~ site, scales = 'free_y', ncol = ncol) +
    labs(
      x = NULL,
      y = 'n samples',
      fill = 'PCR result:',
    ) +
    scale_fill_pcr_dots() +
    scale_size_interactive(guide = NULL, range = c(0, 3)) +
    scale_alpha_interactive(guide = NULL, range = c(0.2, 0.7)) +
    scale_x_date(
      date_breaks = '1 month', date_labels = "%b"
    ) +
    scale_y_continuous(expand = c(0, 0, 0.2, 0)) +
    guides(
      color = guide_legend(),
      fill = guide_legend(),
      alpha = guide_legend(),
      size = guide_legend()
    ) +
    theme_light() +
    theme(
      text = element_text(size = 8, family = 'sans'),
      strip.text.x.top = element_text(
        hjust = 0,
        size = 7,
        color = 'black'
      ),
      strip.background = element_rect(fill = 'white'),
      strip.placement = 'inside',
      panel.grid.minor = element_blank(),
      axis.title.y.left = ggtext::element_markdown(size = 8),
      legend.position = 'right',
      legend.spacing = unit(0.4, 'cm'),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8)
    )

  # make ggplot object interactive
  ggiraph::girafe(
    ggobj = plt1,
    options = list(
      opts_tooltip(
        opacity = .8,
        css = "background-color:black;color:white;margin:0.5em;padding:1em;border-radius:5px;"),
      opts_hover(css = "stroke:#1279BF;cursor:pointer;")
    ),
    ...,
  )

}



# dotplot timeseries by site, location
plot_timeseries_locations <-
  function(swabs, ...) {

    # site+location-level time-series plot
    plt2_df <-
      swabs |>
      mutate(
        location = as.character(floor_location),
        location = ifelse(
          str_detect(site, '^UO_') &
            str_detect(tolower(location), 'air|control'),
          'Negative control',
          location
        ),
        location = str_to_title(location),
        co2_text = ifelse(is.na(co2), '', str_glue('<br>CO2: {co2} ppm')),
        label_text = glue(
          'Date collected: {date_swab}
           Site: {site}
           Floor: {floor}
           Location: {location}
           Replicate: {replicate}
           PCR Ct: {round(pcr_ct, 2)} {co2_text}
           Lot / barcode: {lot} / {barcode}
           Collected by: {worker}
           PCR Lab: {lab}
           Comment: {str_wrap(comments, width = 45)}'
        ),
        swab_id = str_glue('{lot}:{barcode}')
      ) |>
      mutate(location = str_trunc(location, 40)) |>
      arrange(site, date_swab, location) |>
      select(date_swab, site, location, pcr_positive,
             negative_control, label_text, swab_id)

    plt2 <- plt2_df |>
      ggplot(
        aes(
          x = date_swab,
          y = fct_rev(location),
          color = pcr_positive,
          shape = negative_control,
          tooltip = label_text,
          data_id = swab_id
        )
      ) +
      geom_point_interactive(size = 1.4, alpha = 0.8) +
      facet_grid(site ~ .,
                 scales = 'free_y',
                 space = 'free',
                 switch = 'y') +
      labs(
        x = NULL,
        y = NULL,
        shape = 'Control',
        color = 'PCR Result'
      ) +
      scale_x_date(position = 'top') +
      scale_y_discrete(position = 'right') +
      scale_color_pcr_dots() +
      theme_light() +
      theme(
        axis.text.y.right = element_text(size = 6),
        legend.position = 'top',
        plot.caption = ggtext::element_markdown(),
        plot.caption.position = 'plot',
        text = element_text(size = 9.5, family = 'sans'),
        strip.background = element_rect(fill = 'white'),
        strip.text.y.left = element_text(
          angle = 0,
          color = 'black',
          hjust = 1
        )
      )

    # make plot interactive
    girafe(
      ggobj = plt2,
      options = list(
        opts_tooltip(
          opacity = 0.85,
          use_fill = F,
          css = "stroke:#fff;background-color:gray;color:white;padding:5px;border-radius:5px;"
        ),
        opts_hover(css = "fill:#fff;stroke:#000;r:2.5pt;")
      ),
      ...
    )
  }



# wraps three plots into three-panel fig
plot_uo_pred_swab_case <- function(swabs, cases, preds, site_select = '90U'){


  plt1_uo_prob_barchart <- function(preds, min_date, max_date, title = NULL){
    panel <- preds |>
      filter(!is.na(pred)) |>
      ggplot(aes(x = date, y = pred)) +
      geom_col() +
      scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
      scale_x_date(limits = c(min_date, max_date)) +
      labs(y = 'Predicted', x = NULL, color = "PCR",
           title = title) +
      theme(
        panel.grid.minor.y = element_blank(),
        plot.margin = margin(10, 10, 1, 1, "pt"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(vjust = -2, size = 9)
      )
    return(panel)
  }

  # cases jitterplot for a single site
  plt2_site_case_jitter <- function(cases, min_date, max_date){
    panel <- cases |>
      mutate(location = 'Cases') |>
      ggplot() +
      geom_segment(
        aes(
          x = positive_result - 3,
          xend = positive_result,
          y = location,
          yend = location,
          color = role
        ),
        position = position_jitter(width = 0.4),
        size = 5,
        alpha = 0.39
      ) +
      scale_color_manual(
        values = rcartocolor::carto_pal(n = 2, name = 'Tropic'),
        drop = FALSE,
        breaks = c( 'Employee', 'Student'),
        guide = guide_legend(
          override.aes = list(size = 3, alpha = 0.4)
        )
      ) +
      scale_x_date(limits = c(min_date, max_date)) +
      labs(x = NULL, y = NULL, color = 'Case:') +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            plot.margin = margin(0.25, 10, 0.25, 10, "pt")
      )
    return(panel)
  }

  # swabs dotplot for a single site.
  plt3_uo_swab_dotplot <- function(swabs, min_date, max_date){
    panel <- swabs |>
      ggplot() +
      geom_point(aes(x = date_swab,
                     y = location,
                     color = pcr_positive),
                 size = 1, alpha = 0.8) +
      scale_color_pcr_dots() +
      scale_x_date(limits = c(min_date, max_date)) +
      labs(y = NULL, x = NULL, color = "Swab: PCR test") +
      theme(plot.margin = margin(0.1, 10, 10, 10, "pt"))
    return(panel)
  }

  # filter data to site selected
  preds <- preds |> filter(site == site_select)
  swabs <- swabs |>
    mutate(site = str_remove(site, 'UO_')) |>
    mutate(location = str_extract(location, "Site [12]")) |>
    filter(!negative_control, site == site_select)
  cases <- cases |>
    mutate(
      role = ifelse(
        as.character(role) == 'Resident',
        'Student',
        as.character(role)
      ) |> factor(levels = c('Student', 'Employee'))

    ) |>
    filter(site == site_select)

  # set date limits
  min_date <- min(c(swabs$date_swab,
                    cases$positive_result),
                  na.rm = T) - 5
  max_date <- max(c(swabs$date_swab,
                    cases$positive_result),
                  na.rm = T) + 14

  # create plots and combine
  p1 <- plt1_uo_prob_barchart(preds, min_date,
                              max_date,
                              title = site_select)
  p2 <- plt2_site_case_jitter(cases, min_date, max_date)
  p3 <- plt3_uo_swab_dotplot(swabs, min_date, max_date)
  final_plt <- p1/p2/p3 & plot_annotation()

  return(final_plt)
}



plot_UO_wifi_ts <- function(wifi, cube){

  ## to highlight where weekends are
  weekends <-
    tibble(date = seq(min(wifi$date), max(wifi$date), by = "days") |>
             as.Date()) |>
    mutate(day = lubridate::wday(date, label = T)) |>
    filter(day %in% c('Sat', 'Sun'))

  # sampling_days
  swab_days <- cube |>
    filter(str_detect(site, '^UO')) |>
    select(date_swab) |>
    distinct()

  ## create swab result counts to overlay wifi traffic...
  # wifi_swabs <- cube |> filter(site %in% wifi$site)

  # wifi time series plot..
  plt_wifi <- wifi |>
    mutate(site = str_remove(site, 'UO_')) |>
    ggplot(aes(y = clients, x = date, group = site,
               tooltip = tooltip)) +
    geom_vline(data = swab_days, aes(xintercept = date_swab),
               color = 'royalblue', alpha = 0.2, size = 1) +
    geom_path(color = 'darkgray', alpha = 0.5) +
    geom_point_interactive(size = 0.8, alpha = 0.5, shape = 1) +
    facet_wrap(~site, scales = 'free', ncol = 1,
               strip.position = "left") +
    scale_y_continuous(position = 'right') +
    theme_bw() +
    labs(x = NULL, y = NULL) +
    theme(
      text = element_text(size = 9),
      plot.subtitle = element_markdown(),
      strip.text.y.left =  element_text(
        angle = 0, hjust = 1, vjust = 0.5,  size = 8.5,
        color = 'black'
      ),
      strip.background = element_rect(fill = 'white',
                                      color = 'white'),
      panel.grid.minor = element_blank()
    )
  # make ggplot object interactive
  ggiraph::girafe(
    ggobj = plt_wifi,
    pointsize = 1,
    width_svg = 8,
    height_svg = 3.2,
  )

}



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






# Combination plots ----
make_uo_panel <- function(swabs, wifi) {



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


  #### data ------
  swabs_df <-
    rswabs |>
    mutate(date = date_swab) |>
    filter(!negative_control) |>
    mutate(
      locname = str_extract(location, 'Site [0-9]'),
      locname = ifelse(locname == 'Site 1', 'High Traffic', 'Low Traffic')
    )

  co2_df <- swabs_df |>
    mutate(locname = glue('CO<sub>2</sub> {locname}')) |>
    group_by(site, locname)


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

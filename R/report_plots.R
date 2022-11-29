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
        # location = str_remove(location, ' \\(1\\)'),
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



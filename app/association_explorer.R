plot_associations <- function(chart_type, x_axis, y_axis, colour, agg_level = 'Country/Region', log_transform_x = FALSE, log_transform_y = FALSE) {
  if (agg_level == 'Country/Region') {
    plot_dat <- country_grouped %>% as.data.frame()
  } else if (agg_level == 'State/Province') {
    plot_dat <- state_prov_grouped %>% as.data.frame()
  } else {
    plot_dat <- local_grouped %>% as.data.table()
    plot_dat <- plot_dat[!(is.na(Admin2)) & Admin2 != 'Unassigned']
    plot_dat <- plot_dat %>% as.data.frame()
  }
  
  if (is.factor(plot_dat[, x_axis]) |
      x_axis %in% c('Population', 'Density', 'First100Date', 'GDP.PPP', 'Transit.Utilization') |
      grepl('accel|rate', x_axis) | 
      grepl('accel_rate', y_axis)) {
    plot_dat <- plot_dat %>%
      filter(Date == max(Date))
  }
  
  x_title <- str_to_title(gsub('[^[:alnum:]]', ' ', x_axis))
  y_title <- str_to_title(gsub('[^[:alnum:]]', ' ', y_axis))
  legend.position = 'none'
  
  # Plot the data
  g <- ggplot(data = plot_dat,
              mapping = aes_string(x = x_axis, y = y_axis, colour = colour, fill = colour)) +
    labs(title = 'Variable Associations',
         subtitle = sprintf('%s vs. %s', y_title, x_title),
         x = x_title,
         y = y_title) +
    if (chart_type == 'Point') geom_point() else if (chart_type == 'Line') geom_line() else if (chart_type == 'Bar') geom_bar(stat = 'identity')
  
  if (is.numeric(plot_dat[, x_axis]) & log_transform_x) g <- g + scale_x_continuous(trans = 'log2')
  if (is.numeric(plot_dat[, y_axis]) & log_transform_y) g <- g + scale_y_continuous(trans = 'log2') 
  
  g <- g +
    theme(panel.background = element_rect(fill = '#2b3e50'),
          plot.background = element_rect(fill = '#2b3e50'),
          strip.background = element_blank(),
          axis.line.y = element_line(color = 'white'),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text = element_text(color = 'white'),
          axis.title = element_text(color = 'white', face = 'bold'),
          panel.grid.major.x = element_blank(),
          title = element_text(color = 'white', face = 'bold'),
          legend.position = legend.position,
          legend.title = element_blank(),
          legend.text = element_text(color = 'white', face = 'bold'),
          legend.background = element_rect(fill = '#2b3e50'))
  
  ggplotly(g) %>%
    layout(paper_bgcolor = NULL,
           plot_bgcolor = NULL)
}

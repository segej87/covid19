get_summary_dat <- function(countries, state_province, break_out_states, break_out_countries) {
  # start_time <- Sys.time()
  
  if (length(countries) == 1 & break_out_states) {
    plot_dat <- state_prov_grouped %>%
      as.data.table()
    
    plot_dat <- suppressWarnings(
      plot_dat <- plot_dat[Country.Region %in% countries &
                             Province.State %in% state_province]
    )
  } else {
    if (!break_out_countries) {
      plot_dat <- dat_summ %>%
        as.data.table()
      
      plot_dat <- suppressWarnings(
        plot_dat[Country.Region %in% countries &
                             Province.State %in% state_province] %>%
        group_by(Date) %>%
        arrange(Date) %>%
        summarise(Confirmed = sum(Confirmed, na.rm = TRUE),
                  Deaths = sum(Deaths, na.rm = TRUE),
                  Recovered = sum(Recovered, na.rm = TRUE),
                  Population = sum(StatePopulation, na.rm = TRUE)) %>%
        mutate(Confirmed_rate = Confirmed - lag(Confirmed, default = 0),
               Deaths_rate = Deaths - lag(Deaths, default = 0),
               Recovered_rate = Recovered - lag(Recovered, default = 0),
               Confirmed_accel = (2 * Confirmed_rate - lag(Confirmed_rate, default = 0) - lag(Confirmed_rate, n = 2, default = 0))/2,
               Deaths_accel = (2 * Deaths_rate - lag(Deaths_rate, default = 0) - lag(Deaths_rate, n = 2, default = 0))/2,
               Recovered_accel = (2 * Recovered_rate - lag(Recovered_rate, default = 0) - lag(Recovered_rate, n = 2, default = 0))/2) %>%
        mutate(Country.Region = 'World') %>%
        left_join(dat_summ %>%
                    filter(Country.Region %in% countries,
                           Province.State %in% state_province) %>%
                    group_by(Date) %>%
                    summarise(Confirmed = sum(Confirmed, na.rm = TRUE)) %>%
                    filter(Confirmed >= 100) %>%
                    summarise(First100Date = min(Date, na.rm = TRUE)) %>%
                    mutate(Country.Region = 'World'),
                  by = 'Country.Region') %>%
        mutate(normalized_date = as.numeric(difftime(Date, First100Date, unit = 'days')),
               date_lag = difftime(Date, lag(Date), units = 'days'))
      )
    } else {
      plot_dat <- dat_summ %>%
        as.data.table()
      
      plot_dat <- suppressWarnings(
        plot_dat[Country.Region %in% countries &
                               Province.State %in% state_province] %>%
          group_by(Date, Country.Region) %>%
          summarise(Confirmed = sum(Confirmed, na.rm = TRUE),
                    Deaths = sum(Deaths, na.rm = TRUE),
                    Recovered = sum(Recovered, na.rm = TRUE),
                    AggStatePop = sum(StatePopulation, na.rm = TRUE)) %>%
          group_by(Country.Region) %>%
          mutate(Confirmed_rate = Confirmed - lag(Confirmed, default = 0),
                 Deaths_rate = Deaths - lag(Deaths, default = 0),
                 Recovered_rate = Recovered - lag(Recovered, default = 0),
                 Confirmed_accel = (2 * Confirmed_rate - lag(Confirmed_rate, default = 0) - lag(Confirmed_rate, n = 2, default = 0))/2,
                 Deaths_accel = (2 * Deaths_rate - lag(Deaths_rate, default = 0) - lag(Deaths_rate, n = 2, default = 0))/2,
                 Recovered_accel = (2 * Recovered_rate - lag(Recovered_rate, default = 0) - lag(Recovered_rate, n = 2, default = 0))/2) %>%
          left_join(dat_summ %>%
                      filter(Country.Region %in% countries,
                             Province.State %in% state_province) %>%
                      group_by(Date, Country.Region) %>%
                      summarise(Confirmed = sum(Confirmed, na.rm = TRUE)) %>%
                      filter(Confirmed >= 100) %>%
                      group_by(Country.Region) %>%
                      summarise(First100Date = min(Date, na.rm = TRUE)),
                    by = 'Country.Region') %>%
          left_join(country_populations, by = 'Country.Region') %>%
          mutate(Population = ifelse(AggStatePop == 0, CountryPopulation, AggStatePop)) %>%
          mutate(normalized_date = as.numeric(difftime(Date, First100Date, unit = 'days')),
                 date_lag = difftime(Date, lag(Date), units = 'days'))
      )
    }
  }
  
  # cat(sprintf('Load data: %.2f\n', as.numeric(difftime(Sys.time(), start_time, units = 'secs'))))
  
  return(plot_dat)
}

write_summary <- function(countries, state_province) {
  rate_timeframe <- 4
  
  plot_dat <- get_summary_dat(countries, state_province, break_out_states = FALSE, break_out_countries = FALSE) %>%
    top_n(rate_timeframe, wt = Date)
  
  weekly_accel <- plot_dat %>%
    select_at(vars('Date', contains('_rate'))) %>%
    mutate_if(is.numeric, function(x) (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE)))
  
  if (length(which(!is.na(weekly_accel$Confirmed_rate))) > 0) {
    confirmed_weekly_accel <- lm(Confirmed_rate~Date, data = weekly_accel)
    case_cov <- confirmed_weekly_accel$coefficients['Date']
  } else {
    case_cov <- NA
  }
  
  if (length(which(!is.na(weekly_accel$Deaths_rate))) > 0) {
    deaths_weekly_accel <- lm(Deaths_rate~Date, data = weekly_accel)
    deaths_cov <- deaths_weekly_accel$coefficients['Date']
  } else {
    deaths_cov <- NA
  }
  
  top_1 <- plot_dat %>%
    top_n(1, wt = Date)
  
  metric <- c('Confirmed' = '#FED8B1',
              'Deaths' = '#DE5246',
              'Recovered' = '#228C22')
  
  headers <- list()
  contents <- list()
  for (m in names(metric)) {
    headers[m] <- paste0(
      '<font size="6"; face=bold; font-family="lato">',
      m,
      '</font>'
    )
    
    contents[m] <- paste0(
      sprintf('<font size="6"; face=bold; color=%s; font-family="lato">', metric[m]),
      format(as.numeric(top_1[,m]), big.mark = ','),
      '</font>'
    )
  }
  
  assign('top_1', top_1, envir = .GlobalEnv)
  
  summary_text <- paste0(
    '<font size="4"; font-family="lato">In the selected areas there have been<br>',
    sprintf('<font color=%s; face=bold; font-family="lato">',metric['Confirmed']), format(top_1$Confirmed_rate, big.mark = ','), '</font>',
    ' new cases in the past day<br>and ',
    sprintf('<font color=%s; face=bold; font-family="lato">',metric['Deaths']), format(top_1$Deaths_rate, big.mark = ','), '</font>',
    sprintf(' new deaths. Over the past %s days<br>the new case rate is ', rate_timeframe),
    if (is.na(case_cov)) '<font color=#228C22; face = bold; font-family="lato">level at zero</font>' else if (case_cov > 0.05) '<font color=#De5246; face=bold; font-family="lato">accelerating</font>' else if (case_cov > -0.05) '<font color=#FED8B1; face=bold; font-family="lato">about steady</font>' else '<font color=#228C22; face=bold; font-family="lato">decelerating</font>',
    '<br>and the death rate is ',
    if (is.na(deaths_cov)) '<font color=#228C22; face = bold; font-family="lato">level at zero</font>' else if(deaths_cov > 0.05) '<font color=#De5246; face=bold; font-family="lato">accelerating</font>' else if (deaths_cov > -0.05) '<font color=#FED8B1; face=bold; font-family="lato">about steady</font>' else '<font color=#228C22; face=bold; font-family="lato">decelerating</font>',
    '</font>'
  )
  
  header_row = paste0(
    '<table style="width:100%"><tr>',
    paste0(rep('<th></th>', length(metric) + 1), collapse = ''),
    '</tr><tr><td align="center"; valign="bottom">',
    paste(headers, collapse = '</td><td align="center"; valign="bottom">'),
    '</td><td rowspan="3"; align="center">', summary_text ,'</td>',
    '</tr><tr><td align="center"; valign="top">',
    paste(contents, collapse = '</td><td align="center"; valign="top">'),
    '</td><tr>',
    paste0(rep('<td></td>', length(metric) + 1), collapse = ''),
    '</tr></table>'
  )
  
  return(header_row)
}

plot_line <- function(countries, state_province, metric, break_out_states = FALSE, show_lockdowns = FALSE, normalize_dates = FALSE, normalize_pops = FALSE, normalize_tests = FALSE, break_out_countries = TRUE, type = 'Count', log_transform = FALSE, zero_on = FALSE) {
  # start_time <- Sys.time()
  
  plot_dat <- get_summary_dat(countries, state_province, break_out_states, break_out_countries) %>%
    if (type != 'Count') filter(., date_lag == 1) else .
  
  if (length(countries) == 1 & break_out_states & break_out_countries) {
    colour = 'Province.State'
  } else {
    colour = 'Country.Region'
  }
  
  line_types <- c(
    Confirmed = 'solid',
    Deaths = 'longdash',
    Recovered = 'dotted',
    Confirmed_rate = 'solid',
    Deaths_rate = 'longdash',
    Recovered_rate = 'dotted',
    Confirmed_accel = 'solid',
    Deaths_accel = 'longdash',
    Recovered_accel = 'dotted'
  )
  
  if (type == 'Rate') {
    metric = paste0(metric, '_rate')
    y_title = 'New'
    legend.position = 'right'
  } else if (type == 'Acceleration') {
    metric = paste0(metric, '_accel')
    y_title = 'Rate Change'
    legend.position = 'right'
  } else {
    y_title = 'Total'
    legend.position = 'none'
  }
  
  if (normalize_dates) {
    x = 'normalized_date'
  } else {
    x = 'Date'
  }
  
  if (normalize_tests) {
    plot_dat <- plot_dat %>%
      mutate(Confirmed = positive/totalTestResults,
             Confirmed_rate = positiveIncrease/totalTestResults)
  }
  
  if (normalize_pops) {
    plot_dat <- plot_dat %>%
      mutate_at(.vars = metric, .funs = list(normalized = ~./Population))
    
    if (length(metric) > 1) {
      metric = paste0(metric, '_normalized') 
    } else {
      metric = 'normalized'
    }
  }
  
  # Plot the timeseries
  g <- ggplot(data = plot_dat,
              mapping = aes_string(x = x, colour = colour)) +
    labs(title = str_to_title(type),
         y = y_title,
         x = 'Date') +
    geom_line(mapping = aes_string(y = metric[1]), lwd = 0.75)
  
  if (normalize_dates & zero_on) g <- g + scale_x_continuous(limits = c(0, max(plot_dat$normalized_date, na.rm = TRUE)))
  
  if (log_transform) g <- g + scale_y_continuous(trans = 'log2')
  
  if (length(metric) > 1) {
    g <- g + geom_line(mapping = aes_string(y = metric[2]), linetype = line_types[metric[2]], lwd = 0.75)
  }
  
  if (length(metric) == 3) {
    g <- g + geom_line(mapping = aes_string(y = metric[3]), linetype = line_types[metric[3]], lwd = 0.875)
  }
  
  if (show_lockdowns) {
    if (break_out_states) by = c('Province.State' = 'Province.State') else by = c('Country.Region' = 'Country.Region')
    
    active_lockdowns <- suppressWarnings(
      lockdowns %>%
        filter(Country.Region %in% countries,
               Province.State %in% state_province) %>%
        left_join(plot_dat,
                  by = append(by, c('Lockdown.Date' = 'Date')),
                  suffix = c('', '.y'))
    )
    
    if (normalize_dates) lockdown_x = 'normalized_date' else lockdown_x = 'Lockdown.Date'
    
    if (nrow(active_lockdowns) > 0) {
      g <- g + geom_point(data = active_lockdowns,
                          mapping = aes_string(x = lockdown_x, y = metric),
                          pch = 13,
                          size = 4) +
        geom_text(data = active_lockdowns,
                  mapping = aes_string(x = lockdown_x, y = metric, label = 'Province.State'),
                  size = 3,
                  hjust = 'right')
    }
  }
  
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
  
  # cat(sprintf('Line: %.2f\n', as.numeric(difftime(Sys.time(), start_time, units = 'secs'))))
  
  ggplotly(g) %>%
    layout(paper_bgcolor = NULL,
           plot_bgcolor = NULL)
}

plot_map <- function(countries, state_province, metric, normalize_pops = FALSE, type = 'Count', total_limit = 100) {
  # start_time <- Sys.time()
  
  if (type == 'Rate') {
    metric = paste0(metric, '_rate')
    y_title = 'New'
    legend.position = 'right'
  } else if (type == 'Acceleration') {
    metric = paste0(metric, '_accel')
    y_title = 'Rate Change'
    legend.position = 'right'
  } else {
    y_title = 'Total'
    legend.position = 'none'
  }
  
  plot_dat <- suppressWarnings(
    map_data %>%
      rename_at(metric, function(vars) 'metric') %>%
      filter(Country.Region %in% countries,
             Province.State %in% state_province,
             metric >= total_limit)
  )
  
  # TODO: normalize by local population
  # if (normalize_pops) {
  #   plot_dat[,metric] <- plot_dat[,metric]/plot_dat$Population
  # }
  
  hovertemplate <- paste0(
    '<b>Country/Province:</b> %{Country.Region}',
    '<br><b>State/Province:</b> %{Province.State}',
    '<br><b>Confirmed:</b> %{Confirmed}'
  )
  
  map <- ggplot() +
    geom_sf(data = world_base,
            fill = '#2b3e50',
            colour = 'white',
            lwd = 0.2) +
    coord_fixed() +
    scale_colour_distiller(palette = 'Spectral') +
    theme(panel.background = element_rect(fill = '#2b3e50'),
          plot.background = element_rect(fill = '#2b3e50'),
          strip.background = element_blank(),
          axis.line.y = element_line(color = 'white'),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text = element_text(color = 'white'),
          axis.title = element_text(color = 'white', face = 'bold'),
          panel.grid.major = element_blank(),
          title = element_text(color = 'white', face = 'bold'),
          legend.position = 'none',
          legend.title = element_blank(),
          legend.text = element_text(color = 'white', face = 'bold'),
          legend.background = element_rect(fill = '#2b3e50'))
  
  if (nrow(plot_dat) > 0) {
    xlim <- st_bbox(plot_dat)[c('xmin', 'xmax')]
    ylim <- st_bbox(plot_dat)[c('ymin', 'ymax')]
    
    map <- map +
      geom_sf(data = plot_dat,
              mapping = aes_string(size = 'metric',
                                   colour = 'metric',
                                   text = 'CombinedLocation'),
              fill = 'none',
              alpha = 0.75) +
      coord_sf(xlim = xlim, ylim = ylim)
  }
  
  # cat(sprintf('Map: %.2f\n', as.numeric(difftime(Sys.time(), start_time, units = 'secs'))))
  
  return(ggplotly(map, tooltip = append('size', 'CombinedLocation')))
}

top_10_table <- function(table_level = 'Countries') {
  # start_time <- Sys.time()
  
  if (table_level == 'Country') {
    plot_dat <- dat %>%
      group_by(Date, Country.Region) %>%
      summarise(Confirmed = sum(Confirmed, na.rm = TRUE),
                Deaths = sum(Deaths, na.rm = TRUE),
                Recovered = sum(Recovered, na.rm = TRUE)) %>%
      group_by(Country.Region) %>%
      mutate(Confirmed_rate = Confirmed - lag(Confirmed, default = 0),
             Deaths_rate = Deaths - lag(Deaths, default = 0),
             Recovered_rate = Recovered - lag(Recovered, default = 0)) %>%
      filter(Date == max(Date)) %>%
      arrange(desc(Date), desc(Confirmed_rate)) %>%
      rename(Last.Updated = Date) %>%
      select(Country.Region, Last.Updated, Confirmed_rate, Deaths_rate, Recovered_rate, Confirmed, Deaths, Recovered)
  } else if (table_level == 'State') {
    plot_dat <- dat %>%
      group_by(Date, Country.Region, Province.State) %>%
      summarise(Confirmed = sum(Confirmed, na.rm = TRUE),
                Deaths = sum(Deaths, na.rm = TRUE),
                Recovered = sum(Recovered, na.rm = TRUE)) %>%
      group_by(Country.Region, Province.State) %>%
      mutate(Confirmed_rate = Confirmed - lag(Confirmed, default = 0),
             Deaths_rate = Deaths - lag(Deaths, default = 0),
             Recovered_rate = Recovered - lag(Recovered, default = 0)) %>%
      filter(Date == max(Date)) %>%
      arrange(desc(Date), desc(Confirmed_rate)) %>%
      rename(Last.Updated = Date) %>%
      select(Country.Region, Province.State, Last.Updated, Confirmed_rate, Deaths_rate, Recovered_rate, Confirmed, Deaths, Recovered)
  } else {
    plot_dat <- dat %>%
      group_by(Date, Country.Region, Province.State, Admin2) %>%
      summarise(Confirmed = sum(Confirmed, na.rm = TRUE),
                Deaths = sum(Deaths, na.rm = TRUE),
                Recovered = sum(Recovered, na.rm = TRUE)) %>%
      group_by(Country.Region, Province.State, Admin2) %>%
      mutate(Confirmed_rate = Confirmed - lag(Confirmed, default = 0),
             Deaths_rate = Deaths - lag(Deaths, default = 0),
             Recovered_rate = Recovered - lag(Recovered, default = 0)) %>%
      filter(Date == max(Date)) %>%
      arrange(desc(Date), desc(Confirmed_rate)) %>%
      rename(Last.Updated = Date) %>%
      select(Country.Region, Province.State, Admin2, Last.Updated, Confirmed_rate, Deaths_rate, Recovered_rate, Confirmed, Deaths, Recovered)
  }
  
  num_cols <- names(sapply(data.frame(plot_dat), FUN = is.numeric)[which(sapply(data.frame(plot_dat), FUN = is.numeric))])
  bad_cols <- num_cols[which(!grepl('Recovered', num_cols))]
  good_cols <- num_cols[which(grepl('Recovered', num_cols))]
  
  # Bad cols
  bad_brks <- quantile(data.frame(plot_dat)[,bad_cols], probs = seq(.05, .95, .05), na.rm = TRUE)
  bad_clrs <- round(seq(255, 40, length.out = length(bad_brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
  
  dt <- datatable(plot_dat,
                  colnames = str_to_title(gsub('[.]', '/', gsub('_', ' ', names(plot_dat)))),
                  options = list(scrollX = TRUE,
                                 scrollY = TRUE)) %>%
    formatStyle(columns = 1:nrow(plot_dat), color = 'black') %>%
    formatStyle(bad_cols, backgroundColor = styleInterval(bad_brks, bad_clrs)) %>%
    formatDate(columns = 'Last.Updated')
  
  # cat(sprintf('Table: %.2f\n', as.numeric(difftime(Sys.time(), start_time, units = 'secs'))))
  
  return(dt)
}
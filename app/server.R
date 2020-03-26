get_summary_dat <- function(countries, state_province, break_out_states, break_out_countries) {
  if (length(countries) == 1 & break_out_states) {
    plot_dat <- suppressWarnings(
      state_prov_grouped %>%
        filter(Country.Region %in% countries,
               Province.State %in% state_province)
    )
  } else {
    plot_dat <- suppressWarnings(
      dat_summ %>%
        filter(Country.Region %in% countries,
               Province.State %in% state_province) %>%
        group_by(load_date, Country.Region) %>%
        summarise(Confirmed = sum(Confirmed, na.rm = TRUE),
                  Deaths = sum(Deaths, na.rm = TRUE),
                  Recovered = sum(Recovered, na.rm = TRUE),
                  AggStatePop = sum(StatePopulation, na.rm = TRUE)) %>%
        group_by(Country.Region) %>%
        mutate(Confirmed_rate = Confirmed - lag(Confirmed, default = 0),
               Deaths_rate = Deaths - lag(Deaths, default = 0),
               Recovered_rate = Recovered - lag(Recovered, default = 0),
               Confirmed_accel = Confirmed_rate - lag(Confirmed_rate, default = 0),
               Deaths_accel = Deaths_rate - lag(Deaths_rate, default = 0),
               Recovered_accel = Recovered_rate - lag(Recovered_rate, default = 0)) %>%
        left_join(dat_summ %>%
                    filter(Country.Region %in% countries,
                           Province.State %in% state_province) %>%
                    group_by(load_date, Country.Region) %>%
                    summarise(Confirmed = sum(Confirmed, na.rm = TRUE)) %>%
                    filter(Confirmed >= 100) %>%
                    group_by(Country.Region) %>%
                    summarise(First100Date = min(load_date, na.rm = TRUE)),
                  by = 'Country.Region') %>%
        left_join(country_populations, by = 'Country.Region') %>%
        mutate(Population = ifelse(AggStatePop == 0, CountryPopulation, AggStatePop)) %>%
        replace_na(list(Population = 1)) %>%
        mutate(normalized_date = as.numeric(difftime(load_date, First100Date, unit = 'days')))
    )
  }
  
  if (!break_out_countries) {
    plot_dat <- plot_dat %>%
      group_by(load_date) %>%
      arrange(load_date) %>%
      summarise(Confirmed = sum(Confirmed, na.rm = TRUE),
                Deaths = sum(Deaths, na.rm = TRUE),
                Recovered = sum(Recovered, na.rm = TRUE),
                Population = sum(Population)) %>%
      mutate(Confirmed_rate = Confirmed - lag(Confirmed, default = 0),
             Deaths_rate = Deaths - lag(Deaths, default = 0),
             Recovered_rate = Recovered - lag(Recovered, default = 0),
             Confirmed_accel = Confirmed_rate - lag(Confirmed_rate, default = 0),
             Deaths_accel = Deaths_rate - lag(Deaths_rate, default = 0),
             Recovered_accel = Recovered_rate - lag(Recovered_rate, default = 0)) %>%
      mutate(Country.Region = 'World') %>%
      left_join(dat_summ %>%
                  filter(Country.Region %in% countries,
                         Province.State %in% state_province) %>%
                  group_by(load_date) %>%
                  summarise(Confirmed = sum(Confirmed, na.rm = TRUE)) %>%
                  filter(Confirmed >= 100) %>%
                  summarise(First100Date = min(load_date, na.rm = TRUE)) %>%
                  mutate(Country.Region = 'World'),
                by = 'Country.Region') %>%
      mutate(normalized_date = as.numeric(difftime(load_date, First100Date, unit = 'days')))
  }
  
  return(plot_dat)
}

plot_line <- function(countries, state_province, metric, break_out_states = FALSE, show_lockdowns = FALSE, normalize_dates = FALSE, normalize_pops = FALSE, break_out_countries = TRUE, type = 'Count') {
  plot_dat <- get_summary_dat(countries, state_province, break_out_states, break_out_countries)
  
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
    x = 'load_date'
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
    geom_line(mapping = aes_string(y = metric[1]), linetype = line_types[metric[1]], lwd = 0.75)
  
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
                  by = append(by, c('Lockdown.Date' = 'load_date')),
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
  
  ggplotly(g) %>%
    layout(paper_bgcolor = NULL,
           plot_bgcolor = NULL)
}

plot_map <- function(countries, state_province, metric, normalize_pops = FALSE, type = 'Count', total_limit = 100) {
  plot_dat <- suppressWarnings(
    map_data %>%
      filter(Country.Region %in% countries,
             Province.State %in% state_province,
             Confirmed + Deaths + Recovered > total_limit)
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
  
  # TODO: normalize by local population
  # if (normalize_pops) {
  #   plot_dat[,metric] <- plot_dat[,metric]/plot_dat$Population
  # }
  
  hovertemplate <- paste0(
    '<b>Country/Province:</b> %{Country.Region}',
    '<br><b>State/Province:</b> %{Province.State}',
    '<br><b>Confirmed:</b> %{Confirmed}'
  )
  
  xlim <- st_bbox(plot_dat)[c('xmin', 'xmax')]
  ylim <- st_bbox(plot_dat)[c('ymin', 'ymax')]
  
  map <- ggplot() +
    geom_sf(data = world_base,
            fill = '#2b3e50',
            colour = 'white',
            lwd = 0.2) +
    geom_sf(data = plot_dat,
            mapping = aes_string(size = metric,
                                 colour = metric,
                                 text = 'CombinedLocation'),
            fill = 'none',
            alpha = 0.75) +
    coord_sf(xlim = xlim, ylim = ylim) +
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
  
  return(ggplotly(map, tooltip = append('size', 'CombinedLocation')))
}

top_10_table <- function() {
  plot_dat <- dat %>%
    group_by(load_date, Country.Region) %>%
    summarise(Confirmed = sum(Confirmed),
              Deaths = sum(Deaths),
              Recovered = sum(Recovered)) %>%
    group_by(Country.Region) %>%
    mutate(Confirmed_rate = Confirmed - lag(Confirmed, default = 0),
           Deaths_rate = Deaths - lag(Deaths, default = 0),
           Recovered_rate = Recovered - lag(Recovered, default = 0)) %>%
    filter(load_date == max(load_date)) %>%
    arrange(desc(load_date), desc(Confirmed_rate)) %>%
    rename(Last.Updated = load_date) %>%
    select(Country.Region, Last.Updated, Confirmed_rate, Deaths_rate, Recovered_rate, Confirmed, Deaths, Recovered)
  
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
  
  return(dt)
}

server <- function(input, output, session) {
  plot_vals <- eventReactive(
    input$update,
    {
      var_list <- list(
        countries = input$countries,
        state_province =input$state_province,
        metric = input$metric,
        break_out_states = input$break_out_states,
        show_lockdowns = input$show_lockdowns,
        normalize_dates = input$normalize_dates,
        normalize_pops = input$normalize_pops,
        break_out_countries = input$break_out_countries
      )
      
      assign('var_list', var_list, envir = .GlobalEnv)
      
      shinyjs::hide('update')
      
      return(var_list)
    },
    ignoreNULL = FALSE
  )
  
  moment <- reactive({
    req(input$moment)
    
    input$moment
  })
  
  map_metric <- reactive({
    req(input$map_metric)
    
    input$map_metric
  })
  
  map_moment <- reactive({
    req(input$map_moment)
    
    input$map_moment
  })
  
  total_limit <- reactive({
    req(input$total_limit)
    
    input$total_limit
  })
  
  # UI observations
  observe({
    countries <- input$countries
    state_province <- input$state_province
    metric <- input$metric
    break_out_states <- input$break_out_states
    show_lockdowns <- input$show_lockdowns
    normalize_dates <- input$normalize_dates
    normalize_pops <- input$normalize_pops
    break_out_countries <- input$break_out_countries
    
    if (exists('var_list')) {
      list_match <- all.equal(list(countries = countries,
                                   state_province = state_province,
                                   metric = metric,
                                   break_out_states = break_out_states,
                                   show_lockdowns = show_lockdowns,
                                   normalize_dates = normalize_dates,
                                   normalize_pops = normalize_pops,
                                   break_out_countries = break_out_countries),
                              var_list)
      
      if (length(list_match) == 1) {
        if (list_match == TRUE) {
          shinyjs::hide('update')
        } else {
          shinyjs::show('update')
        }
      } else {
        shinyjs::show('update')
      }
    }
    
    if (break_out_states) updateCheckboxInput(session = session, inputId = 'break_out_countries', value = TRUE)
  })
  
  observe({
    x <- input$countries
    
    # Can use character(0) to remove all choices
    if (length(x) == 0)
      x <- c()
    
    if (length(x) == 1) {
      shinyjs::enable('break_out_states')
    }
    else {
      updateCheckboxInput(session = session, inputId = 'break_out_states', value = FALSE)
      shinyjs::disable('break_out_states')
    }
    
    state_prov <- sort(unique(as.character((dat %>%
                                              filter(Country.Region %in% input$countries))$Province.State)))
    
    # Can also set the label and select items
    updatePickerInput(session, 'state_province',
                      choices = state_prov,
                      selected = state_prov
    )
  })
  
  output$data_update <- renderUI({
    connection_string <- ifelse(connected,
                                '<font color=#00e600>Connected to data source</font>',
                                '<font color=red>Not connected to data source</font>')
    
    HTML(paste0('Data last updated: ', strftime(max_data_date, format = '%m/%d/%Y'),
                '<br>',connection_string))
  })
  
  output$top10_table <- DT::renderDataTable({
    top_10_table()
  })
  
  output$plot <- renderPlotly({
    withProgress(message = 'Rendering plot...', expr = {
      do.call(plot_line, c(plot_vals(), type = 'count'))
    })
  })
  
  output$plot_rates <- renderPlotly({
    withProgress(message = 'Rendering plot...', expr = {
      do.call(plot_line, c(plot_vals(), type = moment()))
    })
  })
  
  output$plot_map <- renderPlotly({
    map_vals <- plot_vals()[c('countries', 'state_province')]
    
    withProgress(message = 'Rendering map...', expr = {
      do.call(plot_map, c(map_vals,
                          metric = map_metric(),
                          type = map_moment(),
                          total_limit = total_limit()#,
                          # TODO: fix pop normalizing on map
                          # normalize_pops = normalize_pops()
      ))
    })
  })
}

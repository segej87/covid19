source('overview.R')
source('association_explorer.R')

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
        normalize_tests = input$normalize_tests,
        break_out_countries = input$break_out_countries,
        log_transform = input$log_transform,
        zero_on = input$zero_on
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
  
  table_level <- reactive({
    req(input$table_level)
    
    input$table_level
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
    normalize_tests <- input$normalize_tests
    break_out_countries <- input$break_out_countries
    log_transform <- input$log_transform
    zero_on <- input$zero_on
    
    if (exists('var_list')) {
      list_match <- all.equal(list(countries = countries,
                                   state_province = state_province,
                                   metric = metric,
                                   break_out_states = break_out_states,
                                   show_lockdowns = show_lockdowns,
                                   normalize_dates = normalize_dates,
                                   normalize_pops = normalize_pops,
                                   normalize_tests = normalize_tests,
                                   break_out_countries = break_out_countries,
                                   log_transform = log_transform,
                                   zero_on = zero_on),
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
    x <- input$normalize_dates
    
    if (x == TRUE) {
      shinyjs::show(id = 'zero_on')
    } else {
      updateCheckboxInput(session = session, inputId = 'zero_on', value = FALSE)
      shinyjs::hide(id = 'zero_on')
    }
  })
  
  observe({
    x <- input$countries
    
    # Can use character(0) to remove all choices
    if (length(x) == 0)
      x <- c()
    
    if (length(x) == 1) {
      shinyjs::enable('break_out_states')
      
      if (x == c('US') & input$break_out_states) shinyjs::show('normalize_tests') else {
        updateCheckboxInput(session = session, inputId = 'normalize_tests', value = FALSE)
        shinyjs::hide('normalize_tests')
      }
    } else {
      updateCheckboxInput(session = session, inputId = 'break_out_states', value = FALSE)
      shinyjs::disable('break_out_states')
      
      updateCheckboxInput(session = session, inputId = 'normalize_tests', value = FALSE)
      shinyjs::hide('normalize_tests')
    }
    
    state_prov <- state_prov_list[x]
    
    # Can also set the label and select items
    updatePickerInput(session, 'state_province',
                      choices = state_prov,
                      selected = unlist(state_prov)
    )
  })
  
  # Association explorer inputs
  
  chart_type <- reactive({
    req(input$chart_type)
    
    input$chart_type
  })
  
  x_axis <- reactive({
    req(input$x_axis)
    
    input$x_axis
  })
  
  y_axis <- reactive({
    req(input$y_axis)
    
    input$y_axis
  })
  
  colour <- reactive({
    req(input$colour)
    
    input$colour
  })
  
  log_transform_x <- reactive({
    input$log_transform_x
  })
  
  log_transform_y <- reactive({
    input$log_transform_y
  })
  
  agg_level <- reactive({
    req(input$agg_level)
    
    input$agg_level
  })

  observe({
    x <- input$agg_level
    
    if (x == 'Country/Region') {
      var_options = names(country_grouped)
      colour_selected = 'Country.Region'
    } else if (x == 'State/Province') {
      var_options = names(state_prov_grouped)
      colour_selected = 'Province.State'
    } else {
      var_options = names(local_grouped)
      colour_selected = 'Admin2'
    }
    
    updatePickerInput(
      session = session,
      inputId = 'x_axis',
      choices = var_options,
      selected = 'Confirmed'
    )
    
    updatePickerInput(
      session = session,
      inputId = 'y_axis',
      choices = var_options,
      selected = 'Deaths'
    )
    
    updatePickerInput(
      session = session,
      inputId = 'colour',
      choices = var_options,
      selected = colour_selected
    )
  })

  
# Outputs -----------------------------------------------------------------
  
  output$data_update <- renderUI({
    connection_string <- ifelse(connected,
                                '<font color=#00e600>Connected to data source</font>',
                                '<font color=red>Not connected to data source</font>')
    
    HTML(paste0('<font size="3"; face=bold; font-family="lato">Data last updated in source: ', strftime(max_data_date, format = '%m/%d/%Y'),
                '</font><br><font size="3"; face=bold; font-family="lato">Most recent available data: ', strftime(max_results_date, format = '%m/%d/%Y'),
                '</font><br>',connection_string))
  })
  
  output$top10_table <- DT::renderDataTable({
    top_10_table(
      table_level = table_level()
    )
  })
  
  output$stats_summary <- renderUI({
    HTML(do.call(write_summary, c(plot_vals()[1:2])))
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
  
  output$association_plot <- renderPlotly({
    withProgress(message = 'Rendering plot...', expr = {
      plot_associations(
        chart_type = chart_type(),
        x_axis = x_axis(),
        y_axis = y_axis(),
        colour = colour(),
        agg_level = agg_level(),
        log_transform_x = log_transform_x(),
        log_transform_y = log_transform_y()
      )
    })
  })
}

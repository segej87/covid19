library(yaml)

cfg <- yaml::read_yaml('cfg.yml')

packages <- unlist(cfg['packages'])

for (p in packages) library(p, character.only = TRUE, quietly = TRUE, verbose = FALSE)

source('load_data.R')

  ui <- navbarPage(
  useShinyjs(),
  title = 'COVID-19 Summary Report',
  fluid = TRUE,
  theme = shinythemes::shinytheme('superhero'),
  selected = 'Overview',
  
  tabPanel(
    'Overview',
    
    sidebarLayout(
      sidebarPanel(
        pickerInput(
          inputId = 'countries',
          label = 'Countries',
          multiple = TRUE,
          choices = levels(dat$Country.Region),
          selected = levels(dat$Country.Region),
          options = list(
            'live-search' = TRUE,
            'actions-box' = TRUE
          )
        ),
        
        pickerInput(
          inputId = 'state_province',
          label = 'State/Province',
          multiple = TRUE,
          choices = state_prov_list,
          selected = unlist(state_prov_list),
          options = list(
            'live-search' = TRUE,
            'actions-box' = TRUE
          )
        ),
        
        checkboxInput(
          inputId = 'break_out_countries',
          label = 'Break Out Countries/Regions',
          value = FALSE
        ),
        
        disabled(
          checkboxInput(
            inputId = 'break_out_states',
            label = 'Break Out States/Provinces',
            value = FALSE
          )),
        
        checkboxInput(
          inputId = 'normalize_pops',
          label = 'Normalize Population (beta)',
          value = FALSE
        ),
        
        hidden(
          checkboxInput(
            inputId = 'normalize_tests',
            label = 'Normalize by Number of Tests (beta)',
            value = FALSE
          )
        ),
        
        checkboxInput(
          inputId = 'normalize_dates',
          label = 'Normalize Dates',
          value = FALSE
        ),
        
        hidden(
          checkboxInput(
            inputId = 'zero_on',
            label = 'Only Show from Date Zero',
            value = FALSE
          )),
        
        checkboxInput(
          inputId = 'show_lockdowns',
          label = 'Show Lockdowns',
          value = FALSE
        ),
        
        checkboxInput(
          inputId = 'log_transform',
          label = 'Log Transform y Axis',
          value = FALSE
        ),
        
        selectInput(
          inputId = 'metric',
          label = 'metric',
          choices = c('Confirmed', 'Deaths', 'Recovered'),
          selected = 'Confirmed',
          multiple = TRUE,
          selectize = TRUE
        ),
        
        hidden(
          actionButton(
            inputId = 'update',
            label = 'Click to Update Plots',
            style = 'color: #ffa500'
          )),
        
        htmlOutput(
          outputId = 'data_update'
        ),
        
        plotOutput(
          outputId = 'spacer',
          height = '575px'
        )
      ),
      
      mainPanel(
        fluidRow(
          htmlOutput(
            outputId = 'stats_summary',
            height = '50px'
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            6,
            plotlyOutput(
              outputId = 'plot',
              width = '100%',
              height = 400
            )
          ),
          
          column(
            6,
            align = 'right',
            plotlyOutput(
              outputId = 'plot_rates',
              width = '100%',
              height = 400
            ),
            
            pickerInput(
              inputId = 'moment',
              label = NULL,
              choices = c('Rate', 'Acceleration'),
              width = '150px'
            )
          )
        ),
        
        fluidRow(
          tabsetPanel(
            tabPanel(
              title = 'Map',
              
              column(
                width = 2,
                
                pickerInput(
                  inputId = 'map_metric',
                  label = NULL,
                  choices = c('Confirmed', 'Deaths', 'Recovered'),
                  width = '150px'
                ),
                
                pickerInput(
                  inputId = 'map_moment',
                  label = NULL,
                  choices = c('Count', 'Rate', 'Acceleration'),
                  width = '150px'
                ),
                
                numericInput(
                  inputId = 'total_limit',
                  label = 'Min. # Cases to Show',
                  width = '150px',
                  value = 1000
                )
              ),
              
              column(
                width = 10,
                align = 'right',
                
                plotlyOutput(
                  outputId = 'plot_map',
                  width = '100%',
                  height = 500
                )
              )
            ),
            
            tabPanel(
              title = 'Table',
              
              column(
                width = 1,
                
                pickerInput(
                  inputId = 'table_level',
                  label = NULL,
                  choices = c('Country', 'State', 'Local'),
                  width = '100px',
                  
                ),
              ),
              
              column(
                width = 11,
                align = 'right',
                
                wellPanel(
                  style = 'background:white',
                  
                  DT::dataTableOutput(
                    outputId = 'top10_table'
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  
  tabPanel(
    'Association Explorer (coming soon)',
    
    sidebarLayout(
      sidebarPanel(
        pickerInput(
          inputId = 'agg_level',
          label = 'Aggregation Level',
          choices = c('Country/Region', 'State/Province', 'Local'),
          selected = 'Country/Region'
        ),
        
        pickerInput(
          inputId = 'chart_type',
          label = 'Chart Type',
          choices = c('Point', 'Line', 'Bar'),
          selected = 'Point'
        ),
        
        pickerInput(
          inputId = 'x_axis',
          label = 'x Axis',
          choices = names(country_grouped),
          selected = 'Confirmed'
        ),
        
        pickerInput(
          inputId = 'y_axis',
          label = 'y Axis',
          choices = names(country_grouped),
          selected = 'Deaths'
        ),
        
        pickerInput(
          inputId = 'colour',
          label = 'Color',
          choices = names(country_grouped),
          selected = 'Country.Region'
        ),
        
        checkboxInput(
          inputId = 'log_transform_x',
          label = 'Log Transform x Axis',
          value = FALSE
        ),
        
        checkboxInput(
          inputId = 'log_transform_y',
          label = 'Log Transform y Axis',
          value = FALSE
        )
      ),
      
      mainPanel(
        plotlyOutput(
          outputId = 'association_plot',
          width = '100%',
          height = 600
        )
      )
    )
  )
)

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
      
      checkboxInput(
        inputId = 'normalize_dates',
        label = 'Normalize Dates',
        value = FALSE
      ),
      
      checkboxInput(
        inputId = 'show_lockdowns',
        label = 'Show Lockdowns',
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
        height = '400px'
      )
    ),
    
    mainPanel(
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
                value = 100
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
            
            DT::dataTableOutput(
              outputId = 'top10_table'
            )
          )
        )
      )
    )
  )
)

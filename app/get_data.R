library(yaml)

tryCatch({
  setwd('/srv/shiny-server')
}, error = function(e) {
  setwd('~/Documents/Covid19/app')
})


cfg <- yaml::read_yaml('cfg.yml')

packages <- unlist(cfg['packages'])

for (p in packages) library(p, character.only = TRUE, quietly = TRUE, verbose = FALSE)

library(httr)
library(progress)
library(curl)

urlfile='https://api.github.com/repos/CSSEGISandData/COVID-19/git/trees/master?recursive=1'

urltesting <- 'http://covidtracking.com/api/states/daily.csv'

github_token <- function() {
  token <- Sys.getenv('GITHUB_TOKEN')
  
  if (identical(token, '')) {
    warning('No oAuth token found, going unauthenticated\n')
  }
  
  return(token)
}

load_data <- function() {
  req <- tryCatch({
    token <- github_token()
    cat('Authenticated - fetching\n')
    GET(urlfile, add_headers(Authorization = paste('Bearer', token, sep = ' ')))
  }, warning = function(w) {
    cat(w$message)
    GET(urlfile)
  }, error = function(e) {
    cat(e$message)
    'override'
  })
  
  if (length(req) == 1) {
    if (req == 'override') {
      warning('Could not connect to server - loading most recent data')
      
      files <- list.files('data', pattern = '.RData')
      files_as_dates <- as.Date(gsub('.Rdata', '', files), format = '%m-%d-%Y')
      
      assign('max_data_date', max(files_as_dates), envir = .GlobalEnv)
      assign('connected', FALSE, envir = .GlobalEnv)
      
      load(file.path('data', files[which(files_as_dates == max(files_as_dates))]))
      
      return(dat)
    }
  } else if (req$status_code != 200) {
    warning(paste('Loading most recent data - ', fromJSON(rawToChar(req$content))))
    
    files <- list.files('data', pattern = '.RData')
    files_as_dates <- as.Date(gsub('.Rdata', '', files), format = '%m-%d-%Y')
    
    assign('max_data_date', max(files_as_dates), envir = .GlobalEnv)
    assign('connected', FALSE, envir = .GlobalEnv)
    
    load(file.path('data', files[which(files_as_dates == max(files_as_dates))]))
    
    return(dat)
  }
  
  stop_for_status(req)
  filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
  
  # Get the path with daily reports
  get_path_level <- function(x, ind) {
    unlist(strsplit(paths_with_daily, split = '/'))[ind]
  }
  
  paths_with_daily <- grep('daily_reports', filelist, value = TRUE)
  root_dir <- unique(sapply(paths_with_daily, FUN = get_path_level, ind = 1))
  next_level <- unique(sapply(paths_with_daily, FUN = get_path_level, ind = 2))
  
  # Get all data file names
  data_files <- filelist[which(grepl(file.path(root_dir, next_level), filelist) &
                                 !grepl('README|[.]gitignore', filelist) &
                                 grepl('[.]csv', filelist))]
  
  dates <- as.Date(gsub(paste0(file.path(root_dir, next_level), '|[/]|([.]csv)'), '', data_files), format = '%m-%d-%Y')
  max_date <- max(dates)
  
  assign('max_data_date', max_date, envir = .GlobalEnv)
  assign('connected', TRUE, envir = .GlobalEnv)
  
  # Col specification sets
  col_specs <- list(
    Set_1 = cols(
      FIPS = col_integer(),
      Admin2 = col_character(),
      `Province/State` = col_character(),
      `Country/Region` = col_character(),
      `Last Update` = col_datetime(format = ""),
      Confirmed = col_double(),
      Deaths = col_double(),
      Recovered = col_double(),
      Active = col_double(),
      Latitude = col_double(),
      Longitude = col_double(),
      Combined_Key = col_character()
    ),
    Set_2 = cols(
      FIPS = col_integer(),
      Admin2 = col_character(),
      Province_State = col_character(),
      Country_Region = col_character(),
      Last_Update = col_datetime(format = ""),
      Confirmed = col_double(),
      Deaths = col_double(),
      Recovered = col_double(),
      Active = col_double(),
      Lat = col_double(),
      Long_ = col_double(),
      Combined_Key = col_character()
    )
  )
  
  col_names <- names(col_specs$cols)
  
  path <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/'
  
  data_file <- file.path('data', sprintf('%s.RData', strftime(max_date, '%m-%d-%Y')))
  
  if (!file.exists(data_file)) {
    cat('Newer data detected - downloading')
    
    dat <- c()
    pb <- progress_bar$new(total = length(data_files))
    for (d in data_files) {
      csv_data <- tryCatch({
        readr::read_csv(url(file.path(path, d)), col_types = col_specs[[1]], progress = TRUE)
      }, warning = function(w) {
        if (grepl('don\'t match the column names', w)) {
          tryCatch({
            suppressWarnings(readr::read_csv(url(file.path(path, d)), col_types = col_specs[[2]], progress = TRUE))
          })
        }
      }) %>%
        rename_all(gsub, pattern = 'Lat$', replacement = 'Latitude') %>%
        rename_all(gsub, pattern = 'Long[_.]$', replacement = 'Longitude') %>%
        rename_all(gsub, pattern = "[^[:alnum:]]", replacement = '.')
      
      if (length(class(csv_data$Last.Update)) == 1) {
        if (class(csv_data$Last.Update) == 'character') {
          tryCatch({
            csv_data <- csv_data %>% mutate(Last.Update = as.POSIXct(Last.Update, format = '%m/%d/%Y %H:%M'))
          })
        }
      }
      
      date = as.Date(basename(gsub('[.][[:alnum:]]+$', '', d)), '%m-%d-%Y')
      
      dat <- dat %>%
        bind_rows(csv_data %>%
                    mutate(load_date = date,
                           Date = load_date - days(1)))
      
      pb$tick()
    }
    
    dat$Country.Region <- sapply(dat$Country.Region, FUN = function(x) {ifelse(grepl('China', x), 'China', x)})
    
    dat <- dat %>%
      mutate(Province.State = ifelse(is.na(Province.State) | Province.State == 'None',
                                     Country.Region,
                                     Province.State)) %>%
      replace_na(list(Country.Region = 'Unknown Country')) %>%
      mutate_if(is.character, as.factor)
    
    if (!dir.exists('data')) dir.create('data')
    
    cat(paste('Saving new data to',sprintf('%s.RData', strftime(max_date, '%m-%d-%Y'))))
    save(dat, file = file.path('data', sprintf('%s.RData', strftime(max_date, '%m-%d-%Y'))))
  } else {
    cat('No newer data detected\n')
  }
}

load_data()
library(httr)
library(progress)
library(curl)

urlfile='https://api.github.com/repos/CSSEGISandData/COVID-19/git/trees/master?recursive=1'

load_data <- function() {
  req <- tryCatch({
    GET(urlfile)
  }, error = function(e) {
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
  
  # Define column specifications
  col_specs <- cols(
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
  )
  
  col_specs2 <- cols(
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
  
  col_names <- names(col_specs$cols)
  
  path <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/'
  
  data_file <- file.path('data', sprintf('%s.RData', strftime(max_date, '%m-%d-%Y')))
  
  if (file.exists(data_file)) {
    cat('Loading file\n')
    
    load(data_file)
    
    return(dat)
  } else {
    cat('Newer data detected - downloading')
    
    dat <- tibble()
    pb <- progress_bar$new(total = length(data_files))
    for (d in data_files) {
      csv_data <-tryCatch({
        readr::read_csv(url(file.path(path, d)), col_types = col_specs, progress = TRUE)
      }, warning = function(w) {
        tryCatch({
          readr::read_csv(url(file.path(path, d)), col_types = col_specs2, progress = TRUE)
        }, warning = function(w) {
          tmp <- suppressWarnings(readr::read_csv(url(file.path(path, d)),
                                                  col_types = col_specs, progress = TRUE))
          
          rep_names <-setdiff(col_names, names(tmp))
          
          tmp[rep_names] <- NA
          
          tmp
        })
      }, error = function(e) {
        if (grepl('unique', e)) {
          cat(e)
        }
      }) %>%
        rename_all(gsub, pattern = "[^[:alnum:]]", replacement = '.') %>%
        rename_all(gsub, pattern = 'Lat$', replacement = 'Latitude') %>%
        rename_all(gsub, pattern = 'Long_$', replacement = 'Longitude')
      
      date = as.Date(basename(gsub('[.][[:alnum:]]+$', '', d)), '%m-%d-%Y')
      
      dat <- dat %>%
        bind_rows(csv_data %>%
                    mutate(load_date = date))
      
      pb$tick()
    }
    
    dat$Country.Region <- sapply(dat$Country.Region, FUN = function(x) {ifelse(grepl('China', x), 'China', x)})
    
    dat <- dat %>%
      replace_na(list(Province.State = 'None', Country.Region = 'None')) %>%
      mutate_if(is.character, as.factor)
    
    if (!dir.exists('data')) dir.create('data')
    
    save(dat, file = file.path('data', sprintf('%s.RData', strftime(max_date, '%m-%d-%Y'))))
    
    return(dat)
  }
}

assign(
  'dat',
  load_data() %>%
    mutate_if(is.factor, as.character) %>%
    mutate(Province.State = ifelse(Province.State == Country.Region,
                                   'None',
                                   Province.State)) %>%
    mutate_if(is.character, as.factor),
  envir = .GlobalEnv
)

assign(
  'dat_summ',
  dat %>%
    group_by(load_date, Country.Region, Province.State) %>%
    summarise(Confirmed = sum(Confirmed, na.rm = TRUE),
              Deaths = sum(Deaths, na.rm = TRUE),
              Recovered = sum(Recovered, na.rm = TRUE)) %>%
    arrange(Country.Region, Province.State),
  envir = .GlobalEnv
)

assign(
  'world_base',
  st_read('geo/Countries_WGS84.shp',
          quiet = TRUE),
  envir = .GlobalEnv
)

assign(
  'lockdowns',
  read.csv('data/lockdowns.csv') %>%
    mutate(Lockdown.Date = as.Date(Lockdown.Date)),
  envir = .GlobalEnv
)

assign(
  'oecd_pops',
  read.csv('data/edu_dem.csv') %>%
    replace_na(list(Province.State = 'None')) %>%
    mutate(Country.Region = factor(Country.Region),
           Province.State = factor(Province.State)),
  envir = .GlobalEnv
)

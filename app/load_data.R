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
  files <- list.files('data', pattern = '.RData')
  files_as_dates <- as.Date(gsub('.Rdata', '', files), format = '%m-%d-%Y')
  
  assign('max_data_date', max(files_as_dates, na.rm = TRUE), envir = .GlobalEnv)
  assign('connected', TRUE, envir = .GlobalEnv)
  
  load(file.path('data', files[which(files_as_dates == max(files_as_dates, na.rm = TRUE))]))
  
  return(dat)
}

abbrevs <- read.csv('data/state_abbrevs.csv', stringsAsFactors = F)
abbrev_pattern <- paste0(abbrevs$Code, collapse = '|')

testing <- suppressWarnings(
  read.csv(urltesting) %>%
    left_join(abbrevs, by = c('state' = 'Code')) %>%
    mutate(Date = as.Date(as.character(date), format = '%Y%m%d') - days(1)) %>%
    as_tibble()
)

assign(
  'dat',
  load_data() %>%
    mutate_if(is.factor, as.character) %>%
    mutate(Province.State = ifelse(Province.State == 'None',
                                   Country.Region,
                                   Province.State)) %>%
    # This section looks for sub-state entities that should be rolled up to the state level
    mutate(AbbrevMatch = str_extract(Province.State, abbrev_pattern)) %>%
    left_join(abbrevs, by = c('AbbrevMatch' = 'Code'), suffix = c('', '.y')) %>%
    mutate(Location = Province.State,
           Province.State = ifelse(!is.na(Province.State.y), Province.State.y, Location)) %>%
    select(-AbbrevMatch, -Province.State.y, -Abbrev) %>%
    # End of state cleanup section
    left_join(testing, by = c('Province.State' = 'Province.State', 'Date' = 'Date')) %>%
    mutate_if(is.character, as.factor),
  envir = .GlobalEnv
)

assign('max_results_date', max(dat$Date, na.rm = TRUE), envir = .GlobalEnv)

state_prov_options <- dat %>%
  group_by(Country.Region) %>%
  summarise(state_options = paste(sort(unique(Province.State)), collapse = ';;; '))
state_prov_list <- as.list(state_prov_options$state_options)
names(state_prov_list) <- state_prov_options$Country.Region
assign(
  'state_prov_list',
  sapply(state_prov_list, FUN = function(x) strsplit(x, ';;; ')),
  envir = .GlobalEnv
)

assign(
  'populations',
  read.csv('data/populations.csv', stringsAsFactors = FALSE) %>%
    mutate(Province.State = ifelse(is.na(Province.State) | Province.State == 'None',
                                   as.character(Country.Region),
                                   as.character(Province.State))) %>%
    mutate(Country.Region = factor(Country.Region),
           Province.State = factor(Province.State)),
  envir = .GlobalEnv
)

assign(
  'dat_summ',
  suppressWarnings(
    dat %>%
      group_by(Date, Country.Region, Province.State) %>%
      summarise(Confirmed = sum(Confirmed, na.rm = TRUE),
                Deaths = sum(Deaths, na.rm = TRUE),
                Recovered = sum(Recovered, na.rm = TRUE)) %>%
      arrange(Country.Region, Province.State) %>%
      left_join(populations, by = c('Country.Region', 'Province.State')) %>%
      rename(StatePopulation = Population)
    ),
  envir = .GlobalEnv
)

assign(
  'world_base',
  suppressWarnings(
    st_read('geo/ne_50m_admin_0_countries.shp',
            quiet = TRUE) %>% filter(NAME != 'Antarctica') %>%
      st_simplify(TRUE, dTolerance = 0.05) %>%
      st_cast("MULTIPOLYGON")
  ),
  envir = .GlobalEnv
)

assign(
  'lockdowns',
  read.csv('data/lockdowns.csv') %>%
    mutate(Lockdown.Date = as.Date(Lockdown.Date, format = '%m/%d/%Y'),
           State.Province = ifelse(is.na(Province.State) | Province.State == 'None',
                                   as.character(Country.Region),
                                   as.character(Province.State))),
  envir = .GlobalEnv
)

assign(
  'country_populations',
  read.csv('data/country_pops.csv', stringsAsFactors = FALSE) %>%
    mutate(Province.State = ifelse(is.na(Province.State) | Province.State == 'None',
                                   as.character(Country.Region),
                                   as.character(Province.State))) %>%
    mutate(Country.Region = factor(Country.Region),
           Province.State = factor(Province.State)) %>%
    rename(CountryPopulation = Population),
  envir = .GlobalEnv
)

assign(
  'country_gdp',
  read.csv('data/wb_gdp_percap_ppp.csv', stringsAsFactors = FALSE) %>%
    mutate(country = factor(country)),
  envir = .GlobalEnv
)

assign(
  'us_cities_transit',
  read.csv('data/us_cities_transit.csv', stringsAsFactors = FALSE),
  envir = .GlobalEnv
)


# Groupings for plots -----------------------------------------------------

assign(
  'state_prov_grouped',
  suppressWarnings(
    dat_summ %>%
      group_by(Date, Country.Region, Province.State) %>%
      summarise(Confirmed = sum(Confirmed, na.rm = TRUE),
                Deaths = sum(Deaths, na.rm = TRUE),
                Recovered = sum(Recovered, na.rm = TRUE)) %>%
      group_by(Country.Region, Province.State) %>%
      mutate(Confirmed_rate = Confirmed - lag(Confirmed, default = 0),
             Deaths_rate = Deaths - lag(Deaths, default = 0),
             Recovered_rate = Recovered - lag(Recovered, default = 0),
             Confirmed_accel = (2 * Confirmed_rate - lag(Confirmed_rate, default = 0) - lag(Confirmed_rate, n = 2, default = 0))/2,
             Deaths_accel = (2 * Deaths_rate - lag(Deaths_rate, default = 0) - lag(Deaths_rate, n = 2, default = 0))/2,
             Recovered_accel = (2 * Recovered_rate - lag(Recovered_rate, default = 0) - lag(Recovered_rate, n = 2, default = 0))/2) %>%
      left_join(dat_summ %>%
                  group_by(Date, Country.Region, Province.State) %>%
                  summarise(Confirmed = sum(Confirmed, na.rm = TRUE)) %>%
                  filter(Confirmed >= 100) %>%
                  group_by(Country.Region, Province.State) %>%
                  summarise(First100Date = min(Date, na.rm = TRUE)),
                by = c('Country.Region', 'Province.State')) %>%
      left_join(populations, by = c('Country.Region', 'Province.State')) %>%
      left_join(testing, by = c('Province.State' = 'Province.State', 'Date' = 'Date')) %>%
      mutate(normalized_date = as.numeric(difftime(Date, First100Date, unit = 'days')),
             date_lag = difftime(Date, lag(Date), units = 'days'))
  ),
  envir = .GlobalEnv
)

assign(
  'country_grouped',
  suppressWarnings(
    dat_summ %>%
      group_by(Date, Country.Region) %>%
      summarise(Confirmed = sum(Confirmed, na.rm = TRUE),
                Deaths = sum(Deaths, na.rm = TRUE),
                Recovered = sum(Recovered, na.rm = TRUE)) %>%
      group_by(Country.Region) %>%
      mutate(Confirmed_rate = Confirmed - lag(Confirmed, default = 0),
             Deaths_rate = Deaths - lag(Deaths, default = 0),
             Recovered_rate = Recovered - lag(Recovered, default = 0),
             Confirmed_accel = (2 * Confirmed_rate - lag(Confirmed_rate, default = 0) - lag(Confirmed_rate, n = 2, default = 0))/2,
             Deaths_accel = (2 * Deaths_rate - lag(Deaths_rate, default = 0) - lag(Deaths_rate, n = 2, default = 0))/2,
             Recovered_accel = (2 * Recovered_rate - lag(Recovered_rate, default = 0) - lag(Recovered_rate, n = 2, default = 0))/2) %>%
      left_join(dat_summ %>%
                  group_by(Date, Country.Region) %>%
                  summarise(Confirmed = sum(Confirmed, na.rm = TRUE)) %>%
                  filter(Confirmed >= 100) %>%
                  group_by(Country.Region) %>%
                  summarise(First100Date = min(Date, na.rm = TRUE)),
                by = c('Country.Region')) %>%
      left_join(country_populations, by = c('Country.Region')) %>%
      rename(Population = CountryPopulation) %>%
      left_join(country_gdp, by = c('Country.Region' = 'country')) %>%
      mutate(normalized_date = as.numeric(difftime(Date, First100Date, unit = 'days')),
             date_lag = difftime(Date, lag(Date), units = 'days'))
  ),
  envir = .GlobalEnv
)

assign(
  'local_grouped',
  suppressWarnings(
    dat %>%
      group_by(Date, Country.Region, Province.State, Admin2) %>%
      summarise(Confirmed = sum(Confirmed, na.rm = TRUE),
                Deaths = sum(Deaths, na.rm = TRUE),
                Recovered = sum(Recovered, na.rm = TRUE)) %>%
      group_by(Country.Region, Province.State, Admin2) %>%
      mutate(Confirmed_rate = Confirmed - lag(Confirmed, default = 0),
             Deaths_rate = Deaths - lag(Deaths, default = 0),
             Recovered_rate = Recovered - lag(Recovered, default = 0),
             Confirmed_accel = (2 * Confirmed_rate - lag(Confirmed_rate, default = 0) - lag(Confirmed_rate, n = 2, default = 0))/2,
             Deaths_accel = (2 * Deaths_rate - lag(Deaths_rate, default = 0) - lag(Deaths_rate, n = 2, default = 0))/2,
             Recovered_accel = (2 * Recovered_rate - lag(Recovered_rate, default = 0) - lag(Recovered_rate, n = 2, default = 0))/2) %>%
      left_join(dat %>%
                  group_by(Date, Country.Region, Province.State, Admin2) %>%
                  summarise(Confirmed = sum(Confirmed, na.rm = TRUE)) %>%
                  filter(Confirmed >= 100) %>%
                  group_by(Country.Region, Province.State, Admin2) %>%
                  summarise(First100Date = min(Date, na.rm = TRUE)),
                by = c('Country.Region', 'Province.State', 'Admin2')) %>%
      left_join(us_cities_transit %>% select(-Ind),
                by = c('Province.State', 'Admin2'))
  ),
  envir = .GlobalEnv
)

assign(
  'map_data',
  dat %>%
    mutate(ind = apply(dat, MARGIN = 1, FUN = function(x) {paste(x[c('Latitude', 'Longitude')], collapse = '.')})) %>%
    filter(!(Latitude == 0 & Longitude == 0),
           !(is.na(Latitude) | is.na(Longitude))) %>%
    st_as_sf(coords = c('Longitude', 'Latitude'), crs = st_crs(world_base)) %>%
    mutate(CombinedLocation = ifelse(is.na(Admin2), as.character(Location), paste(Admin2, Location, sep = ', '))) %>%
    group_by(Country.Region, CombinedLocation)  %>%
    mutate(Confirmed_rate = Confirmed - lag(Confirmed, default = 0),
           Deaths_rate = Deaths - lag(Deaths, default = 0),
           Recovered_rate = Recovered - lag(Recovered, default = 0),
           Confirmed_accel = Confirmed_rate - lag(Confirmed_rate, default = 0),
           Deaths_accel = Deaths_rate - lag(Deaths_rate, default = 0),
           Recovered_accel = Recovered_rate - lag(Recovered_rate, default = 0)) %>%
    # left_join(populations, by = c('Country.Region', 'Province.State')) %>%
    filter(Date == max(Date, na.rm = TRUE)) %>%
    mutate(Location_name = ifelse(Location == 'None', as.character(Country.Region), as.character(Location))),
  envir = .GlobalEnv
)

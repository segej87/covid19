library(yaml)

cfg <- yaml::read_yaml('cfg.yml')

packages <- append(unlist(cfg['packages']), c('broom', 'ggrepel', 'httr', 'progress', 'curl'))

for (p in packages) library(p, character.only = TRUE, quietly = TRUE, verbose = FALSE)

urlfile='https://api.github.com/repos/CSSEGISandData/COVID-19/git/trees/master?recursive=1'

urltesting <- 'http://covidtracking.com/api/states/daily.csv'

data_date <- 'max'
if (data_date == 'max') data_date <- max(gsub('.RData', '',
                                              list.files('data',
                                                         pattern = '.RData')[which(grepl('^[0-9]{2}-[0-9]{2}-[0-9]{4}',
                                                                                         list.files('data', pattern = '.RData')))]))

t <- tryCatch({
  load(file.path('data', paste0(data_date, '.RData')))
}, error = function(e) {
  'error'
})

if (t == 'error') next

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

model_dat <- country_grouped %>%
  mutate(Confirmed2 = Confirmed^2) %>%
  as.data.table()

model_dat$Country.Region <- replace(model_dat$Country.Region, model_dat$Country.Region == 'Korea, South', 'South Korea')
model_dat$Country.Region <- replace(model_dat$Country.Region, model_dat$Country.Region == 'United Kingdom', 'UK')
model_dat$Country.Region <- replace(model_dat$Country.Region, model_dat$Country.Region == 'Iran (Islamic Republic of)', 'Iran')

models <- model_dat %>%
  group_by(Country.Region) %>%
  do(Country.Fit = lm(formula = Confirmed_rate ~ Confirmed + I(Confirmed^2), data = .))

model.coeffs = tidy(models, Country.Fit)

signif_models <- (model.coeffs %>%
                    group_by(Country.Region) %>%
                    summarise(avgP = median(p.value)) %>%
                    filter(avgP <= 1e-6) %>%
                    select(Country.Region) %>%
                    as.data.frame())$Country.Region

signif_data <- model_dat %>%
  filter(Country.Region %in% signif_models)

country_predictions <- tibble()
for (c in signif_models) {
  subset <- model_dat %>%
    filter(Country.Region == c)
  
  country.mod <- lm(formula = Confirmed_rate ~ Confirmed + I(Confirmed^2), data = subset)
  
  assign(paste0('model.', c), country.mod)
  
  subset$Predicted <- predict(object = country.mod, newdata = subset)
  
  country_predictions <- country_predictions %>%
    bind_rows(subset)
}

get_x_intercepts <- function(coefs) {
  a <- coefs['I(Confirmed^2)']
  b <- coefs['Confirmed']
  c <- coefs['(Intercept)']
  
  x = c((-b + sqrt(b^2 - 4 * a * c))/(2 * a),
        (-b - sqrt(b^2 - 4 * a * c))/(2 * a))
  
  return(x)
}

num_in_km <- function(num) {
  if (num > 1000000) out <- paste(round(num/1000000), 'M') else if (num > 1000) out <- paste(round(num/1000), 'K') else out <- round(num)
  
  return(out)
}

# Country Data ------------------------------------------------------------

countries <- c()
predict_starts <- c()
predict_ends <- c()
prediction_curves <- data.frame()
for (country in unique(country_predictions$Country.Region)) {
  coefs <- coef(get(paste0('model.', country)))
  
  tryCatch({
    intercepts <- get_x_intercepts(coefs)
  }, warning = function(w) {
    cat(c)
    intercepts <- get_x_intercepts(coefs)
  })
  
  countries <- append(countries, country)
  predict_starts <- append(predict_starts, intercepts[1])
  predict_ends <- append(predict_ends, intercepts[2])
  
  prediction_x <- seq(intercepts[1], intercepts[2], length.out = 100)
  prediction_y <- coefs['(Intercept)'] + coefs['Confirmed'] * prediction_x + coefs['I(Confirmed^2)'] * (prediction_x^2)
  
  curve_data <- data.frame(Country.Region = country, Confirmed = prediction_x, Confirmed_rate = prediction_y)
  
  prediction_curves %>%
    bind_rows(curve_data) -> prediction_curves
}

predicted_totals <- data.frame(Country.Region = countries,
                               Predicted.Start = predict_starts,
                               Predicted.End = predict_ends)

# Plot the data
g <- ggplot(data = prediction_curves,
            mapping = aes(x = Confirmed, y = Confirmed_rate, colour = Country.Region, fill = Country.Region)) +
  labs(title = 'Case Trajectories',
       subtitle = sprintf('With Fitted Parabolic Models: %s', data_date),
       x = 'Total Cases',
       y = 'Daily New Case Rate') +
  geom_line() +
  geom_point(data = country_predictions) +
  geom_text_repel(data = predicted_totals, mapping = aes(x = predict_ends,
                                                         label = paste(Country.Region, sapply(predict_ends, FUN = num_in_km))),
                  y = 1000, hjust = -0.1) +
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
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(color = 'white', face = 'bold'),
        legend.background = element_rect(fill = '#2b3e50'))

png(filename = sprintf('plots/case_prediction_curves%s.png', data_date), width = 1200, height = 800, res = 100)
print(g)
dev.off()

save(model.coeffs, country_predictions, prediction_curves, predicted_totals, file = sprintf('data/parabolic_models%s.RData', data_date))

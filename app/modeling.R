library(yaml)

cfg <- yaml::read_yaml('cfg.yml')

packages <- append(unlist(cfg['packages']), 'broom', 'ggrepel')

for (p in packages) library(p, character.only = TRUE, quietly = TRUE, verbose = FALSE)

source('load_data.R')

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

if (!('South Korea' %in% signif_models)) signif_models <- append(signif_models, 'South Korea')

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
       subtitle = 'With Fitted Parabolic Models',
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

png(filename = sprintf('plots/case_prediction_curves%s.png', strftime(Sys.time(), format = '%m_%d_%Y')), width = 1200, height = 800, res = 100)
print(g)
dev.off()

save(model.coeffs, country_predictions, prediction_curves, predicted_totals, file = sprintf('data/parabolic_models%s.RData', strftime(Sys.time(), format = '%m_%d_%Y')))
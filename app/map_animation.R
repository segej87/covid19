library(tidyverse)
library(sf)
library(gganimate)
library(gifski)
library(animation)
library(transformr)

source('load_data.R')

plot_map <- function(metric = 'Confirmed', type = 'Count', total_limit = 100) {
  y_title = 'Total'
  legend.position = 'none'
  
  plot_dat <- suppressWarnings(
    dat %>%
      mutate(ind = apply(dat, MARGIN = 1, FUN = function(x) {paste(x[c('Latitude', 'Longitude')], collapse = '.')})) %>%
      filter(!(Latitude == 0 & Longitude == 0),
             !(is.na(Latitude) | is.na(Longitude))) %>%
      st_as_sf(coords = c('Longitude', 'Latitude'), crs = st_crs(world_base)) %>%
      mutate(CombinedLocation = ifelse(is.na(Admin2), as.character(Location), paste(Admin2, Location, sep = ', '))) %>%
      group_by(Date, Country.Region, CombinedLocation)  %>%
      mutate(Confirmed_rate = Confirmed - lag(Confirmed, default = 0),
             Deaths_rate = Deaths - lag(Deaths, default = 0),
             Recovered_rate = Recovered - lag(Recovered, default = 0),
             Confirmed_accel = Confirmed_rate - lag(Confirmed_rate, default = 0),
             Deaths_accel = Deaths_rate - lag(Deaths_rate, default = 0),
             Recovered_accel = Recovered_rate - lag(Recovered_rate, default = 0)) %>%
      # left_join(populations, by = c('Country.Region', 'Province.State')) %>%
      mutate(Location_name = ifelse(Location == 'None', as.character(Country.Region), as.character(Location)),
             NormalDate = as.numeric(difftime(Date, min(Date, na.rm = TRUE), units = 'days')),
             FactorDate = as.factor(strftime(Date)))
  )
  
  if (!is.factor(plot_dat$FactorDate)) plot_dat$FactorDate <- as.factor(plot_dat$FactorDate)
  
  map <- ggplot() +
    geom_sf(data = world_base,
            fill = '#2b3e50',
            colour = 'white',
            lwd = 0.2) +
    geom_sf(data = plot_dat %>% filter(Date == max(Date)),
            mapping = aes(size = reorder(-Confirmed), colour = Confirmed, fill = Confirmed),
            alpha = 0.75) +
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
          legend.background = element_rect(fill = '#2b3e50')) +
    transition_states(FactorDate) +
    labs(title = '{closest_state}')
  
  options(gganimate.dev_args = list(width = 1800, height = 860))
  
  anim <- animate(plot = map,
                  nframes = length(levels(plot_dat$FactorDate)) * 2,
                  fps = 5,
                  end_pause = 10)
  
  anim_save(filename = 'cases.gif', animation = anim, path = 'plots')
}

plot_map()
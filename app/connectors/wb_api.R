library(WDI)
library(lubridate)

datasets <- WDIsearch()

dataset_names <- datasets[,2]

pop_datasets <- dataset_names[which(grepl('[Ii]nd', dataset_names))]

urb_dat <- WDI(indicator = 'SP.URB.TOTL.IN.ZS', start = 2000, end = year(Sys.Date()))

country_urbs <- urb_dat %>%
  rename(Urban.Pop.Perc.Total = SP.URB.TOTL.IN.ZS) %>%
  group_by(country) %>%
  filter(!is.na(Urban.Pop.Perc.Total)) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  summarise(Urban.Pop.Perc.Total = max(Urban.Pop.Perc.Total, na.rm = TRUE))

pop_dat <- WDI(indicator = 'SP.POP.TOTL', start = 2000, end = year(Sys.Date()))

country_pops <- pop_dat %>%
  rename(Population = SP.POP.TOTL) %>%
  group_by(country) %>%
  filter(!is.na(Population)) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  summarise(Population = max(Population, na.rm = TRUE))

joined <- country_pops %>%
  left_join(country_urbs, by = 'country')

write.csv(joined, 'data/wb_pop_urb.csv')

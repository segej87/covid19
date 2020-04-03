library(WDI)
library(lubridate)
library(DT)
library(dplyr)

datasets <- datatable(WDIsearch())

ppp_dat <- WDI(indicator = 'NY.GDP.PCAP.PP.CD', start = 2000, end = year(Sys.Date()))

country_gdp <- ppp_dat %>%
  rename(GDP.PPP = `NY.GDP.PCAP.PP.CD`) %>%
  group_by(country) %>%
  filter(!is.na(GDP.PPP)) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  group_by(country, year) %>%
  summarise(GDP.PPP = max(GDP.PPP, na.rm = TRUE))

write.csv(country_gdp, '../data/wb_gdp_percap_ppp.csv')

dens_dat <- WDI(indicator = 'EN.POP.DNST', start = 2000, end = year(Sys.Date()))

country_density <- dens_dat %>%
  rename(density = `EN.POP.DNST`) %>%
  group_by(country) %>%
  filter(!is.na(density)) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  group_by(country, year) %>%
  summarise(density = max(density, na.rm = TRUE))

write.csv(country_density, '../data/wb_dens.csv')

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

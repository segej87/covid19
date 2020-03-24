library(RCurl)
library(rjson)
library(yaml)
library(rsdmx)

cfg <- yaml::read_yaml('../cfg.yml')
oecd_dbs <- unlist(cfg['oecd_dbs'])

oecd_dat <- list()
for (o in oecd_dbs) {
  fullURL <- sprintf('http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/%s', o)
  dataset <- readSDMX(fullURL)
  stats <- as.data.frame(dataset)

  assign(o, stats)
}

test <- EDU_DEM %>%
  filter(SEX == 'T',
         AGE == 'T') %>%
  group_by(COUNTRY) %>%
  filter(obsTime == max(obsTime, na.rm = TRUE))
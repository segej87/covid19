library(RCurl)
library(curl)
library(rjson)
library(yaml)
library(xml2)

cfg <- yaml::read_yaml('../cfg.yml')
wb_dbs <- unlist(cfg['wb_dbs'])

for (w in wb_dbs) {
  fullURL <- sprintf('http://api.worldbank.org/v2/indicator/%s?format=json', w)
  dataset <- curl_fetch_memory(fullURL)
}
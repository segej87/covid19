library(httr)
library(progress)
library(curl)

fullURL = "https://www.wolframcloud.com/obj/60a03e8a-f7c0-4453-bf4c-d6ec0f9f753d"

med_data <- readr::read_csv(url(fullURL), progress = TRUE)

library(dplyr)
library(censusapi)

census_key <- '2c907f8ecb6e7348c1e192c0b4a2d02a0733bdaf'
Sys.setenv(CENSUS_KEY = census_key)

apis <- listCensusApis()

pops <- apis %>%
  filter(grepl('[Pp]op', title)) %>%
  arrange(desc(vintage))

vars <- listCensusMetadata(name = 'pep/population', vintage = '2019', type = "variables")
geos <- listCensusMetadata(name = 'pep/population', vintage = '2019', type = "geographies")

pop_dat <- getCensus(
  name = 'pep/population',
  vintage = '2019',
  vars = c('NAME', 'DATE_CODE', 'DATE_DESC', 'GEO_ID', 'POP', 'DENSITY'),
  region = 'STATE:*'
)

state_pops <- pop_dat %>%
  mutate(DATE_CODE = as.numeric(DATE_CODE),
         POP = as.numeric(POP),
           DENSITY = as.numeric(DENSITY)) %>%
  group_by(NAME) %>%
  filter(DATE_CODE == max(DATE_CODE)) %>%
  summarise(POP = mean(POP),
            DENSITY = mean(DENSITY),
            DATE_DESC = max(DATE_DESC)) %>%
  arrange(desc(POP)) %>%
  rename(Province.State = NAME,
         Population = POP,
         Density = DENSITY)

save(state_pops, file = '../data/us_state_pops.csv')

install.packages('yaml')

library(yaml)

cfg <- yaml::read_yaml('cfg.yml')

packages <- unlist(cfg['packages'])

install.packages(packages, repos = 'http://lib.stat.cmu.edu/R/CRAN/', verbose = FALSE)
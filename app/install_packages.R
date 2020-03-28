install.packages('yaml')

library(yaml)

cfg <- yaml::read_yaml('cfg.yml')

install_from_binary <- unlist(cfg['install_from_binary'])

if (install_from_binary) {
  packages <- list.files('packages')
  repos <- NULL
} else {
  packages <- unlist(cfg['packages'])
  repos <- 'http://lib.stat.cmu.edu/R/CRAN/'
}

install.packages(pkgs = packages, repos = repos, verbose = FALSE)

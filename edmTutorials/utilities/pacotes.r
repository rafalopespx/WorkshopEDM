pacotes <- c('knitr', 'zoo', 'xts', 'ggplot2', 'cowplot', 'plyr', 'dplyr',
             'tidyr', 'plotly', 'rEDM', 'reshape2', 'deSolve',
             'ggmap','unmarked', 'rjags', 'R2jags', 'devtools', 'caTools',
             'foreach', 'doParallel', 'fields', 'lme4', 'markdown',
             'rmarkdown', 'MASS', 'pomp', 'stringi', 'iterators', 'shiny',
             'foreign', 'tibble', 'assertthat')

update.packages(ask=F, checkBuilt=T, repos="http://vps.fmvz.usp.br/CRAN/")
install.packages(pacotes, repos="http://vps.fmvz.usp.br/CRAN/")


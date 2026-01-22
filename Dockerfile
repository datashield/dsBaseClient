FROM rocker/r-ver:4.3.0

# 1. System Libraries
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo software-properties-common dirmngr wget curl \
    libxml2-dev libcurl4-openssl-dev libssl-dev libgsl-dev libgit2-dev \
    libharfbuzz-dev libfribidi-dev libmagick++-dev xml-twig-tools \
    docker.io docker-compose-v2 \
    && rm -rf /var/lib/apt/lists/*

# 2. R Packages (Stable dependencies)
RUN R -e "options(repos = c(RSPM = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest', CRAN = 'https://cloud.r-project.org')); \
    install.packages(c('devtools','remotes','covr','fields','meta','metafor','ggplot2','gridExtra','data.table','DSI','DSOpal','DSLite','MolgenisAuth','MolgenisArmadillo','DSMolgenisArmadillo','DescTools','e1071', \
    'rcmdcheck','git2r','RCurl','readr','magrittr','xml2','purrr','dplyr','stringr','tidyr','knitr','kableExtra','rmarkdown','downlit'))"

# 3. dsDangerClient (Pass the version as a build argument)
ARG DANGER_VERSION=master
RUN R -e "remotes::install_github('datashield/dsDangerClient', ref='${DANGER_VERSION}')"

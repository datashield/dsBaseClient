#!/usr/bin/env Rscript

#
# Install dsbaseclient package on R client
#

library(RCurl)
devtools::install_github('dsbaseclient', username='datashield', ref='master')

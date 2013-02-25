#!/usr/bin/env Rscript

#
# Do some datashield using dsbaseclient package on R client
#

library(dsbaseclient)
# https login
o1<-opal.login('dsuser1', 'password', 'https://some.opal.host:8443')
# https login with ssl options
o2<-opal.login('dsuser2', 'password', 'https://some.other.opal.host',opts=list(ssl.verifyhost=0,ssl.verifypeer=0,sslversion=3))
opals<-list(o1,o2)

message("**** assign some opal variable values to R symbols...")
datashield.assign(opals,'PM_BMI_CONTINUOUS','opal-data.HOP:PM_BMI_CONTINUOUS')
datashield.assign(opals,'PM_BMI_CATEGORIAL','opal-data.HOP:PM_BMI_CATEGORIAL')
datashield.assign(opals,'PM_HEIGHT_MEASURE','opal-data.HOP:PM_HEIGHT_MEASURE')
datashield.assign(opals,'PM_WEIGHT_MEASURE','opal-data.HOP:PM_WEIGHT_MEASURE')
datashield.assign(opals,'SEX','opal-data.HOP:GENDER')

message("**** get symbol length:")
datashield.length(opals, as.symbol('PM_BMI_CATEGORIAL'))

message("**** display the symbols:")
datashield.symbols(opals)

message("**** get some summaries:")
datashield.aggregate(opals, 'summary(PM_BMI_CONTINUOUS)')
datashield.aggregate(opals, 'summary(PM_BMI_CATEGORIAL)')

message("**** other ways to get the summary:")
datashield.aggregate(opals, call('summary', as.symbol('PM_BMI_CATEGORIAL')))
datashield.summary(opals, as.symbol('PM_BMI_CATEGORIAL'))

#datashield.glm(opals, PM_BMI_CONTINUOUS ~ SEX + PM_HEIGHT_MEASURE + PM_WEIGHT_MEASURE, quote(binomial))
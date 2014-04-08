#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

context("dsbaseclient::ds.meanByClass")

options(datashield.variables=list("LAB_TSC","LAB_HDL","GENDER","DIS_DIAB","PM_BMI_CATEGORICAL"))
source("setup.R")

#
# Tests
#

context("dsbaseclient::ds.meanByClass() calculate the mean proportion for LAB_HDL across gender categories")
res <- ds.meanByClass(datasources=opals, dataset='D', outvar='LAB_HDL', covar='GENDER')
print(res)
# TODO do more than a smoke test

context("dsbaseclient::ds.meanByClass() calculate the mean proportion for LAB_HDL across gender and bmi categories")
res <- ds.meanByClass(datasources=opals, dataset='D', outvar=c('LAB_HDL','LAB_TSC'), covar=c('GENDER'))
print(res)
# TODO do more than a smoke test

context("dsbaseclient::ds.meanByClass() calculate the mean proportion for LAB_HDL across gender bmi and diabetes status categories")
res <- ds.meanByClass(datasources=opals, dataset='D', outvar=c('LAB_HDL','LAB_TSC'), covar=c('GENDER','PM_BMI_CATEGORICAL','DIS_DIAB'))
print(res)
# TODO do more than a smoke test

context("dsbaseclient::ds.meanByClass() calculate the mean proportion for LAB_HDL across gender categories for each study separately")
res <- ds.meanByClass(datasources=opals, dataset='D', outvar='LAB_HDL', covar='GENDER', type='split')
print(res)
# TODO do more than a smoke test

#
# Tear down
#

source("teardown.R")
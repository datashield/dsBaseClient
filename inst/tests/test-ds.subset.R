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

context("dsbaseclient::ds.subset")

options(datashield.variables=list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER"))
source("setup.R")

#
# Tests
#

context("dsbaseclient::ds.subset() generate a subset of the assigned table (by default the table is named 'D') with the first 50 observations and the two first columns")
ds.subset(datasources=opals, subset='subD', data='D', rows=c(1:50), cols=c(1,2))
# TODO do more than a smoke test

context("dsbaseclient::ds.subset() generate a subset of the assigned table (by default the table is named 'D') with the first 50 observations and the two first columns refered to by their names")
ds.subset(datasources=opals, subset='subD', data='D', rows=c(1:50), cols <- c('DIS_DIAB','PM_BMI_CONTINUOUS'))
# TODO do more than a smoke test

context("dsbaseclient::ds.subset() generate a subset of the table D with bmi values greater than or equal to 25.")
ds.subset(datasources=opals, subset='subD', data='D', logical='PM_BMI_CONTINUOUS>=', threshold=25)
# TODO do more than a smoke test

context("dsbaseclient::ds.subset() get the logarithmic values of the variable 'lab_hdl' and generate a subset with the first 50 observations of that new vector.")
ds.assign(opals, "logHDL", "log(D$LAB_HDL)")
ds.subset(datasources=opals, subset="subLAB_HDL", data="logHDL", rows=c(1:50))
# TODO do more than a smoke test

context("dsbaseclient::ds.subset() get the variable 'PM_BMI_CONTINUOUS' from the dataframe 'D' and generate a subset bmi vector with bmi values greater than or equal to 25")
ds.assign(opals, "BMI", "D$PM_BMI_CONTINUOUS")
ds.subset(datasources=opals, subset='subBMI', data='BMI', logical='>=', threshold=25)
# TODO do more than a smoke test

#
# Tear down
#

source("teardown.R")
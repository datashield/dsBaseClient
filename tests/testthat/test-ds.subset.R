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

context("dsClient::ds.subset")

options(datashield.variables=list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER"))
source("setup.R")

#
# Tests
#

context("dsClient::ds.subset() generate a subset of the assigned table (by default the table is named 'D') with the first 50 observations and the two first columns")
ds.subset(datasources=opals, subset='subD', x='D', rows=c(1:50), cols=c(1,2))
res <- ds.exists('subD')
test_that("subD_exists", {
    expect_true(res$sim1)
    expect_true(res$sim2)
    expect_true(res$sim3)
})

context("dsClient::ds.subset() generate a subset of the assigned table (by default the table is named 'D') with the first 50 observations and the two first columns refered to by their names")
ds.subset(subset='subD2', x='D', rows=c(1:50), cols = c('DIS_DIAB','PM_BMI_CONTINUOUS'))
res <- ds.exists('subD2')
test_that("subD2_exists", {
    expect_true(res$sim1)
    expect_true(res$sim2)
    expect_true(res$sim3)
})

context("dsClient::ds.subset() generate a subset of the table D with bmi values greater than or equal to 25.")
ds.subset(datasources=opals, subset='subD3', x='D', logical='PM_BMI_CONTINUOUS>=', threshold=25)
res <- ds.exists('subD3')
test_that("subD3_exists", {
    expect_true(res$sim1)
    expect_true(res$sim2)
    expect_true(res$sim3)
})

context("dsClient::ds.subset() get the logarithmic values of the variable 'lab_hdl' and generate a subset with the first 50 observations of that new vector.")
ds.assign(toAssign='log(D$LAB_HDL)', newobj='logHDL')
ds.subset(datasources=opals, subset="subLAB_HDL", x="logHDL", rows=c(1:50))
res <- ds.exists('subLAB_HDL')
test_that("subLAB_HDL_exists", {
    expect_true(res$sim1)
    expect_true(res$sim2)
    expect_true(res$sim3)
})

context("dsClient::ds.subset() get the variable 'PM_BMI_CONTINUOUS' from the dataframe 'D' and generate a subset bmi vector with bmi values greater than or equal to 25")
ds.assign(toAssign='D$PM_BMI_CONTINUOUS', newobj='BMI')
ds.subset(datasources=opals, subset='subBMI', x='BMI', logical='>=', threshold=25)
res <- ds.exists('subBMI')
test_that("subBMI_exists", {
    expect_true(res$sim1)
    expect_true(res$sim2)
    expect_true(res$sim3)
})

context("dsClient::ds.subset() test errors")
test_that("subset_erros", {
    expect_error(ds.subset(), "Please provide the name of the object to subset from!", fixed=TRUE)
})

#
# Tear down
#

source("teardown.R")
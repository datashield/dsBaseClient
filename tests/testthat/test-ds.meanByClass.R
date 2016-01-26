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

context("dsBaseClient::ds.meanByClass")

options(datashield.variables=list("LAB_TSC","LAB_HDL","GENDER","DIS_DIAB","PM_BMI_CATEGORICAL"))
source("setup.R")

#
# Tests
#

context("dsBaseClient::ds.meanByClass() calculate the mean proportion for LAB_HDL across gender categories")
res <- ds.meanByClass(datasources=opals, x='D', outvar='LAB_HDL', covar='GENDER')
#print(res)
test_that("LAB_HDL_across_gender", {
    expect_equal(res[[1]], '4768')
    expect_equal(res[[2]], '1.51(0.44)')
    expect_equal(res[[4]], '1.63(0.39)')
})


context("dsBaseClient::ds.meanByClass() calculate the mean proportion for LAB_HDL across gender and bmi categories")
res <- ds.meanByClass(datasources=opals, x='D', outvar=c('LAB_HDL','LAB_TSC'), covar=c('GENDER'))
#print(res)
test_that("LAB_HDL_across_gender_bmi", {
    expect_equal(res[[1]], '4768')
    expect_equal(res[[7]], '4611')
    expect_equal(res[[2]], '1.51(0.44)')
    expect_equal(res[[4]], '5.92(1.11)')
    expect_equal(res[[8]], '5.78(1.03)')
})

context("dsBaseClient::ds.meanByClass() calculate the mean proportion for LAB_HDL across gender bmi and diabetes status categories")
res <- ds.meanByClass(datasources=opals, x='D', outvar=c('LAB_HDL','LAB_TSC'), covar=c('GENDER','PM_BMI_CATEGORICAL','DIS_DIAB'))
#print(res)
test_that("LAB_HDL_across_gender_bmi_diabetes", {
    expect_equal(res[[1]], '1168')
    expect_equal(res[[7]], '12')
    expect_equal(res[[2]], '1.59(0.42)')
    expect_equal(res[[4]], '5.88(1.04)')
    expect_equal(res[[8]], '6.05(0.57)')
    expect_equal(res[[12]], '5.88(1.08)')
})

context("dsBaseClient::ds.meanByClass() calculate the mean proportion for LAB_HDL across gender categories for each study separately")
res <- ds.meanByClass(datasources=opals, x='D', outvar='LAB_HDL', covar='GENDER', type='split')
#print(res)
test_that("LAB_HDL_across_gender_split", {
    expect_equal(res$sim1[[1]], '1092')
    expect_equal(res$sim1[[4]], '1.62(0.39)')
    expect_equal(res$sim2[[2]], '1.5(0.42)')
    expect_equal(res$sim2[[3]], '1503')
    expect_equal(res$sim3[[1]], '2091')
    expect_equal(res$sim3[[4]], '1.65(0.39)')
})
#
# Tear down
#

source("teardown.R")
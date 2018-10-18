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

context("dsClient::ds.dsClient")

options(datashield.variables=list("PM_BMI_CATEGORICAL"))
source("setup.R")

#
# Tests
#

context("dsClient::ds.recodeLevels()")
ds.recodeLevels(opals, x='D$PM_BMI_CATEGORICAL', newCategories=c('normal', 'overweight', 'obesity'), newobj='bmi_new')
levels <- ds.levels(x='bmi_new')
test_that("new levels", {
  expected <- c("normal", "overweight", "obesity")
  expect_equal(levels$sim1, expected)
  expect_equal(levels$sim2, expected)
  expect_equal(levels$sim3, expected)
})

context("dsClient::ds.recodeLevels() no opals or newobj")
ds.recodeLevels(x='D$PM_BMI_CATEGORICAL', newCategories=c('normal', 'overweight', 'obesity'))
levels <- ds.levels(x='PM_BMI_CATEGORICAL_new')
test_that("new levels auto", {
  expected <- c("normal", "overweight", "obesity")
  expect_equal(levels$sim1, expected)
  expect_equal(levels$sim2, expected)
  expect_equal(levels$sim3, expected)
})

context("dsClient::ds.recodeLevels() test errors")
test_that("recodeLevels_erros", {
    expect_error(ds.recodeLevels(), "Please provide a valid numeric of character vector", fixed=TRUE)
    expect_error(ds.levels(), "Please provide the name of the input vector!", fixed=TRUE)
    expect_error(ds.recodeLevels(x='D$PM_BMI_CATEGORICAL'), "Please specify the new categories to recode to", fixed=TRUE)
    expect_error(ds.recodeLevels(x='D$PM_BMI_CATEGORICAL', newCategories=c('normal', 'overweight')), "The number of levels you specified is smaller than the levels of the input vector!", fixed=TRUE)
})
#
# Tear down
#

source("teardown.R")
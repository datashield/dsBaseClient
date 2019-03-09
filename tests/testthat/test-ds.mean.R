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

context("dsClient::ds.mean")

options(datashield.variables=list("LAB_TSC"))
source("setup.R")
#
# Tests
#

context("dsClient::ds.mean(type=combine)")

stat.mean <- ds.mean(x='D$LAB_TSC')
#print(stat.mean)
test_that("mean values [combine]", {
  expect_false(is.na(stat.mean))
  expect_equal(as.numeric(stat.mean), 5.85192485623003, tolerance = .000000000000001)
})

context("dsClient::ds.mean(type=combine) loose")
 ds.assign("D$LAB_TSC", "tsc")
stat.mean <- ds.mean(x='tsc')
#print(stat.mean)
test_that("mean values [combine] loose", {
  expect_false(is.na(stat.mean))
  expect_equal(as.numeric(stat.mean), 5.85192485623003, tolerance = .000000000000001)
})

context("dsClient::ds.mean(type=split)")

stat.mean <- ds.mean(datasources=opals, x='D$LAB_TSC', type='split')
#print(stat.mean)
test_that("mean values [split]", {
  expect_false(is.na(stat.mean$sim1))
  expect_equal(stat.mean$sim1, 5.87211344770338, tolerance = .000000000000001)
  expect_false(is.na(stat.mean$sim2))
  expect_equal(stat.mean$sim2, 5.84526388341867, tolerance = .000000000000001)
  expect_false(is.na(stat.mean$sim3))
  expect_equal(stat.mean$sim3, 5.84630008623168, tolerance = .000000000000001)
})

context("dsClient::ds.mean() test errors")
ds.asCharacter(x='D$LAB_TSC', newobj="not_a_numeric")
test_that("mean_erros", {
    expect_error(ds.mean(), "Please provide the name of the input vector!", fixed=TRUE)
    expect_error(ds.mean(x='D$LAB_TSC', type='datashield'), 'Function argument "type" has to be either "combine" or "split"', fixed=TRUE)
    expect_error(ds.mean(x='not_a_numeric'), "The input object must be an integer or a numeric vector.", fixed=TRUE)
})
#
# Tear down
#

source("teardown.R")
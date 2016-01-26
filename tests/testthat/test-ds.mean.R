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

context("dsBaseClient::ds.mean")

options(datashield.variables=list("LAB_TSC"))
source("setup.R")
#
# Tests
#

context("dsBaseClient::ds.mean(type=combine)")

stat.mean <- ds.mean(datasources=opals, x='D$LAB_TSC')
#print(stat.mean)
test_that("mean values [combine]", {
  expect_false(is.na(stat.mean))
  expect_equal(as.numeric(stat.mean), 5.85192485623003, tolerance = .000000000000001)
})

context("dsBaseClient::ds.mean(type=split)")

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

#
# Tear down
#

source("teardown.R")
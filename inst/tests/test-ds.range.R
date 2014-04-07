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

context("dsbaseclient::ds.range")

options(datashield.variables=list("LAB_TSC"))
source("setup.R")

#
# Tests
#

context("dsbaseclient::ds.range(type=combine)")

range <- ds.range(datasources=opals, xvect=quote(D$LAB_TSC))
#print(range)
test_that("range values [combine]", { 
  expect_equal(range[[1]], 1.01850643658633, tolerance = .1)
  expect_equal(range[[2]], 11.6696202638349, tolerance = .1)
})

context("dsbaseclient::ds.range(type=split)")

ranges <- ds.range(datasources=opals, xvect=quote(D$LAB_TSC), type="split")
#print(ranges)
test_that("range values [split]", { 
  expect_equal(ranges$sim1[[1]], 2.31046076260577, tolerance = .1)
  expect_equal(ranges$sim1[[2]], 10.5389243119606, tolerance = .1)
})

#
# Tear down
#

source("teardown.R")
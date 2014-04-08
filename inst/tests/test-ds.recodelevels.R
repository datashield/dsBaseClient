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

context("dsbaseclient::ds.recodelevels")

options(datashield.variables=list("PM_BMI_CATEGORICAL"))
source("setup.R")

#
# Tests
#

context("dsbaseclient::ds.recodelevels()")
ds.recodelevels(opals, quote(D$PM_BMI_CATEGORICAL), newlabels=c('normal', 'overweight', 'obesity'), 'bmi_new')
levels <- ds.levels(opals, quote(bmi_new))
#print(levels)
test_that("new levels", { 
  expected <- c("0_normal", "1_overweight", "2_obesity")
  expect_equal(levels$sim1, expected)
  expect_equal(levels$sim2, expected)
  expect_equal(levels$sim3, expected)
})

#
# Tear down
#

source("teardown.R")
#-------------------------------------------------------------------------------
# Copyright (c) 2018-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

# context("ds.names::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "DIS_DIAB", "PM_BMI_CONTINUOUS", "LAB_HDL", "GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.names::smk")
test_that("level_names", {
  myobjects <- c('D$LAB_TSC', 'D$LAB_HDL')
  res_list <- ds.list(x=myobjects, newobj='my_newobj')

  res <- ds.names('my_newobj')

  expect_length(res, 3)
  expect_length(res$sim1, 2)
  expect_equal(res$sim1[1], 'LAB_TSC')
  expect_equal(res$sim1[2], 'LAB_HDL')
  expect_length(res$sim3, 2)
  expect_equal(res$sim2[1], 'LAB_TSC')
  expect_equal(res$sim2[2], 'LAB_HDL')
  expect_length(res$sim3, 2)
  expect_equal(res$sim3[1], 'LAB_TSC')
  expect_equal(res$sim3[2], 'LAB_HDL')
})

test_that("names, wrong input class returns a server error", {
    expect_error(ds.names(x="D$LAB_TSC"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)

    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 3)
    expect_match(res.errors$sim1, "The input object is not of class <list>. 'D$LAB_TSC' is type numeric", fixed = TRUE)
    expect_match(res.errors$sim2, "The input object is not of class <list>. 'D$LAB_TSC' is type numeric", fixed = TRUE)
    expect_match(res.errors$sim3, "The input object is not of class <list>. 'D$LAB_TSC' is type numeric", fixed = TRUE)
})

#
# Tear down
#

# context("ds.names::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "my_newobj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.names::smk::done")

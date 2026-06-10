#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

# context("ds.list::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_HDL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.list::smk")
test_that("Is List", {
  myobjects <- c('D$LAB_TSC', 'D$LAB_HDL')
  res <- ds.list(x=myobjects, newobj='my_newobj')

  expect_null(res)

  res_ls <- ds.ls()

  expect_length(res_ls, 3)
  expect_length(res_ls$sim1, 2)
  expect_length(res_ls$sim1$objects.found, 2)
  expect_equal(res_ls$sim1$objects.found[1], 'D')
  expect_equal(res_ls$sim1$objects.found[2], 'my_newobj')
  expect_length(res_ls$sim3, 2)
  expect_length(res_ls$sim2$objects.found, 2)
  expect_equal(res_ls$sim2$objects.found[1], 'D')
  expect_equal(res_ls$sim2$objects.found[2], 'my_newobj')
  expect_length(res_ls$sim3, 2)
  expect_length(res_ls$sim3$objects.found, 2)
  expect_equal(res_ls$sim3$objects.found[1], 'D')
  expect_equal(res_ls$sim3$objects.found[2], 'my_newobj')

  res_class <- ds.class('my_newobj')

  expect_length(res_class, 3)
  expect_equal(res_class$sim1, 'list')
  expect_equal(res_class$sim2, 'list')
  expect_equal(res_class$sim3, 'list')

  res_names <- ds.names('my_newobj')

  expect_length(res_names, 3)
  expect_length(res_names$sim1, 2)
  expect_equal(res_names$sim1[1], 'LAB_TSC')
  expect_equal(res_names$sim1[2], 'LAB_HDL')
  expect_length(res_names$sim3, 2)
  expect_equal(res_names$sim2[1], 'LAB_TSC')
  expect_equal(res_names$sim2[2], 'LAB_HDL')
  expect_length(res_names$sim3, 2)
  expect_equal(res_names$sim3[1], 'LAB_TSC')
  expect_equal(res_names$sim3[2], 'LAB_HDL')
})

#
# Tear down
#

# context("ds.list::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "my_newobj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.list::smk::down")

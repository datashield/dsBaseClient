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

# context("ds.colnames::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.colnames::smk")
test_that("simple colnames", {
    myvectors <- c("D$LAB_TSC", "D$LAB_TRIG")
    ds.dataFrame(x=myvectors, newobj="new_df")

    res <- ds.colnames("new_df")

    expect_length(res, 3)
    expect_length(res$sim1, 2)
    expect_equal(res$sim1[1], "LAB_TSC")
    expect_equal(res$sim1[2], "LAB_TRIG")
    expect_length(res$sim2, 2)
    expect_equal(res$sim2[1], "LAB_TSC")
    expect_equal(res$sim2[2], "LAB_TRIG")
    expect_length(res$sim3, 2)
    expect_equal(res$sim3[1], "LAB_TSC")
    expect_equal(res$sim3[2], "LAB_TRIG")
})

test_that("fails if the object does not exist", {
  expect_error(
    ds.colnames("non_existing_df"),
    regexp = "There are some DataSHIELD errors, list them with datashield.error()",
    ignore.case = TRUE
  )
})

###########################################
###     Remote checks not performed     ###
###########################################
# test_that("fails if object is not a data frame or matrix", {
#   expect_error(
#     ds.colnames("D$LAB_TSC"),
#     regexp = "must be of type data.frame or matrix",
#     ignore.case = TRUE
#   )
# })

#
# Done
#

# context("ds.colnames::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "new_df"))
})

disconnect.studies.dataset.cnsim()

# context("ds.colnames::smk::done")

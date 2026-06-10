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

# context("ds.testObjExists::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_HDL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.testObjExists::smk")
test_that("simple testObjExists", {
    res <- ds.testObjExists('D')

    expect_length(res, 1)
    expect_equal(res$return.message, "A valid copy of data object <D> exists in all specified data sources")
})

test_that("data.frame testObjExists", {
    myvectors <- c('D$LAB_TSC', 'D$LAB_HDL')
    ds.dataFrame(x=myvectors)

    res <- ds.testObjExists("dataframe.newobj")

    expect_length(res, 1)
    expect_equal(res$return.message, "A valid copy of data object <dataframe.newobj> exists in all specified data sources")
})

test_that("data.frame testObjExists", {
    res <- ds.testObjExists("D$TEST")

    expect_length(res, 2)
    expect_length(res$return.info, 3)
    expect_length(res$return.info$sim1, 2)
    expect_false(res$return.info$sim1$test.obj.exists)
    expect_true(is.null(res$return.info$sim1$test.obj.class))
    expect_length(res$return.info$sim2, 2)
    expect_false(res$return.info$sim2$test.obj.exists)
    expect_true(is.null(res$return.info$sim2$test.obj.class))
    expect_length(res$return.info$sim3, 2)
    expect_false(res$return.info$sim3$test.obj.exists)
    expect_true(is.null(res$return.info$sim3$test.obj.class))
    expect_length(res$return.info, 3)
    expect_length(res$return.info, 3)
    expect_length(res$return.message, 2)
    expect_length(res$return.message[[1]], 1)
    expect_equal(res$return.message[[1]], "Error: A valid data object <D$TEST> does NOT exist in ALL specified data sources")
    expect_length(res$return.message[[2]], 1)
    expect_equal(res$return.message[[2]], "It is either ABSENT and/or has no valid content/class, for details see return.info above")
})

test_that("data.frame testObjExists", {
    res <- ds.testObjExists("TEST")

    expect_length(res, 2)
    expect_length(res$return.info, 3)
    expect_length(res$return.info$sim1, 2)
    expect_false(res$return.info$sim1$test.obj.exists)
    expect_true(is.null(res$return.info$sim1$test.obj.class))
    expect_length(res$return.info$sim2, 2)
    expect_false(res$return.info$sim2$test.obj.exists)
    expect_true(is.null(res$return.info$sim2$test.obj.class))
    expect_length(res$return.info$sim3, 2)
    expect_false(res$return.info$sim3$test.obj.exists)
    expect_true(is.null(res$return.info$sim3$test.obj.class))
    expect_length(res$return.info, 3)
    expect_length(res$return.info, 3)
    expect_length(res$return.message, 2)
    expect_length(res$return.message[[1]], 1)
    expect_equal(res$return.message[[1]], "Error: A valid data object <TEST> does NOT exist in ALL specified data sources")
    expect_length(res$return.message[[2]], 1)
    expect_equal(res$return.message[[2]], "It is either ABSENT and/or has no valid content/class, for details see return.info above")
})

#
# Done
#

# context("ds.testObjExists::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "dataframe.newobj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.testObjExists::smk::done")

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

# context("ds.exists::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.exists::smk")
test_that("simple exists", {
    res <- ds.exists("no_exists_obj")

    expect_length(res, 3)
    expect_length(res$sim1, 1)
    expect_equal(res$sim1, FALSE)
    expect_length(res$sim2, 1)
    expect_equal(res$sim2, FALSE)
    expect_length(res$sim3, 1)
    expect_equal(res$sim3, FALSE)
})


# ds.exists(...) can't be used to check within environment
# context("ds.exists::smk")
test_that("simple exists, with dollar", {
    res <- ds.exists('D$LAB_TSC')

    expect_length(res, 3)
    expect_length(res$sim1, 1)
    expect_equal(res$sim1, FALSE)
    expect_length(res$sim2, 1)
    expect_equal(res$sim2, FALSE)
    expect_length(res$sim3, 1)
    expect_equal(res$sim3, FALSE)
})

# context("ds.exists::smk")
test_that("simple exists", {
    ds.assign('D$LAB_TSC', 'exists_obj')

    res <- ds.exists("exists_obj")

    expect_length(res, 3)
    expect_length(res$sim1, 1)
    expect_equal(res$sim1, TRUE)
    expect_length(res$sim2, 1)
    expect_equal(res$sim2, TRUE)
    expect_length(res$sim3, 1)
    expect_equal(res$sim3, TRUE)
})

#
# Done
#

# context("ds.exists::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "exists_obj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.exists::smk::done")

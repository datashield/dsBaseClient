#-------------------------------------------------------------------------------
# Copyright (c) 2018-2022 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.cov::smk::setup")

connect.studies.dataset.survival(list("survtime", "time.id", "female"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.cov::smk")
test_that("simple test, split", {
    res <- ds.cov(x="D$survtime", y="D$time.id", type="split")

    expect_length(res, 3)
    expect_length(res[[1]], 5)
    expect_true("matrix" %in% class(res[[1]]$`Number of missing values in each variable`))
    expect_true("matrix" %in% class(res[[1]]$`Number of missing values pairwise`))
    expect_true("matrix" %in% class(res[[1]]$`Variance-Covariance Matrix`))
    expect_true("matrix" %in% class(res[[1]]$`Number of complete cases used`))
    expect_equal(res[[1]]$`Error message`, NA)
    expect_length(res[[2]], 5)
    expect_true("matrix" %in% class(res[[2]]$`Number of missing values in each variable`))
    expect_true("matrix" %in% class(res[[2]]$`Number of missing values pairwise`))
    expect_true("matrix" %in% class(res[[2]]$`Variance-Covariance Matrix`))
    expect_true("matrix" %in% class(res[[2]]$`Number of complete cases used`))
    expect_equal(res[[2]]$`Error message`, NA)
    expect_length(res[[3]], 5)
    expect_true("matrix" %in% class(res[[3]]$`Number of missing values in each variable`))
    expect_true("matrix" %in% class(res[[3]]$`Number of missing values pairwise`))
    expect_true("matrix" %in% class(res[[3]]$`Variance-Covariance Matrix`))
    expect_true("matrix" %in% class(res[[3]]$`Number of complete cases used`))
    expect_equal(res[[3]]$`Error message`, NA)
})

test_that("simple test, combine", {
    res <- ds.cov(x="D$survtime", y="D$time.id", type="combine")

    expect_length(res, 5)
    expect_true("matrix" %in% class(res$`Number of missing values in each variable`))
    expect_true("matrix" %in% class(res$`Number of missing values pairwise`))
    expect_true("matrix" %in% class(res$`Variance-Covariance Matrix`))
    expect_true("matrix" %in% class(res$`Number of complete cases used`))
    expect_length(res$`Error message`, 3)
    expect_equal(res$`Error message`[[1]], NA)
    expect_equal(res$`Error message`[[2]], NA)
    expect_equal(res$`Error message`[[3]], NA)
})

#
# Done
#

context("ds.cov::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.survival()

context("ds.cov::smk::done")

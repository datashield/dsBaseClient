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

# context("ds.cor::smk::setup")

connect.studies.dataset.survival(list("survtime", "time.id", "female"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.cor::smk")

#test_that("simple test, combine, pairwise.complete", {
#    res <- ds.cor(x="D$survtime", y="D$time.id", type="combine", naAction="pairwise.complete")
#
#    expect_length(res, 4)
#    expect_true("matrix" %in% class(res$`Number of missing values in each variable`))
#    expect_true("matrix" %in% class(res$`Number of missing values pairwise`))
#    expect_true("matrix" %in% class(res$`Correlation Matrix`))
#    expect_true("matrix" %in% class(res$`Number of complete cases used`))
#})

test_that("simple test, combine, casewise.complete", {
#    res <- ds.cor(x="D$survtime", y="D$time.id", type="combine", naAction="casewise.complete")
    res <- ds.cor(x="D$survtime", y="D$time.id", type="combine")

    expect_length(res, 4)
    expect_true("matrix" %in% class(res$`Number of missing values in each variable`))
    expect_true("matrix" %in% class(res$`Number of missing values casewise`))
    expect_true("matrix" %in% class(res$`Correlation Matrix`))
    expect_true("matrix" %in% class(res$`Number of complete cases used`))
})

#test_that("simple test, split, pairwise.complete", {
#    res <- ds.cor(x="D$survtime", y="D$time.id", type="split", naAction="pairwise.complete")
#
#    expect_length(res, 3)
#    expect_length(res[[1]], 4)
#    expect_true("matrix" %in% class(res[[1]]$`Number of missing values in each variable`))
#    expect_true("matrix" %in% class(res[[1]]$`Number of missing values pairwise`))
#    expect_true("matrix" %in% class(res[[1]]$`Correlation Matrix`))
#    expect_true("matrix" %in% class(res[[1]]$`Number of complete cases used`))
#    expect_length(res[[2]], 4)
#    expect_true("matrix" %in% class(res[[2]]$`Number of missing values in each variable`))
#    expect_true("matrix" %in% class(res[[2]]$`Number of missing values pairwise`))
#    expect_true("matrix" %in% class(res[[2]]$`Correlation Matrix`))
#    expect_true("matrix" %in% class(res[[2]]$`Number of complete cases used`))
#    expect_length(res[[3]], 4)
#    expect_true("matrix" %in% class(res[[3]]$`Number of missing values in each variable`))
#    expect_true("matrix" %in% class(res[[3]]$`Number of missing values pairwise`))
#    expect_true("matrix" %in% class(res[[3]]$`Correlation Matrix`))
#    expect_true("matrix" %in% class(res[[3]]$`Number of complete cases used`))
#})

test_that("simple test, split, casewise.complete", {
#    res <- ds.cor(x="D$survtime", y="D$time.id", type="split", naAction="casewise.complete")
    res <- ds.cor(x="D$survtime", y="D$time.id", type="split")

    expect_length(res, 3)
    expect_length(res[[1]], 4)
    expect_true("matrix" %in% class(res[[1]]$`Number of missing values in each variable`))
    expect_true("matrix" %in% class(res[[1]]$`Number of missing values casewise`))
    expect_true("matrix" %in% class(res[[1]]$`Correlation Matrix`))
    expect_true("matrix" %in% class(res[[1]]$`Number of complete cases used`))
    expect_length(res[[2]], 4)
    expect_true("matrix" %in% class(res[[2]]$`Number of missing values in each variable`))
    expect_true("matrix" %in% class(res[[2]]$`Number of missing values casewise`))
    expect_true("matrix" %in% class(res[[2]]$`Correlation Matrix`))
    expect_true("matrix" %in% class(res[[2]]$`Number of complete cases used`))
    expect_length(res[[3]], 4)
    expect_true("matrix" %in% class(res[[3]]$`Number of missing values in each variable`))
    expect_true("matrix" %in% class(res[[3]]$`Number of missing values casewise`))
    expect_true("matrix" %in% class(res[[3]]$`Correlation Matrix`))
    expect_true("matrix" %in% class(res[[3]]$`Number of complete cases used`))
})

#
# Done
#

# context("ds.cor::smk::setup::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.survival()

# context("ds.cor::smk::setup::done")

#-------------------------------------------------------------------------------
# Copyright (c) 2018 University of Newcastle upon Tyne. All rights reserved.
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

connect.studies.dataset.survival(list("survtime", "time.id", "female"))

#
# Tests
#

context("ds.cor::smk")

test_that("simple test, combine, pairwise.complete", {
    res <- ds.cor(x="D$survtime", y="D$time.id", type="combine", naAction="pairwise.complete")

    expect_length(res, 5)
    expect_equal(class(res$`Number of missing values in each variable`), "matrix")
    expect_equal(class(res$`Number of missing values pairwise`), "matrix")
    expect_equal(class(res$`Correlation Matrix`), "matrix")
    expect_equal(class(res$`Number of complete cases used`), "matrix")
    expect_length(res$`Error message`, 3)
    expect_equal(res$`Error message`[[1]], NA)
    expect_equal(res$`Error message`[[2]], NA)
    expect_equal(res$`Error message`[[3]], NA)
})

test_that("simple test, combine, casewise.complete", {
    res <- ds.cor(x="D$survtime", y="D$time.id", type="combine", naAction="casewise.complete")

    expect_length(res, 5)
    expect_equal(class(res$`Number of missing values in each variable`), "matrix")
    expect_equal(class(res$`Number of missing values casewise`), "matrix")
    expect_equal(class(res$`Correlation Matrix`), "matrix")
    expect_equal(class(res$`Number of complete cases used`), "matrix")
    expect_length(res$`Error message`, 3)
    expect_equal(res$`Error message`[[1]], NA)
    expect_equal(res$`Error message`[[2]], NA)
    expect_equal(res$`Error message`[[3]], NA)
})

test_that("simple test, split, pairwise.complete", {
    res <- ds.cor(x="D$survtime", y="D$time.id", type="split", naAction="pairwise.complete")

    expect_length(res, 3)
    expect_length(res[[1]], 5)
    expect_equal(class(res[[1]]$`Number of missing values in each variable`), "matrix")
    expect_equal(class(res[[1]]$`Number of missing values pairwise`), "matrix")
    expect_equal(class(res[[1]]$`Correlation Matrix`), "matrix")
    expect_equal(class(res[[1]]$`Number of complete cases used`), "matrix")
    expect_equal(res[[1]]$`Error message`, NA)
    expect_length(res[[2]], 5)
    expect_equal(class(res[[2]]$`Number of missing values in each variable`), "matrix")
    expect_equal(class(res[[2]]$`Number of missing values pairwise`), "matrix")
    expect_equal(class(res[[2]]$`Correlation Matrix`), "matrix")
    expect_equal(class(res[[2]]$`Number of complete cases used`), "matrix")
    expect_equal(res[[2]]$`Error message`, NA)
    expect_length(res[[3]], 5)
    expect_equal(class(res[[3]]$`Number of missing values in each variable`), "matrix")
    expect_equal(class(res[[3]]$`Number of missing values pairwise`), "matrix")
    expect_equal(class(res[[3]]$`Correlation Matrix`), "matrix")
    expect_equal(class(res[[3]]$`Number of complete cases used`), "matrix")
    expect_equal(res[[3]]$`Error message`, NA)
})

test_that("simple test, split, casewise.complete", {
    res <- ds.cor(x="D$survtime", y="D$time.id", type="split", naAction="casewise.complete")

    expect_length(res, 3)
    expect_length(res[[1]], 5)
    expect_equal(class(res[[1]]$`Number of missing values in each variable`), "matrix")
    expect_equal(class(res[[1]]$`Number of missing values casewise`), "matrix")
    expect_equal(class(res[[1]]$`Correlation Matrix`), "matrix")
    expect_equal(class(res[[1]]$`Number of complete cases used`), "matrix")
    expect_equal(res[[1]]$`Error message`, NA)
    expect_length(res[[2]], 5)
    expect_equal(class(res[[2]]$`Number of missing values in each variable`), "matrix")
    expect_equal(class(res[[2]]$`Number of missing values casewise`), "matrix")
    expect_equal(class(res[[2]]$`Correlation Matrix`), "matrix")
    expect_equal(class(res[[2]]$`Number of complete cases used`), "matrix")
    expect_equal(res[[2]]$`Error message`, NA)
    expect_length(res[[3]], 5)
    expect_equal(class(res[[3]]$`Number of missing values in each variable`), "matrix")
    expect_equal(class(res[[3]]$`Number of missing values casewise`), "matrix")
    expect_equal(class(res[[3]]$`Correlation Matrix`), "matrix")
    expect_equal(class(res[[3]]$`Number of complete cases used`), "matrix")
    expect_equal(res[[3]]$`Error message`, NA)
})

#
# Done
#

disconnect.studies.dataset.survival()

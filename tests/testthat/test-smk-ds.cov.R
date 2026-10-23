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

# context("ds.cov::smk::setup")

connect.studies.dataset.survival(list("survtime", "time.id", "female"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.cov::smk")
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

test_that("fails if the object does not exist", {
    expect_error(
        ds.cov(x = "nonexistent_object"),
        "There are some DataSHIELD errors, list them with datashield.errors()",
        fixed = TRUE
    )

    res.errors <- DSI::datashield.errors()
    for (study.errors in res.errors) {
        expect_match(study.errors, "The server-side object 'nonexistent_object' does not exist")
    }
})

test_that("fails if the object is the wrong type", {
    ds.asCharacter(x = "D$survtime", newobj = "not_a_numeric")
    expect_error(
        ds.cov(x = "not_a_numeric", y = "D$survtime"),
        "There are some DataSHIELD errors, list them with datashield.errors()",
        fixed = TRUE
    )

    res.errors <- DSI::datashield.errors()
    for (study.errors in res.errors) {
        expect_match(study.errors, "The server-side object must be of type numeric, integer, factor, matrix or data.frame. 'not_a_numeric' is type character.", fixed = TRUE)
    }

    ds.rm("not_a_numeric")
})

#
# Done
#

test_that("ds.cov ignores y with a warning when x is a data.frame", {
    res.x <- ds.cov(x = "D", type = "split")

    expect_warning(
        res.xy <- ds.cov(x = "D", y = "D$survtime", type = "split"),
        "y will be ignored", fixed = TRUE
    )

    expect_equal(res.xy, res.x)
})

# context("ds.cov::smk::shutdown")

test_that("error, x numeric vector requires y", {
    expect_error(ds.cov(x="D$survtime"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed=TRUE)
    res.errors <- DSI::datashield.errors()
    expect_match(res.errors[[1]], "If x is a numeric vector, y must also be a numeric vector")
})

test_that("cov accepts a factor variable", {
    ds.asFactorSimple(input.var.name = "D$female", newobj.name = "female.factor")

    expect_no_error(ds.cov(x = "female.factor", y = "D$survtime", type = "split"))
})

test_that("shutdown", {
    ds_expect_variables(c("D", "female.factor"))
})

disconnect.studies.dataset.survival()

# context("ds.cov::smk::done")

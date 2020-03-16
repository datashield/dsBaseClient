#-------------------------------------------------------------------------------
# Copyright (c) 2018-2020 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.glm::smk::poisson::setup")

connect.studies.dataset.survival(list("survtime", "time.id", "female", "age.60"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.glm::smk::poisson")
test_that("glm_gaussian", {
    res <- ds.glm("D$survtime~1+D$time.id+D$female",family="poisson")

    expect_length(res, 13)
    expect_equal(res$Nvalid, 6299)
    expect_equal(res$Nmissing, 89)
    expect_equal(res$Ntotal, 6388)
    expect_length(res$disclosure.risk, 3)
    expect_equal(res$disclosure[1], 0)
    expect_equal(res$disclosure[3], 0)
    expect_equal(res$disclosure[2], 0)
    expect_length(res$errorMessage, 3)
    expect_equal(res$errorMessage[1], "No errors")
    expect_equal(res$errorMessage[2], "No errors")
    expect_equal(res$errorMessage[3], "No errors")
    expect_equal(res$nsubs, 6299)
    expect_equal(res$iter, 5)
    expect_equal(class(res$family), "family")
    expect_equal(res$formula, "D$survtime ~ 1 + D$time.id + D$female")
    expect_equal(class(res$coefficients), "matrix")
    expect_equal(res$dev, 3522.598, tolerance=0.00001)
    expect_equal(res$df, 6296)
    expect_equal(res$output.information, "SEE TOP OF OUTPUT FOR INFORMATION ON MISSING DATA AND ERROR MESSAGES")
})

context("ds.glm::smk::poisson, with check")
test_that("glm_gaussian, which check", {
    expect_warning(res <- ds.glm("D$survtime~1+D$time.id+D$female", family="poisson", check=TRUE), "NAs introduced by coercion")

    expect_length(res, 13)
    expect_equal(res$Nvalid, 6299)
    expect_equal(res$Nmissing, 89)
    expect_equal(res$Ntotal, 6388)
    expect_length(res$disclosure.risk, 3)
    expect_equal(res$disclosure[1], 0)
    expect_equal(res$disclosure[3], 0)
    expect_equal(res$disclosure[2], 0)
    expect_length(res$errorMessage, 3)
    expect_equal(res$errorMessage[1], "No errors")
    expect_equal(res$errorMessage[2], "No errors")
    expect_equal(res$errorMessage[3], "No errors")
    expect_equal(res$nsubs, 6299)
    expect_equal(res$iter, 5)
    expect_equal(class(res$family), "family")
    expect_equal(res$formula, "D$survtime ~ 1 + D$time.id + D$female")
    expect_equal(class(res$coefficients), "matrix")
    expect_equal(res$dev, 3522.598, tolerance=0.00001)
    expect_equal(res$df, 6296)
    expect_equal(res$output.information, "SEE TOP OF OUTPUT FOR INFORMATION ON MISSING DATA AND ERROR MESSAGES")
})

#
# Done
#

context("ds.glm::smk::poisson::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "female", "survtime", "time.id"))
})

disconnect.studies.dataset.survival()

context("ds.glm::smk::poisson::done")

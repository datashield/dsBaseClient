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

context("ds.glm::smk::binomial::setup")

connect.studies.dataset.cnsim(list("DIS_DIAB", "GENDER", "PM_BMI_CONTINUOUS", "LAB_HDL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.glm::smk::binomial")
test_that("glm_binomial", {
    res <- ds.glm('D$DIS_DIAB~D$GENDER*D$PM_BMI_CONTINUOUS+D$LAB_HDL', family="binomial")

    expect_length(res, 13)
    expect_equal(res$Nvalid, 7485)
    expect_equal(res$Nmissing, 1894)
    expect_equal(res$Ntotal, 9379)
    expect_length(res$disclosure.risk, 3)
    expect_equal(res$disclosure[1], 0)
    expect_equal(res$disclosure[3], 0)
    expect_equal(res$disclosure[2], 0)
    expect_length(res$errorMessage, 3)
    expect_equal(res$errorMessage[1], "No errors")
    expect_equal(res$errorMessage[2], "No errors")
    expect_equal(res$errorMessage[3], "No errors")
    expect_equal(res$nsubs, 7485)
    expect_equal(res$iter, 9)
    expect_equal(class(res$family), "family")
    expect_equal(res$formula, "D$DIS_DIAB ~ D$GENDER * D$PM_BMI_CONTINUOUS + D$LAB_HDL")
    expect_equal(class(res$coefficients), "matrix")
    expect_equal(res$dev, 1036.031, tolerance=0.00001)
    expect_equal(res$df, 7480)
    expect_equal(res$output.information, "SEE TOP OF OUTPUT FOR INFORMATION ON MISSING DATA AND ERROR MESSAGES")
})

context("ds.glm::smk::binomial, with check")
test_that("glm_binomial, with check", {
    expect_warning(res <- ds.glm('D$DIS_DIAB~D$GENDER*D$PM_BMI_CONTINUOUS+D$LAB_HDL', family="binomial", check=TRUE), "NAs introduced by coercion")

    expect_length(res, 13)
    expect_equal(res$Nvalid, 7485)
    expect_equal(res$Nmissing, 1894)
    expect_equal(res$Ntotal, 9379)
    expect_length(res$disclosure.risk, 3)
    expect_equal(res$disclosure[1], 0)
    expect_equal(res$disclosure[3], 0)
    expect_equal(res$disclosure[2], 0)
    expect_length(res$errorMessage, 3)
    expect_equal(res$errorMessage[1], "No errors")
    expect_equal(res$errorMessage[2], "No errors")
    expect_equal(res$errorMessage[3], "No errors")
    expect_equal(res$nsubs, 7485)
    expect_equal(res$iter, 9)
    expect_equal(class(res$family), "family")
    expect_equal(res$formula, "D$DIS_DIAB ~ D$GENDER * D$PM_BMI_CONTINUOUS + D$LAB_HDL")
    expect_equal(class(res$coefficients), "matrix")
    expect_equal(res$dev, 1036.031, tolerance=0.00001)
    expect_equal(res$df, 7480)
    expect_equal(res$output.information, "SEE TOP OF OUTPUT FOR INFORMATION ON MISSING DATA AND ERROR MESSAGES")
})

#
# Done
#

context("ds.glm::smk::binomial::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "DIS_DIAB", "GENDER", "LAB_HDL", "PM_BMI_CONTINUOUS"))
})

disconnect.studies.dataset.cnsim()

context("ds.glm::smk::binomial::done")

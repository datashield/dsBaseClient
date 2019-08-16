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

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

#
# Tests
#

context("ds.glm::smk::gaussian")
test_that("glm_gaussian", {
    res <- ds.glm('D$LAB_TSC~D$LAB_TRIG',family="gaussian")

    expect_length(res, 13)
    expect_equal(res$Nvalid, 7800)
    expect_equal(res$Nmissing, 1579)
    expect_equal(res$Ntotal, 9379)
    expect_length(res$disclosure.risk, 3)
    expect_equal(res$disclosure[1], 0)
    expect_equal(res$disclosure[3], 0)
    expect_equal(res$disclosure[2], 0)
    expect_length(res$errorMessage, 3)
    expect_equal(res$errorMessage[1], "No errors")
    expect_equal(res$errorMessage[2], "No errors")
    expect_equal(res$errorMessage[3], "No errors")
    expect_equal(res$nsubs, 7800)
    expect_equal(res$iter, 3)
    expect_equal(class(res$family), "family")
    expect_equal(res$formula, "D$LAB_TSC ~ D$LAB_TRIG")
    expect_equal(class(res$coefficients), "matrix")
    expect_equal(res$dev, 8936.32, tolerance=0.00001)
    expect_equal(res$df, 7798)
    expect_equal(res$output.information, "SEE TOP OF OUTPUT FOR INFORMATION ON MISSING DATA AND ERROR MESSAGES")
})

#
# Done
#

disconnect.studies.dataset.cnsim()

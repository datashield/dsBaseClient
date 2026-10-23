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

# context("ds.asLogical::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.asLogical::smk::simple test")
test_that("simple test", {
    expect_no_error(ds.asLogical("D$LAB_TSC"))

    res.class <- ds.class("aslogical.newobj")
    expect_equal(res.class$sim1, "logical")
    expect_equal(res.class$sim2, "logical")
    expect_equal(res.class$sim3, "logical")
})

test_that("fails if the object does not exist", {
    expect_error(
        ds.asLogical(x.name = "nonexistent_object"),
        "There are some DataSHIELD errors, list them with datashield.errors()",
        fixed = TRUE
    )

    res.errors <- DSI::datashield.errors()
    for (study.errors in res.errors) {
        expect_match(study.errors, "The server-side object 'nonexistent_object' does not exist")
    }
})

test_that("fails if the object is the wrong type", {
    expect_error(
        ds.asLogical(x.name = "D"),
        "There are some DataSHIELD errors, list them with datashield.errors()",
        fixed = TRUE
    )

    res.errors <- DSI::datashield.errors()
    for (study.errors in res.errors) {
        expect_match(study.errors, "The server-side object must be of type numeric, integer, character or matrix. 'D' is type data.frame.", fixed = TRUE)
    }
})

#
# Done
#

# context("ds.asLogical::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "aslogical.newobj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.asLogical::smk::done")

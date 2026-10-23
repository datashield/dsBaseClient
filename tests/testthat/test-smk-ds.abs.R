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

# context("ds.abs::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.abs::smk")
test_that("simple c", {
    expect_no_error(ds.abs("D$LAB_TSC", newobj = "abs.newobj"))

    res.length <- ds.length("abs.newobj")

    expect_length(res.length, 4)
    expect_equal(res.length$`length of abs.newobj in sim1`, 2163)
    expect_equal(res.length$`length of abs.newobj in sim2`, 3088)
    expect_equal(res.length$`length of abs.newobj in sim3`, 4128)
    expect_equal(res.length$`total length of abs.newobj in all studies combined`, 9379)

    res.class <- ds.class("abs.newobj")

    expect_length(res.class, 3)
    expect_equal(res.class$sim1, "numeric")
    expect_equal(res.class$sim2, "numeric")
    expect_equal(res.class$sim3, "numeric")
})

test_that("fails if the object does not exist", {
    expect_error(
        ds.abs(x = "nonexistent_object"),
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
        ds.abs(x = "D"),
        "There are some DataSHIELD errors, list them with datashield.errors()",
        fixed = TRUE
    )

    res.errors <- DSI::datashield.errors()
    for (study.errors in res.errors) {
        expect_match(study.errors, "The server-side object must be of type numeric or integer. 'D' is type data.frame.", fixed = TRUE)
    }
})

#
# Done
#

# context("ds.abs::smk::shutdown")

test_that("setup", {
    ds_expect_variables(c("D", "abs.newobj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.abs::smk::done")

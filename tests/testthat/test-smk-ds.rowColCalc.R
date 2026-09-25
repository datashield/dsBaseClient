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

# context("ds.rowColCalc::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_HDL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.rowColCalc::smk")
ds.rowColCalc(x='D', operation="rowSums", newobj="rsum_hdl_tsc")
res <- ds.exists('rsum_hdl_tsc')
test_that("rowColCalc_exists", {
    expect_length(res, 3)
    expect_true(res$sim1)
    expect_true(res$sim2)
    expect_true(res$sim3)
})

# context("ds.rowColCalc::smk::no newobj")
ds.rowColCalc(x='D', operation="rowSums")
res <- ds.exists('rowcolcalc.newobj')
test_that("rowColCalc_out_exists", {
    expect_length(res, 3)
    expect_true(res$sim1)
    expect_true(res$sim2)
    expect_true(res$sim3)
})


#
# Tear down
#
test_that("fails if the object does not exist", {
    expect_error(
        ds.rowColCalc(x = "nonexistent_object", operation = "rowSums"),
        "There are some DataSHIELD errors, list them with datashield.errors()",
        fixed = TRUE
    )

    res.errors <- DSI::datashield.errors()
    for (study.errors in res.errors) {
        expect_match(study.errors, "The server-side object 'nonexistent_object' does not exist")
    }
})

test_that("fails if the object is not a data.frame or matrix", {
    expect_error(
        ds.rowColCalc(x = "D$LAB_TSC", operation = "rowSums"),
        "There are some DataSHIELD errors, list them with datashield.errors()",
        fixed = TRUE
    )

    res.errors <- DSI::datashield.errors()
    for (study.errors in res.errors) {
        expect_match(study.errors, "The server-side object must be of type data.frame or matrix")
    }
})

test_that("fails if a column is not numeric", {
    ds.asCharacter(x.name = "D$LAB_TSC", newobj = "tsc_chr")
    ds.dataFrame(x = c("D$LAB_HDL", "tsc_chr"), newobj = "mixed_df")

    expect_error(
        ds.rowColCalc(x = "mixed_df", operation = "rowSums"),
        "There are some DataSHIELD errors, list them with datashield.errors()",
        fixed = TRUE
    )

    res.errors <- DSI::datashield.errors()
    for (study.errors in res.errors) {
        expect_match(study.errors, "are not of numeric type")
    }
})

# context("ds.rowColCalc::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "rsum_hdl_tsc", "rowcolcalc.newobj", "tsc_chr", "mixed_df"))
})

disconnect.studies.dataset.cnsim()

# context("ds.rowColCalc::smk::done")

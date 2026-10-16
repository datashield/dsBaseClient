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

# context("ds.exp::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.exp::smk")
test_that("simple exp", {
    expect_no_error(ds.exp("D$LAB_TSC", newobj="exp1_obj"))

    res1_class <- ds.class("exp1_obj")

    expect_length(res1_class, 3)
    expect_length(res1_class$sim1, 1)
    expect_equal(res1_class$sim1, "numeric")
    expect_length(res1_class$sim2, 1)
    expect_equal(res1_class$sim2, "numeric")
    expect_length(res1_class$sim3, 1)
    expect_equal(res1_class$sim3, "numeric")

    res_as <- ds.asInteger("D$LAB_TSC", newobj="new_data")

    expect_no_error(ds.exp("new_data", newobj="exp2_obj"))

    res2_class <- ds.class("exp2_obj")

    expect_length(res2_class, 3)
    expect_length(res2_class$sim1, 1)
    expect_equal(res2_class$sim1, "numeric")
    expect_length(res2_class$sim2, 1)
    expect_equal(res2_class$sim2, "numeric")
    expect_length(res2_class$sim3, 1)
    expect_equal(res2_class$sim3, "numeric")
})

test_that("fails if the object does not exist", {
    expect_error(
        ds.exp(x = "nonexistent_object"),
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
        ds.exp(x = "D"),
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

# context("ds.exp::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "exp1_obj", "new_data", "exp2_obj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.exp::smk::done")

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
    print(DSI::datashield.errors())

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

#
# Done
#

# context("ds.exp::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "exp1_obj", "new_data", "exp2_obj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.exp::smk::done")

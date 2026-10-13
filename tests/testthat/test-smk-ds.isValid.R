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

# context("ds.isValid::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.isValid::smk")
test_that("isValid", {
    res1 <- ds.isValid(x='D$LAB_TSC')

    expect_length(res1, 3)
    expect_length(res1$sim1, 1)
    expect_true(res1$sim1)
    expect_length(res1$sim2, 1)
    expect_true(res1$sim2)
    expect_length(res1$sim3, 1)
    expect_true(res1$sim3)

    res2 <- ds.isValid(x='D')

    expect_length(res2, 3)
    expect_true(res2$sim1)
    expect_true(res2$sim2)
    expect_true(res2$sim3)

#    myvectors <- c("D$LAB_TSC", "D$LAB_TRIG")
#    ds.dataFrame(x=myvectors, newobj="unsubset_df")
#    ds.dataFrameSubset(df.name="unsubset_df", V1.name="D$LAB_TSC", V2.name="D$LAB_TRIG", Boolean.operator=">", newobj="subset_df")

#    res2 <- ds.isValid(x="subset_df")

#    expect_length(res2, 3)
#    expect_length(res2$sim1, 1)
#    expect_false(res2$sim1)
#    expect_length(res2$sim2, 1)
#    expect_false(res2$sim2)
#    expect_length(res2$sim3, 1)
#    expect_false(res2$sim3)
})

test_that("fails if the object does not exist", {
    expect_error(
        ds.isValid(x='nonexistent_object'),
        "There are some DataSHIELD errors, list them with datashield.errors()",
        fixed = TRUE
    )

    res.errors <- DSI::datashield.errors()
    for (study.errors in res.errors) {
        expect_match(study.errors, "The server-side object 'nonexistent_object' does not exist")
    }
})

test_that("fails if the object is of a type that cannot be checked", {
    ds.asList(x.name='D', newobj='D_list')

    expect_error(
        ds.isValid(x='D_list'),
        "There are some DataSHIELD errors, list them with datashield.errors()",
        fixed = TRUE
    )

    res.errors <- DSI::datashield.errors()
    for (study.errors in res.errors) {
        expect_match(study.errors, "The server-side object must be of type")
    }
})

#
# Tear down
#

# context("ds.isValid::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "D_list"))
})

disconnect.studies.dataset.cnsim()

# context("ds.isValid::smk::done")

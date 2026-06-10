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

# context("ds.merge::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG", "LAB_HDL", "DIS_CVA", "DIS_AMI"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.merge::smk")
test_that("simple test", {
    spec_vectors_1 <- c('D$LAB_TSC', 'D$LAB_HDL')
    spec_vectors_2 <- c('D$LAB_TSC', 'D$DIS_AMI')
    ds.dataFrame(x=spec_vectors_1, newobj="test_1_df")
    ds.dataFrame(x=spec_vectors_2, newobj="test_2_df")

    res <- ds.merge(x.name="test_1_df", y.name="test_2_df", by.x.names="LAB_TSC", by.y.names="LAB_TSC", newobj="merge_newobj")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <merge_newobj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<merge_newobj> appears valid in all sources")

    class.res <- ds.class("merge_newobj")

    expect_length(class.res, 3)
    expect_equal(class.res$sim1, "data.frame")
    expect_equal(class.res$sim2, "data.frame")
    expect_equal(class.res$sim3, "data.frame")
})

#
# Done
#

# context("ds.merge::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "test_1_df", "test_2_df", "merge_newobj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.merge::smk::done")

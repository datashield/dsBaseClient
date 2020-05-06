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

context("ds.dataFrame::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_HDL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.dataFrame::smk::create a dataframe")
test_that("dataframe_exists", {
    vectors <- c('D$LAB_TSC', 'D$LAB_HDL')
    ds.dataFrame(x=vectors)
    res <- ds.ls()

    expect_length(res, 3)
    expect_length(res$sim1$objects.found, 2)
    expect_equal(res$sim1$objects.found[2], "dataframe.newobj")
    expect_length(res$sim2$objects.found, 2)
    expect_equal(res$sim2$objects.found[2], "dataframe.newobj")
    expect_length(res$sim3$objects.found, 2)
    expect_equal(res$sim3$objects.found[2], "dataframe.newobj")
})

context("ds.dataFrame::smk::create a dataframe, with DataSHIELD.checks")
test_that("dataframe_exists, with DataSHIELD.checks", {
    vectors <- c('D$LAB_TSC', 'D$LAB_HDL')
    ds.dataFrame(x=vectors, DataSHIELD.checks=TRUE)
    res <- ds.ls()

    expect_length(res, 3)
    expect_length(res$sim1$objects.found, 2)
    expect_equal(res$sim1$objects.found[2], "dataframe.newobj")
    expect_length(res$sim2$objects.found, 2)
    expect_equal(res$sim2$objects.found[2], "dataframe.newobj")
    expect_length(res$sim3$objects.found, 2)
    expect_equal(res$sim3$objects.found[2], "dataframe.newobj")
})

#
# Done
#

context("ds.dataFrame::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "dataframe.newobj"))
})

disconnect.studies.dataset.cnsim()

context("ds.dataFrame::smk::done")

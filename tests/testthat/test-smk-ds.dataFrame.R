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

# context("ds.dataFrame::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_HDL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.dataFrame::smk::create a dataframe, from root variables")
test_that("dataframe_exists", {
    ds.assign('D$LAB_TSC', 'LAB_TSC')
    ds.assign('D$LAB_HDL', 'LAB_HDL')
    vectors <- c('LAB_TSC', 'LAB_HDL')

    res <- ds.dataFrame(x=vectors)

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <dataframe.newobj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<dataframe.newobj> appears valid in all sources")

    ds.rm("LAB_TSC")
    ds.rm("LAB_HDL")

    res.ls <- ds.ls()
    expect_length(res.ls, 3)
    expect_length(res.ls$sim1, 2)
    expect_length(res.ls$sim1$objects.found, 2)
    expect_equal(res.ls$sim1$objects.found[2], "dataframe.newobj")
    expect_length(res.ls$sim2, 2)
    expect_length(res.ls$sim2$objects.found, 2)
    expect_length(res.ls$sim2$objects.found, 2)
    expect_equal(res.ls$sim2$objects.found[2], "dataframe.newobj")
    expect_length(res.ls$sim3, 2)
    expect_length(res.ls$sim3$objects.found, 2)
    expect_length(res.ls$sim3$objects.found, 2)
    expect_equal(res.ls$sim3$objects.found[2], "dataframe.newobj")

    res.colnames <- ds.colnames("dataframe.newobj")
    expect_length(res.colnames, 3)
    expect_length(res.colnames$sim1, 2)
    expect_equal(res.colnames$sim1[1], "LAB_TSC")
    expect_equal(res.colnames$sim1[2], "LAB_HDL")
    expect_length(res.colnames$sim2, 2)
    expect_equal(res.colnames$sim2[1], "LAB_TSC")
    expect_equal(res.colnames$sim2[2], "LAB_HDL")
    expect_length(res.colnames$sim3, 2)
    expect_equal(res.colnames$sim3[1], "LAB_TSC")
    expect_equal(res.colnames$sim3[2], "LAB_HDL")

    ds.rm("dataframe.newobj")
})

# context("ds.dataFrame::smk::create a dataframe, with DataSHIELD.checks, from root variables")
test_that("dataframe_exists, with DataSHIELD.checks", {
    ds.assign('D$LAB_TSC', 'LAB_TSC')
    ds.assign('D$LAB_HDL', 'LAB_HDL')
    vectors <- c('LAB_TSC', 'LAB_HDL')

    res <- ds.dataFrame(x=vectors, DataSHIELD.checks=TRUE, newobj="dataframe1")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <dataframe1> has been created in all specified data sources")
    expect_equal(res$validity.check, "<dataframe1> appears valid in all sources")

    ds.rm("LAB_TSC")
    ds.rm("LAB_HDL")

    res.ls <- ds.ls()
    expect_length(res.ls, 3)
    expect_length(res.ls$sim1$objects.found, 2)
    expect_equal(res.ls$sim1$objects.found[2], "dataframe1")
    expect_length(res.ls$sim2$objects.found, 2)
    expect_equal(res.ls$sim2$objects.found[2], "dataframe1")
    expect_length(res.ls$sim3$objects.found, 2)
    expect_equal(res.ls$sim3$objects.found[2], "dataframe1")

    res.colnames <- ds.colnames("dataframe1")
    expect_length(res.colnames, 3)
    expect_length(res.colnames$sim1, 2)
    expect_equal(res.colnames$sim1[1], "LAB_TSC")
    expect_equal(res.colnames$sim1[2], "LAB_HDL")
    expect_length(res.colnames$sim2, 2)
    expect_equal(res.colnames$sim2[1], "LAB_TSC")
    expect_equal(res.colnames$sim2[2], "LAB_HDL")
    expect_length(res.colnames$sim3, 2)
    expect_equal(res.colnames$sim3[1], "LAB_TSC")
    expect_equal(res.colnames$sim3[2], "LAB_HDL")

    ds.rm("dataframe1")
})

# context("ds.dataFrame::smk::create a dataframe, from dataframe variables")
test_that("dataframe_exists, from dataframe variables", {
    vectors <- c('D$LAB_TSC', 'D$LAB_HDL')
    res <- ds.dataFrame(x=vectors)

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <dataframe.newobj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<dataframe.newobj> appears valid in all sources")

    res.ls <- ds.ls()
    expect_length(res.ls, 3)
    expect_length(res.ls$sim1, 2)
    expect_length(res.ls$sim1$objects.found, 2)
    expect_equal(res.ls$sim1$objects.found[2], "dataframe.newobj")
    expect_length(res.ls$sim2, 2)
    expect_length(res.ls$sim2$objects.found, 2)
    expect_length(res.ls$sim2$objects.found, 2)
    expect_equal(res.ls$sim2$objects.found[2], "dataframe.newobj")
    expect_length(res.ls$sim3, 2)
    expect_length(res.ls$sim3$objects.found, 2)
    expect_length(res.ls$sim3$objects.found, 2)
    expect_equal(res.ls$sim3$objects.found[2], "dataframe.newobj")

    res.colnames <- ds.colnames("dataframe.newobj")
    expect_length(res.colnames, 3)
    expect_length(res.colnames$sim1, 2)
    expect_equal(res.colnames$sim1[1], "LAB_TSC")
    expect_equal(res.colnames$sim1[2], "LAB_HDL")
    expect_length(res.colnames$sim2, 2)
    expect_equal(res.colnames$sim2[1], "LAB_TSC")
    expect_equal(res.colnames$sim2[2], "LAB_HDL")
    expect_length(res.colnames$sim3, 2)
    expect_equal(res.colnames$sim3[1], "LAB_TSC")
    expect_equal(res.colnames$sim3[2], "LAB_HDL")

    ds.rm("dataframe.newobj")
})

# context("ds.dataFrame::smk::create a dataframe, with DataSHIELD.checks, from dataframe variables")
test_that("dataframe_exists, with DataSHIELD.checks, from dataframe variables", {
    vectors <- c('D$LAB_TSC', 'D$LAB_HDL')
    res <- ds.dataFrame(x=vectors, DataSHIELD.checks=TRUE, newobj="dataframe1")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <dataframe1> has been created in all specified data sources")
    expect_equal(res$validity.check, "<dataframe1> appears valid in all sources")

    res.ls <- ds.ls()
    expect_length(res.ls, 3)
    expect_length(res.ls$sim1$objects.found, 2)
    expect_equal(res.ls$sim1$objects.found[2], "dataframe1")
    expect_length(res.ls$sim2$objects.found, 2)
    expect_equal(res.ls$sim2$objects.found[2], "dataframe1")
    expect_length(res.ls$sim3$objects.found, 2)
    expect_equal(res.ls$sim3$objects.found[2], "dataframe1")

    res.colnames <- ds.colnames("dataframe1")
    expect_length(res.colnames, 3)
    expect_length(res.colnames$sim1, 2)
    expect_equal(res.colnames$sim1[1], "LAB_TSC")
    expect_equal(res.colnames$sim1[2], "LAB_HDL")
    expect_length(res.colnames$sim2, 2)
    expect_equal(res.colnames$sim2[1], "LAB_TSC")
    expect_equal(res.colnames$sim2[2], "LAB_HDL")
    expect_length(res.colnames$sim3, 2)
    expect_equal(res.colnames$sim3[1], "LAB_TSC")
    expect_equal(res.colnames$sim3[2], "LAB_HDL")

    ds.rm("dataframe1")
})

#
# Done
#

# context("ds.dataFrame::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.dataFrame::smk::done")

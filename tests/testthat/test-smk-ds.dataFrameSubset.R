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

# context("ds.dataFrameSubset::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_HDL", "PM_BMI_CATEGORICAL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.dataFrameSubset::smk::create a subset dataframe")
test_that("dataFrameSubset_exists", {
    myvectors <- c('D$LAB_TSC', 'D$LAB_HDL')
    ds.dataFrame(x=myvectors, newobj="unsubset_df")

    res.dim1 <- ds.dim('unsubset_df')
    expect_length(res.dim1, 4)
    expect_length(res.dim1$`dimensions of unsubset_df in sim1`, 2)
    expect_equal(res.dim1$`dimensions of unsubset_df in sim1`[1], 2163)
    expect_equal(res.dim1$`dimensions of unsubset_df in sim1`[2], 2)
    expect_length(res.dim1$`dimensions of unsubset_df in sim2`, 2)
    expect_equal(res.dim1$`dimensions of unsubset_df in sim2`[1], 3088)
    expect_equal(res.dim1$`dimensions of unsubset_df in sim2`[2], 2)
    expect_length(res.dim1$`dimensions of unsubset_df in sim3`, 2)
    expect_equal(res.dim1$`dimensions of unsubset_df in sim3`[1], 4128)
    expect_equal(res.dim1$`dimensions of unsubset_df in sim3`[2], 2)

    res <- ds.dataFrameSubset(df.name="unsubset_df", V1.name="D$LAB_TSC", V2.name="D$LAB_HDL", Boolean.operator="!=", newobj="subset_df")

    res.dim2 <- ds.dim('subset_df')
    expect_length(res.dim2, 4)
    expect_length(res.dim2$`dimensions of subset_df in sim1`, 2)
    expect_equal(res.dim2$`dimensions of subset_df in sim1`[1], 1803)
    expect_equal(res.dim2$`dimensions of subset_df in sim1`[2], 2)
    expect_length(res.dim2$`dimensions of subset_df in sim2`, 2)
    expect_equal(res.dim2$`dimensions of subset_df in sim2`[1], 2533)
    expect_equal(res.dim2$`dimensions of subset_df in sim2`[2], 2)
    expect_length(res.dim2$`dimensions of subset_df in sim3`, 2)
    expect_equal(res.dim2$`dimensions of subset_df in sim3`[1], 3472)
    expect_equal(res.dim2$`dimensions of subset_df in sim3`[2], 2)

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <subset_df> has been created in all specified data sources")
    expect_equal(res$validity.check, "<subset_df> appears valid in all sources")
})

# context("ds.dataFrameSubset::smk::create a subset dataframe, based on scalar")
test_that("dataFrameSubset_exists scalar", {
    myvectors <- c('D$LAB_TSC', 'D$LAB_HDL', 'D$PM_BMI_CATEGORICAL')
    ds.dataFrame(x=myvectors, newobj="unsubset_df")

    res.dim1 <- ds.dim('unsubset_df')
    expect_length(res.dim1, 4)
    expect_length(res.dim1$`dimensions of unsubset_df in sim1`, 2)
    expect_equal(res.dim1$`dimensions of unsubset_df in sim1`[1], 2163)
    expect_equal(res.dim1$`dimensions of unsubset_df in sim1`[2], 3)
    expect_length(res.dim1$`dimensions of unsubset_df in sim2`, 2)
    expect_equal(res.dim1$`dimensions of unsubset_df in sim2`[1], 3088)
    expect_equal(res.dim1$`dimensions of unsubset_df in sim2`[2], 3)
    expect_length(res.dim1$`dimensions of unsubset_df in sim3`, 2)
    expect_equal(res.dim1$`dimensions of unsubset_df in sim3`[1], 4128)
    expect_equal(res.dim1$`dimensions of unsubset_df in sim3`[2], 3)

    res <- ds.dataFrameSubset(df.name="unsubset_df", V1.name="D$PM_BMI_CATEGORICAL", V2.name="3", Boolean.operator="!=", newobj="subset_df")

    res.dim2 <- ds.dim('subset_df')
    expect_length(res.dim2, 4)
    expect_length(res.dim2$`dimensions of subset_df in sim1`, 2)
    expect_equal(res.dim2$`dimensions of subset_df in sim1`[1], 1457)
    expect_equal(res.dim2$`dimensions of subset_df in sim1`[2], 3)
    expect_length(res.dim2$`dimensions of subset_df in sim2`, 2)
    expect_equal(res.dim2$`dimensions of subset_df in sim2`[1], 2072)
    expect_equal(res.dim2$`dimensions of subset_df in sim2`[2], 3)
    expect_length(res.dim2$`dimensions of subset_df in sim3`, 2)
    expect_equal(res.dim2$`dimensions of subset_df in sim3`[1], 2769)
    expect_equal(res.dim2$`dimensions of subset_df in sim3`[2], 3)
    
    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <subset_df> has been created in all specified data sources")
    expect_equal(res$validity.check, "<subset_df> appears valid in all sources")
})

#
# Done
#

# context("ds.dataFrameSubset::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "unsubset_df", "subset_df"))
})

disconnect.studies.dataset.cnsim()

# context("ds.dataFrameSubset::smk::down")

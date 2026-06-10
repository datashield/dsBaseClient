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

# context("ds.dataFrameSort::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_HDL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.dataFrameSort::smk::create a sorted dataframe")
test_that("dataFrameSort_exists", {
    myvectors <- c('D$LAB_TSC', 'D$LAB_HDL')
    ds.dataFrame(x=myvectors, newobj="unsorted_df")

    res <- ds.dataFrameSort(df.name="unsorted_df", sort.key.name="D$LAB_TSC", newobj="sorted_df")

    expect_length(res, 3)
    expect_equal(res$is.object.created, "A data object <sorted_df> has been created in all specified data sources")
    expect_equal(res$validity.check, "<sorted_df> appears valid in all sources")
    expect_length(res$studyside.messages, 3)
    expect_length(res$studyside.messages$sim1, 1)
    expect_equal(res$studyside.messages$sim1, "ALL OK: there are no studysideMessage(s) on this datasource")
    expect_length(res$studyside.messages$sim2, 1)
    expect_equal(res$studyside.messages$sim2, "ALL OK: there are no studysideMessage(s) on this datasource")
    expect_length(res$studyside.messages$sim3, 1)
    expect_equal(res$studyside.messages$sim3, "ALL OK: there are no studysideMessage(s) on this datasource")
})

#
# Shutdown
#

# context("ds.dataFrameSort::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "unsorted_df", "sorted_df"))
})

disconnect.studies.dataset.cnsim()

#
# Done
#

# context("ds.dataFrameSort::smk::done")

#-------------------------------------------------------------------------------
# Copyright (c) 2018 University of Newcastle upon Tyne. All rights reserved.
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

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_HDL"))

#
# Tests
#

context("ds.dataFrameSort::smk::create a sorted dataframe")
test_that("dataFrameSort_exists", {
    myvectors <- c('D$LAB_TSC', 'D$LAB_HDL')
    ds.dataFrame(x=myvectors, newobj="unsorted_df")

    res <- ds.dataFrameSort(df.name="unsorted_df", sort.key.name="D$LAB_TSC", newobj="sorted_df")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <sorted_df> has been created in all specified data sources")
    expect_equal(res$validity.check, "<sorted_df> appears valid in all sources")
})

#
# Done
#

disconnect.studies.dataset.cnsim()

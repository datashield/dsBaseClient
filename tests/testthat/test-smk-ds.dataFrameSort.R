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

    ds.dataFrameSort(df.name="unsorted_df", sort.key.name="D$LAB_TSC", newobj="sorted_df")

    ds_expect_variables(c("D", "unsorted_df", "sorted_df"))
})

test_that("fails if the object does not exist", {
    expect_error(
        ds.dataFrameSort(df.name = "nonexistent_object", sort.key.name = "D$LAB_TSC"),
        "There are some DataSHIELD errors, list them with datashield.errors()",
        fixed = TRUE
    )

    res.errors <- DSI::datashield.errors()
    for (study.errors in res.errors) {
        expect_match(study.errors, "The server-side object 'nonexistent_object' does not exist")
    }
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

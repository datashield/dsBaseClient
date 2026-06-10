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

connect.studies.dataset.cnsim(list("LAB_TSC", "GENDER"))

#
# Tests
#

# context("ds.dataFrameSort::arg::no args")
test_that("dataFrameSort_noargs", {
    expect_error(ds.dataFrameSort(), "There are some DataSHIELD errors, list them with datashield.errors()", fixed=TRUE)

    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 3)
    expect_match(res.errors$sim1, "* Error in strsplit\\(df.name, split = \"\"\\) : non-character argument*")
    expect_match(res.errors$sim2, "* Error in strsplit\\(df.name, split = \"\"\\) : non-character argument*")
    expect_match(res.errors$sim3, "* Error in strsplit\\(df.name, split = \"\"\\) : non-character argument*")
})

# context("ds.dataFrameSort::arg::sorted dataframe, of factors")
test_that("dataFrameSort_factors", {
    myvectors <- c('D$LAB_TSC', 'D$GENDER')
    ds.dataFrame(x=myvectors, newobj="unsorted_df")

    expect_error(ds.dataFrameSort(df.name="unsorted_df", sort.key.name="D$GENDER", newobj="sorted_df"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed=TRUE)

    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 3)
    expect_match(res.errors$sim1, "* Error : specified sort.key variable is of type 'factor'*")
    expect_match(res.errors$sim2, "* Error : specified sort.key variable is of type 'factor'*")
    expect_match(res.errors$sim3, "* Error : specified sort.key variable is of type 'factor'*")
})

#
# Done
#

disconnect.studies.dataset.cnsim()

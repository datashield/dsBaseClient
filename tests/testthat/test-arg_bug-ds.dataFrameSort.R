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

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_HDL"))

#
# Tests
#

context("ds.dataFrameSort::arg::create a sorted dataframe")
test_that("dataFrameSort_exists", {
    expect_error(ds.dataFrameSort(), "", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()

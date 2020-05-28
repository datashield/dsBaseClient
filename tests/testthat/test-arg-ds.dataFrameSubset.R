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

context("ds.dataFrameSubset::arg::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.dataFrameSubset::arg::test errors")
test_that("dataFrameSubset_erros", {
    expect_error(ds.dataFrameSubset(), "Please provide the name of the data.frame to be subsetted as a character string: eg 'xxx'", fixed=TRUE)
})


#
# Shutdown
#

context("ds.dataFrameSubset::arg::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

#
# Done
#

context("ds.dataFrameSubset::arg::done")

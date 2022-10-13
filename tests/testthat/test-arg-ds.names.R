#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

context("ds.names::arg::test errors")
test_that("simple ds.names errors", {
    expect_error(ds.names(), "Please provide the name of the input list!", fixed=TRUE)

    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 0)

    expect_error(ds.names(x="D$LAB_TSC"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)

    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 3)
    expect_equal(res.errors$sim1, "Execution failed: Error : The input object is not of class <list>numeric", fixed=TRUE)
    expect_equal(res.errors$sim2, "Execution failed: Error : The input object is not of class <list>numeric", fixed=TRUE)
    expect_equal(res.errors$sim3, "Execution failed: Error : The input object is not of class <list>numeric", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()

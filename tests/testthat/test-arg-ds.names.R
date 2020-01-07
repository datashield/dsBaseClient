#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
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
    expect_error(ds.names(x="D$LAB_TSC"), "The input object must be a list.", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()

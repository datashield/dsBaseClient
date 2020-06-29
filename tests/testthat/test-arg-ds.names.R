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

    res <- ds.names(x="D$LAB_TSC")

    expect_length(res, 3)
    expect_length(res$sim1, 2)
    expect_length(res$sim1$error.message, 1)
    expect_equal(res$sim1$error.message, "The input object is not of class <list>", fixed=TRUE)
    expect_length(res$sim1$trace.message, 1)
    expect_equal(res$sim1$trace.message, "numeric")
})

#
# Done
#

disconnect.studies.dataset.cnsim()

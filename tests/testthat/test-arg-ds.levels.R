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

context("ds.levels::arg")
test_that("simple levels", {
    expect_error(ds.levels(), "Please provide the name of the input vector!", fixed=TRUE)
    expect_error(ds.levels("LAB_TSC"), "The input object LAB_TSC is not defined in sim1, sim2, sim3!", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()

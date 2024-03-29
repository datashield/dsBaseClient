#-------------------------------------------------------------------------------
# Copyright (c) 2018-2022 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.dim::arg::test errors")
test_that("dim_erros", {
    expect_error(ds.dim(), "Please provide the name of a data.frame or matrix!", fixed=TRUE)
    expect_error(ds.dim(x="F", checks = TRUE), "The input object must be a table structure!", fixed=TRUE)
    expect_error(ds.dim(x="D", type = "other"), 'Function argument "type" has to be either "both", "combine" or "split"', fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()

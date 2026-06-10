#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

# context("ds.ns::arg::setup")

connect.studies.dataset.cnsim(list("PM_BMI_CONTINUOUS"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.ns::arg::test errors")
test_that("ns_errors", {
    expect_error(ds.ns(), "argument \"x\" is missing, with no default", fixed=TRUE)
    expect_error(ds.qlspline(x=NULL), "Please provide the name of the input variable x!", fixed=TRUE)
})

#
# Done
#

# context("ds.ns::arg::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.ns::arg::done")

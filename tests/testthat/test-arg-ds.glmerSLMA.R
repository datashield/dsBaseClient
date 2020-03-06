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

context("ds.glmerSLMA::arg::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.glmerSLMA::arg")
test_that("simple glmerSLMA", {
    expect_error(ds.glmerSLMA(), " Please provide a valid regression formula!", fixed=TRUE)
})

#
# Done
#

context("ds.glmerSLMA::arg::shutdown")

test_that("setup", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

context("ds.glmerSLMA::arg::done")

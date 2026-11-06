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

# context("ds.kurtosis::arg::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.kurtosis::arg::test errors")
test_that("kurtosis_erros", {
    expect_error(ds.kurtosis(), "must be the name of the input vector", fixed=TRUE)
    expect_error(ds.kurtosis(x='D$LAB_TSC', type='datashield'), "must be \"both\", \"combine\", or \"split\"", fixed=TRUE)
    expect_error(ds.kurtosis(x='D$LAB_TSC', method="0"), "must be 1, 2 or 3", fixed=TRUE)
    expect_error(ds.kurtosis(x='D$LAB_TSC', method="4"), "must be 1, 2 or 3", fixed=TRUE)
})

#
# Done
#

# context("ds.kurtosis::arg::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.kurtosis::arg::done")

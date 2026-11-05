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

# context("ds.sample::arg::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

#
# Tests
#

# context("ds.sample::arg::test errors")
test_that("cov_erros", {
    expect_error(ds.sample(), "must be a character string that names a server-side object")
    expect_error(ds.sample(x='D$LAB_TSC'), "must be an integer that sets how many records")
    expect_error(ds.sample(x='D$LAB_TSC', size=1234, seed.as.integer="FooBar"), "must be an integer or")
})

#
# Shutdown
#

# context("ds.sample::arg::shutdown")

disconnect.studies.dataset.cnsim()

#
# Done
#

# context("ds.sample::arg::done")

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
    res1 <- ds.sample()
    expect_equal(res1, 'Error: x must denote a character string naming the serverside object to be sampled or an integer N denoting permute 1:N', fixed=TRUE)
    res2 <- ds.sample(x='D$LAB_TSC')
    expect_equal(res2, 'Error: size must have a value which is an integer denoting how many records to be sampled', fixed=TRUE)
    res3 <- ds.sample(x='D$LAB_TSC', size=1234, seed.as.integer="FooBar")
    expect_equal(res3, 'ERROR failed: seed.as.integer must be set as an integer [numeric] or left NULL', fixed=TRUE)
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

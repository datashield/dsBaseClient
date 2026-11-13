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

# context("ds.ls::arg::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.ls::arg")
test_that("containing escape sequence", {
    expect_error(ds.ls(search.filter="_:A:_Environment"), "also appears")

    expect_error(ds.ls(search.filter="Envir_:A:_oment"), "also appears")

    expect_error(ds.ls(search.filter="Environment_:A:_"), "also appears")
})

#
# Shutdown
#

# context("ds.ls::arg::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

#
# Done
#

# context("ds.ls::arg::done")

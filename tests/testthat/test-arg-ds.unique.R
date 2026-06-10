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

connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

# context("ds.unique::arg::no arguments")
test_that("simple ds.unique no arguments", {
    expect_error(ds.unique(), "x.name=NULL. Please provide the names of the objects to de-duplicated!", fixed = TRUE)
})

# context("ds.unique::arg::NULL arguments")
test_that("simple ds.unique NULL arguments", {
    expect_error(ds.unique(NULL), "x.name=NULL. Please provide the names of the objects to de-duplicated!", fixed = TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()

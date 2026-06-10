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

connect.studies.dataset.survival(list("survtime", "time.id"))

#
# Tests
#

# context("ds.corTest::arg")

test_that("simple arg test", {
    expect_error(ds.corTest(), "x=NULL. Please provide the names of the 1st numeric vector!", fixed=TRUE)
    expect_error(ds.corTest(x="D$survtime"), "y=NULL. Please provide the names of the 2nd numeric vector!", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.survival()

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

# context("ds.replaceNA::arg::test errors")
test_that("simple ds.replaceNA errors", {
    expect_error(ds.replaceNA(), "Please provide the name of a vector!", fixed=TRUE)
    expect_error(ds.replaceNA(x="D$LAB_TSC"), "Please provide a list of replacement values!", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()

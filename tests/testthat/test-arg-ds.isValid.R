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

connect.studies.dataset.cnsim(list("LAB_HDL"))

#
# Tests
#

# context("ds.isValid::arg::errors")
test_that("isValid errors", {
    expect_error(ds.isValid(), "Please provide the name of the input vector!", fixed=TRUE)
    expect_error(ds.isValid("D$NOT_THERE"), "The input object D$NOT_THERE is not defined in sim1, sim2, sim3!", fixed=TRUE)
})

#
# Tear down
#

disconnect.studies.dataset.cnsim()

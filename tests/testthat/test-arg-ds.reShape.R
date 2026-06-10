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

connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

# context("ds.reShape::arg::test errors")
test_that("reShape_erros", {
    expect_error(ds.reShape(), "Please provide the name of the list that holds the input vectors!", fixed=TRUE)
    expect_error(ds.reShape(data.name="test", sep=TRUE), "'sep' must be a character string", fixed=TRUE)
    expect_error(ds.reShape(data.name="test", sep=""), "'sep' must be a character string", fixed=TRUE)
    expect_error(ds.reShape(data.name="test", sep="ts"), "'sep' must be a character string", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()

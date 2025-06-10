#-------------------------------------------------------------------------------
# Copyright (c) 2018-2021 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.dataFrameFill::arg::test errors")
test_that("dataFrameFill_erros", {
    expect_error(ds.dataFrameFill(), "Please provide the name of the data.frame to be filled as a character string: eg 'xxx'", fixed=TRUE)
    expect_error(ds.dataFrameFill("NonDF"), "The input object(s) NonDF is(are) not defined on one or more of the studies!", fixed=TRUE)
    expect_error(ds.dataFrameFill("D$LAB_TSC"), "The input vector must be of type 'data.frame' or a 'matrix'!", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()

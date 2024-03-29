#-------------------------------------------------------------------------------
# Copyright (c) 2018-2022 University of Newcastle upon Tyne. All rights reserved.
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

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

#
# Tests
#

context("ds.cor::arg::test errors")
test_that("cor_erros", {
    expect_error(ds.cor(), 'x=NULL. Please provide the name of a matrix or dataframe or the names of two numeric vectors!', fixed=TRUE)
    expect_error(ds.cor(x='D$LAB_TSC'), 'If x is a numeric vector, y must be a numeric vector!', fixed=TRUE)
    expect_error(ds.cor(x='D$LAB_TSC', y='D$LAB_TRIG', type='datashield'), 'Function argument "type" has to be either "combine" or "split"', fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()

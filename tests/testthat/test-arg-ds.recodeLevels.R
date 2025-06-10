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

connect.studies.dataset.cnsim(list("PM_BMI_CATEGORICAL", "LAB_TSC"))

#
# Tests
#

context("ds.recodeLevels::arg::test errors")
test_that("recodeLevels_erros", {
    expect_error(ds.recodeLevels(), " End of process!", fixed=TRUE)
    expect_error(ds.recodeLevels(x='D$PM_BMI_CATEGORICAL'), " End of process!", fixed=TRUE)
    expect_error(ds.recodeLevels(x='D$LAB_TSC', newCategories=c('normal', 'overweight')), "The input vector must be a factor!", fixed=TRUE)
    expect_error(ds.recodeLevels(x='D$PM_BMI_CATEGORICAL', newCategories=c('normal', 'overweight')), "The number of levels you specified is smaller than the levels of the input vector!", fixed=TRUE)
})

#
# Tear down
#

disconnect.studies.dataset.cnsim()

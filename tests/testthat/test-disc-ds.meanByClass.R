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

# context("ds.meanByClass::disc::setup")

connect.studies.dataset.cnsim(list("LAB_TSC","LAB_HDL","GENDER","DIS_DIAB","PM_BMI_CATEGORICAL"))

#
# Tests
#

# context("ds.meanByClass::disc::test errors")
test_that("meanByClass_erros", {
#    expect_error(ds.meanByClass(x='D', outvar='LAB_HDL', covar='GENDER', type='split'), 'Failed to get levels from study: FAILED: Result less than nfilter.subset', fixed=TRUE)
})

#
# Tear down
#

# context("ds.meanByClass::disc::teardown")

disconnect.studies.dataset.cnsim()

#
# Done
#

# context("ds.meanByClass::disc::done")

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

connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

context("ds.glmSLMA::arg::test errors")
test_that("glmSLMA_errors", {
  expect_error(ds.glmSLMA(), " Please provide a valid regression formula!", fixed=TRUE)
  
  expect_error(ds.glmSLMA('D$LAB_TSC~D$LAB_TRIG', family="gaussian", newobj="gaussian.glmslma.obj"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed=TRUE)
  res.errors<-datashield.errors()

  expect_length(res.errors, 3)
  expect_equal(res.errors$sim1, "Execution failed: Error in model.frame.default(formula = formula2use, data = dataTable,  : \n  invalid type (NULL) for variable 'D$LAB_TRIG'", fixed=TRUE)
  expect_equal(res.errors$sim2, "Execution failed: Error in model.frame.default(formula = formula2use, data = dataTable,  : \n  invalid type (NULL) for variable 'D$LAB_TRIG'", fixed=TRUE)
  expect_equal(res.errors$sim3, "Execution failed: Error in model.frame.default(formula = formula2use, data = dataTable,  : \n  invalid type (NULL) for variable 'D$LAB_TRIG'", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()

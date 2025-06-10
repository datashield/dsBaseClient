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

context("ds.glmSLMA::arg::test errors")
test_that("glmSLMA_errors", {
  expect_error(ds.glmSLMA(), " Please provide a valid regression formula!", fixed=TRUE)
  
  expect_error(ds.glmSLMA('D$LAB_TSC~D$LAB_TRIG', family="gaussian", newobj="gaussian.glmslma.obj"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed=TRUE)
  res.errors<-datashield.errors()
  expect_length(res.errors, 3)
  expect_equal(res.errors[[1]], "Command 'glmSLMADS1(D$LAB_TSC ~ D$LAB_TRIG, \"gaussian\", NULL, NULL, NULL)' failed on 'sim1': Error while evaluating 'dsBase::glmSLMADS1(D$LAB_TSC~D$LAB_TRIG, \"gaussian\", NULL, NULL, NULL)' -> Error in model.frame.default(formula = formula2use, data = dataTable,  : \n  invalid type (NULL) for variable 'D$LAB_TRIG'\n", fixed=TRUE)
  expect_equal(res.errors[[2]], "Command 'glmSLMADS1(D$LAB_TSC ~ D$LAB_TRIG, \"gaussian\", NULL, NULL, NULL)' failed on 'sim2': Error while evaluating 'dsBase::glmSLMADS1(D$LAB_TSC~D$LAB_TRIG, \"gaussian\", NULL, NULL, NULL)' -> Error in model.frame.default(formula = formula2use, data = dataTable,  : \n  invalid type (NULL) for variable 'D$LAB_TRIG'\n", fixed=TRUE)
  expect_equal(res.errors[[3]], "Command 'glmSLMADS1(D$LAB_TSC ~ D$LAB_TRIG, \"gaussian\", NULL, NULL, NULL)' failed on 'sim3': Error while evaluating 'dsBase::glmSLMADS1(D$LAB_TSC~D$LAB_TRIG, \"gaussian\", NULL, NULL, NULL)' -> Error in model.frame.default(formula = formula2use, data = dataTable,  : \n  invalid type (NULL) for variable 'D$LAB_TRIG'\n", fixed=TRUE)
  
})

#
# Done
#

disconnect.studies.dataset.cnsim()

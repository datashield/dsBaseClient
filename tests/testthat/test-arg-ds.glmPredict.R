#-------------------------------------------------------------------------------
# Copyright (c) 2018-2020 University of Newcastle upon Tyne. All rights reserved.
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

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG", "DIS_AMI", "DIS_DIAB", "GENDER"))

#
# Tests
#

context("ds.glmPredict::arg::test _glmname_ arg is correct object")
test_that("glmPredict_errors", {
  res<-ds.glmPredict()
  expect_equal(res, "<glmname> is not set, please specify it as a character string containing the name of a valid glm class object on the serverside", fixed=TRUE)
  
  #expect_error(ds.glmPredict(), " Please provide a valid 'family' argument!", fixed=TRUE)
  #res<-ds.glmPredict(ABC)
  #expect_equal(res, "object 'ABC' not found", fixed=TRUE)
  expect_error(ds.glmPredict("ABC"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed=TRUE)
  # TODO: "some DataSHIELD errors" is unhelpful and needs improvement
  expect_equal(class(datashield.errors()),"list")
  expect_length(datashield.errors(),3)
  expect_equal(names(datashield.errors()),c("sim1","sim2","sim3"))
})

context("ds.glmPredict::arg::setting up glm obj for further testing")
test_that("glmPredict_errors", {
  #glm.obj <- ds.glmSLMA('D$LAB_TSC~D$LAB_TSC', family="gaussian", newobj="gaussian.glmslma.obj")
  #glmSLMA.res <- ds.glmSLMA('D$DIS_DIAB~D$LAB_TRIG', family="binomial", newobj="binomial.glmslma.obj")
  glmSLMA.res <- ds.glmSLMA('D$LAB_TSC~D$LAB_TRIG', family="gaussian", newobj="gaussian.glmslma.obj")
  
  expect_length(glmSLMA.res, 9)
  expect_equal(glmSLMA.res$num.valid.studies, 3)
  expect_length(glmSLMA.res$validity.check, 1)
  expect_equal(glmSLMA.res$validity.check, "<gaussian.glmslma.obj> appears valid in all sources")
})

context("ds.glmPredict::arg::test _newdataname_ arg is correct object")
test_that("glmPredict_errors", {
  expect_error(ds.glmPredict("gaussian.glmslma.obj", newdataname = "help"),"There are some DataSHIELD errors, list them with datashield.errors()")
  # TODO: "some DataSHIELD errors" is unhelpful and needs improvement
  expect_equal(class(datashield.errors()),"list")
  expect_length(datashield.errors(),3)
  expect_equal(names(datashield.errors()),c("sim1","sim2","sim3"))
})

context("ds.glmPredict::arg::test _output.type_ arg is correct object")
test_that("glmPredict_errors", {
  expect_error(ds.glmPredict("gaussian.glmslma.obj", newdataname = NULL, output.type = NULL), "missing value where TRUE/FALSE needed")
  # TODO: "true/false" is wrong message to get here, error message needs to be fixed
  res<-ds.glmPredict("gaussian.glmslma.obj", newdataname = NULL, output.type = 'blah')
  expect_equal(res,"<output.type> is not correctly set, please specify it as one of three character strings: 'link', 'response', or 'terms'")
  
})

context("ds.glmPredict::arg::test _se.fit_ arg is correct object")
test_that("glmPredict_errors", {
  expect_error(ds.glmPredict("gaussian.glmslma.obj", newdataname = NULL, output.type = "response", se.fit = "1"), "There are some DataSHIELD errors, list them with datashield.errors()")

  expect_equal(class(datashield.errors()),"list")
  expect_length(datashield.errors(),3)
  expect_equal(names(datashield.errors()),c("sim1","sim2","sim3"))
})

context("ds.glmPredict::arg::test _dispersion_ arg is correct object")
test_that("glmPredict_errors", {
  # res<-ds.glmPredict("gaussian.glmslma.obj", newdataname = NULL, output.type = "response", se.fit = FALSE, dispersion = "thereisnothingontheservercalledthis")
  # TODO: why is dispersion passing nonsensical values without questioning it? needs error message
})

context("ds.glmPredict::arg::test _terms_ arg is correct object")
test_that("glmPredict_errors", {
  expect_error(ds.glmPredict("gaussian.glmslma.obj", newdataname = NULL, output.type = "terms", se.fit = FALSE, dispersion = NULL, terms="thereisnothingontheservercalledthis2"), "There are some DataSHIELD errors, list them with datashield.errors()")
  
  # TODO: "some DataSHIELD errors" is unhelpful and needs improvement
  expect_equal(class(datashield.errors()),"list")
  expect_length(datashield.errors(),3)
  expect_equal(names(datashield.errors()),c("sim1","sim2","sim3"))
})

context("ds.glmPredict::arg::test _na.action_ arg is correct object")
test_that("glmPredict_errors", {
  res<-ds.glmPredict("gaussian.glmslma.obj", newdataname = NULL, output.type = "response", se.fit = FALSE, dispersion = NULL, terms=NULL, na.action= "na.other")
  expect_equal(res, "<na.action> is not correctly set, please specify it as one of four character strings: 'na.fail', 'na.omit', 'na.exclude' or 'na.pass'")
})

  
context("ds.glmPredict::arg::test _newobj_ arg is correct object")
test_that("glmPredict_errors", {
  # TODO: come up with a way to create an invalid object to then test newobj argument
  # res<-ds.glmPredict("gaussian.glmslma.obj", newdataname = NULL, output.type = "response", se.fit = FALSE, dispersion = NULL, terms=NULL, na.action= "na.other")
  # print(res)
})


#
# Done
#

disconnect.studies.dataset.cnsim()


#res.errors<-datashield.errors()

#expect_length(res.errors, 3)
#expect_equal(res.errors[[1]], "Command 'glmSLMADS1(D$LAB_TSC ~ D$LAB_TRIG, \"gaussian\", NULL, NULL, NULL)' failed on 'sim1': Error while evaluating 'dsBase::glmSLMADS1(D$LAB_TSC~D$LAB_TRIG, \"gaussian\", NULL, NULL, NULL)' -> Error in model.frame.default(formula = formula2use, data = dataTable,  : \n  invalid type (NULL) for variable 'D$LAB_TRIG'\n", fixed=TRUE)
#expect_equal(res.errors[[2]], "Command 'glmSLMADS1(D$LAB_TSC ~ D$LAB_TRIG, \"gaussian\", NULL, NULL, NULL)' failed on 'sim2': Error while evaluating 'dsBase::glmSLMADS1(D$LAB_TSC~D$LAB_TRIG, \"gaussian\", NULL, NULL, NULL)' -> Error in model.frame.default(formula = formula2use, data = dataTable,  : \n  invalid type (NULL) for variable 'D$LAB_TRIG'\n", fixed=TRUE)
#expect_equal(res.errors[[3]], "Command 'glmSLMADS1(D$LAB_TSC ~ D$LAB_TRIG, \"gaussian\", NULL, NULL, NULL)' failed on 'sim3': Error while evaluating 'dsBase::glmSLMADS1(D$LAB_TSC~D$LAB_TRIG, \"gaussian\", NULL, NULL, NULL)' -> Error in model.frame.default(formula = formula2use, data = dataTable,  : \n  invalid type (NULL) for variable 'D$LAB_TRIG'\n", fixed=TRUE)
#expect_length(glm.obj$validity.check, 1)
#expect_equal(glm.obj$validity.check, "<binomial.glmslma.obj> appears valid in all sources")

#res <- ds.glmPredict("gaussian.glmslma.obj", newdataname = NULL, output.type = "response", se.fit = FALSE, na.action = "na.pass")
#res <- ds.glmPredict("gaussian.glmslma.obj")
#print(res)

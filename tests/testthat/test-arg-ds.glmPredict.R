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

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG", "DIS_AMI", "DIS_DIAB", "GENDER"))

#
# Tests
#

# context("ds.glmPredict::arg::test _glmname_ arg is correct object")
test_that("glmPredict_errors", {
  expect_error(ds.glmPredict(), "<glmname> is not set, please specify it as a character string containing the name of a valid glm class object on the serverside", fixed=TRUE)
  
  expect_error(ds.glmPredict("ABC"), "The input object ABC is not defined in sim1, sim2, sim3!", fixed=TRUE)
})

# context("ds.glmPredict::arg::setting up glm obj for further testing")
test_that("glmPredict_errors", {
  #glm.obj <- ds.glmSLMA('D$LAB_TSC~D$LAB_TSC', family="gaussian", newobj="gaussian.glmslma.obj")
  #glmSLMA.res <- ds.glmSLMA('D$DIS_DIAB~D$LAB_TRIG', family="binomial", newobj="binomial.glmslma.obj")
  glmSLMA.res <- ds.glmSLMA('D$LAB_TSC~D$LAB_TRIG', family="gaussian", newobj="gaussian.glmslma.obj")
  
  expect_length(glmSLMA.res, 9)
  expect_equal(glmSLMA.res$num.valid.studies, 3)
  expect_length(glmSLMA.res$validity.check, 1)
  expect_equal(glmSLMA.res$validity.check, "<gaussian.glmslma.obj> appears valid in all sources")
})

# context("ds.glmPredict::arg::test _newdataname_ arg is correct object")
test_that("glmPredict_errors", {
  expect_error(ds.glmPredict("gaussian.glmslma.obj", newdataname = "help"),"There are some DataSHIELD errors, list them with datashield.errors()", fixed=TRUE)
  # TODO: "some DataSHIELD errors" is unhelpful and needs improvement
  expect_equal(class(datashield.errors()),"list")
  expect_length(datashield.errors(),3)
  expect_true(all(c("sim1","sim2","sim3") %in% names(datashield.errors())))
})

# context("ds.glmPredict::arg::test _output.type_ arg is correct object")
test_that("glmPredict_errors", {
  expect_error(ds.glmPredict("gaussian.glmslma.obj", newdataname = NULL, output.type = NULL), "missing value where TRUE/FALSE needed", fixed=TRUE)
  expect_error(ds.glmPredict("gaussian.glmslma.obj", newdataname = NULL, output.type = 'blah'), "<output.type> is not correctly set, please specify it as one of three character strings: 'link', 'response', or 'terms'", fixed=TRUE)
})

# context("ds.glmPredict::arg::test _se.fit_ arg is correct object")
test_that("glmPredict_errors", {
  expect_error(ds.glmPredict("gaussian.glmslma.obj", newdataname = NULL, output.type = "response", se.fit = "1"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)

  expect_equal(class(datashield.errors()),"list")
  expect_length(datashield.errors(),3)
  expect_true(all(c("sim1","sim2","sim3") %in% names(datashield.errors())))
})

#context("ds.glmPredict::arg::test _dispersion_ arg is correct object")
#test_that("glmPredict_errors", {
#  res <- ds.glmPredict("gaussian.glmslma.obj", newdataname = NULL, output.type = "response", se.fit = FALSE, dispersion = "thereisnothingontheservercalledthis")
#})

# context("ds.glmPredict::arg::test _terms_ arg is correct object")
test_that("glmPredict_errors", {
  expect_error(ds.glmPredict("gaussian.glmslma.obj", newdataname = NULL, output.type = "terms", se.fit = FALSE, dispersion = NULL, terms="thereisnothingontheservercalledthis2"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)
})

# context("ds.glmPredict::arg::test _na.action_ arg is correct object")
test_that("glmPredict_errors", {
  expect_error(ds.glmPredict("gaussian.glmslma.obj", newdataname = NULL, output.type = "response", se.fit = FALSE, dispersion = NULL, terms=NULL, na.action= "na.other"), "<na.action> is not correctly set, please specify it as one of four character strings: 'na.fail', 'na.omit', 'na.exclude' or 'na.pass'", fixed = TRUE)
})


#
# Done
#

disconnect.studies.dataset.cnsim()

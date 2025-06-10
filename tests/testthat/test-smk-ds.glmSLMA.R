#-------------------------------------------------------------------------------
# Copyright (c) 2019-2021 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.glmSLMA::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG", "DIS_AMI", "DIS_DIAB", "GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.glmSLMA::smk::gaussian")
test_that("simple glmSLMA, gaussian", {
    glmSLMA.res <- ds.glmSLMA('D$LAB_TSC~D$LAB_TRIG', family="gaussian")

    expect_length(glmSLMA.res, 9)
    expect_equal(glmSLMA.res$num.valid.studies, 3)
    expect_true("matrix" %in% class(glmSLMA.res$betamatrix.all))
    expect_true("matrix" %in% class(glmSLMA.res$sematrix.all))
    expect_true("matrix" %in% class(glmSLMA.res$betamatrix.valid))
    expect_true("matrix" %in% class(glmSLMA.res$sematrix.valid))
    expect_true("matrix" %in% class(glmSLMA.res$SLMA.pooled.ests.matrix))
    expect_length(glmSLMA.res$output.summary, 5)
    expect_true("matrix" %in% class(glmSLMA.res$output.summary$input.beta.matrix.for.SLMA))
    expect_true("matrix" %in% class(glmSLMA.res$output.summary$input.se.matrix.for.SLMA))
    expect_length(glmSLMA.res$output.summary$study1, 29)
    expect_true("family" %in% class(glmSLMA.res$output.summary$study1$family))
    expect_true("matrix" %in% class(glmSLMA.res$output.summary$study1$coefficients))
    expect_equal(glmSLMA.res$output.summary$study1$rank, 2)
    expect_equal(glmSLMA.res$output.summary$study1$aic, 5460.549, tolerance = 0.001)
    expect_equal(glmSLMA.res$output.summary$study1$iter, 2)
    expect_equal(glmSLMA.res$output.summary$study1$contrasts, NULL)
    expect_equal(glmSLMA.res$output.summary$study1$dispersion, 1.211496, tolerance = 0.001)
    expect_equal(glmSLMA.res$output.summary$study1$Ntotal, 2163)
    expect_equal(glmSLMA.res$output.summary$study1$Nvalid, 1801)
    expect_equal(glmSLMA.res$output.summary$study1$Nmissing, 362)
    expect_length(glmSLMA.res$output.summary$study2, 29)
    expect_true("family" %in% class(glmSLMA.res$output.summary$study2$family))
    expect_true("matrix" %in% class(glmSLMA.res$output.summary$study2$coefficients))
    expect_equal(glmSLMA.res$output.summary$study2$rank, 2)
    expect_equal(glmSLMA.res$output.summary$study2$aic, 7490.000, tolerance = 0.001)
    expect_equal(glmSLMA.res$output.summary$study2$iter, 2)
    expect_equal(glmSLMA.res$output.summary$study2$contrasts, NULL)
    expect_equal(glmSLMA.res$output.summary$study2$dispersion, 1.13414, tolerance = 0.001)
    expect_equal(glmSLMA.res$output.summary$study2$Ntotal, 3088)
    expect_equal(glmSLMA.res$output.summary$study2$Nvalid, 2526)
    expect_equal(glmSLMA.res$output.summary$study2$Nmissing, 562)
    expect_length(glmSLMA.res$output.summary$study3, 29)
    expect_true("family" %in% class(glmSLMA.res$output.summary$study3$family))
    expect_true("matrix" %in% class(glmSLMA.res$output.summary$study3$coefficients))
    expect_equal(glmSLMA.res$output.summary$study3$rank, 2)
    expect_equal(glmSLMA.res$output.summary$study3$aic, 10256.000, tolerance = 0.001)
    expect_equal(glmSLMA.res$output.summary$study3$iter, 2)
    expect_equal(glmSLMA.res$output.summary$study3$contrasts, NULL)
    expect_equal(glmSLMA.res$output.summary$study3$dispersion, 1.12, tolerance = 0.001)
    expect_equal(glmSLMA.res$output.summary$study3$Ntotal, 4128)
    expect_equal(glmSLMA.res$output.summary$study3$Nvalid, 3473)
    expect_equal(glmSLMA.res$output.summary$study3$Nmissing, 655)
    expect_length(glmSLMA.res$is.object.created, 1)
    expect_equal(glmSLMA.res$is.object.created, "A data object <new.glm.obj> has been created in all specified data sources")
    expect_length(glmSLMA.res$validity.check, 1)
    expect_equal(glmSLMA.res$validity.check, "<new.glm.obj> appears valid in all sources")
})

context("ds.glmSLMA::smk::binomial")
test_that("simple glmSLMA, binomial", {
    ds.asCharacter('D$DIS_AMI',  'str.dis.ami')
    ds.asNumeric('str.dis.ami',  'num.dis.ami')
    ds.asCharacter('D$GENDER',   'str.gender')
    ds.asNumeric('str.gender',   'num.gender')
    ds.asCharacter('D$DIS_DIAB', 'str.dis.diab')
    ds.asNumeric('str.dis.diab', 'num.dis.diab')

    expect_error(ds.glmSLMA('num.dis.ami~num.gender*num.dis.diab', family="binomial"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)

    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 3)
    expect_equal(res.errors$sim1, "Command 'glmSLMADS1(num.dis.ami ~ num.gender * num.dis.diab, \"binomial\", \n    NULL, NULL, NULL)' failed on 'sim1': Error while evaluating 'dsBase::glmSLMADS1(num.dis.ami~num.gender*num.dis.diab, \"binomial\", NULL, NULL, NULL)' -> Error : ERROR: y vector is binary with one category less than filter threshold for table cell size\n", fixed = TRUE)
    expect_equal(res.errors$sim2, "Command 'glmSLMADS1(num.dis.ami ~ num.gender * num.dis.diab, \"binomial\", \n    NULL, NULL, NULL)' failed on 'sim2': Error while evaluating 'dsBase::glmSLMADS1(num.dis.ami~num.gender*num.dis.diab, \"binomial\", NULL, NULL, NULL)' -> Error : ERROR: y vector is binary with one category less than filter threshold for table cell size\n", fixed = TRUE)
    expect_equal(res.errors$sim3, "Command 'glmSLMADS1(num.dis.ami ~ num.gender * num.dis.diab, \"binomial\", \n    NULL, NULL, NULL)' failed on 'sim3': Error while evaluating 'dsBase::glmSLMADS1(num.dis.ami~num.gender*num.dis.diab, \"binomial\", NULL, NULL, NULL)' -> Error : ERROR: y vector is binary with one category less than filter threshold for table cell size\n", fixed = TRUE)

#    expect_length(glmSLMA.res, 9)
#    expect_length(glmSLMA.res[[1]], 1)
#    expect_equal(glmSLMA.res[[1]][1], "EVERY STUDY HAS DATA THAT COULD BE POTENTIALLY DISCLOSIVE UNDER THE CURRENT MODEL:")
#    expect_length(glmSLMA.res[[2]], 1)
#    expect_equal(glmSLMA.res[[2]][1], "Any values of 1 in the following tables denote potential disclosure risks.")
#    expect_length(glmSLMA.res[[3]], 1)
#    expect_equal(glmSLMA.res[[3]][1], "Errors by study are as follows:")
#    expect_true("matrix" %in% class(glmSLMA.res$y.vector.error))
#    expect_length(glmSLMA.res$y.vector.error["sim1", ], 1)
#    expect_length(glmSLMA.res$y.vector.error["sim2", ], 1)
#    expect_length(glmSLMA.res$y.vector.error["sim3", ], 1)
#    expect_length(glmSLMA.res$y.vector.error[, 1], 3)
#    expect_true("matrix" %in% class(glmSLMA.res$X.matrix.error))
#    expect_length(glmSLMA.res$X.matrix.error[1, ], 4)
#    expect_length(glmSLMA.res$X.matrix.error[2, ], 4)
#    expect_length(glmSLMA.res$X.matrix.error[3, ], 4)
#    expect_length(glmSLMA.res$X.matrix.error[, 1], 3)
#    expect_length(glmSLMA.res$X.matrix.error[, 2], 3)
#    expect_length(glmSLMA.res$X.matrix.error[, 3], 3)
#    expect_length(glmSLMA.res$X.matrix.error[, 4], 3)
#    expect_true("matrix" %in% class(glmSLMA.res$weight.vector.error))
#    expect_length(glmSLMA.res$weight.vector.error["sim1", ], 1)
#    expect_length(glmSLMA.res$weight.vector.error["sim2", ], 1)
#    expect_length(glmSLMA.res$weight.vector.error["sim3", ], 1)
#    expect_length(glmSLMA.res$weight.vector.error[, 1], 3)
#    expect_true("matrix" %in% class(glmSLMA.res$offset.vector.error))
#    expect_length(glmSLMA.res$offset.vector.error["sim1", ], 1)
#    expect_length(glmSLMA.res$offset.vector.error["sim2", ], 1)
#    expect_length(glmSLMA.res$offset.vector.error["sim3", ], 1)
#    expect_length(glmSLMA.res$offset.vector.error[, 1], 3)
#    expect_true("matrix" %in% class(glmSLMA.res$glm.overparameterized))
#    expect_length(glmSLMA.res$glm.overparameterized["sim1", ], 1)
#    expect_length(glmSLMA.res$glm.overparameterized["sim2", ], 1)
#    expect_length(glmSLMA.res$glm.overparameterized["sim3", ], 1)
#    expect_length(glmSLMA.res$glm.overparameterized[, 1], 3)
#    expect_true("matrix" %in% class(glmSLMA.res$errorMessage))
#    expect_length(glmSLMA.res$errorMessage["sim1", ], 1)
#    expect_length(glmSLMA.res$errorMessage["sim2", ], 1)
#    expect_length(glmSLMA.res$errorMessage["sim3", ], 1)
#    expect_length(glmSLMA.res$errorMessage[, 1], 3)
})

context("ds.glmSLMA::smk::poisson")
test_that("simple glmSLMA, poisson", {
    glmSLMA.res <- ds.glmSLMA('D$LAB_TSC~D$LAB_TRIG', family="poisson")

    expect_length(glmSLMA.res, 9)
    expect_equal(glmSLMA.res$num.valid.studies, 3)
    expect_true("matrix" %in% class(glmSLMA.res$betamatrix.all))
    expect_true("matrix" %in% class(glmSLMA.res$sematrix.all))
    expect_true("matrix" %in% class(glmSLMA.res$betamatrix.valid))
    expect_true("matrix" %in% class(glmSLMA.res$sematrix.valid))
    expect_true("matrix" %in% class(glmSLMA.res$SLMA.pooled.ests.matrix))
    expect_length(glmSLMA.res$output.summary, 5)
    expect_true("matrix" %in% class(glmSLMA.res$output.summary$input.beta.matrix.for.SLMA))
    expect_true("matrix" %in% class(glmSLMA.res$output.summary$input.se.matrix.for.SLMA))
    expect_length(glmSLMA.res$output.summary$study1, 29)
    expect_true("family" %in% class(glmSLMA.res$output.summary$study1$family))
    expect_true("matrix" %in% class(glmSLMA.res$output.summary$study1$coefficients))
    expect_equal(glmSLMA.res$output.summary$study1$rank, 2)
    expect_equal(glmSLMA.res$output.summary$study1$aic, Inf, tolerance = 0.001)
    expect_equal(glmSLMA.res$output.summary$study1$iter, 4)
    expect_equal(glmSLMA.res$output.summary$study1$contrasts, NULL)
    expect_equal(glmSLMA.res$output.summary$study1$dispersion, 1.000, tolerance = 0.001)
    expect_equal(glmSLMA.res$output.summary$study1$Ntotal, 2163)
    expect_equal(glmSLMA.res$output.summary$study1$Nvalid, 1801)
    expect_equal(glmSLMA.res$output.summary$study1$Nmissing, 362)
    expect_length(glmSLMA.res$output.summary$study2, 29)
    expect_true("family" %in% class(glmSLMA.res$output.summary$study2$family))
    expect_true("matrix" %in% class(glmSLMA.res$output.summary$study2$coefficients))
    expect_equal(glmSLMA.res$output.summary$study2$rank, 2)
    expect_equal(glmSLMA.res$output.summary$study2$aic, Inf, tolerance = 0.001)
    expect_equal(glmSLMA.res$output.summary$study2$iter, 4)
    expect_equal(glmSLMA.res$output.summary$study2$contrasts, NULL)
    expect_equal(glmSLMA.res$output.summary$study2$dispersion, 1.00, tolerance = 0.001)
    expect_equal(glmSLMA.res$output.summary$study2$Ntotal, 3088)
    expect_equal(glmSLMA.res$output.summary$study2$Nvalid, 2526)
    expect_equal(glmSLMA.res$output.summary$study2$Nmissing, 562)
    expect_length(glmSLMA.res$output.summary$study3, 29)
    expect_true("family" %in% class(glmSLMA.res$output.summary$study3$family))
    expect_true("matrix" %in% class(glmSLMA.res$output.summary$study3$coefficients))
    expect_equal(glmSLMA.res$output.summary$study3$rank, 2)
    expect_equal(glmSLMA.res$output.summary$study3$aic, Inf, tolerance = 0.001)
    expect_equal(glmSLMA.res$output.summary$study3$iter, 4)
    expect_equal(glmSLMA.res$output.summary$study3$contrasts, NULL)
    expect_equal(glmSLMA.res$output.summary$study3$dispersion, 1.00, tolerance = 0.001)
    expect_equal(glmSLMA.res$output.summary$study3$Ntotal, 4128)
    expect_equal(glmSLMA.res$output.summary$study3$Nvalid, 3473)
    expect_equal(glmSLMA.res$output.summary$study3$Nmissing, 655)
    expect_length(glmSLMA.res$is.object.created, 1)
    expect_equal(glmSLMA.res$is.object.created, "A data object <new.glm.obj> has been created in all specified data sources")
    expect_length(glmSLMA.res$validity.check, 1)
    expect_equal(glmSLMA.res$validity.check, "<new.glm.obj> appears valid in all sources")
})

#
# Done
#

context("ds.glmSLMA::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "new.glm.obj", "num.dis.ami", "num.dis.diab", "num.gender", "str.dis.ami", "str.dis.diab", "str.gender"))
})

disconnect.studies.dataset.cnsim()

context("ds.glmSLMA::smk::done")

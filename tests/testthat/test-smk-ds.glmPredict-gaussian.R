#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.glmPredict::smk::gaussian::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG", "DIS_AMI", "DIS_DIAB", "GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#
context("ds.glmPredict::smk::gaussian::without_newobj")
test_that("simple glmPredict, gaussian, without newobj, se.fit=FALSE",{
    glmSLMA.res <- ds.glmSLMA('D$LAB_TSC~D$LAB_TRIG', family="gaussian", newobj="gaussian.glmslma.obj")

    expect_length(glmSLMA.res, 9)
    expect_equal(glmSLMA.res$num.valid.studies, 3)
    expect_length(glmSLMA.res$validity.check, 1)
    expect_equal(glmSLMA.res$validity.check, "<gaussian.glmslma.obj> appears valid in all sources")

    res <- ds.glmPredict("gaussian.glmslma.obj", newdataname = NULL, output.type = "response", se.fit = FALSE, na.action = "na.pass")
    
    expect_length(res, 3)
    expect_equal(class(res), "list")

    expect_length(res$sim1, 1)
    expect_length(res$sim1$safe.list, 10)
    expect_equal(class(res$sim1$safe.list), "list")
    expect_equal(res$sim1$safe.list$glm.object, "gaussian.glmslma.obj")
    expect_true(is.null(res$sim1$safe.list$newdfname))
    expect_equal(res$sim1$safe.list$output.type, "response")
    expect_true(is.null(res$sim1$safe.list$dispersion))
    expect_equal(res$sim1$safe.list$fit.Ntotal, 1801)
    expect_equal(res$sim1$safe.list$fit.Nvalid, 1801)
    expect_equal(res$sim1$safe.list$fit.Nmiss, 0)
    expect_equal(res$sim1$safe.list$fit.mean, 5.872024, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.sd, 0.3719756, tolerance = ds.test_env$tolerance)
    expect_length(res$sim1$safe.list$fit.quantiles, 7)
    expect_equal(class(res$sim1$safe.list$fit.quantiles), "numeric")
    expect_equal(res$sim1$safe.list$fit.quantiles[[1]], 5.656127, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.quantiles[[2]], 5.703222, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.quantiles[[3]], 5.778930, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.quantiles[[4]], 5.873137, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.quantiles[[5]], 5.961123, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.quantiles[[6]], 6.044376, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.quantiles[[7]], 6.091611, tolerance = ds.test_env$tolerance)

    expect_length(res$sim2, 1)
    expect_length(res$sim2$safe.list, 10)
    expect_equal(class(res$sim2$safe.list), "list")
    expect_equal(res$sim2$safe.list$glm.object, "gaussian.glmslma.obj")
    expect_true(is.null(res$sim2$safe.list$newdfname))
    expect_equal(res$sim2$safe.list$output.type, "response")
    expect_true(is.null(res$sim2$safe.list$dispersion))
    expect_equal(res$sim2$safe.list$fit.Ntotal, 2526)
    expect_equal(res$sim2$safe.list$fit.Nvalid, 2526)
    expect_equal(res$sim2$safe.list$fit.Nmiss, 0)
    expect_equal(res$sim2$safe.list$fit.mean, 5.843564, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.sd, 0.3027628, tolerance = ds.test_env$tolerance)
    expect_length(res$sim2$safe.list$fit.quantiles, 7)
    expect_equal(class(res$sim2$safe.list$fit.quantiles), "numeric")
    expect_equal(res$sim2$safe.list$fit.quantiles[[1]], 5.692873, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.quantiles[[2]], 5.725140, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.quantiles[[3]], 5.782866, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.quantiles[[4]], 5.843818, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.quantiles[[5]], 5.902650, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.quantiles[[6]], 5.958954, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.quantiles[[7]], 5.990324, tolerance = ds.test_env$tolerance)

    expect_length(res$sim3, 1)
    expect_length(res$sim3$safe.list, 10)
    expect_equal(class(res$sim3$safe.list), "list")
    expect_equal(res$sim3$safe.list$glm.object, "gaussian.glmslma.obj")
    expect_true(is.null(res$sim3$safe.list$newdfname))
    expect_equal(res$sim3$safe.list$output.type, "response")
    expect_true(is.null(res$sim3$safe.list$dispersion))
    expect_equal(res$sim3$safe.list$fit.Ntotal, 3473)
    expect_equal(res$sim3$safe.list$fit.Nvalid, 3473)
    expect_equal(res$sim3$safe.list$fit.Nmiss, 0)
    expect_equal(res$sim3$safe.list$fit.mean, 5.846405, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.sd, 0.3546622, tolerance = ds.test_env$tolerance)
    expect_length(res$sim3$safe.list$fit.quantiles, 7)
    expect_equal(class(res$sim3$safe.list$fit.quantiles), "numeric")
    expect_equal(res$sim3$safe.list$fit.quantiles[[1]], 5.639923, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.quantiles[[2]], 5.692488, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.quantiles[[3]], 5.763677, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.quantiles[[4]], 5.845627, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.quantiles[[5]], 5.928798, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.quantiles[[6]], 6.004785, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.quantiles[[7]], 6.054574, tolerance = ds.test_env$tolerance)
})

context("ds.glmPredict::smk::gaussian::with_newobj")
test_that("simple glmPredict, gaussian, with newobj, se.fit=FALSE", {
    glmSLMA.res <- ds.glmSLMA('D$LAB_TSC~D$LAB_TRIG', family="gaussian", newobj="gaussian.glmslma.obj")

    expect_length(glmSLMA.res, 9)
    expect_equal(glmSLMA.res$num.valid.studies, 3)
    expect_length(glmSLMA.res$validity.check, 1)
    expect_equal(glmSLMA.res$validity.check, "<gaussian.glmslma.obj> appears valid in all sources")

    res <- ds.glmPredict("gaussian.glmslma.obj", output.type = "response", se.fit = FALSE, newobj="gaussian.glm.predict.obj")
    
    expect_length(res, 3)
    expect_equal(class(res), "list")

    expect_length(res$sim1, 1)
    expect_length(res$sim1$safe.list, 10)
    expect_equal(class(res$sim1$safe.list), "list")
    expect_equal(res$sim1$safe.list$glm.object, "gaussian.glmslma.obj")
    expect_true(is.null(res$sim1$safe.list$newdfname))
    expect_equal(res$sim1$safe.list$output.type, "response")
    expect_true(is.null(res$sim1$safe.list$dispersion))
    expect_equal(res$sim1$safe.list$fit.Ntotal, 1801)
    expect_equal(res$sim1$safe.list$fit.Nvalid, 1801)
    expect_equal(res$sim1$safe.list$fit.Nmiss, 0)
    expect_equal(res$sim1$safe.list$fit.mean, 5.872024, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.sd, 0.3719756, tolerance = ds.test_env$tolerance)
    expect_length(res$sim1$safe.list$fit.quantiles, 7)
    expect_equal(class(res$sim1$safe.list$fit.quantiles), "numeric")
    expect_equal(res$sim1$safe.list$fit.quantiles[[1]], 5.656127, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.quantiles[[2]], 5.703222, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.quantiles[[3]], 5.778930, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.quantiles[[4]], 5.873137, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.quantiles[[5]], 5.961123, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.quantiles[[6]], 6.044376, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.quantiles[[7]], 6.091611, tolerance = ds.test_env$tolerance)

    expect_length(res$sim2, 1)
    expect_length(res$sim2$safe.list, 10)
    expect_equal(class(res$sim2$safe.list), "list")
    expect_equal(res$sim2$safe.list$glm.object, "gaussian.glmslma.obj")
    expect_true(is.null(res$sim2$safe.list$newdfname))
    expect_equal(res$sim2$safe.list$output.type, "response")
    expect_true(is.null(res$sim2$safe.list$dispersion))
    expect_equal(res$sim2$safe.list$fit.Ntotal, 2526)
    expect_equal(res$sim2$safe.list$fit.Nvalid, 2526)
    expect_equal(res$sim2$safe.list$fit.Nmiss, 0)
    expect_equal(res$sim2$safe.list$fit.mean, 5.843564, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.sd, 0.3027628, tolerance = ds.test_env$tolerance)
    expect_length(res$sim2$safe.list$fit.quantiles, 7)
    expect_equal(class(res$sim2$safe.list$fit.quantiles), "numeric")
    expect_equal(res$sim2$safe.list$fit.quantiles[[1]], 5.692873, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.quantiles[[2]], 5.725140, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.quantiles[[3]], 5.782866, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.quantiles[[4]], 5.843818, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.quantiles[[5]], 5.902650, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.quantiles[[6]], 5.958954, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.quantiles[[7]], 5.990324, tolerance = ds.test_env$tolerance)

    expect_length(res$sim3, 1)
    expect_length(res$sim3$safe.list, 10)
    expect_equal(class(res$sim3$safe.list), "list")
    expect_equal(res$sim3$safe.list$glm.object, "gaussian.glmslma.obj")
    expect_true(is.null(res$sim3$safe.list$newdfname))
    expect_equal(res$sim3$safe.list$output.type, "response")
    expect_true(is.null(res$sim3$safe.list$dispersion))
    expect_equal(res$sim3$safe.list$fit.Ntotal, 3473)
    expect_equal(res$sim3$safe.list$fit.Nvalid, 3473)
    expect_equal(res$sim3$safe.list$fit.Nmiss, 0)
    expect_equal(res$sim3$safe.list$fit.mean, 5.846405, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.sd, 0.3546622, tolerance = ds.test_env$tolerance)
    expect_length(res$sim3$safe.list$fit.quantiles, 7)
    expect_equal(class(res$sim3$safe.list$fit.quantiles), "numeric")
    expect_equal(res$sim3$safe.list$fit.quantiles[[1]], 5.639923, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.quantiles[[2]], 5.692488, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.quantiles[[3]], 5.763677, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.quantiles[[4]], 5.845627, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.quantiles[[5]], 5.928798, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.quantiles[[6]], 6.004785, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.quantiles[[7]], 6.054574, tolerance = ds.test_env$tolerance)
})

context("ds.glmPredict::smk::gaussian::sefit_true")
test_that("simple glmPredict, gaussian, with newobj, se.fit=TRUE", {
    glmSLMA.res <- ds.glmSLMA('D$LAB_TSC~D$LAB_TRIG', family="gaussian", newobj="gaussian.glmslma.obj")
    
    expect_length(glmSLMA.res, 9)
    expect_equal(glmSLMA.res$num.valid.studies, 3)
    expect_length(glmSLMA.res$validity.check, 1)
    expect_equal(glmSLMA.res$validity.check, "<gaussian.glmslma.obj> appears valid in all sources")
    
    res <- ds.glmPredict("gaussian.glmslma.obj", newdataname = NULL, output.type = "response", se.fit = TRUE, na.action = "na.pass", newobj="gaussian.glm.predict.sefit.obj")
   
    expect_length(res, 3)
    expect_equal(class(res), "list")
    
    expect_length(res$sim1, 1)
    expect_length(res$sim1$safe.list, 17)
    expect_equal(class(res$sim1$safe.list), "list")
    expect_equal(res$sim1$safe.list$glm.object, "gaussian.glmslma.obj")
    expect_true(is.null(res$sim1$safe.list$newdfname))
    expect_equal(res$sim1$safe.list$output.type, "response")
    expect_true(is.null(res$sim1$safe.list$dispersion))
    expect_equal(res$sim1$safe.list$fit.Ntotal, 1801)
    expect_equal(res$sim1$safe.list$fit.Nvalid, 1801)
    expect_equal(res$sim1$safe.list$fit.Nmiss, 0)
    expect_equal(res$sim1$safe.list$fit.mean, 5.872024, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.sd, 0.1383659, tolerance = ds.test_env$tolerance)
    expect_length(res$sim1$safe.list$fit.quantiles, 7)
    expect_equal(class(res$sim1$safe.list$fit.quantiles), "numeric")
    expect_equal(res$sim1$safe.list$fit.quantiles[[1]], 5.656127, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.quantiles[[2]], 5.703222, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.quantiles[[3]], 5.778930, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.quantiles[[4]], 5.873137, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.quantiles[[5]], 5.961123, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.quantiles[[6]], 6.044376, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$fit.quantiles[[7]], 6.091611, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$se.fit.Ntotal, 1801)
    expect_equal(res$sim1$safe.list$se.fit.Nvalid, 1801)
    expect_equal(res$sim1$safe.list$se.fit.Nmiss, 0)
    expect_equal(res$sim1$safe.list$se.fit.mean, 0.03490339, tolerance = 1e-8)
    expect_equal(res$sim1$safe.list$se.fit.sd, 0.01127756, tolerance = 1e-8)
    expect_length(res$sim1$safe.list$se.fit.quantiles, 7)
    expect_equal(class(res$sim1$safe.list$se.fit.quantiles), "numeric")
    expect_equal(res$sim1$safe.list$se.fit.quantiles[[1]], 0.02600046, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$se.fit.quantiles[[2]], 0.02614820, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$se.fit.quantiles[[3]], 0.02722465, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$se.fit.quantiles[[4]], 0.03103519, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$se.fit.quantiles[[5]], 0.03876266, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$se.fit.quantiles[[6]], 0.04856302, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$se.fit.quantiles[[7]], 0.05607628, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim1$safe.list$residual.scale, 1.10068, tolerance = ds.test_env$tolerance)
    
    
    expect_length(res$sim2, 1)
    expect_length(res$sim2$safe.list, 17)
    expect_equal(class(res$sim2$safe.list), "list")
    expect_equal(res$sim2$safe.list$glm.object, "gaussian.glmslma.obj")
    expect_true(is.null(res$sim2$safe.list$newdfname))
    expect_equal(res$sim2$safe.list$output.type, "response")
    expect_true(is.null(res$sim2$safe.list$dispersion))
    expect_equal(res$sim2$safe.list$fit.Ntotal, 2526)
    expect_equal(res$sim2$safe.list$fit.Nvalid, 2526)
    expect_equal(res$sim2$safe.list$fit.Nmiss, 0)
    expect_equal(res$sim2$safe.list$fit.mean, 5.843564, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.sd, 0.0916653, tolerance = ds.test_env$tolerance)
    expect_length(res$sim2$safe.list$fit.quantiles, 7)
    expect_equal(class(res$sim2$safe.list$fit.quantiles), "numeric")
    expect_equal(res$sim2$safe.list$fit.quantiles[[1]], 5.692873, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.quantiles[[2]], 5.725140, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.quantiles[[3]], 5.782866, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.quantiles[[4]], 5.843818, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.quantiles[[5]], 5.902650, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.quantiles[[6]], 5.958954, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$fit.quantiles[[7]], 5.990324, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$se.fit.Ntotal, 2526)
    expect_equal(res$sim2$safe.list$se.fit.Nvalid, 2526)
    expect_equal(res$sim2$safe.list$se.fit.Nmiss, 0)
    expect_equal(res$sim2$safe.list$se.fit.mean, 0.02855807, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$se.fit.sd, 0.009079763, tolerance = ds.test_env$tolerance)
    expect_length(res$sim2$safe.list$se.fit.quantiles, 7)
    expect_equal(class(res$sim2$safe.list$se.fit.quantiles), "numeric")
    expect_equal(res$sim2$safe.list$se.fit.quantiles[[1]], 0.02121918, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$se.fit.quantiles[[2]], 0.02131688, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$se.fit.quantiles[[3]], 0.02207091, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$se.fit.quantiles[[4]], 0.02530085, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$se.fit.quantiles[[5]], 0.03215874, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$se.fit.quantiles[[6]], 0.04034853, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$se.fit.quantiles[[7]], 0.04605307, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim2$safe.list$residual.scale, 1.06496, tolerance = ds.test_env$tolerance)
    
    expect_length(res$sim3, 1)
    expect_length(res$sim3$safe.list, 17)
    expect_equal(class(res$sim3$safe.list), "list")
    expect_equal(res$sim3$safe.list$glm.object, "gaussian.glmslma.obj")
    expect_true(is.null(res$sim3$safe.list$newdfname))
    expect_equal(res$sim3$safe.list$output.type, "response")
    expect_true(is.null(res$sim3$safe.list$dispersion))
    expect_equal(res$sim3$safe.list$fit.Ntotal, 3473)
    expect_equal(res$sim3$safe.list$fit.Nvalid, 3473)
    expect_equal(res$sim3$safe.list$fit.Nmiss, 0)
    expect_equal(res$sim3$safe.list$fit.mean, 5.846405, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.sd, 0.1257853, tolerance = ds.test_env$tolerance)
    expect_length(res$sim3$safe.list$fit.quantiles, 7)
    expect_equal(class(res$sim3$safe.list$fit.quantiles), "numeric")
    expect_equal(res$sim3$safe.list$fit.quantiles[[1]], 5.639923, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.quantiles[[2]], 5.692488, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.quantiles[[3]], 5.763677, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.quantiles[[4]], 5.845627, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.quantiles[[5]], 5.928798, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.quantiles[[6]], 6.004785, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$fit.quantiles[[7]], 6.054574, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$se.fit.Ntotal, 3473)
    expect_equal(res$sim3$safe.list$se.fit.Nvalid, 3473)
    expect_equal(res$sim3$safe.list$se.fit.Nmiss, 0)
    expect_equal(res$sim3$safe.list$se.fit.mean, 0.02423093, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$se.fit.sd, 0.007641005, tolerance = ds.test_env$tolerance)
    expect_length(res$sim3$safe.list$se.fit.quantiles, 7)
    expect_equal(class(res$sim3$safe.list$se.fit.quantiles), "numeric")
    expect_equal(res$sim3$safe.list$se.fit.quantiles[[1]], 0.01801017, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$se.fit.quantiles[[2]], 0.01813653, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$se.fit.quantiles[[3]], 0.01882936, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$se.fit.quantiles[[4]], 0.02149076, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$se.fit.quantiles[[5]], 0.02687285, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$se.fit.quantiles[[6]], 0.03463314, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$se.fit.quantiles[[7]], 0.03887559, tolerance = ds.test_env$tolerance)
    expect_equal(res$sim3$safe.list$residual.scale, 1.0587356, tolerance = ds.test_env$tolerance)
})

#
# Shutdown
#

context("ds.glmPredict::smk::gaussian::shutdown")

test_that("shutdown", {
  ds_expect_variables(c("D", "gaussian.glm.predict.obj", "gaussian.glm.predict.sefit.obj", "gaussian.glmslma.obj", "predict_glm" ))
})

disconnect.studies.dataset.cnsim()

#
# Done
#

context("ds.glmPredict::smk::gaussian::done")

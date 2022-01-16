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

context("ds.elspline::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TRIG", "PM_BMI_CONTINUOUS"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.elspline::smk::test1")
test_that("elspline", {
   
    ds.elspline(x="D$PM_BMI_CONTINUOUS", n=3, newobj="elsplineDS", datasources=ds.test_env$connections)

    res.class <- ds.class("elsplineDS", datasources=ds.test_env$connections)

    expect_length(res.class, 3)
    expect_equal(res.class$sim1[1], "lspline")
    expect_equal(res.class$sim1[2], "matrix")
    expect_equal(res.class$sim2[1], "lspline")
    expect_equal(res.class$sim2[2], "matrix")
    expect_equal(res.class$sim3[1], "lspline")
    expect_equal(res.class$sim3[2], "matrix")
    
    res.mod <- ds.glm(formula = "D$LAB_TRIG~elsplineDS", family='gaussian', datasources=ds.test_env$connections)
    
    expect_length(res.mod, 13)
    expect_equal(res.mod$Nvalid, 7477)
    expect_equal(res.mod$Nmissing, 1902)
    expect_equal(res.mod$Ntotal, 9379)
    expect_length(res.mod$disclosure.risk, 3)
    expect_equal(res.mod$disclosure[1], 0)
    expect_equal(res.mod$disclosure[3], 0)
    expect_equal(res.mod$disclosure[2], 0)
    expect_length(res.mod$errorMessage, 3)
    expect_equal(res.mod$errorMessage[1], "No errors")
    expect_equal(res.mod$errorMessage[2], "No errors")
    expect_equal(res.mod$errorMessage[3], "No errors")
    expect_equal(res.mod$nsubs, 7477)
    expect_equal(res.mod$iter, 3)
    expect_true("family" %in% class(res.mod$family))
    expect_equal(res.mod$formula, "D$LAB_TRIG ~ elsplineDS")
    expect_true("matrix" %in% class(res.mod$coefficients))
    expect_true("array" %in% class(res.mod$coefficients))
    expect_equal(res.mod$coefficients['(Intercept)','Estimate'], -0.35935028)
    expect_equal(res.mod$coefficients['elsplineDS1','Estimate'], 0.09176889)
    expect_equal(res.mod$coefficients['elsplineDS2','Estimate'], 0.06794047)
    expect_equal(res.mod$coefficients['elsplineDS3','Estimate'], 0.21256245)
    expect_equal(res.mod$coefficients['(Intercept)','Std. Error'], 0.144607412)
    expect_equal(res.mod$coefficients['elsplineDS1','Std. Error'], 0.006245815)
    expect_equal(res.mod$coefficients['elsplineDS2','Std. Error'], 0.004274001)
    expect_equal(res.mod$coefficients['elsplineDS3','Std. Error'], 0.021359728)
    expect_equal(res.mod$dev, 16981.52, tolerance=0.00001)
    expect_equal(res.mod$df, 7473)
    expect_equal(res.mod$output.information, "SEE TOP OF OUTPUT FOR INFORMATION ON MISSING DATA AND ERROR MESSAGES")
    
})


context("ds.elspline::smk::test2")
test_that("elspline", {
    
    ds.elspline(x="D$PM_BMI_CONTINUOUS", n=5, newobj="elsplineDS2", datasources=ds.test_env$connections)
    
    res.class <- ds.class("elsplineDS2", datasources=ds.test_env$connections)
    
    expect_length(res.class, 3)
    expect_equal(res.class$sim1[1], "lspline")
    expect_equal(res.class$sim1[2], "matrix")
    expect_equal(res.class$sim2[1], "lspline")
    expect_equal(res.class$sim2[2], "matrix")
    expect_equal(res.class$sim3[1], "lspline")
    expect_equal(res.class$sim3[2], "matrix")
    
    res.mod <- ds.glm(formula = "D$LAB_TRIG~elsplineDS2", family='gaussian', datasources=ds.test_env$connections)
    
    expect_length(res.mod, 13)
    expect_equal(res.mod$Nvalid, 7477)
    expect_equal(res.mod$Nmissing, 1902)
    expect_equal(res.mod$Ntotal, 9379)
    expect_length(res.mod$disclosure.risk, 3)
    expect_equal(res.mod$disclosure[1], 0)
    expect_equal(res.mod$disclosure[3], 0)
    expect_equal(res.mod$disclosure[2], 0)
    expect_length(res.mod$errorMessage, 3)
    expect_equal(res.mod$errorMessage[1], "No errors")
    expect_equal(res.mod$errorMessage[2], "No errors")
    expect_equal(res.mod$errorMessage[3], "No errors")
    expect_equal(res.mod$nsubs, 7477)
    expect_equal(res.mod$iter, 3)
    expect_true("family" %in% class(res.mod$family))
    expect_equal(res.mod$formula, "D$LAB_TRIG ~ elsplineDS2")
    expect_true("matrix" %in% class(res.mod$coefficients))
    expect_true("array" %in% class(res.mod$coefficients))
    expect_equal(res.mod$coefficients['(Intercept)','Estimate'], -0.29808175)
    expect_equal(res.mod$coefficients['elsplineDS21','Estimate'], 0.08295113)
    expect_equal(res.mod$coefficients['elsplineDS22','Estimate'], 0.10199060)
    expect_equal(res.mod$coefficients['elsplineDS23','Estimate'], 0.05891621)
    expect_equal(res.mod$coefficients['elsplineDS24','Estimate'], 0.07761427)
    expect_equal(res.mod$coefficients['elsplineDS25','Estimate'], 0.65329224)
    expect_equal(res.mod$coefficients['(Intercept)','Std. Error'], 0.142080199)
    expect_equal(res.mod$coefficients['elsplineDS21','Std. Error'], 0.007062320)
    expect_equal(res.mod$coefficients['elsplineDS22','Std. Error'], 0.009361885)
    expect_equal(res.mod$coefficients['elsplineDS23','Std. Error'], 0.007798950)
    expect_equal(res.mod$coefficients['elsplineDS24','Std. Error'], 0.016254429)
    expect_equal(res.mod$coefficients['elsplineDS25','Std. Error'], 0.067541719)
    expect_equal(res.mod$dev, 16876.12, tolerance=0.00001)
    expect_equal(res.mod$df, 7471)
    expect_equal(res.mod$output.information, "SEE TOP OF OUTPUT FOR INFORMATION ON MISSING DATA AND ERROR MESSAGES")
    
})


#
# Done
#

context("ds.elspline::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "elsplineDS", "elsplineDS2", "LAB_TRIG"))
})

disconnect.studies.dataset.cnsim()

context("ds.elspline::smk::done")

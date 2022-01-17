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

context("ds.qlspline::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TRIG", "PM_BMI_CONTINUOUS"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.qlspline::smk::test1")
test_that("qlspline", {
   
    ds.qlspline(x="D$PM_BMI_CONTINUOUS", q=4, na.rm=TRUE, newobj="qlsplineDS", datasources=ds.test_env$connections)

    res.class <- ds.class("qlsplineDS", datasources=ds.test_env$connections)

    expect_length(res.class, 3)
    expect_equal(res.class$sim1[1], "lspline")
    expect_equal(res.class$sim1[2], "matrix")
    expect_equal(res.class$sim2[1], "lspline")
    expect_equal(res.class$sim2[2], "matrix")
    expect_equal(res.class$sim3[1], "lspline")
    expect_equal(res.class$sim3[2], "matrix")
    
    res.mod <- ds.glm(formula = "D$LAB_TRIG~qlsplineDS", family='gaussian', datasources=ds.test_env$connections)
    
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
    expect_equal(res.mod$formula, "D$LAB_TRIG ~ qlsplineDS")
    expect_true("matrix" %in% class(res.mod$coefficients))
    expect_true("array" %in% class(res.mod$coefficients))
    expect_equal(res.mod$coefficients['(Intercept)','Estimate'], -1.019282794)
    expect_equal(res.mod$coefficients['qlsplineDS1','Estimate'], 0.120196255)
    expect_equal(res.mod$coefficients['qlsplineDS2','Estimate'], 0.067803329)
    expect_equal(res.mod$coefficients['qlsplineDS3','Estimate'], 0.006651009)
    expect_equal(res.mod$coefficients['qlsplineDS4','Estimate'], 0.133116769)
    expect_equal(res.mod$coefficients['(Intercept)','Std. Error'], 0.27768921)
    expect_equal(res.mod$coefficients['qlsplineDS1','Std. Error'], 0.01259812)
    expect_equal(res.mod$coefficients['qlsplineDS2','Std. Error'], 0.02097099)
    expect_equal(res.mod$coefficients['qlsplineDS3','Std. Error'], 0.02072403)
    expect_equal(res.mod$coefficients['qlsplineDS4','Std. Error'], 0.01201930)
    expect_equal(res.mod$dev, 17000.26, tolerance=0.00001)
    expect_equal(res.mod$df, 7472)
    expect_equal(res.mod$output.information, "SEE TOP OF OUTPUT FOR INFORMATION ON MISSING DATA AND ERROR MESSAGES")
    
})


context("ds.qlspline::smk::test2")
test_that("qlspline", {
    
    ds.qlspline(x="D$PM_BMI_CONTINUOUS", q=c(0.3,0.6), na.rm=TRUE, newobj="qlsplineDS2", datasources=ds.test_env$connections)

    res.class <- ds.class("qlsplineDS2", datasources=ds.test_env$connections)
    
    expect_length(res.class, 3)
    expect_equal(res.class$sim1[1], "lspline")
    expect_equal(res.class$sim1[2], "matrix")
    expect_equal(res.class$sim2[1], "lspline")
    expect_equal(res.class$sim2[2], "matrix")
    expect_equal(res.class$sim3[1], "lspline")
    expect_equal(res.class$sim3[2], "matrix")
    
    res.mod <- ds.glm(formula = "D$LAB_TRIG~qlsplineDS2", family='gaussian', datasources=ds.test_env$connections)
    
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
    expect_equal(res.mod$formula, "D$LAB_TRIG ~ qlsplineDS2")
    expect_true("matrix" %in% class(res.mod$coefficients))
    expect_true("array" %in% class(res.mod$coefficients))
    expect_equal(res.mod$coefficients['(Intercept)','Estimate'], -1.04058508)
    expect_equal(res.mod$coefficients['qlsplineDS21','Estimate'], 0.12128692)
    expect_equal(res.mod$coefficients['qlsplineDS22','Estimate'], 0.01708104)
    expect_equal(res.mod$coefficients['qlsplineDS23','Estimate'], 0.10189071)
    expect_equal(res.mod$coefficients['(Intercept)','Std. Error'], 0.248311459)
    expect_equal(res.mod$coefficients['qlsplineDS21','Std. Error'], 0.010959394)
    expect_equal(res.mod$coefficients['qlsplineDS22','Std. Error'], 0.015233708)
    expect_equal(res.mod$coefficients['qlsplineDS23','Std. Error'], 0.008551969)
    expect_equal(res.mod$dev, 17031.72, tolerance=0.00001)
    expect_equal(res.mod$df, 7473)
    expect_equal(res.mod$output.information, "SEE TOP OF OUTPUT FOR INFORMATION ON MISSING DATA AND ERROR MESSAGES")
    
})


#
# Done
#

context("ds.qlspline::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "LAB_TRIG", "qlsplineDS", "qlsplineDS2"))
})

disconnect.studies.dataset.cnsim()

context("ds.qlspline::smk::done")

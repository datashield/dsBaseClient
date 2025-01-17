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

context("ds.lspline::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TRIG", "PM_BMI_CONTINUOUS"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.lspline::smk::test1")
test_that("lspline", {
   
    ds.lspline(x="D$PM_BMI_CONTINUOUS", knots=c(15,25,35), newobj="lsplineDS", datasources=ds.test_env$connections)

    res.class <- ds.class("lsplineDS", datasources=ds.test_env$connections)

    expect_length(res.class, 3)
    expect_equal(res.class$sim1[1], "lspline")
    expect_equal(res.class$sim1[2], "matrix")
    expect_equal(res.class$sim2[1], "lspline")
    expect_equal(res.class$sim2[2], "matrix")
    expect_equal(res.class$sim3[1], "lspline")
    expect_equal(res.class$sim3[2], "matrix")
    
    res.mod <- ds.glm(formula = "D$LAB_TRIG~lsplineDS", family='gaussian', datasources=ds.test_env$connections)
    
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
    expect_equal(res.mod$formula, "D$LAB_TRIG ~ lsplineDS")
    expect_true("matrix" %in% class(res.mod$coefficients))
    expect_true("array" %in% class(res.mod$coefficients))
    expect_equal(res.mod$coefficients['(Intercept)','Estimate'], -5.46584429, tolerance = ds.test_env$tolerance)
    expect_equal(res.mod$coefficients['lsplineDS1','Estimate'], 0.42499237, tolerance = ds.test_env$tolerance)
    expect_equal(res.mod$coefficients['lsplineDS2','Estimate'], 0.10396672, tolerance = ds.test_env$tolerance)
    expect_equal(res.mod$coefficients['lsplineDS3','Estimate'], 0.05051422, tolerance = ds.test_env$tolerance)
    expect_equal(res.mod$coefficients['lsplineDS4','Estimate'], 0.24522708, tolerance = ds.test_env$tolerance)
    expect_equal(res.mod$coefficients['(Intercept)','Std. Error'], 1.340072623, tolerance = ds.test_env$tolerance)
    expect_equal(res.mod$coefficients['lsplineDS1','Std. Error'], 0.090653306, tolerance = ds.test_env$tolerance)
    expect_equal(res.mod$coefficients['lsplineDS2','Std. Error'], 0.010498879, tolerance = ds.test_env$tolerance)
    expect_equal(res.mod$coefficients['lsplineDS3','Std. Error'], 0.006418973, tolerance = ds.test_env$tolerance)
    expect_equal(res.mod$coefficients['lsplineDS4','Std. Error'], 0.023139549, tolerance = ds.test_env$tolerance)
    expect_equal(res.mod$dev, 16909.8, tolerance = ds.test_env$tolerance)
    expect_equal(res.mod$df, 7472)
    expect_equal(res.mod$output.information, "SEE TOP OF OUTPUT FOR INFORMATION ON MISSING DATA AND ERROR MESSAGES")
    
})



#
# Done
#

context("ds.lspline::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "LAB_TRIG", "lsplineDS"))
})

disconnect.studies.dataset.cnsim()

context("ds.qlspline::smk::done")

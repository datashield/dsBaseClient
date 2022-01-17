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

context("ds.ns::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TRIG", "PM_BMI_CONTINUOUS"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.ns::smk::test1")
test_that("ns", {
   
    ds.ns(x="D$PM_BMI_CONTINUOUS", knots=c(15,25,35), newobj="nsDS", datasources=ds.test_env$connections)

    res.class <- ds.class("nsDS", datasources=ds.test_env$connections)

    expect_length(res.class, 3)
    expect_equal(res.class$sim1[1], "ns")
    expect_equal(res.class$sim1[2], "basis")
    expect_equal(res.class$sim1[3], "matrix")
    expect_equal(res.class$sim2[1], "ns")
    expect_equal(res.class$sim2[2], "basis")
    expect_equal(res.class$sim2[3], "matrix")
    expect_equal(res.class$sim3[1], "ns")
    expect_equal(res.class$sim3[2], "basis")
    expect_equal(res.class$sim3[3], "matrix")
    
    
    res.mod <- ds.glm(formula = "D$LAB_TRIG~nsDS", family='gaussian', datasources=ds.test_env$connections)
    
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
    expect_equal(res.mod$formula, "D$LAB_TRIG ~ nsDS")
    expect_true("matrix" %in% class(res.mod$coefficients))
    expect_true("array" %in% class(res.mod$coefficients))
    expect_equal(res.mod$coefficients['(Intercept)','Estimate'], -0.1844121)
    expect_equal(res.mod$coefficients['nsDS1','Estimate'], 2.4510178)
    expect_equal(res.mod$coefficients['nsDS2','Estimate'], 1.8749516)
    expect_equal(res.mod$coefficients['nsDS3','Estimate'], 5.9260697)
    expect_equal(res.mod$coefficients['nsDS4','Estimate'], 6.3067806)
    expect_equal(res.mod$coefficients['(Intercept)','Std. Error'], 0.4190860, tolerance=0.00001)
    expect_equal(res.mod$coefficients['nsDS1','Std. Error'], 0.3972526, tolerance=0.00001)
    expect_equal(res.mod$coefficients['nsDS2','Std. Error'], 0.3097700, tolerance=0.00001)
    expect_equal(res.mod$coefficients['nsDS3','Std. Error'], 0.8376620, tolerance=0.00001)
    expect_equal(res.mod$coefficients['nsDS4','Std. Error'], 0.4078086, tolerance=0.00001)
    expect_equal(res.mod$dev, 16938.36, tolerance=0.00001)
    expect_equal(res.mod$df, 7472)
    expect_equal(res.mod$output.information, "SEE TOP OF OUTPUT FOR INFORMATION ON MISSING DATA AND ERROR MESSAGES")
    
})



#
# Done
#

context("ds.ns::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "LAB_TRIG", "nsDS"))
})

disconnect.studies.dataset.cnsim()

context("ds.ns::smk::done")

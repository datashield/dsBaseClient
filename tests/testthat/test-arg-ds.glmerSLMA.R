#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.glmerSLMA::arg::setup")

connect.studies.dataset.cluster.int(list("incid_rate", "trtGrp", "Male", "idDoctor", "idSurgery"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.glmerSLMA::arg")
test_that("simple glmerSLMA tesing (mis)use of arguments", {
    expect_error(ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', dataName = 'D'), " Please provide a valid 'family' argument!", fixed=TRUE)
    expect_error(ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male', dataName = 'D', family = 'poisson'), "object 'mg' not found", fixed=TRUE)
    expect_error(ds.glmerSLMA(formula = 'diab_dis ~ trtGrp + Male + (1|idDoctor)', family='poisson', dataName = 'D'), "Command 'glmerSLMADS2(diab_dis ~ trtGrp + Male + yyy1xxxidDoctorzzz, NULL, \n    NULL, \"D\", \"poisson\", NULL, NULL, 0, NULL, NULL)' failed on 'cluster.int1': Error while evaluating 'dsBase::glmerSLMADS2(diab_dis~trtGrp+Male+yyy1xxxidDoctorzzz, NULL, NULL, \"D\", \"poisson\", NULL, NULL, 0, NULL, NULL)' -> Error in eval(predvars, data, env) : object 'diab_dis' not found\n", fixed=TRUE)
    res <- ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', family='poisson', dataName = 'D', control_type = 'xtol_rel')
    expect_equal(res$errorMessage, "ERROR: if control_type is non-null, you must specify a valid control_value eg control_value<-1.0e-7")
    expect_error(ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', family='poisson', dataName = 'D', control_type = 'check.conv.grad',control_value = 'nothing'), "Command 'glmerSLMADS2(incid_rate ~ trtGrp + Male + yyy1xxxidDoctorzzz, \n    NULL, NULL, \"D\", \"poisson\", \"check.conv.grad\", \"nothing\", \n    0, NULL, NULL)' failed on 'cluster.int1': Error while evaluating 'dsBase::glmerSLMADS2(incid_rate~trtGrp+Male+yyy1xxxidDoctorzzz, NULL, NULL, \"D\", \"poisson\", \"check.conv.grad\", \"nothing\", 0, NULL, NULL)' -> Error in lme4::.makeCC(\"warning\", tol = NA, relTol = NULL) : \n  must have a numeric 'tol' component\n", fixed=TRUE)
    expect_error(ds.glmerSLMA(), " Please provide a valid regression formula!", fixed=TRUE)
    
    res = ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', family='poisson', dataName = "D", start_theta = c(1))
    expect_length(res, 8)
    expect_error(ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', family='poisson', dataName = "D", start_theta = c(1,1,1)), "Error in summary(mg) : object 'mg' not found", fixed=TRUE)
    
    res = ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', family='poisson', dataName = "D", start_fixef = c(1,1,1), start_theta = c(1))
    expect_length(res, 8)

})

test_that("test offsets and weights", {
    ds.make('D$incid_rate/D$incid_rate', "some.weights")
    ds.make('D$incid_rate/D$incid_rate', "some.offsets")
    ds.dataFrame(x=c("D", "some.weights", "some.offsets"), newobj = "D2")
    res = ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', family='poisson', weights = "some.weights", dataName = "D")
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    res = ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', family='poisson', offset = "some.offsets", dataName = "D")
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    res = ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', family='poisson', weights = "D2$some.weights", dataName = "D")
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    res = ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', family='poisson', offset = "D2$some.offsets", dataName = "D")
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    
})

## try some different formulae structures?
test_that("alternative formulae for nested groups", {
    res = ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idSurgery/idDoctor)', family='poisson', dataName = "D")
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    res = ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idSurgery) +(1|idSurgery:idDoctor)', family='poisson', dataName = "D")
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    
})


#
# Switch to slope dataset
#

ds.ls()

context("ds.glmerSLMA::arg::switch")

disconnect.studies.dataset.cluster.int()

#
# Set up
#

context("ds.glmerSLMA::arg::setup")

connect.studies.dataset.cluster.slo(list("incid_rate", "trtGrp", "Male", "idDoctor", "BMI", "idSurgery"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.glmerSLMA::arg")

test_that("check slope formulae", {
    res = ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor) + (1|idSurgery) + (0+trtGrp|idSurgery)', family='poisson', dataName = 'D', control_type = 'check.conv.grad',control_value = 0.1)
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    res = ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor) + (trtGrp||idSurgery)', family='poisson', dataName = 'D', control_type = 'check.conv.grad',control_value = 0.1)
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    
})


#
# Done
#

context("ds.glmerSLMA::arg::shutdown")

disconnect.studies.dataset.cluster.slo()

context("ds.glmerSLMA::arg::done")

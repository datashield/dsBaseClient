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

context("ds.lmerSLMA::arg::setup")

connect.studies.dataset.cluster.int(list("incid_rate", "trtGrp", "Male", "idDoctor", "BMI", "idSurgery"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.lmerSLMA::arg")
test_that("simple lmerSLMA tesing (mis)use of arguments", {
    expect_error(ds.lmerSLMA(formula = 'incid_rate ~ trtGrp + Male', dataName = 'D'), "object 'mg' not found", fixed=TRUE)
    expect_error(ds.lmerSLMA(formula = 'diab_dis ~ trtGrp + Male + (1|idDoctor)', dataName = 'D'), "Command 'lmerSLMADS2(diab_dis ~ trtGrp + Male + yyy1xxxidDoctorzzz, NULL, \n    NULL, \"D\", TRUE, NULL, NULL, NULL, 0)' failed on 'cluster.int1': Error while evaluating 'dsBase::lmerSLMADS2(diab_dis~trtGrp+Male+yyy1xxxidDoctorzzz, NULL, NULL, \"D\", TRUE, NULL, NULL, NULL, 0)' -> Error in eval(predvars, data, env) : object 'diab_dis' not found\n", fixed=TRUE)
    res <- ds.lmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', dataName = 'D', control_type = 'xtol_rel')
    expect_equal(res$errorMessage, "ERROR: if control_type is non-null, you must specify a valid control_value eg control_value<-1.0e-7")
    expect_error(ds.lmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', dataName = 'D', control_type = 'xtol_rel',control_value = 'nothing'), "Command 'lmerSLMADS2(incid_rate ~ trtGrp + Male + yyy1xxxidDoctorzzz, \n    NULL, NULL, \"D\", TRUE, \"xtol_rel\", \"nothing\", NULL, 0)' failed on 'cluster.int1': Error while evaluating 'dsBase::lmerSLMADS2(incid_rate~trtGrp+Male+yyy1xxxidDoctorzzz, NULL, NULL, \"D\", TRUE, \"xtol_rel\", \"nothing\", NULL, 0)' -> Error in summary(mg) : object 'mg' not found\n", fixed=TRUE)
    expect_error(ds.lmerSLMA(), " Please provide a valid regression formula!", fixed=TRUE)
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idDoctor)', dataName = "D", REML = FALSE)
    expect_equal(res$output.summary$study1$methTitle, "Linear mixed model fit by maximum likelihood ")
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idSurgery)', dataName = 'D', optimizer = 'nloptwrap')
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    res=ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idSurgery)', dataName = 'D', optimizer = 'not_this_one')
    expect_equal(res$errorMessage, "ERROR: the only optimizer currently available for lmer is 'nloptwrap', please respecify")
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idDoctor)', dataName = "D", combine.with.metafor = FALSE)
    expect_length(res, 5)
    
})

## try some different formulae structures?
test_that("alternative formulae for nested groups", {
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idSurgery/idDoctor)', dataName = "D")
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idSurgery) +(1|idSurgery:idDoctor)', dataName = "D")
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    # different behaviour for normal DS versus DSLite...
    #res = ds.lmerSLMA(formula = 'D$BMI ~ D$trtGrp + D$Male + (1|D$idSurgery)')
    #expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
})


test_that("test offsets and weights", {
    ds.make('D$BMI/D$BMI', "some.weights")
    ds.make('D$BMI/D$BMI', "some.offsets")
    ds.dataFrame(x=c("D", "some.weights", "some.offsets"), newobj = "D2")
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idDoctor)', weights = "some.weights", dataName = "D")
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idDoctor)', offset = "some.offsets", dataName = "D")
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idDoctor)', weights = "D2$some.weights", dataName = "D")
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idDoctor)', offset = "D2$some.offsets", dataName = "D")
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    
})


#
# Switch to slope dataset
#

context("ds.lmerSLMA::arg::switch")

disconnect.studies.dataset.cluster.int()

#
# Set up
#

context("ds.lmerSLMA::arg::setup")

connect.studies.dataset.cluster.slo(list("incid_rate", "trtGrp", "Male", "idDoctor", "BMI", "idSurgery"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.lmerSLMA::arg")

test_that("check slope formulae", {
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idDoctor) + (1|idSurgery) + (0+trtGrp|idSurgery)', dataName = 'D', control_type = 'check.conv.grad',control_value = 0.1)
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idDoctor) + (trtGrp||idSurgery)', dataName = 'D', control_type = 'check.conv.grad',control_value = 0.1)
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    
})

#
# Done
#

ds.ls()

context("ds.lmerSLMA::arg::shutdown")


disconnect.studies.dataset.cluster.slo()

context("ds.lmerSLMA::arg::done")

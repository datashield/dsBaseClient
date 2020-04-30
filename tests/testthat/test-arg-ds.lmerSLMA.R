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
    expect_equal(res$errorMessage, "ERROR: if control_type is non-null, you must specify a valid control_value eg control_value<-1.0e-7", fixed=TRUE)
    expect_error(ds.lmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', dataName = 'D', control_type = 'xtol_rel',control_value = 'nothing'), "Command 'lmerSLMADS2(incid_rate ~ trtGrp + Male + yyy1xxxidDoctorzzz, \n    NULL, NULL, \"D\", TRUE, \"xtol_rel\", \"nothing\", NULL, 0)' failed on 'cluster.int1': Error while evaluating 'dsBase::lmerSLMADS2(incid_rate~trtGrp+Male+yyy1xxxidDoctorzzz, NULL, NULL, \"D\", TRUE, \"xtol_rel\", \"nothing\", NULL, 0)' -> Error in summary(mg) : object 'mg' not found\n", fixed=TRUE)
    expect_error(ds.lmerSLMA(), " Please provide a valid regression formula!", fixed=TRUE)
})

#
# Shutdown
#

context("ds.lmerSLMA::arg::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "offset", "offset.to.use", "weights", "weights.to.use"))
})

disconnect.studies.dataset.cluster.int()

#
# Done
#

context("ds.lmerSLMA::arg::done")

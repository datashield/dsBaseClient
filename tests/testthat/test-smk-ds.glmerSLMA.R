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
# Set up Phase 1
#

context("ds.glmerSLMA::smk::setup - phase 1")

connect.studies.dataset.cluster.int(list("incid_rate", "trtGrp", "Male", "idDoctor", "idSurgery"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests Phase 1
#

context("ds.glmerSLMA::smk::phase 1")
test_that("simple glmerSLMA tesing (mis)use of arguments", {
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

test_that("simple glmerSLMA", {
    res <- ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', family="poisson", dataName = "D")

    expect_length(res, 8)
})

#
# Shutdown phase 1
#

context("ds.glmerSLMA::smk::shutdown - phase 1")

test_that("setup", {
  #note the offset and weights objects below are artefacts 
    ds_expect_variables(c("D", "D2", "offset", "some.offsets", "some.weights", "weights"))
})

disconnect.studies.dataset.cluster.int()

#
# Set up phase 2
#

context("ds.glmerSLMA::smk::setup - phase 2")

connect.studies.dataset.cluster.slo(list("incid_rate", "trtGrp", "Male", "idDoctor", "BMI", "idSurgery"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests phase 2
#

context("ds.glmerSLMA::smk:test - phase 2")

test_that("check slope formulae", {
#    res = ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor) + (1|idSurgery) + (0+trtGrp|idSurgery)', family='poisson', dataName = 'D', control_type = 'check.conv.grad',control_value = 0.1)
#    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
#    res = ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor) + (trtGrp||idSurgery)', family='poisson', dataName = 'D', control_type = 'check.conv.grad',control_value = 0.1)
#    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")   
})


#
# Shutdown phase 2
#

context("ds.glmerSLMA::smk::shutdown - phase 2")

test_that("setup", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cluster.slo()

#
# Done
#

context("ds.glmerSLMA::smk::done")

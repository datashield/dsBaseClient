#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

# context("ds.lmerSLMA::arg::setup")

connect.studies.dataset.cluster.int(list("incid_rate", "trtGrp", "Male", "idDoctor", "BMI", "idSurgery"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.lmerSLMA::arg")
test_that("simple lmerSLMA tesing (mis)use of arguments", {
    res <- ds.lmerSLMA(formula = 'incid_rate ~ trtGrp + Male', dataName = 'D')
    expect_equal(res$study1$errorMessage, "No random effects terms specified in formula", fixed=TRUE)

    expect_error(ds.lmerSLMA(formula = 'diab_dis ~ trtGrp + Male + (1|idDoctor)', dataName = 'D'), "There are some DataSHIELD errors, list them with datashield.errors()", fixed=TRUE)

    errs <- datashield.errors()
    expect_length(errs, 3)
    expect_length(errs$sim1, 0)
    expect_length(errs$sim2, 0)
    expect_length(errs$sim3, 0)

    res <- ds.lmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', dataName = 'D', control_type = 'xtol_rel')
    expect_equal(res$errorMessage, "ERROR: if control_type is non-null, you must specify a valid control_value eg control_value<-1.0e-7", fixed=TRUE)
#    res <- ds.lmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', dataName = 'D', control_type = 'xtol_rel',control_value = 'nothing')
#    expect_equal(res$study1$errorMessage, "REAL() can only be applied to a 'numeric', not a 'logical'", fixed=TRUE)


    expect_error(ds.lmerSLMA(), " Please provide a valid regression formula!", fixed=TRUE)
})

#
# Shutdown
#

# context("ds.lmerSLMA::arg::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "offset", "offset.to.use", "weights", "weights.to.use"))
})

disconnect.studies.dataset.cluster.int()

#
# Done
#

# context("ds.lmerSLMA::arg::done")

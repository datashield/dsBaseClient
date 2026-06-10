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

# context("ds.glmerSLMA::arg::setup")

connect.studies.dataset.cluster.int(list("incid_rate", "trtGrp", "Male", "idDoctor", "idSurgery"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Test
#

# context("ds.glmerSLMA::arg::testing")
test_that("simple glmerSLMA tesing (mis)use of arguments", {
    expect_error(ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', dataName = 'D'), " Please provide a valid 'family' argument!", fixed=TRUE)

    expect_error(ds.glmerSLMA(formula = 'diab_dis ~ trtGrp + Male + (1|idDoctor)', family='poisson', dataName = 'D'), "There are some DataSHIELD errors, list them with datashield.errors()", fixed=TRUE)

    errs <- datashield.errors()
    expect_length(errs, 3)
    expect_length(errs$sim1, 0)
    expect_length(errs$sim2, 0)
    expect_length(errs$sim3, 0)

    res <- ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', family='poisson', dataName = 'D', control_type = 'xtol_rel')
    expect_equal(res$errorMessage, "ERROR: if control_type is non-null, you must specify a valid control_value eg control_value<-1.0e-7",fixed=TRUE)
    expect_error(ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', family='poisson', dataName = 'D', control_type = 'check.conv.grad',control_value = 'nothing'), "There are some DataSHIELD errors, list them with datashield.errors()", fixed=TRUE)

    errs <- datashield.errors()
    expect_length(errs, 3)
    expect_length(errs$sim1, 0)
    expect_length(errs$sim2, 0)
    expect_length(errs$sim3, 0)

    expect_error(ds.glmerSLMA(), " Please provide a valid regression formula!", fixed=TRUE)
})

#
# Shutdown
#

# context("ds.glmerSLMA::arg::shutdown")

test_that("setup", {
    ds_expect_variables(c("D", "offset", "weights"))
})

disconnect.studies.dataset.cluster.int()

#
# Done
#

# context("ds.glmerSLMA::arg::done")

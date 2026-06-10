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

# context("ds.var::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.var::smk::split")
test_that("simple var, split", {
    var.res <- ds.var(x = 'D$LAB_TSC', type='split')

    expect_length(var.res, 3)
    expect_length(var.res$Variance.by.Study, 12)
    expect_equal(var.res$Variance.by.Study[1], 1.229163, tolerance = .000001)
    expect_equal(var.res$Variance.by.Study[2], 1.140606, tolerance = .000001)
    expect_equal(var.res$Variance.by.Study[3], 1.134995, tolerance = .000001)
    expect_equal(var.res$Variance.by.Study[4], 356)
    expect_equal(var.res$Variance.by.Study[5], 549)
    expect_equal(var.res$Variance.by.Study[6], 649)
    expect_equal(var.res$Variance.by.Study[7], 1807)
    expect_equal(var.res$Variance.by.Study[8], 2539)
    expect_equal(var.res$Variance.by.Study[9], 3479)
    expect_equal(var.res$Variance.by.Study[10], 2163)
    expect_equal(var.res$Variance.by.Study[11], 3088)
    expect_equal(var.res$Variance.by.Study[12], 4128)
    expect_equal(var.res$Nstudies, 3)
    expect_length(var.res$ValidityMessage, 3)
    expect_equal(var.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(var.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(var.res$ValidityMessage[3], "VALID ANALYSIS")
})

# context("ds.var::smk::combine")
test_that("simple var, combine", {
    var.res <- ds.var(x = 'D$LAB_TSC', type='combine')

    expect_length(var.res, 3)
    expect_length(var.res$Global.Variance, 4)
    expect_equal(var.res$Global.Variance[1], 1.158384, tolerance = .000001)
    expect_equal(var.res$Global.Variance[2], 1554)
    expect_equal(var.res$Global.Variance[3], 7825)
    expect_equal(var.res$Global.Variance[4], 9379)
    expect_equal(var.res$Nstudies, 3)
    expect_length(var.res$ValidityMessage, 3)
    expect_equal(var.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(var.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(var.res$ValidityMessage[3], "VALID ANALYSIS")
})

# context("ds.var::smk::both")
test_that("simple var, both", {
    var.res <- ds.var(x = 'D$LAB_TSC', type='both')

    expect_length(var.res, 4)
    expect_length(var.res$Variance.by.Study, 12)
    expect_equal(var.res$Variance.by.Study[1], 1.229163, tolerance = .000001)
    expect_equal(var.res$Variance.by.Study[2], 1.140606, tolerance = .000001)
    expect_equal(var.res$Variance.by.Study[3], 1.134995, tolerance = .000001)
    expect_equal(var.res$Variance.by.Study[4], 356)
    expect_equal(var.res$Variance.by.Study[5], 549)
    expect_equal(var.res$Variance.by.Study[6], 649)
    expect_equal(var.res$Variance.by.Study[7], 1807)
    expect_equal(var.res$Variance.by.Study[8], 2539)
    expect_equal(var.res$Variance.by.Study[9], 3479)
    expect_equal(var.res$Variance.by.Study[10], 2163)
    expect_equal(var.res$Variance.by.Study[11], 3088)
    expect_equal(var.res$Variance.by.Study[12], 4128)
    expect_length(var.res$Global.Variance, 4)
    expect_equal(var.res$Global.Variance[1], 1.158384, tolerance = .000001)
    expect_equal(var.res$Global.Variance[2], 1554)
    expect_equal(var.res$Global.Variance[3], 7825)
    expect_equal(var.res$Global.Variance[4], 9379)
    expect_equal(var.res$Nstudies, 3)
    expect_length(var.res$ValidityMessage, 3)
    expect_equal(var.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(var.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(var.res$ValidityMessage[3], "VALID ANALYSIS")
})

#
# Done
#

# context("ds.var::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.var::smk::done")

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

# context("ds.levels::disc::setup")

connect.studies.dataset.cnsim(list("GENDER", "PM_BMI_CATEGORICAL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#
# context("ds.levels::disc")
test_that("simple levels", {
#    res <- ds.levels("D$GENDER")
    
#    expect_length(res, 3)
#    expect_length(res$sim1, 2)
#    expect_length(res$sim1$ValidityMessage, 1)
#    expect_equal(res$sim1$ValidityMessage, "VALID ANALYSIS")
#    expect_length(res$sim1$Levels, 2)
#    expect_equal(res$sim1$Levels, NA)
    
#    expect_length(res$sim2, 2)
#    expect_length(res$sim2$ValidityMessage, 1)
#    expect_equal(res$sim2$ValidityMessage, "VALID ANALYSIS")
#    expect_length(res$sim2$Levels, 2)
#    expect_equal(res$sim2$Levels, NA)
    
#    expect_length(res$sim3, 2)
#    expect_length(res$sim3$ValidityMessage, 1)
#    expect_equal(res$sim3$ValidityMessage, "VALID ANALYSIS")
#    expect_length(res$sim3$Levels, 2)
#    expect_equal(res$sim3$Levels, NA)
})

#
# Done
#

# context("ds.levels::disc::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.levels::disc::done")

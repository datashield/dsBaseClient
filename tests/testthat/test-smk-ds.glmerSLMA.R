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

context("ds.glmerSLMA::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.glmerSLMA::smk")
test_that("simple glmerSLMA", {
    res <- ds.glmerSLMA('D$LAB_TSC~D$LAB_TRIG', family="binomial")

    expect_length(res, 0)
})

#
# Done
#

context("ds.glmerSLMA::smk::shutdown")

test_that("setup", {
    ds_expect_variables(c("D", "LAB_TRIG", "LAB_TSC", "offset", "weights"))
})

disconnect.studies.dataset.cnsim()

context("ds.glmerSLMA::smk::done")

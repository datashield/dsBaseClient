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

connect.studies.dataset.cluster.int(list("incid_rate", "trtGrp", "Male", "idDoctor"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.glmerSLMA::smk")
test_that("simple glmerSLMA", {
    res <- ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', family="poisson", dataName = "D")

    expect_length(res, 8)
})

#
# Done
#

context("ds.glmerSLMA::smk::shutdown")

test_that("setup", {
  #note the offset and weights objects below are artefacts 
    ds_expect_variables(c("D", "offset", "weights"))
})

disconnect.studies.dataset.cluster.int()

context("ds.glmerSLMA::smk::done")

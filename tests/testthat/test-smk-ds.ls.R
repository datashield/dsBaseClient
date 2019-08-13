#-------------------------------------------------------------------------------
# Copyright (c) 2019 University of Newcastle upon Tyne. All rights reserved.
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

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

#
# Tests
#

context("ds.ls::smk")
test_that("simple ls", {
    res <- ds.ls()

    expect_length(res, 3)
    expect_length(res$sim1, 1)
    expect_equal(res$sim1[1], "D")
    expect_length(res$sim2, 1)
    expect_equal(res$sim2[1], "D")
    expect_length(res$sim3, 1)
    expect_equal(res$sim3[1], "D")
})

#
# Done
#

disconnect.studies.dataset.cnsim()
